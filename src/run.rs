use std::{
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    fs,
    hash::{Hash, Hasher},
    mem::take,
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use instant::Duration;
use parking_lot::Mutex;

use crate::{
    ast::*,
    check::instrs_signature,
    function::*,
    lex::{CodeSpan, Sp, Span},
    parse::parse,
    primitive::{Primitive, CONSTANTS},
    value::Value,
    Diagnostic, DiagnosticKind, Handle, Ident, NativeSys, SysBackend, SysOp, TraceFrame, UiuaError,
    UiuaResult,
};

/// The Uiua runtime
#[derive(Clone)]
pub struct Uiua {
    /// Functions which are under construction
    new_functions: Vec<Vec<Instr>>,
    /// Global values
    globals: Arc<Mutex<Vec<Value>>>,
    /// Indexable spans
    spans: Arc<Mutex<Vec<Span>>>,
    /// The thread's stack
    stack: Vec<Value>,
    /// The thread's temp stack
    temp_stack: Vec<Value>,
    /// The current scope
    scope: Scope,
    /// Ancestor scopes of the current one
    higher_scopes: Vec<Scope>,
    /// Determines which How test scopes are run
    mode: RunMode,
    /// A limit on the execution duration in milliseconds
    execution_limit: Option<f64>,
    /// The time at which execution started
    execution_start: f64,
    /// The paths of files currently being imported (used to detect import cycles)
    current_imports: Arc<Mutex<HashSet<PathBuf>>>,
    /// The stacks of imported files
    imports: Arc<Mutex<HashMap<PathBuf, Vec<Value>>>>,
    /// Accumulated diagnostics
    diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    print_diagnostics: bool,
    /// Arguments passed from the command line
    cli_arguments: Vec<String>,
    /// File that was passed to the interpreter for execution
    cli_file_path: PathBuf,
    /// The system backend
    pub(crate) backend: Arc<dyn SysBackend>,
}

#[derive(Clone)]
pub struct Scope {
    /// The stack height at the start of each array currently being built
    array: Vec<usize>,
    /// The call stack
    call: Vec<StackFrame>,
    /// Map local names to global indices
    names: HashMap<Ident, usize>,
    /// Whether this scope is local
    local: bool,
    /// The current fill values
    fills: Fills,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            array: Vec::new(),
            call: vec![StackFrame {
                function: Arc::new(Function::new(
                    FunctionId::Main,
                    Vec::new(),
                    FunctionKind::Normal,
                    Signature::new(0, 0),
                )),
                call_span: 0,
                pc: 0,
                spans: Vec::new(),
            }],
            names: HashMap::new(),
            local: false,
            fills: Fills::default(),
        }
    }
}

#[derive(Default, Clone)]
struct Fills {
    nums: Vec<f64>,
    chars: Vec<char>,
    functions: Vec<Arc<Function>>,
}

#[derive(Clone)]
struct StackFrame {
    /// The function being executed
    function: Arc<Function>,
    /// The span at which the function was called
    call_span: usize,
    /// The program counter for the function
    pc: usize,
    /// Additional spans for error reporting
    spans: Vec<(usize, Option<Primitive>)>,
}

impl Default for Uiua {
    fn default() -> Self {
        Self::with_native_sys()
    }
}

/// A mode that affects how non-binding lines are run
///
/// Regardless of the mode, lines with a call to `import` will always be run
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum RunMode {
    /// Only run lines outside of test blocks
    #[default]
    Normal,
    /// Only run non-binding lines inside of test blocks
    Test,
    /// Run everything
    All,
}

impl FromStr for RunMode {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "normal" => Ok(RunMode::Normal),
            "test" => Ok(RunMode::Test),
            "all" => Ok(RunMode::All),
            _ => Err(format!("unknown run mode `{}`", s)),
        }
    }
}

impl Uiua {
    /// Create a new Uiua runtime with the standard IO backend
    pub fn with_native_sys() -> Self {
        let mut scope = Scope::default();
        let mut globals = Vec::new();
        for def in &*CONSTANTS {
            scope.names.insert(def.name.into(), globals.len());
            globals.push(def.value.clone());
        }
        Uiua {
            spans: Arc::new(Mutex::new(vec![Span::Builtin])),
            stack: Vec::new(),
            temp_stack: Vec::new(),
            scope,
            higher_scopes: Vec::new(),
            globals: Arc::new(Mutex::new(globals)),
            new_functions: Vec::new(),
            current_imports: Arc::new(Mutex::new(HashSet::new())),
            imports: Arc::new(Mutex::new(HashMap::new())),
            mode: RunMode::Normal,
            diagnostics: BTreeSet::new(),
            backend: Arc::new(NativeSys),
            print_diagnostics: false,
            cli_arguments: Vec::new(),
            cli_file_path: PathBuf::new(),
            execution_limit: None,
            execution_start: 0.0,
        }
    }
    /// Create a new Uiua runtime with a custom IO backend
    pub fn with_backend(backend: impl SysBackend) -> Self {
        Uiua {
            backend: Arc::new(backend),
            ..Default::default()
        }
    }
    pub fn backend(&self) -> &dyn SysBackend {
        &*self.backend
    }
    pub fn downcast_backend<T: SysBackend>(&self) -> Option<&T> {
        self.backend.any().downcast_ref()
    }
    pub fn print_diagnostics(mut self, print_diagnostics: bool) -> Self {
        self.print_diagnostics = print_diagnostics;
        self
    }
    /// Limit the execution duration
    pub fn with_execution_limit(mut self, limit: Duration) -> Self {
        self.execution_limit = Some(limit.as_millis() as f64);
        self
    }
    /// Set the [`RunMode`]
    ///
    /// Default is [`RunMode::Normal`]
    pub fn with_mode(mut self, mode: RunMode) -> Self {
        self.mode = mode;
        self
    }
    /// Get the [`RunMode`]
    pub fn mode(&self) -> RunMode {
        self.mode
    }
    /// Set the command line arguments
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.cli_arguments = args;
        self
    }
    /// Get the command line arguments
    pub fn args(&self) -> &[String] {
        self.cli_arguments.as_slice()
    }
    /// Set the path of the file that is being executed
    pub fn with_file_path(mut self, file_path: impl Into<PathBuf>) -> Self {
        self.cli_file_path = file_path.into();
        self
    }
    /// Get the path of the file that is being executed
    pub fn file_path(&self) -> &Path {
        self.cli_file_path.as_path()
    }
    /// Load a Uiua file from a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e.into()))?;
        self.load_impl(&input, Some(path))
    }
    /// Load a Uiua file from a string
    pub fn load_str(&mut self, input: &str) -> UiuaResult {
        self.load_impl(input, None)
    }
    /// Load a Uiua file from a string with a path for error reporting
    pub fn load_str_path<P: AsRef<Path>>(&mut self, input: &str, path: P) -> UiuaResult {
        self.load_impl(input, Some(path.as_ref()))
    }
    /// Run in a scoped context. Names defined in this context will be removed when the scope ends.
    ///
    /// While names defined in this context will be removed when the scope ends, values *bound* to
    /// those names will not.
    ///
    /// All other runtime state other than the stack, will also be restored.
    pub fn in_scope<T>(
        &mut self,
        local: bool,
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<Vec<Value>> {
        self.higher_scopes.push(take(&mut self.scope));
        self.scope.local = local;
        let start_height = self.stack.len();
        f(self)?;
        let end_height = self.stack.len();
        self.scope = self.higher_scopes.pop().unwrap();
        Ok(self.stack.split_off(start_height.min(end_height)))
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult {
        self.execution_start = instant::now();
        let (items, errors) = parse(input, path);
        if !errors.is_empty() {
            return Err(errors.into());
        }
        if let Some(path) = path {
            self.current_imports.lock().insert(path.into());
        }
        let res = match catch_unwind(AssertUnwindSafe(|| self.items(items, false))) {
            Ok(res) => res,
            Err(_) => Err(self.error(format!(
                "\
The interpreter has crashed!
Hooray! You found a bug!
Please report this at http://github.com/uiua-lang/uiua/issues/new

code:
{}
{}",
                self.span(),
                input
            ))),
        };
        if let Some(path) = path {
            self.current_imports.lock().remove(path);
        }
        res
    }
    fn trace_error(&self, mut error: UiuaError, frame: StackFrame) -> UiuaError {
        let mut frames = Vec::new();
        for (span, prim) in &frame.spans {
            if let Some(prim) = prim {
                frames.push(TraceFrame {
                    id: FunctionId::Primitive(*prim),
                    span: self.spans.lock()[*span].clone(),
                });
            }
        }
        frames.push(TraceFrame {
            id: frame.function.id.clone(),
            span: self.spans.lock()[frame.call_span].clone(),
        });
        if let UiuaError::Traced { trace, .. } = &mut error {
            trace.extend(frames);
            error
        } else {
            UiuaError::Traced {
                error: error.into(),
                trace: frames,
            }
        }
    }
    pub(crate) fn import(&mut self, input: &str, path: &Path) -> UiuaResult {
        if self.current_imports.lock().contains(path) {
            return Err(self.error(format!(
                "Cycle detected importing {}",
                path.to_string_lossy()
            )));
        }
        if !self.imports.lock().contains_key(path) {
            let import = self.in_scope(false, |env| env.load_str_path(input, path).map(drop))?;
            self.imports.lock().insert(path.into(), import);
        }
        self.stack.extend(self.imports.lock()[path].iter().cloned());
        Ok(())
    }
    fn items(&mut self, items: Vec<Item>, in_test: bool) -> UiuaResult {
        for item in items {
            self.item(item, in_test)?;
        }
        Ok(())
    }
    fn item(&mut self, item: Item, in_test: bool) -> UiuaResult {
        fn words_have_import(words: &[Sp<Word>]) -> bool {
            words
                .iter()
                .any(|w| matches!(w.value, Word::Primitive(Primitive::Sys(SysOp::Import))))
        }
        match item {
            Item::Scoped { items, test } => {
                let scope_stack = self.in_scope(true, |env| env.items(items, test))?;
                self.stack.extend(scope_stack);
            }
            Item::Words(words) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::Test => in_test,
                    RunMode::All => true,
                };
                if can_run || words_have_import(&words) {
                    let instrs = self.compile_words(words, true)?;
                    self.exec_global_instrs(instrs)?;
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::All | RunMode::Test => true,
                };
                if can_run || words_have_import(&binding.words) {
                    self.binding(binding)?;
                }
            }
            Item::ExtraNewlines(_) => {}
        }
        Ok(())
    }
    fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let mut spans = self.spans.lock();
        let idx = spans.len();
        spans.push(span.into());
        idx
    }
    fn binding(&mut self, binding: Binding) -> UiuaResult {
        let instrs = self.compile_words(binding.words, true)?;
        let make_fn = |instrs: Vec<Instr>, sig: Signature| {
            let func = Function::new(
                FunctionId::Named(binding.name.value.clone()),
                instrs,
                FunctionKind::Normal,
                sig,
            );
            Value::from(func)
        };
        let mut val = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &binding.signature {
                    if declared_sig.value.is_superset_of(sig) {
                        sig = declared_sig.value;
                    } else {
                        return Err(UiuaError::Run(Span::Code(declared_sig.span.clone()).sp(
                            format!(
                                "Function signature mismatch: \
                                 declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        )));
                    }
                }

                if sig.args == 0 {
                    self.exec_global_instrs(instrs)?;
                    if let Some(value) = self.stack.pop() {
                        match value {
                            Value::Func(fs) => match fs.into_scalar() {
                                Ok(mut f) => {
                                    Arc::make_mut(&mut f).id =
                                        FunctionId::Named(binding.name.value.clone());
                                    f.into()
                                }
                                Err(fs) => fs.into(),
                            },
                            val => val,
                        }
                    } else {
                        Function::new(
                            FunctionId::Named(binding.name.value.clone()),
                            Vec::new(),
                            FunctionKind::Normal,
                            sig,
                        )
                        .into()
                    }
                } else {
                    make_fn(instrs, sig)
                }
            }
            Err(e) => {
                if let Some(sig) = binding.signature {
                    make_fn(instrs, sig.value)
                } else {
                    return Err(UiuaError::Run(
                        Span::Code(binding.name.span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        };
        val.compress();
        let mut globals = self.globals.lock();
        let idx = globals.len();
        globals.push(val);
        self.scope.names.insert(binding.name.value, idx);
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult<Vec<Instr>> {
        self.new_functions.push(Vec::new());
        self.words(words, call)?;
        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.show(true));
            }
        }
        let instrs = self.new_functions.pop().unwrap();
        Ok(instrs)
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word, call)?;
        }
        Ok(())
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr) {
        use Primitive::*;
        let instrs = self.new_functions.last_mut().unwrap();
        // Optimizations
        match (instrs.as_mut_slice(), instr) {
            // Ignore noops
            (_, Instr::Prim(Noop, _)) => {}
            // Cancel out inverses
            ([.., Instr::Prim(a, _)], Instr::Prim(b, _)) if a.inverse() == Some(b) => {
                instrs.pop();
            }
            // Cosine
            ([.., Instr::Prim(Eta, _), Instr::Prim(Add, _)], Instr::Prim(Sin, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr::Prim(Cos, span));
            }
            // First reverse = last
            ([.., Instr::Prim(top @ Reverse, _)], Instr::Prim(First, _)) => *top = Last,
            (_, instr) => instrs.push(instr),
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                self.push_instr(Instr::push(n));
            }
            Word::Char(c) => self.push_instr(Instr::push(c)),
            Word::String(s) => self.push_instr(Instr::push(s)),
            Word::FormatString(frags) => {
                let signature = Signature::new(frags.len() - 1, 1);
                let f = Function::new(
                    FunctionId::Anonymous(word.span.clone()),
                    Vec::new(),
                    FunctionKind::Dynamic(DynamicFunctionKind {
                        id: {
                            let mut hasher = DefaultHasher::new();
                            frags.hash(&mut hasher);
                            hasher.finish()
                        },
                        f: Arc::new(move |env| {
                            let mut formatted = String::new();
                            for (i, frag) in frags.iter().enumerate() {
                                if i > 0 {
                                    let val = env.pop(format!("format argument {i}"))?;
                                    formatted.push_str(&format!("{}", val));
                                }
                                formatted.push_str(frag);
                            }
                            env.push(formatted);
                            Ok(())
                        }),
                    }),
                    signature,
                );
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::MultilineString(lines) => {
                let signature = Signature::new(lines.iter().map(|l| l.value.len() - 1).sum(), 1);
                let f = Function::new(
                    FunctionId::Anonymous(word.span.clone()),
                    Vec::new(),
                    FunctionKind::Dynamic(DynamicFunctionKind {
                        id: {
                            let mut hasher = DefaultHasher::new();
                            lines.hash(&mut hasher);
                            hasher.finish()
                        },
                        f: Arc::new(move |env| {
                            let mut formatted = String::new();
                            let mut i = 0;
                            for (j, line) in lines.iter().enumerate() {
                                if j > 0 {
                                    formatted.push_str("\r\n");
                                }
                                for (k, frag) in line.value.iter().enumerate() {
                                    if k > 0 {
                                        let val = env.pop(format!("format argument {i}"))?;
                                        formatted.push_str(&format!("{}", val));
                                    }
                                    formatted.push_str(frag);
                                    i += 1;
                                }
                            }
                            env.push(formatted);
                            Ok(())
                        }),
                    }),
                    signature,
                );
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                let inner = self.compile_words(items, false)?;
                let span = self.add_span(word.span);
                let instrs = self.new_functions.last_mut().unwrap();
                if inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    // Inline constant arrays
                    instrs.pop();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => *v,
                        _ => unreachable!(),
                    });
                    self.push_span(span, None);
                    let val = Value::from_row_values(values, self)?;
                    self.pop_span();
                    self.push_instr(Instr::push(val));
                } else {
                    // Normal case
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray {
                        span,
                        constant: false,
                    });
                }
            }
            Word::Array(arr) => {
                if !call {
                    self.new_functions.push(Vec::new());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                for lines in arr.lines.into_iter().rev() {
                    inner.extend(self.compile_words(lines, true)?);
                }
                let span = self.add_span(word.span.clone());
                let instrs = self.new_functions.last_mut().unwrap();
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    instrs.pop();
                    let values = inner.into_iter().rev().map(|instr| match instr {
                        Instr::Push(v) => *v,
                        _ => unreachable!(),
                    });
                    self.push_span(span, None);
                    let val = if arr.constant {
                        Value::from_row_values(values.map(Function::constant), self)
                    } else {
                        Value::from_row_values(values, self)
                    }?;
                    self.pop_span();
                    self.push_instr(Instr::push(val));
                } else {
                    instrs.extend(inner);
                    self.push_instr(Instr::EndArray {
                        span,
                        constant: arr.constant,
                    });
                    if !call {
                        let instrs = self.new_functions.pop().unwrap();
                        let sig =
                            instrs_signature(&instrs).unwrap_or_else(|_| Signature::new(0, 0));
                        let func = Function::new(
                            FunctionId::Anonymous(word.span),
                            instrs,
                            FunctionKind::Normal,
                            sig,
                        );
                        self.push_instr(Instr::push(func));
                    }
                }
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Primitive(p) => self.primitive(p, word.span, call)?,
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Spaces | Word::Comment(_) => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(idx) = self.scope.names.get(&ident).or_else(|| {
            self.higher_scopes
                .last()
                .filter(|_| self.scope.local)?
                .names
                .get(&ident)
        }) {
            // Name exists in scope
            let value = self.globals.lock()[*idx].clone();
            let should_call = matches!(&value, Value::Func(f) if f.shape.is_empty());
            self.push_instr(Instr::push(value));
            if should_call && call {
                let span = self.add_span(span);
                self.push_instr(Instr::Call(span));
            }
        } else if let Some(prim) =
            Primitive::all().find(|p| p.names().is_some_and(|n| n.text == ident.as_ref()))
        {
            return Err(span
                .sp(format!(
                    "Unknown identifier `{ident}`, did you mean `{prim}`?"
                ))
                .into());
        } else {
            return Err(span.sp(format!("Unknown identifier `{ident}`")).into());
        }
        Ok(())
    }
    fn func(&mut self, func: Func, span: CodeSpan) -> UiuaResult {
        let mut instrs = Vec::new();
        for line in func.lines {
            instrs.extend(self.compile_words(line, true)?);
        }

        // Validate signature
        let sig = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &func.signature {
                    if declared_sig.value.is_superset_of(sig) {
                        sig = declared_sig.value;
                    } else {
                        return Err(UiuaError::Run(Span::Code(declared_sig.span.clone()).sp(
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        )));
                    }
                }
                sig
            }
            Err(e) => {
                if let Some(declared_sig) = &func.signature {
                    declared_sig.value
                } else {
                    return Err(UiuaError::Run(
                        Span::Code(span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        };

        // De-nest function calls
        if let [Instr::Push(val), Instr::Call(_)] = instrs.as_slice() {
            if let Some(f) = val.as_function() {
                self.push_instr(Instr::push(f.clone()));
                return Ok(());
            }
        }

        let function = Function::new(func.id, instrs, FunctionKind::Normal, sig);
        self.push_instr(Instr::push(function));
        Ok(())
    }
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        // Give advice about redundant each
        match modified.modifier.value {
            Primitive::Each => {
                if let [word] = modified.operands.as_slice() {
                    if let Word::Primitive(prim) = &word.value {
                        if prim.class().is_pervasive() {
                            let span = modified.modifier.span.clone().merge(word.span.clone());
                            self.diagnostics.insert(Diagnostic::new(
                                format!(
                                    "Using each with a pervasive primitive like {prim} is \
                                    redundant. Just use {prim} by itself."
                                ),
                                span,
                                DiagnosticKind::Advice,
                            ));
                        }
                    }
                }
            }
            _ => {}
        }

        if call {
            self.words(modified.operands, false)?;
            let span = self.add_span(modified.modifier.span);
            self.push_instr(Instr::Prim(modified.modifier.value, span));
        } else {
            self.new_functions.push(Vec::new());
            self.words(modified.operands, false)?;
            self.primitive(
                modified.modifier.value,
                modified.modifier.span.clone(),
                true,
            )?;
            let instrs = self.new_functions.pop().unwrap();
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = Function::new(
                        FunctionId::Anonymous(modified.modifier.span),
                        instrs,
                        FunctionKind::Normal,
                        sig,
                    );
                    self.push_instr(Instr::push(func));
                }
                Err(e) => {
                    return Err(UiuaError::Run(
                        Span::Code(modified.modifier.span.clone())
                            .sp(format!("Cannot infer function signature: {e}")),
                    ));
                }
            }
        }
        Ok(())
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(suggestion) = prim.deprecation_suggestion() {
            let suggestion = if suggestion.is_empty() {
                String::new()
            } else {
                format!(", {suggestion}")
            };
            self.diagnostics.insert(Diagnostic::new(
                format!(
                    "Warning: {}{} is deprecated and will be removed in a future version{}",
                    prim.name().unwrap_or_default(),
                    prim,
                    suggestion
                ),
                span.clone(),
                DiagnosticKind::Warning,
            ));
        }
        let span = self.add_span(span);
        if call || prim.as_constant().is_some() {
            self.push_instr(Instr::Prim(prim, span));
        } else {
            let instrs = [Instr::Prim(prim, span)];
            let func =
                Function::new_inferred(FunctionId::Primitive(prim), instrs, FunctionKind::Normal);
            match func {
                Ok(func) => self.push_instr(Instr::push(func)),
                Err(e) => {
                    return Err(self.error(format!(
                        "{prim} cannot be used here because it does \
                        not have a well-defined signature: {e}"
                    )))
                }
            }
        }
        Ok(())
    }
    fn exec_global_instrs(&mut self, instrs: Vec<Instr>) -> UiuaResult {
        let func = Function::new(
            FunctionId::Main,
            instrs,
            FunctionKind::Normal,
            Signature::new(0, 0),
        );
        self.exec(StackFrame {
            function: Arc::new(func),
            call_span: 0,
            spans: Vec::new(),
            pc: 0,
        })
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        let ret_height = self.scope.call.len();
        self.scope.call.push(frame);
        while self.scope.call.len() > ret_height {
            let frame = self.scope.call.last().unwrap();
            let Some(instr) = frame.function.instrs.get(frame.pc) else {
                self.scope.call.pop();
                continue;
            };
            // for val in &self.stack {
            //     print!("{:?} ", val);
            // }
            // println!();
            // println!("  {:?}", instr);
            let res = match instr {
                Instr::Push(val) => {
                    self.stack.push(Value::clone(val));
                    Ok(())
                }
                Instr::BeginArray => {
                    self.scope.array.push(self.stack.len());
                    Ok(())
                }
                &Instr::EndArray { span, constant } => (|| {
                    let start = self.scope.array.pop().unwrap();
                    self.push_span(span, None);
                    let values = self.stack.drain(start..).rev();
                    let values: Vec<Value> = if constant {
                        values
                            .map(Function::constant)
                            .map(Arc::new)
                            .map(Value::from)
                            .collect()
                    } else {
                        values.collect()
                    };
                    let val = Value::from_row_values(values, self)?;
                    self.pop_span();
                    self.push(val);
                    Ok(())
                })(),
                &Instr::Prim(prim, span) => (|| {
                    self.push_span(span, Some(prim));
                    prim.run(self)?;
                    self.pop_span();
                    Ok(())
                })(),
                &Instr::Call(span) => self
                    .pop("called function")
                    .and_then(|f| self.call_with_span(f, span)),
            };
            if let Err(mut err) = res {
                // Trace errors
                let frames = self
                    .scope
                    .call
                    .split_off(ret_height.min(self.scope.call.len()));
                for frame in frames {
                    err = self.trace_error(err, frame);
                }
                return Err(err);
            } else {
                // Go to next instruction
                self.scope.call.last_mut().unwrap().pc += 1;
                if let Some(limit) = self.execution_limit {
                    if instant::now() - self.execution_start > limit {
                        return Err(UiuaError::Timeout(self.span()));
                    }
                }
            }
        }
        Ok(())
    }
    fn push_span(&mut self, span: usize, prim: Option<Primitive>) {
        self.scope.call.last_mut().unwrap().spans.push((span, prim));
    }
    fn pop_span(&mut self) {
        self.scope.call.last_mut().unwrap().spans.pop();
    }
    fn call_with_span(&mut self, mut f: Value, call_span: usize) -> UiuaResult {
        let mut first_pass = true;
        loop {
            match f {
                Value::Func(f) if f.shape.is_empty() => {
                    // Call function
                    let f = f.into_scalar().unwrap();
                    match &f.kind {
                        FunctionKind::Normal => {}
                        FunctionKind::Dynamic(dfk) => {
                            self.scope.call.push(StackFrame {
                                function: f.clone(),
                                call_span,
                                spans: Vec::new(),
                                pc: 0,
                            });
                            (dfk.f)(self)?;
                            self.scope.call.pop();
                            break Ok(());
                        }
                    }
                    break self.exec(StackFrame {
                        function: f,
                        call_span,
                        spans: Vec::new(),
                        pc: 0,
                    });
                }
                Value::Func(_) if first_pass => {
                    // Call non-scalar function array
                    let index = self.pop("index")?;
                    f = index.pick(f, self)?;
                    first_pass = false;
                }
                value => {
                    self.push(value);
                    break Ok(());
                }
            }
        }
    }
    /// Call the top of the stack as a function
    #[inline]
    pub fn call(&mut self, f: impl Into<Value>) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(f.into(), call_span)
    }
    #[inline]
    pub fn recur(&mut self, n: usize) -> UiuaResult {
        if n == 0 {
            return Ok(());
        }
        if n > self.scope.call.len() {
            return Err(self.error(format!(
                "Cannot recur {} levels up, only {} levels down",
                n,
                self.scope.call.len()
            )));
        }
        let f = self.scope.call[self.scope.call.len() - n].function.clone();
        self.call(f)
    }
    pub fn call_catch_break(&mut self, f: Value) -> UiuaResult<bool> {
        match self.call(f) {
            Ok(_) => Ok(false),
            Err(e) => match e.break_data() {
                Ok((0, _)) => Ok(true),
                Ok((n, span)) => Err(UiuaError::Break(n - 1, span)),
                Err(e) => Err(e),
            },
        }
    }
    pub fn call_error_on_break(&mut self, f: Value, message: &str) -> UiuaResult {
        self.call_error_on_break_with(f, || message.into())
    }
    pub fn call_error_on_break_with(
        &mut self,
        f: Value,
        message: impl FnOnce() -> String,
    ) -> UiuaResult {
        match self.call(f) {
            Ok(_) => Ok(()),
            Err(e) => match e.break_data() {
                Ok((0, span)) => Err(span.sp(message()).into()),
                Ok((n, span)) => Err(UiuaError::Break(n - 1, span)),
                Err(e) => Err(e),
            },
        }
    }
    pub(crate) fn span_index(&self) -> usize {
        self.scope.call.last().map_or(0, |frame| {
            frame
                .spans
                .last()
                .map(|(i, _)| *i)
                .unwrap_or(frame.call_span)
        })
    }
    /// Get the span of the current function call
    pub fn span(&self) -> Span {
        self.spans.lock()[self.span_index()].clone()
    }
    /// Construct an error with the current span
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaError::Run(self.span().clone().sp(message.to_string()))
    }
    pub fn diagnostic(&mut self, message: impl Into<String>, kind: DiagnosticKind) {
        self.diagnostics
            .insert(Diagnostic::new(message.into(), self.span(), kind));
    }
    /// Pop a value from the stack
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        let res = self.stack.pop().ok_or_else(|| {
            self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))
        });
        if let Some(bottom) = self.scope.array.last_mut() {
            *bottom = (*bottom).min(self.stack.len());
        }
        res
    }
    /// Push a value onto the stack
    pub fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }
    pub(crate) fn push_temp(&mut self) -> UiuaResult {
        let value = self.pop("value to move to temp")?;
        self.temp_stack.push(value);
        Ok(())
    }
    pub(crate) fn pop_temp(&mut self) -> UiuaResult {
        let value = self
            .temp_stack
            .pop()
            .ok_or_else(|| self.error("Temp stack was empty when evaluating value to pop"))?;
        self.push(value);
        Ok(())
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        take(&mut self.stack)
    }
    pub fn diagnostics(&self) -> &BTreeSet<Diagnostic> {
        &self.diagnostics
    }
    pub fn take_diagnostics(&mut self) -> BTreeSet<Diagnostic> {
        take(&mut self.diagnostics)
    }
    pub fn clone_stack_top(&mut self, n: usize) -> Vec<Value> {
        self.stack.iter().rev().take(n).rev().cloned().collect()
    }
    pub(crate) fn monadic_ref<V: Into<Value>>(&mut self, f: fn(&Value) -> V) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value));
        Ok(())
    }
    pub(crate) fn monadic_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(value, self)?);
        Ok(())
    }
    pub(crate) fn monadic_ref_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(&value, self)?);
        Ok(())
    }
    pub(crate) fn monadic_mut(&mut self, f: fn(&mut Value)) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a);
        self.push(a);
        Ok(())
    }
    pub(crate) fn dyadic_rr<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b));
        Ok(())
    }
    pub(crate) fn dyadic_oo_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(a, b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_rr_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_ro_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, b, self)?);
        Ok(())
    }
    pub(crate) fn stack_size(&self) -> usize {
        self.stack.len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) {
        self.stack.truncate(size);
    }
    pub(crate) fn num_fill(&self) -> Option<f64> {
        self.scope.fills.nums.last().copied()
    }
    pub(crate) fn byte_fill(&self) -> Option<u8> {
        let n = self.scope.fills.nums.last().copied()?;
        (n.fract() == 0.0 && (0.0..=255.0).contains(&n)).then_some(n as u8)
    }
    pub(crate) fn char_fill(&self) -> Option<char> {
        self.scope.fills.chars.last().copied()
    }
    pub(crate) fn func_fill(&self) -> Option<Arc<Function>> {
        self.scope.fills.functions.last().cloned()
    }
    /// Do something with the fill context set
    pub(crate) fn with_fill(
        &mut self,
        fill: Value,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult,
    ) -> UiuaResult {
        let mut set = false;
        match &fill {
            Value::Num(n) => {
                if let Some(&n) = n.as_scalar() {
                    self.scope.fills.nums.push(n);
                    set = true;
                }
            }
            Value::Byte(b) => {
                if let Some(&b) = b.as_scalar() {
                    self.scope.fills.nums.push(b as f64);
                    set = true;
                }
            }
            Value::Char(c) => {
                if let Some(&c) = c.as_scalar() {
                    self.scope.fills.chars.push(c);
                    set = true;
                }
            }
            Value::Func(f) => {
                if let Some(f) = f.as_scalar() {
                    self.scope.fills.functions.push(f.clone());
                    set = true;
                }
            }
        }
        if !set {
            return Err(self.error(format!(
                "Fill values must be scalar, but its shape is {}",
                fill.format_shape()
            )));
        }
        let res = in_ctx(self);
        match fill {
            Value::Num(_) | Value::Byte(_) => {
                self.scope.fills.nums.pop();
            }
            Value::Char(_) => {
                self.scope.fills.chars.pop();
            }
            Value::Func(_) => {
                self.scope.fills.functions.pop();
            }
        }
        res
    }
    /// Spawn a thread
    pub(crate) fn spawn(
        &mut self,
        capture_count: usize,
        f: impl FnOnce(&mut Self) -> UiuaResult + Send + 'static,
    ) -> UiuaResult<Value> {
        if self.stack.len() < capture_count {
            return Err(self.error(format!(
                "Excepted at least {} value(s) on the stack, but there are {}",
                capture_count,
                self.stack.len()
            )))?;
        }
        let env = Uiua {
            new_functions: Vec::new(),
            globals: self.globals.clone(),
            spans: self.spans.clone(),
            stack: self
                .stack
                .drain(self.stack.len() - capture_count..)
                .collect(),
            temp_stack: Vec::new(),
            scope: self.scope.clone(),
            higher_scopes: self.higher_scopes.last().cloned().into_iter().collect(),
            mode: self.mode,
            current_imports: self.current_imports.clone(),
            imports: self.imports.clone(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: self.print_diagnostics,
            cli_arguments: self.cli_arguments.clone(),
            cli_file_path: self.cli_file_path.clone(),
            backend: self.backend.clone(),
            execution_limit: self.execution_limit,
            execution_start: self.execution_start,
        };
        self.backend
            .spawn(env, Box::new(f))
            .map(Value::from)
            .map_err(|e| self.error(e))
    }
    /// Wait for a thread to finish
    pub(crate) fn wait(&mut self, handle: Value) -> UiuaResult {
        let handles = handle.as_number_array(
            self,
            "Handle must be an array of natural numbers",
            |_| true,
            |n| n.fract() == 0.0 && n >= 0.0,
            |n| Handle(n as u64),
        )?;
        if handles.shape.is_empty() {
            let handle = handles.data.into_iter().next().unwrap();
            let thread_stack = self
                .backend
                .wait(handle)
                .map_err(|e| e.unwrap_or_else(|e| self.error(e)))?;
            self.stack.extend(thread_stack);
        } else {
            let mut rows = Vec::new();
            for handle in handles.data {
                let thread_stack = self
                    .backend
                    .wait(handle)
                    .map_err(|e| e.unwrap_or_else(|e| self.error(e)))?;
                let row = if thread_stack.len() == 1 {
                    thread_stack.into_iter().next().unwrap()
                } else {
                    Value::from_row_values(thread_stack, self)?
                };
                rows.push(row);
            }
            self.push(Value::from_row_values(rows, self)?);
        }
        Ok(())
    }
}

/// A trait for types that can be used as argument specifiers for [`Uiua::pop`] and [`Uiua::antipop`]
///
/// If the stack is empty, the error message will be "Stack was empty when evaluating {arg_name}"
pub trait StackArg {
    fn arg_name(self) -> String;
}

impl StackArg for usize {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl StackArg for u8 {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl StackArg for i32 {
    fn arg_name(self) -> String {
        format!("argument {self}")
    }
}
impl<'a> StackArg for &'a str {
    fn arg_name(self) -> String {
        self.to_string()
    }
}

impl StackArg for String {
    fn arg_name(self) -> String {
        self
    }
}

impl<F, T> StackArg for F
where
    F: FnOnce() -> T,
    T: StackArg,
{
    fn arg_name(self) -> String {
        self().arg_name()
    }
}

pub struct FunctionArg<T>(pub T);
pub struct ArrayArg<T>(pub T);
pub struct FunctionNArg<T>(pub usize, pub T);

impl<T: StackArg> StackArg for FunctionArg<T> {
    fn arg_name(self) -> String {
        format!("function {}", self.0.arg_name())
    }
}

impl<T> StackArg for ArrayArg<T>
where
    T: StackArg,
{
    fn arg_name(self) -> String {
        format!("array {}", self.0.arg_name())
    }
}

impl<T> StackArg for FunctionNArg<T>
where
    T: StackArg,
{
    fn arg_name(self) -> String {
        format!("function {}'s {}", self.0, self.1.arg_name())
    }
}
