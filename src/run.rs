use std::{
    collections::{HashMap, HashSet},
    fs,
    mem::take,
    path::{Path, PathBuf},
    sync::Arc,
};

use parking_lot::Mutex;

use crate::{
    ast::*,
    check::instrs_stack_delta,
    function::*,
    lex::{CodeSpan, Sp, Span},
    parse::parse,
    primitive::Primitive,
    value::Value,
    Handle, Ident, NativeSys, SysBackend, SysOp, TraceFrame, UiuaError, UiuaResult,
};

/// The Uiua runtime
#[derive(Clone)]
pub struct Uiua {
    // Compilation
    new_functions: Vec<Vec<Instr>>,
    new_dfns: Vec<Vec<u8>>,
    // Statics
    globals: Arc<Mutex<Vec<Value>>>,
    spans: Arc<Mutex<Vec<Span>>>,
    // Runtime
    stack: Vec<Value>,
    scope: Scope,
    lower_scopes: Vec<Scope>,
    mode: RunMode,
    // IO
    current_imports: Arc<Mutex<HashSet<PathBuf>>>,
    imports: Arc<Mutex<HashMap<PathBuf, Vec<Value>>>>,
    pub(crate) backend: Arc<dyn SysBackend>,
}

#[derive(Default, Clone)]
pub struct Scope {
    array: Vec<usize>,
    dfn: Vec<DfnFrame>,
    call: Vec<StackFrame>,
    names: HashMap<Ident, usize>,
    local: bool,
}

#[derive(Clone)]
struct StackFrame {
    function: Arc<Function>,
    call_span: usize,
    pc: usize,
    spans: Vec<(usize, Option<Primitive>)>,
    dfn: bool,
}

#[derive(Clone)]
struct DfnFrame {
    function: Arc<Function>,
    args: Vec<Value>,
}

impl Default for Uiua {
    fn default() -> Self {
        Self::with_native_sys()
    }
}

/// A mode that affects how non-binding lines are run
///
/// Regardless of the mode, lines with a call to `import` will always be run
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RunMode {
    /// Only run lines outside of test blocks
    Normal,
    /// Only run non-binding lines inside of test blocks
    Test,
    /// Run everything
    Watch,
}

impl Uiua {
    /// Create a new Uiua runtime with the standard IO backend
    pub fn with_native_sys() -> Self {
        Uiua {
            spans: Arc::new(Mutex::new(vec![Span::Builtin])),
            stack: Vec::new(),
            scope: Scope::default(),
            lower_scopes: Vec::new(),
            globals: Arc::new(Mutex::new(Vec::new())),
            new_functions: Vec::new(),
            new_dfns: Vec::new(),
            current_imports: Arc::new(Mutex::new(HashSet::new())),
            imports: Arc::new(Mutex::new(HashMap::new())),
            mode: RunMode::Normal,
            backend: Arc::new(NativeSys),
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
    /// Set the [`RunMode`]
    ///
    /// Default is [`RunMode::Normal`]
    pub fn mode(mut self, mode: RunMode) -> Self {
        self.mode = mode;
        self
    }
    /// Load a Uiua file from a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load_impl(&input, Some(path))
    }
    /// Load a Uiua file from a string
    pub fn load_str(&mut self, input: &str) -> UiuaResult<&mut Self> {
        self.load_impl(input, None)
    }
    /// Load a Uiua file from a string with a path for error reporting
    pub fn load_str_path<P: AsRef<Path>>(&mut self, input: &str, path: P) -> UiuaResult<&mut Self> {
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
        self.lower_scopes.push(take(&mut self.scope));
        self.scope.local = local;
        let start_height = self.stack.len();
        f(self)?;
        let end_height = self.stack.len();
        self.scope = self.lower_scopes.pop().unwrap();
        Ok(self.stack.split_off(start_height.min(end_height)))
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult<&mut Self> {
        let (items, errors) = parse(input, path);
        if !errors.is_empty() {
            return Err(errors.into());
        }
        if let Some(path) = path {
            self.current_imports.lock().insert(path.into());
        }
        let res = self.items(items, false);
        if let Some(path) = path {
            self.current_imports.lock().remove(path);
        }
        res.map(|_| self)
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
                "cycle detected importing {}",
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
                    RunMode::Watch => true,
                };
                if can_run || words_have_import(&words) {
                    let instrs = self.compile_words(words)?;
                    // println!("executing: {:?}", instrs);
                    self.exec_global_instrs(instrs)?;
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal | RunMode::Watch => true,
                    RunMode::Test => in_test,
                };
                if can_run || words_have_import(&binding.words) {
                    self.binding(binding)?;
                }
            }
            Item::Newlines => {}
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
        let instrs = self.compile_words(binding.words)?;
        let val = match instrs_stack_delta(&instrs) {
            Some((n, _)) if n <= self.stack.len() => {
                self.exec_global_instrs(instrs)?;
                self.stack.pop().unwrap_or_default()
            }
            _ => {
                let func = Function {
                    id: FunctionId::Named(binding.name.value.clone()),
                    instrs,
                    kind: FunctionKind::Normal,
                };
                Value::from(func)
            }
        };
        let mut globals = self.globals.lock();
        let idx = globals.len();
        globals.push(val);
        self.scope.names.insert(binding.name.value, idx);
        Ok(())
    }
    fn compile_words(&mut self, words: Vec<Sp<Word>>) -> UiuaResult<Vec<Instr>> {
        self.new_functions.push(Vec::new());
        self.words(words, true)?;
        let instrs = self.new_functions.pop().unwrap();
        Ok(instrs)
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        for word in words.into_iter().rev() {
            self.word(word, call)?;
        }
        Ok(())
    }
    fn push_instr(&mut self, instr: Instr) {
        use Primitive::*;
        let instrs = self.new_functions.last_mut().unwrap();
        // Optimizations
        match (instrs.as_mut_slice(), instr) {
            // Cosine
            ([.., Instr::Prim(Eta, _), Instr::Prim(Add, _)], Instr::Prim(Sin, span)) => {
                instrs.pop();
                instrs.pop();
                instrs.push(Instr::Prim(Cos, span));
            }
            // First reverse = last
            ([.., Instr::Prim(top, _)], Instr::Prim(new, new_span)) => match (&top, new) {
                (Reverse, First) => *top = Last,
                (Reverse, Last) => *top = First,
                _ => instrs.push(Instr::Prim(new, new_span)),
            },
            // Ignore noops
            (_, Instr::Prim(Noop, _)) => {}
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
                let f = Function {
                    id: FunctionId::Anonymous(word.span.clone()),
                    instrs: Vec::new(),
                    kind: FunctionKind::Dynamic {
                        inputs: frags.len() as u8 - 1,
                        delta: 1 - frags.len() as i8,
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
                    },
                };
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::MultilineString(lines) => {
                let f = Function {
                    id: FunctionId::Anonymous(word.span.clone()),
                    instrs: Vec::new(),
                    kind: FunctionKind::Dynamic {
                        inputs: lines.iter().map(|l| l.value.len() as u8 - 1).sum(),
                        delta: 1 - lines.len() as i8,
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
                    },
                };
                self.push_instr(Instr::push(f));
                if call {
                    let span = self.add_span(word.span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                self.push_instr(Instr::BeginArray);
                self.words(items, false)?;
                let span = self.add_span(word.span);
                self.push_instr(Instr::EndArray(span));
            }
            Word::Array(items) => {
                self.push_instr(Instr::BeginArray);
                for item in items {
                    self.words(item, true)?;
                }
                let span = self.add_span(word.span);
                self.push_instr(Instr::EndArray(span));
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Dfn(func) => self.dfn(func, word.span, call)?,
            Word::Primitive(p) => self.primitive(p, word.span, call),
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Spaces | Word::Comment(_) => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if let Some(idx) = self.scope.names.get(&ident).or_else(|| {
            self.lower_scopes
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
            Primitive::all().find(|p| p.names().is_some_and(|n| ident == n.text))
        {
            // Name is a non-formattable primitive
            let span = self.add_span(span);
            self.push_instr(Instr::Prim(prim, span));
        } else {
            if let Some(dfn) = self.new_dfns.last_mut() {
                if ident.as_str().len() == 1 {
                    let c = ident.as_str().chars().next().unwrap();
                    if c.is_ascii_lowercase() {
                        // Name is a dfn argument
                        let idx = c as u8 - b'a';
                        dfn.push(idx);
                        self.push_instr(Instr::DfnVal(idx as usize));
                        return Ok(());
                    }
                }
            }
            return Err(span.sp(format!("unknown identifier `{}`", ident)).into());
        }
        Ok(())
    }
    fn func(&mut self, func: Func, _span: CodeSpan) -> UiuaResult {
        let mut instrs = Vec::new();
        for line in func.body {
            instrs.extend(self.compile_words(line)?);
        }
        if let [Instr::Push(f), Instr::Call(..)] = instrs.as_slice() {
            if matches!(**f, Value::Func(_)) {
                self.push_instr(Instr::Push(f.clone()));
                return Ok(());
            }
        }
        let func = Function {
            id: func.id,
            instrs,
            kind: FunctionKind::Normal,
        };
        self.push_instr(Instr::push(func));
        Ok(())
    }
    fn dfn(&mut self, func: Func, span: CodeSpan, call: bool) -> UiuaResult {
        self.new_dfns.push(Vec::new());
        let mut instrs = Vec::new();
        for line in func.body {
            instrs.extend(self.compile_words(line)?);
        }
        let refs = self.new_dfns.pop().unwrap();
        let dfn_size = refs.into_iter().max().map(|n| n + 1).unwrap_or(0);
        let func = Function {
            id: func.id,
            instrs,
            kind: FunctionKind::Dfn(dfn_size),
        };
        self.push_instr(Instr::push(func));
        if call {
            let span = self.add_span(span);
            self.push_instr(Instr::Call(span));
        }
        Ok(())
    }
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        if call {
            self.words(modified.words, false)?;
            let span = self.add_span(modified.modifier.span);
            self.push_instr(Instr::Prim(modified.modifier.value, span));
        } else {
            self.new_functions.push(Vec::new());
            self.words(modified.words, false)?;
            self.primitive(
                modified.modifier.value,
                modified.modifier.span.clone(),
                true,
            );
            let instrs = self.new_functions.pop().unwrap();
            let func = Function {
                id: FunctionId::Anonymous(modified.modifier.span),
                instrs,
                kind: FunctionKind::Normal,
            };
            self.push_instr(Instr::push(func));
        }
        Ok(())
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) {
        let span = self.add_span(span);
        if call {
            self.push_instr(Instr::Prim(prim, span));
        } else {
            self.push_instr(Instr::push(Function {
                id: FunctionId::Primitive(prim),
                instrs: vec![Instr::Prim(prim, span)],
                kind: FunctionKind::Normal,
            }));
        }
    }
    fn exec_global_instrs(&mut self, instrs: Vec<Instr>) -> UiuaResult {
        let func = Function {
            id: FunctionId::Main,
            instrs,
            kind: FunctionKind::Normal,
        };
        self.exec(StackFrame {
            function: Arc::new(func),
            call_span: 0,
            spans: Vec::new(),
            pc: 0,
            dfn: false,
        })
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        let ret_height = self.scope.call.len();
        self.scope.call.push(frame);
        while self.scope.call.len() > ret_height {
            let frame = self.scope.call.last().unwrap();
            let Some(instr) = frame.function.instrs.get(frame.pc) else {
                if let Some(frame) = self.scope.call.pop() {
                    if frame.dfn {
                        self.scope.dfn.pop();
                    }
                }
                continue;
            };
            // println!("{:?}", self.stack);
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
                &Instr::EndArray(span) => (|| {
                    let start = self.scope.array.pop().unwrap();
                    let values: Vec<_> = self.stack.drain(start..).rev().collect();
                    self.push_span(span, None);
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
                &Instr::Call(span) => self.call_with_span(span),
                Instr::DfnVal(n) => {
                    if let Some(dfn) = self.scope.dfn.last() {
                        if let Some(value) = dfn.args.get(*n).cloned() {
                            self.stack.push(value);
                            Ok(())
                        } else {
                            Err(self.error(format!(
                                "Function references dfn argument `{}` outside of its dfn",
                                (*n as u8 + b'a') as char
                            )))
                        }
                    } else {
                        Err(self.error(format!(
                            "Function references dfn argument `{}` outside of a dfn",
                            (*n as u8 + b'a') as char
                        )))
                    }
                }
            };
            if let Err(mut err) = res {
                // Trace errors
                let frames = self.scope.call.split_off(ret_height);
                for frame in frames {
                    err = self.trace_error(err, frame);
                }
                return Err(err);
            } else {
                // Go to next instruction
                self.scope.call.last_mut().unwrap().pc += 1;
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
    fn call_with_span(&mut self, call_span: usize) -> UiuaResult {
        let mut value = self.pop("called function")?;
        let mut first_pass = true;
        loop {
            match value {
                Value::Func(f) if f.shape.is_empty() => {
                    // Call function
                    let f = f.into_scalar().unwrap();
                    let mut dfn = false;
                    match &f.kind {
                        FunctionKind::Normal => {}
                        FunctionKind::Dfn(n) => {
                            let n = *n as usize;
                            if self.stack.len() < n {
                                let message = format!("not enough arguments for dfn of {n} values");
                                break Err(self.spans.lock()[call_span].clone().sp(message).into());
                            }
                            let args = self.stack.drain(self.stack.len() - n..).rev().collect();
                            if let Some(bottom) = self.scope.array.last_mut() {
                                *bottom = (*bottom).min(self.stack.len());
                            }
                            self.scope.dfn.push(DfnFrame {
                                function: f.clone(),
                                args,
                            });
                            dfn = true;
                        }
                        FunctionKind::Dynamic { f: ff, .. } => {
                            self.scope.call.push(StackFrame {
                                function: f.clone(),
                                call_span,
                                spans: Vec::new(),
                                pc: 0,
                                dfn: false,
                            });
                            ff(self)?;
                            self.scope.call.pop();
                            break Ok(());
                        }
                    }
                    break self.exec(StackFrame {
                        function: f,
                        call_span,
                        spans: Vec::new(),
                        pc: 0,
                        dfn,
                    });
                }
                Value::Func(_) if first_pass => {
                    // Call non-scalar function array
                    let index = self.pop("index")?;
                    value = index.pick(value, self)?;
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
    pub fn call(&mut self) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(call_span)
    }
    #[inline]
    pub fn recur(&mut self) -> UiuaResult {
        let dfn = self
            .scope
            .dfn
            .last()
            .ok_or_else(|| self.error("Cannot recur outside of dfn"))?;
        self.push(dfn.function.clone());
        self.call()
    }
    pub fn call_catch_break(&mut self) -> UiuaResult<bool> {
        match self.call() {
            Ok(_) => Ok(false),
            Err(e) => match e.break_data() {
                Ok((0, _)) => Ok(true),
                Ok((n, span)) => Err(UiuaError::Break(n - 1, span)),
                Err(e) => Err(e),
            },
        }
    }
    pub fn call_error_on_break(&mut self, message: &str) -> UiuaResult {
        match self.call() {
            Ok(_) => Ok(()),
            Err(e) => match e.break_data() {
                Ok((0, span)) => Err(span.sp(message.into()).into()),
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
    /// Pop a result value from the stack
    ///
    /// Equivalent to `Self::pop("result")`
    pub fn pop_result(&mut self) -> UiuaResult<Value> {
        self.pop("result")
    }
    /// Push a value onto the stack
    pub fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        take(&mut self.stack)
    }
    /// Clone the entire stack
    pub fn clone_stack(&self) -> Vec<Value> {
        self.stack.clone()
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
    pub(crate) fn dyadic_ref<V: Into<Value>>(&mut self, f: fn(&Value, &Value) -> V) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b));
        Ok(())
    }
    pub(crate) fn dyadic_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(a, b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_ref_env<V: Into<Value>>(
        &mut self,
        f: fn(&Value, &Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(&a, &b, self)?);
        Ok(())
    }
    pub(crate) fn dyadic_ref_own_env<V: Into<Value>>(
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
            new_dfns: Vec::new(),
            globals: self.globals.clone(),
            spans: self.spans.clone(),
            stack: self
                .stack
                .drain(self.stack.len() - capture_count..)
                .collect(),
            scope: self.scope.clone(),
            lower_scopes: self.lower_scopes.last().cloned().into_iter().collect(),
            mode: self.mode,
            current_imports: self.current_imports.clone(),
            imports: self.imports.clone(),
            backend: self.backend.clone(),
        };
        self.backend
            .spawn(env, Box::new(f))
            .map(Value::from)
            .map_err(|e| self.error(e))
    }
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
