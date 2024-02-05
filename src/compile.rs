use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt, fs,
    mem::{replace, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    sync::Arc,
};

use ecow::{eco_vec, EcoString, EcoVec};

use crate::{
    algorithm::invert::{invert_instrs, under_instrs},
    ast::*,
    check::{instrs_signature, SigCheckError},
    constants, example_ua,
    function::*,
    lex::{CodeSpan, Sp, Span},
    optimize::{optimize_instrs, optimize_instrs_mut},
    parse::{count_placeholders, ident_modifier_args, parse, split_words, unsplit_words},
    Array, Assembly, Boxed, Diagnostic, DiagnosticKind, Global, Ident, ImplPrimitive, InputSrc,
    IntoInputSrc, IntoSysBackend, Primitive, RunMode, SafeSys, SysBackend, SysOp, Uiua, UiuaError,
    UiuaResult, Value,
};

/// The Uiua compiler
#[derive(Clone)]
pub struct Compiler {
    pub(crate) asm: Assembly,
    /// Functions which are under construction
    pub(crate) new_functions: Vec<EcoVec<Instr>>,
    pub(crate) next_global: usize,
    /// The current scope
    pub(crate) scope: Scope,
    /// Ancestor scopes of the current one
    pub(crate) higher_scopes: Vec<Scope>,
    /// Determines which How test scopes are run
    pub(crate) mode: RunMode,
    /// The paths of files currently being imported (used to detect import cycles)
    pub(crate) current_imports: Vec<PathBuf>,
    /// The bindings of imported files
    pub(crate) imports: HashMap<PathBuf, HashMap<Ident, usize>>,
    /// Accumulated errors
    pub(crate) errors: Vec<UiuaError>,
    /// Accumulated diagnostics
    pub(crate) diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    pub(crate) print_diagnostics: bool,
    /// The backend used to run comptime code
    pub(crate) backend: Arc<dyn SysBackend>,
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler {
            asm: Assembly::default(),
            new_functions: Vec::new(),
            next_global: 0,
            scope: Scope::default(),
            higher_scopes: Vec::new(),
            mode: RunMode::All,
            current_imports: Vec::new(),
            imports: HashMap::new(),
            errors: Vec::new(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: false,
            backend: Arc::new(SafeSys),
        }
    }
}

impl AsRef<Assembly> for Compiler {
    fn as_ref(&self) -> &Assembly {
        &self.asm
    }
}

impl AsMut<Assembly> for Compiler {
    fn as_mut(&mut self) -> &mut Assembly {
        &mut self.asm
    }
}

#[derive(Clone)]
pub(crate) struct Scope {
    /// Map local names to global indices
    pub names: HashMap<Ident, usize>,
    /// Whether to allow experimental features
    pub experimental: bool,
    /// The stack height between top-level statements
    pub stack_height: Result<usize, Sp<SigCheckError>>,
    /// The stack of referenced locals
    pub locals: Vec<HashSet<usize>>,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            names: HashMap::new(),
            experimental: false,
            stack_height: Ok(0),
            locals: Vec::new(),
        }
    }
}

impl Compiler {
    /// Create a new compiler
    pub fn new() -> Self {
        Self::default()
    }
    /// Create a new compiler with a custom backend for `comptime` code
    pub fn with_backend(backend: impl IntoSysBackend) -> Self {
        Self {
            backend: backend.into_sys_backend(),
            ..Self::default()
        }
    }
    /// Get a reference to the assembly
    pub fn assembly(&self) -> &Assembly {
        &self.asm
    }
    /// Get a mutable reference to the assembly
    pub fn assembly_mut(&mut self) -> &mut Assembly {
        &mut self.asm
    }
    /// Take a completed assembly from the compiler
    pub fn finish(&mut self) -> Assembly {
        take(&mut self.asm)
    }
    /// Set whether to print diagnostics as they are encountered
    ///
    /// If this is set to false, diagnostics will be accumulated and can be retrieved with [`Compiler::take_diagnostics`]
    ///
    /// Defaults to false
    pub fn print_diagnostics(&mut self, print_diagnostics: bool) -> &mut Self {
        self.print_diagnostics = print_diagnostics;
        self
    }
    /// Set the run mode
    pub fn mode(&mut self, mode: RunMode) -> &mut Self {
        self.mode = mode;
        self
    }
    /// Compile a Uiua file from a file at a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input: EcoString = fs::read_to_string(path)
            .map_err(|e| UiuaError::Load(path.into(), e.into()))?
            .into();
        self.asm.inputs.files.insert(path.into(), input.clone());
        self.load_impl(&input, InputSrc::File(path.into()))
    }
    /// Compile a Uiua file from a string
    pub fn load_str(&mut self, input: &str) -> UiuaResult<&mut Self> {
        let src = self.asm.inputs.add_src((), input);
        self.load_impl(input, src)
    }
    /// Compile a Uiua file from a string with a path for error reporting
    pub fn load_str_src(&mut self, input: &str, src: impl IntoInputSrc) -> UiuaResult<&mut Self> {
        let src = self.asm.inputs.add_src(src, input);
        self.load_impl(input, src)
    }
    /// Run in a scoped context. Names defined in this context will be removed when the scope ends.
    ///
    /// While names defined in this context will be removed when the scope ends, values *bound* to
    /// those names will not.
    ///
    /// All other runtime state other than the stack, will also be restored.
    pub fn in_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<HashMap<Ident, usize>> {
        let experimental = self.scope.experimental;
        self.higher_scopes.push(take(&mut self.scope));
        self.scope.experimental = experimental;
        let res = f(self);
        let scope = replace(&mut self.scope, self.higher_scopes.pop().unwrap());
        res?;
        Ok(scope.names)
    }
    fn load_impl(&mut self, input: &str, src: InputSrc) -> UiuaResult<&mut Self> {
        let instrs_start = self.asm.instrs.len();
        let top_slices_start = self.asm.top_slices.len();
        let (items, errors, diagnostics) = parse(input, src.clone(), &mut self.asm.inputs);
        if self.print_diagnostics {
            for diagnostic in diagnostics {
                println!("{}", diagnostic.report());
            }
        } else {
            self.diagnostics.extend(diagnostics);
        }
        if !errors.is_empty() {
            return Err(UiuaError::Parse(errors, self.asm.inputs.clone().into()));
        }
        if let InputSrc::File(path) = &src {
            self.current_imports.push(path.to_path_buf());
        }

        let res = self.catching_crash(input, |env| env.items(items, false));

        if let InputSrc::File(_) = &src {
            self.current_imports.pop();
        }
        match res {
            Err(e) | Ok(Err(e)) => {
                self.asm.instrs.truncate(instrs_start);
                self.asm.top_slices.truncate(top_slices_start);
                self.errors.push(e);
            }
            _ => {}
        }
        match self.errors.len() {
            0 => Ok(self),
            1 => Err(self.errors.pop().unwrap()),
            _ => Err(UiuaError::Multi(self.errors.drain(..).collect())),
        }
    }
    fn catching_crash<T>(
        &mut self,
        input: impl fmt::Display,
        f: impl FnOnce(&mut Self) -> T,
    ) -> UiuaResult<T> {
        match catch_unwind(AssertUnwindSafe(|| f(self))) {
            Ok(res) => Ok(res),
            Err(_) => Err(UiuaError::Panic(format!(
                "\
The compiler has crashed!
Hooray! You found a bug!
Please report this at http://github.com/uiua-lang/uiua/issues/new

code:
{input}"
            ))),
        }
    }
    pub(crate) fn items(&mut self, items: Vec<Item>, in_test: bool) -> UiuaResult {
        let mut prev_comment = None;
        for item in items {
            if let Err(e) = self.item(item, in_test, &mut prev_comment) {
                self.errors.push(e);
            }
        }
        Ok(())
    }
    fn item(
        &mut self,
        item: Item,
        in_test: bool,
        prev_comment: &mut Option<Arc<str>>,
    ) -> UiuaResult {
        fn words_should_run_anyway(words: &[Sp<Word>]) -> bool {
            words.iter().any(|w| {
                matches!(&w.value, Word::Primitive(Primitive::Sys(SysOp::Import)))
                    || matches!(&w.value, Word::Comment(com) if com.trim() == "Experimental!")
            })
        }
        let prev_com = prev_comment.take();
        match item {
            Item::TestScope(items) => {
                self.in_scope(|env| env.items(items.value, true))?;
            }
            Item::Words(mut lines) => {
                if lines.iter().flatten().all(|w| !w.value.is_code()) {
                    let mut comment = String::new();
                    for (i, line) in lines.iter().enumerate() {
                        if line.is_empty() {
                            comment.clear();
                            continue;
                        }
                        for word in line {
                            if let Word::Comment(c) = &word.value {
                                if i > 0 {
                                    comment.push('\n');
                                }
                                comment.push_str(c);
                            }
                        }
                    }
                    *prev_comment = Some(comment.into());
                }
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::Test => in_test,
                    RunMode::All => true,
                };
                lines = unsplit_words(lines.into_iter().flat_map(split_words));
                for line in lines {
                    if line.is_empty() {
                        continue;
                    }
                    if can_run || words_should_run_anyway(&line) {
                        let span = (line.first().unwrap().span.clone())
                            .merge(line.last().unwrap().span.clone());
                        if count_placeholders(&line) > 0 {
                            self.add_error(
                                span.clone(),
                                "Cannot use placeholder outside of function",
                            );
                        }
                        let instrs = self.compile_words(line, true)?;
                        match instrs_signature(&instrs) {
                            Ok(sig) => {
                                if let Ok(height) = &mut self.scope.stack_height {
                                    *height = (*height + sig.outputs).saturating_sub(sig.args);
                                }
                            }
                            Err(e) => self.scope.stack_height = Err(span.sp(e)),
                        }
                        let start = self.asm.instrs.len();
                        self.asm.instrs.extend(optimize_instrs(instrs, true));
                        let end = self.asm.instrs.len();
                        self.asm.top_slices.push(FuncSlice {
                            start,
                            len: end - start,
                        });
                    }
                }
            }
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::All | RunMode::Test => true,
                };
                if can_run || words_should_run_anyway(&binding.words) {
                    self.binding(binding, prev_com)?;
                }
            }
        }
        Ok(())
    }
    #[must_use]
    pub(crate) fn add_function<I>(&mut self, id: FunctionId, sig: Signature, instrs: I) -> Function
    where
        I: IntoIterator<Item = Instr> + fmt::Debug,
        I::IntoIter: ExactSizeIterator,
    {
        let instrs = optimize_instrs(instrs, true);
        let len = instrs.len();
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("({id}").into()));
        }
        let start = self.asm.instrs.len();
        self.asm.instrs.extend(instrs);
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("{id})").into()));
        }
        Function::new(id, sig, FuncSlice { start, len })
    }
    fn binding(&mut self, binding: Binding, comment: Option<Arc<str>>) -> UiuaResult {
        let name = binding.name.value;
        let span = &binding.name.span;
        let placeholder_count = count_placeholders(&binding.words);

        let make_fn = |mut instrs: EcoVec<Instr>, sig: Signature, comp: &mut Self| {
            // Diagnostic for function that doesn't consume its arguments
            if let Some((Instr::Prim(Primitive::Dup, span), rest)) = instrs.split_first() {
                if let Ok(rest_sig) = instrs_signature(rest) {
                    if rest_sig.args == sig.args && rest_sig.outputs + 1 == sig.outputs {
                        comp.emit_diagnostic(
                            "Functions should consume their arguments. \
                            Try removing this duplicate.",
                            DiagnosticKind::Style,
                            comp.get_span(*span),
                        );
                        comp.flush_diagnostics();
                    }
                }
            }

            // Handle placeholders
            if placeholder_count > 0 {
                instrs.insert(0, Instr::PushTempFunctions(placeholder_count));
                instrs.push(Instr::PopTempFunctions(placeholder_count));
            }
            let f = comp.add_function(FunctionId::Named(name.clone()), sig, instrs);
            if placeholder_count > 0 {
                comp.increment_placeholders(&f, &mut 0, &mut HashMap::new());
            }
            f
        };
        // Compile the body
        let mut instrs = self.compile_words(binding.words, true)?;
        let span_index = self.add_span(span.clone());
        let global_index = self.next_global;
        self.next_global += 1;

        // Check if binding is an import
        let mut is_import = false;
        let mut sig = None;
        if let [init @ .., Instr::Prim(Primitive::Sys(SysOp::Import), _)] = instrs.as_slice() {
            is_import = true;
            match init {
                [Instr::Push(path)] => {
                    if let Some(sig) = &binding.signature {
                        self.add_error(
                            sig.span.clone(),
                            "Cannot declare a signature for a module import",
                        );
                    }
                    match path {
                        Value::Char(arr) if arr.rank() == 1 => {
                            let path: String = arr.data.iter().copied().collect();
                            let module = self.import_compile(path.as_ref(), span)?;
                            self.asm.add_global_at(
                                global_index,
                                Global::Module { module },
                                Some(binding.name.span.clone()),
                                comment.clone(),
                            );
                            self.scope.names.insert(name.clone(), global_index);
                        }
                        _ => self.add_error(span.clone(), "Import path must be a string"),
                    }
                }
                [Instr::Push(item), Instr::Push(path)] => match path {
                    Value::Char(arr) if arr.rank() == 1 => {
                        let path: String = arr.data.iter().copied().collect();
                        let module = self.import_compile(path.as_ref(), span)?;
                        match item {
                            Value::Char(arr) if arr.rank() == 1 => {
                                let item: String = arr.data.iter().copied().collect();
                                if let Some(&index) = self.imports[&module].get(item.as_str()) {
                                    self.scope.names.insert(name.clone(), index);
                                    if let Some(s) = self.asm.bindings[index].global.signature() {
                                        sig = Some(s);
                                    } else {
                                        self.add_error(
                                            span.clone(),
                                            "Cannot define a signature for a module rebind",
                                        )
                                    }
                                } else {
                                    self.add_error(
                                        span.clone(),
                                        format!("Item `{item}` not found in module `{path}`"),
                                    )
                                }
                            }
                            _ => self.add_error(span.clone(), "Import item must be a string"),
                        };
                    }
                    _ => self.add_error(span.clone(), "Import path must be a string"),
                },
                _ => self.add_error(span.clone(), "&i must be followed by one or two strings"),
            }
        }

        // Resolve signature
        match instrs_signature(&instrs) {
            Ok(s) => {
                let mut sig = sig.unwrap_or(s);
                // Runtime-dependent binding
                if instrs.is_empty() {
                    // Binding from the stack set above
                    match &mut self.scope.stack_height {
                        Ok(height) => {
                            if *height > 0 {
                                sig = Signature::new(0, 1);
                            }
                            *height = height.saturating_sub(1);
                        }
                        Err(sp) => {
                            let sp = sp.clone();
                            self.add_error(
                                sp.span,
                                format!(
                                    "This line's signature is undefined: {}. \
                                    This prevents the later binding of {}.",
                                    sp.value, name
                                ),
                            );
                        }
                    }
                }
                // Validate signature
                if let Some(declared_sig) = &binding.signature {
                    let sig_to_check = if let [Instr::PushFunc(f)] = instrs.as_slice() {
                        // If this is a function wrapped in parens, check the signature of the
                        // function rather than the signature of the binding's words
                        f.signature()
                    } else {
                        sig
                    };
                    if declared_sig.value == sig_to_check {
                        sig = declared_sig.value;
                    } else {
                        self.add_error(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig_to_check
                            ),
                        );
                    }
                }
                #[rustfmt::skip]
                let is_setinv = matches!(
                    instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetInverse, _)]
                );
                #[rustfmt::skip]
                let is_setund = matches!(
                    instrs.as_slice(),
                    [Instr::PushFunc(_), Instr::PushFunc(_), Instr::PushFunc(_), Instr::Prim(Primitive::SetUnder, _)]
                );
                if is_import {
                } else if let [Instr::PushFunc(f)] = instrs.as_slice() {
                    // Binding is a single inline function
                    let func = make_fn(f.instrs(self).into(), f.signature(), self);
                    self.compile_bind_function(&name, global_index, func, span_index, comment)?;
                } else if sig.args == 0
                    && sig.outputs <= 1
                    && (sig.outputs > 0 || instrs.is_empty())
                    && placeholder_count == 0
                    && !is_setinv
                    && !is_setund
                {
                    self.compile_bind_sig(&name, global_index, sig, span_index, comment)?;
                    // Add binding instrs to top slices
                    instrs.push(Instr::BindGlobal {
                        span: span_index,
                        index: global_index,
                    });
                    let start = self.asm.instrs.len();
                    self.asm.instrs.extend(optimize_instrs(instrs, true));
                    let end = self.asm.instrs.len();
                    self.asm.top_slices.push(FuncSlice {
                        start,
                        len: end - start,
                    });
                } else {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig, self);
                    self.compile_bind_function(&name, global_index, func, span_index, comment)?;
                }
            }
            Err(e) => {
                if is_import {
                } else if let Some(sig) = binding.signature {
                    // Binding is a normal function
                    let func = make_fn(instrs, sig.value, self);
                    self.compile_bind_function(&name, global_index, func, span_index, comment)?;
                } else {
                    self.add_error(
                        binding.name.span.clone(),
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the `←`."
                            } else {
                                ""
                            }
                        ),
                    );
                }
            }
        }
        Ok(())
    }
    pub(crate) fn compile_bind_sig(
        &mut self,
        name: &Ident,
        index: usize,
        sig: Signature,
        span: usize,
        comment: Option<Arc<str>>,
    ) -> UiuaResult {
        self.scope.names.insert(name.clone(), index);
        self.asm.bind_sig(index, sig, span, comment);
        Ok(())
    }
    pub(crate) fn compile_bind_function(
        &mut self,
        name: &Ident,
        index: usize,
        function: Function,
        span: usize,
        comment: Option<Arc<str>>,
    ) -> UiuaResult {
        let temp_function_count =
            self.count_temp_functions(function.instrs(self), &mut HashSet::new());
        let name_marg_count = ident_modifier_args(name);
        if temp_function_count != name_marg_count {
            let trimmed = name.trim_end_matches('!');
            let this = format!("{}{}", trimmed, "!".repeat(temp_function_count));
            self.add_error(
                self.get_span(span),
                format!(
                    "The name {name} implies {name_marg_count} modifier arguments, \
                    but the binding body references {temp_function_count}. Try `{this}`."
                ),
            );
        }
        self.scope.names.insert(name.clone(), index);
        self.asm.bind_function(index, function, span, comment);
        Ok(())
    }
    pub(crate) fn import_compile(&mut self, path: &Path, span: &CodeSpan) -> UiuaResult<PathBuf> {
        let path = self.resolve_import_path(Path::new(&path));
        if self.asm.import_inputs.get(&path).is_some() {
            return Ok(path);
        }
        let bytes = fs::read(&path)
            .or_else(|e| {
                if path.ends_with(Path::new("example.ua")) {
                    Ok(example_ua(|ex| ex.as_bytes().to_vec()))
                } else {
                    Err(e)
                }
            })
            .map_err(|e| self.fatal_error(span.clone(), e))?;
        let input: EcoString = String::from_utf8(bytes)
            .map_err(|e| self.fatal_error(span.clone(), format!("Failed to read file: {e}")))?
            .into();
        self.asm.import_inputs.insert(path.clone(), input.clone());
        if self.current_imports.iter().any(|p| p == &path) {
            return Err(self.fatal_error(
                span.clone(),
                format!("Cycle detected importing {}", path.to_string_lossy()),
            ));
        }
        if !self.imports.contains_key(&path) {
            let import = self.in_scope(|env| env.load_str_src(&input, &path).map(drop))?;
            self.imports.insert(path.clone(), import);
        }
        Ok(path)
    }
    /// Resolve a declared import path relative to the path of the file that is being executed
    pub(crate) fn resolve_import_path(&self, path: &Path) -> PathBuf {
        let target = if let Some(parent) = self.current_imports.last().and_then(|p| p.parent()) {
            parent.join(path)
        } else {
            path.to_path_buf()
        };
        let base = Path::new(".");
        if let (Ok(canon_target), Ok(canon_base)) = (target.canonicalize(), base.canonicalize()) {
            pathdiff::diff_paths(canon_target, canon_base).unwrap_or(target)
        } else {
            pathdiff::diff_paths(&target, base).unwrap_or(target)
        }
    }
    fn compile_words(&mut self, mut words: Vec<Sp<Word>>, call: bool) -> UiuaResult<EcoVec<Instr>> {
        words = unsplit_words(split_words(words))
            .into_iter()
            .flatten()
            .collect();

        self.new_functions.push(EcoVec::new());
        self.words(words, call)?;
        self.flush_diagnostics();
        Ok(self.new_functions.pop().unwrap())
    }
    fn flush_diagnostics(&mut self) {
        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.report());
            }
        }
    }
    fn compile_operand_words(
        &mut self,
        words: Vec<Sp<Word>>,
    ) -> UiuaResult<(EcoVec<Instr>, Signature)> {
        let span = words
            .first()
            .zip(words.last())
            .map(|(first, last)| first.span.clone().merge(last.span.clone()));
        let mut instrs = self.compile_words(words, true)?;
        let mut sig = None;
        // Extract function instrs if possible
        if let [Instr::PushFunc(f)] = instrs.as_slice() {
            sig = Some(f.signature());
            let slice = f.slice;
            instrs = f.instrs(self).into();
            if slice.start + slice.len >= self.asm.instrs.len() - 1 {
                self.asm.instrs.truncate(slice.start);
                if matches!(self.asm.instrs.last(), Some(Instr::Comment(com)) if com.starts_with('('))
                {
                    self.asm.instrs.pop();
                }
            }
        }
        let sig = if let Some(sig) = sig {
            sig
        } else {
            instrs_signature(&instrs).map_err(|e| {
                self.fatal_error(
                    span.unwrap(),
                    format!("Cannot infer function signature: {e}"),
                )
            })?
        };
        let instrs = optimize_instrs(instrs, false);
        Ok((instrs, sig))
    }
    fn words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        let mut words = words
            .into_iter()
            .rev()
            .filter(|word| word.value.is_code() || matches!(&word.value, Word::Comment(_)))
            .peekable();
        while let Some(word) = words.next() {
            if let Some(next) = words.peek() {
                // Handle imports
                if let Word::Ident(name) = &next.value {
                    if let Some(index) = self.scope.names.get(name) {
                        if let Global::Module { module } = &self.asm.bindings[*index].global {
                            if let Word::String(item_name) = &word.value {
                                let index = self.imports[module]
                                    .get(item_name.as_str())
                                    .copied()
                                    .ok_or_else(|| {
                                    self.fatal_error(
                                        next.span.clone(),
                                        format!(
                                            "Item `{item_name}` not found in module `{}`",
                                            module.display()
                                        ),
                                    )
                                })?;
                                self.global_index(index, next.span.clone(), false);
                                words.next();
                                continue;
                            } else {
                                self.add_error(
                                    next.span.clone(),
                                    format!(
                                        "Expected a string after `{name}` \
                                        to specify an item to import",
                                    ),
                                );
                            }
                        }
                    }
                }
                // First select diagnostic
                if let (Word::Primitive(Primitive::Select), Word::Primitive(Primitive::First)) =
                    (&word.value, &next.value)
                {
                    self.emit_diagnostic(
                        format!(
                            "Flip the order of {} and {} to improve performance",
                            Primitive::First.format(),
                            Primitive::Select.format()
                        ),
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
            }
            self.word(word, call)?;
        }
        Ok(())
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr) {
        let instrs = self.new_functions.last_mut().unwrap();
        optimize_instrs_mut(instrs, instr, false);
    }
    fn push_all_instrs(&mut self, instrs: impl IntoIterator<Item = Instr>) {
        for instr in instrs {
            self.push_instr(instr);
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                if call {
                    self.push_instr(Instr::push(n));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(n)],
                    );
                    self.push_instr(Instr::PushFunc(f))
                }
            }
            Word::Char(c) => {
                let val: Value = if c.chars().count() == 1 {
                    c.chars().next().unwrap().into()
                } else {
                    c.into()
                };
                if call {
                    self.push_instr(Instr::push(val));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(val)],
                    );
                    self.push_instr(Instr::PushFunc(f))
                }
            }
            Word::String(s) => {
                if call {
                    self.push_instr(Instr::push(s));
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![Instr::push(s)],
                    );
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::Label(label) => {
                if !self.scope.experimental {
                    self.add_error(
                        word.span.clone(),
                        "Labels are experimental. To use them, add \
                        `# Experimental!` to the top of the file.",
                    );
                }
                let instr = Instr::Label {
                    label: label.into(),
                    span: self.add_span(word.span.clone()),
                };
                if call {
                    self.push_instr(instr);
                } else {
                    let f = self.add_function(
                        FunctionId::Anonymous(word.span),
                        Signature::new(1, 1),
                        vec![instr],
                    );
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::FormatString(frags) => {
                let signature = Signature::new(frags.len() - 1, 1);
                let parts = frags.into_iter().map(Into::into).collect();
                let span = self.add_span(word.span.clone());
                let instr = Instr::Format { parts, span };
                if call {
                    self.push_instr(instr)
                } else {
                    let f =
                        self.add_function(FunctionId::Anonymous(word.span), signature, vec![instr]);
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::MultilineString(lines) => {
                let signature = Signature::new(
                    lines.iter().map(|l| l.value.len().saturating_sub(1)).sum(),
                    1,
                );
                let span = self.add_span(word.span.clone());
                let mut curr_part = EcoString::new();
                let mut parts = EcoVec::new();
                for (l, line) in lines.into_iter().enumerate() {
                    if l > 0 {
                        curr_part.push('\n');
                    }
                    for (f, frag) in line.value.into_iter().enumerate() {
                        if f > 0 {
                            parts.push(take(&mut curr_part));
                        }
                        curr_part.push_str(&frag);
                    }
                }
                parts.push(curr_part);
                let instr = Instr::Format { parts, span };
                if call {
                    self.push_instr(instr)
                } else {
                    let f =
                        self.add_function(FunctionId::Anonymous(word.span), signature, vec![instr]);
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Strand(items) => {
                if !call {
                    self.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
                let inner = self.compile_words(items, true)?;
                // Diagnostic for strand of characters
                if !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Instr::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.emit_diagnostic(
                        "Stranded characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
                // Validate items
                let mut instrs = inner.iter();
                while let Some(instr) = instrs.next() {
                    match instr {
                        Instr::Push(_) => {}
                        Instr::Prim(p, _)
                            if p.args() == Some(0)
                                && p.outputs() == Some(1)
                                && p.modifier_args().is_none() => {}
                        Instr::BeginArray => {
                            while (instrs.next())
                                .is_some_and(|instr| !matches!(instr, Instr::EndArray { .. }))
                            {
                            }
                        }
                        Instr::CallGlobal { sig, .. } if *sig == (0, 1) => {}
                        _ => self.add_error(word.span.clone(), "Strand cannot contain functions"),
                    }
                }
                let span_index = self.add_span(word.span.clone());
                let instrs = self.new_functions.last_mut().unwrap();
                // Inline constant arrays
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    instrs.pop();
                    let values = inner.iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v.clone(),
                        _ => unreachable!(),
                    });
                    match Value::from_row_values(values, &(&word.span, &self.asm.inputs)) {
                        Ok(val) => {
                            self.push_instr(Instr::push(val));
                            return Ok(());
                        }
                        Err(e) if e.is_fill() => {}
                        Err(e) => return Err(e),
                    }
                }
                // Normal case
                instrs.extend(inner);
                self.push_instr(Instr::EndArray {
                    span: span_index,
                    boxed: false,
                });
                if !call {
                    let instrs = self.new_functions.pop().unwrap();
                    let sig = instrs_signature(&instrs).unwrap_or(Signature::new(0, 0));
                    let func = self.add_function(FunctionId::Anonymous(word.span), sig, instrs);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Word::Array(arr) => {
                if !call {
                    self.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                let line_count = arr.lines.len();
                for lines in arr.lines.into_iter().rev() {
                    inner.extend(self.compile_words(lines, true)?);
                }
                // Diagnostic for array of characters
                if line_count <= 1
                    && !arr.boxes
                    && !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Instr::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.emit_diagnostic(
                        "An array of characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
                let span = self.add_span(word.span.clone());
                let instrs = self.new_functions.last_mut().unwrap();
                // Inline constant arrays
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    instrs.pop();
                    let empty = inner.is_empty();
                    let values = inner.iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v.clone(),
                        _ => unreachable!(),
                    });
                    let res = if arr.boxes {
                        if empty {
                            Ok(Array::<Boxed>::default().into())
                        } else {
                            Value::from_row_values(
                                values.map(|v| Value::Box(Boxed(v).into())),
                                &(&word.span, &self.asm.inputs),
                            )
                        }
                    } else {
                        Value::from_row_values(values, &(&word.span, &self.asm.inputs))
                    };
                    match res {
                        Ok(val) => {
                            self.push_instr(Instr::push(val));
                            return Ok(());
                        }
                        Err(e) if e.is_fill() => {}
                        Err(e) => return Err(e),
                    }
                }
                // Normal case
                instrs.extend(inner);
                self.push_instr(Instr::EndArray {
                    span,
                    boxed: arr.boxes,
                });
                if !call {
                    let instrs = self.new_functions.pop().unwrap();
                    let sig = instrs_signature(&instrs).unwrap_or(Signature::new(0, 0));
                    let func = self.add_function(FunctionId::Anonymous(word.span), sig, instrs);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Word::Func(func) => self.func(func, word.span, call)?,
            Word::Switch(sw) => self.switch(sw, word.span, call)?,
            Word::Primitive(p) => self.primitive(p, word.span, call),
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Placeholder(sig) => {
                let span = self.add_span(word.span);
                self.push_instr(Instr::GetTempFunction {
                    offset: 0,
                    sig,
                    span,
                });
                if call {
                    self.push_instr(Instr::Call(span));
                }
            }
            Word::Comment(comment) => {
                if comment.trim() == "Experimental!" {
                    self.scope.experimental = true;
                }
            }
            Word::OutputComment { i, n } => self.push_instr(Instr::SetOutputComment { i, n }),
            Word::Spaces | Word::BreakLine | Word::UnbreakLine => {}
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool) -> UiuaResult {
        if !self.scope.locals.is_empty() && ident.chars().all(|c| c.is_ascii_lowercase()) {
            // Name is a local variable
            let span = self.add_span(span);
            for c in ident.chars().rev() {
                let index = c as usize - 'a' as usize;
                self.scope.locals.last_mut().unwrap().insert(index);
                self.push_instr(Instr::GetLocal { index, span });
            }
        } else if let Some(index) = (self.scope.names.get(&ident))
            .or_else(|| self.higher_scopes.last()?.names.get(&ident))
            .copied()
        {
            // Name exists in scope
            (self.asm.global_references).insert(span.clone().sp(ident), index);
            self.global_index(index, span, call);
        } else if let Some(constant) = constants().iter().find(|c| c.name == ident) {
            // Name is a built-in constant
            let instr = Instr::push(constant.value.clone());
            if call {
                self.push_instr(instr);
            } else {
                let f = self.add_function(
                    FunctionId::Anonymous(span.clone()),
                    Signature::new(0, 1),
                    vec![instr],
                );
                self.push_instr(Instr::PushFunc(f));
            }
        } else {
            return Err(self.fatal_error(span, format!("Unknown identifier `{ident}`")));
        }
        Ok(())
    }
    fn global_index(&mut self, index: usize, span: CodeSpan, call: bool) {
        let global = self.asm.bindings[index].global.clone();
        match global {
            Global::Const(val) if call => self.push_instr(Instr::push(val)),
            Global::Const(val) => {
                let f = self.add_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    vec![Instr::push(val)],
                );
                self.push_instr(Instr::PushFunc(f));
            }
            Global::Func(f)
                if self.count_temp_functions(f.instrs(self), &mut HashSet::new()) == 0
                    && !self.has_tracing(f.instrs(self)) =>
            {
                if call {
                    // Inline instructions
                    self.push_instr(Instr::PushSig(f.signature()));
                    let instrs = f.instrs(self).to_vec();
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            Global::Func(f) => {
                self.push_instr(Instr::PushFunc(f));
                if call {
                    let span = self.add_span(span);
                    self.push_instr(Instr::Call(span));
                }
            }
            Global::Sig(sig) if call => self.push_instr(Instr::CallGlobal { index, call, sig }),
            Global::Sig(sig) => {
                let f = self.add_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    vec![Instr::CallGlobal { index, call, sig }],
                );
                self.push_instr(Instr::PushFunc(f));
            }
            Global::Module { .. } => self.add_error(span, "Cannot import module item here."),
        }
    }
    fn func(&mut self, func: Func, span: CodeSpan, call: bool) -> UiuaResult {
        if (func.lines.iter().flatten().filter(|w| w.value.is_code())).count() == 1 {
            // Inline single ident
            if let Some(
                word @ Sp {
                    value: Word::Ident(_),
                    ..
                },
            ) = func.lines.iter().flatten().find(|w| w.value.is_code())
            {
                return self.word(word.clone(), call);
            }
        }
        let function = self.compile_func(func, span)?;
        self.push_instr(Instr::PushFunc(function));
        Ok(())
    }
    fn compile_func(&mut self, func: Func, span: CodeSpan) -> UiuaResult<Function> {
        let mut instrs = Vec::new();
        for line in func.lines {
            instrs.extend(self.compile_words(line, true)?);
        }

        // Validate signature
        let sig = match instrs_signature(&instrs) {
            Ok(mut sig) => {
                if let Some(declared_sig) = &func.signature {
                    if declared_sig.value == sig {
                        sig = declared_sig.value;
                    } else {
                        return Err(self.fatal_error(
                            declared_sig.span.clone(),
                            format!(
                                "Function signature mismatch: declared {} but inferred {}",
                                declared_sig.value, sig
                            ),
                        ));
                    }
                }
                sig
            }
            Err(e) => {
                if let Some(declared_sig) = &func.signature {
                    declared_sig.value
                } else {
                    return Err(self.fatal_error(
                        span,
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            }
        };

        if let [Instr::PushFunc(f), Instr::Call(_)] = instrs.as_slice() {
            return Ok(Function::clone(f));
        }

        Ok(self.add_function(func.id, sig, instrs))
    }
    fn switch(&mut self, sw: Switch, span: CodeSpan, call: bool) -> UiuaResult {
        let count = sw.branches.len();
        if !call {
            self.new_functions.push(EcoVec::new());
        }
        let mut branches = sw.branches.into_iter();
        let first_branch = branches.next().expect("switch cannot have no branches");
        let f = self.compile_func(first_branch.value, first_branch.span)?;
        let mut sig = f.signature();
        self.push_instr(Instr::PushFunc(f));
        for branch in branches {
            let f = self.compile_func(branch.value, branch.span.clone())?;
            let f_sig = f.signature();
            if f_sig.is_compatible_with(sig) {
                sig = sig.max_with(f_sig);
            } else if f_sig.outputs == sig.outputs {
                sig.args = sig.args.max(f_sig.args)
            } else {
                self.add_error(
                    branch.span,
                    format!(
                        "Switch branch's signature {f_sig} is \
                        incompatible with previous branches {sig}",
                    ),
                );
            }
            self.push_instr(Instr::PushFunc(f));
        }
        let span_idx = self.add_span(span.clone());
        self.push_instr(Instr::Switch {
            count,
            sig,
            span: span_idx,
        });
        if !call {
            let instrs = self.new_functions.pop().unwrap();
            let sig = match instrs_signature(&instrs) {
                Ok(sig) => sig,
                Err(e) => {
                    return Err(self.fatal_error(
                        span,
                        format!(
                            "Cannot infer function signature: {e}{}",
                            if e.ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            };
            let function = self.add_function(FunctionId::Anonymous(span), sig, instrs);
            self.push_instr(Instr::PushFunc(function));
        }
        Ok(())
    }
    #[allow(clippy::collapsible_match)]
    fn modified(&mut self, modified: Modified, call: bool) -> UiuaResult {
        let op_count = modified.code_operands().count();

        // De-sugar switched
        if op_count == 1 {
            let operand = modified.code_operands().next().unwrap().clone();
            if let Sp {
                value: Word::Switch(sw),
                span,
            } = operand
            {
                match &modified.modifier.value {
                    Modifier::Primitive(Primitive::Dip) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: vec![branches.next().unwrap().map(Word::Func)],
                        };
                        for branch in branches {
                            let mut lines = branch.value.lines;
                            (lines.last_mut().unwrap())
                                .push(span.clone().sp(Word::Modified(Box::new(new))));
                            new = Modified {
                                modifier: modified.modifier.clone(),
                                operands: vec![branch.span.clone().sp(Word::Func(Func {
                                    id: FunctionId::Anonymous(branch.span.clone()),
                                    signature: None,
                                    lines,
                                    closed: true,
                                }))],
                            };
                        }
                        return self.modified(new, call);
                    }
                    Modifier::Primitive(Primitive::Fork | Primitive::Bracket) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: {
                                let mut ops: Vec<_> = branches
                                    .by_ref()
                                    .take(2)
                                    .map(|w| w.map(Word::Func))
                                    .collect();
                                ops.reverse();
                                ops
                            },
                        };
                        for branch in branches {
                            new = Modified {
                                modifier: modified.modifier.clone(),
                                operands: vec![
                                    branch.map(Word::Func),
                                    span.clone().sp(Word::Modified(Box::new(new))),
                                ],
                            };
                        }
                        return self.modified(new, call);
                    }
                    Modifier::Primitive(Primitive::Cascade) => {
                        let mut branches = sw.branches.into_iter().rev();
                        let mut new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: {
                                let mut ops: Vec<_> = branches
                                    .by_ref()
                                    .take(2)
                                    .map(|w| w.map(Word::Func))
                                    .collect();
                                ops.reverse();
                                ops
                            },
                        };
                        for branch in branches {
                            new = Modified {
                                modifier: modified.modifier.clone(),
                                operands: vec![
                                    branch.map(Word::Func),
                                    span.clone().sp(Word::Modified(Box::new(new))),
                                ],
                            };
                        }
                        return self.modified(new, call);
                    }
                    modifier if modifier.args() >= 2 => {
                        if sw.branches.len() != modifier.args() {
                            return Err(self.fatal_error(
                                modified.modifier.span.clone().merge(span),
                                format!(
                                    "{} requires {} function arguments, but the \
                                    function pack has {} functions",
                                    modifier,
                                    modifier.args(),
                                    sw.branches.len()
                                ),
                            ));
                        }
                        let new = Modified {
                            modifier: modified.modifier.clone(),
                            operands: sw.branches.into_iter().map(|w| w.map(Word::Func)).collect(),
                        };
                        return self.modified(new, call);
                    }
                    _ => {}
                }
            }
        }

        if let Modifier::Primitive(prim) = modified.modifier.value {
            // Give advice about redundancy
            match prim {
                m @ Primitive::Each => {
                    if let [Sp {
                        value: Word::Primitive(prim),
                        span,
                    }] = modified.operands.as_slice()
                    {
                        if prim.class().is_pervasive() {
                            let span = modified.modifier.span.clone().merge(span.clone());
                            self.emit_diagnostic(
                                format!(
                                    "Using {m} with a pervasive primitive like {p} is \
                                    redundant. Just use {p} by itself.",
                                    m = m.format(),
                                    p = prim.format(),
                                ),
                                DiagnosticKind::Advice,
                                span,
                            );
                        }
                    } else if words_look_pervasive(&modified.operands) {
                        let span = modified.modifier.span.clone();
                        self.emit_diagnostic(
                            format!(
                                "{m}'s function is pervasive, \
                                so {m} is redundant here.",
                                m = m.format()
                            ),
                            DiagnosticKind::Advice,
                            span,
                        );
                    }
                }
                _ => {}
            }
        }

        if op_count == modified.modifier.value.args() {
            // Inlining
            if self.inline_modifier(&modified, call)? {
                return Ok(());
            }
        } else {
            // Validate operand count
            return Err(self.fatal_error(
                modified.modifier.span.clone(),
                format!(
                    "{} requires {} function argument{}, but {} {} provided",
                    modified.modifier.value,
                    modified.modifier.value.args(),
                    if modified.modifier.value.args() == 1 {
                        ""
                    } else {
                        "s"
                    },
                    op_count,
                    if op_count == 1 { "was" } else { "were" }
                ),
            ));
        }

        let instrs = self.compile_words(modified.operands, false)?;

        // Reduce monadic deprectation message
        if let (Modifier::Primitive(Primitive::Reduce), [Instr::PushFunc(f)]) =
            (&modified.modifier.value, instrs.as_slice())
        {
            if f.signature().args == 1 {
                self.emit_diagnostic(
                    format!(
                        "{} with a monadic function is deprecated. \
                        Prefer {} with stack array notation.",
                        Primitive::Reduce.format(),
                        Primitive::Un.format()
                    ),
                    DiagnosticKind::Warning,
                    modified.modifier.span.clone(),
                );
            }
        }

        if call {
            self.push_all_instrs(instrs);
            match modified.modifier.value {
                Modifier::Primitive(prim) => self.primitive(prim, modified.modifier.span, true),
                Modifier::Ident(ident) => self.ident(ident, modified.modifier.span, true)?,
            }
        } else {
            self.new_functions.push(EcoVec::new());
            self.push_all_instrs(instrs);
            match modified.modifier.value {
                Modifier::Primitive(prim) => {
                    self.primitive(prim, modified.modifier.span.clone(), true)
                }
                Modifier::Ident(ident) => {
                    self.ident(ident, modified.modifier.span.clone(), true)?
                }
            }
            let instrs = self.new_functions.pop().unwrap();
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
                Err(e) => {
                    self.add_error(
                        modified.modifier.span.clone(),
                        format!("Cannot infer function signature: {e}"),
                    );
                }
            }
        }
        Ok(())
    }
    fn inline_modifier(&mut self, modified: &Modified, call: bool) -> UiuaResult<bool> {
        use Primitive::*;
        let Modifier::Primitive(prim) = modified.modifier.value else {
            return Ok(false);
        };
        match prim {
            Dip | Gap => {
                // Compile operands
                let (mut instrs, sig) = self.compile_operand_words(modified.operands.clone())?;
                // Dip (|1 …) . diagnostic
                if prim == Dip && sig == (1, 1) {
                    if let Some(Instr::Prim(Dup, dup_span)) =
                        self.new_functions.last().and_then(|instrs| instrs.last())
                    {
                        let span = Span::Code(modified.modifier.span.clone())
                            .merge(self.get_span(*dup_span));
                        self.emit_diagnostic(
                            "Prefer `⊃∘(…)` over `⊙(…).` for clarity",
                            DiagnosticKind::Style,
                            span,
                        );
                    }
                }

                let span = self.add_span(modified.modifier.span.clone());
                let sig = match prim {
                    Dip => {
                        instrs.insert(
                            0,
                            Instr::PushTemp {
                                stack: TempStack::Inline,
                                count: 1,
                                span,
                            },
                        );
                        instrs.push(Instr::PopTemp {
                            stack: TempStack::Inline,
                            count: 1,
                            span,
                        });
                        Signature::new(sig.args + 1, sig.outputs + 1)
                    }
                    Gap => {
                        instrs.insert(0, Instr::Prim(Pop, span));
                        Signature::new(sig.args + 1, sig.outputs)
                    }
                    _ => unreachable!(),
                };
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Fork => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                let count = a_sig.args.max(b_sig.args);
                let mut instrs = vec![Instr::PushTemp {
                    stack: TempStack::Inline,
                    count,
                    span,
                }];
                if b_sig.args > 0 {
                    instrs.push(Instr::CopyFromTemp {
                        stack: TempStack::Inline,
                        offset: count - b_sig.args,
                        count: b_sig.args,
                        span,
                    });
                }
                instrs.extend(b_instrs);
                if count - a_sig.args > 0 {
                    instrs.push(Instr::DropTemp {
                        stack: TempStack::Inline,
                        count: count - a_sig.args,
                        span,
                    });
                }
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args.max(b_sig.args), a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Cascade => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                let count = a_sig.args.saturating_sub(b_sig.outputs);
                if a_sig.args < b_sig.outputs {
                    self.emit_diagnostic(
                        format!(
                            "{}'s second function has more outputs \
                            than its first function has arguments, \
                            so {} is redundant here.",
                            prim.format(),
                            prim.format()
                        ),
                        DiagnosticKind::Advice,
                        modified.modifier.span.clone(),
                    );
                }
                let mut instrs = Vec::new();
                if count > 0 {
                    instrs.push(Instr::CopyToTemp {
                        stack: TempStack::Inline,
                        count,
                        span,
                    });
                }
                instrs.extend(b_instrs);
                if count > 0 {
                    instrs.push(Instr::PopTemp {
                        stack: TempStack::Inline,
                        count,
                        span,
                    });
                }
                instrs.extend(a_instrs);
                let sig = Signature::new(
                    b_sig.args.max(count),
                    a_sig.outputs.max(count.saturating_sub(b_sig.outputs)),
                );
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Bracket => {
                let mut operands = modified.code_operands().cloned();
                let (a_instrs, a_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let (b_instrs, b_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                let mut instrs = vec![Instr::PushTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                }];
                instrs.extend(b_instrs);
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: a_sig.args,
                    span,
                });
                instrs.extend(a_instrs);
                let sig = Signature::new(a_sig.args + b_sig.args, a_sig.outputs + b_sig.outputs);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Un => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let span = f.span.clone();
                let (instrs, _) = self.compile_operand_words(vec![f])?;
                if let Some(inverted) = invert_instrs(&instrs, self) {
                    let sig = instrs_signature(&inverted).map_err(|e| {
                        self.fatal_error(
                            span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    if call {
                        self.push_all_instrs(inverted);
                    } else {
                        let id = FunctionId::Anonymous(modified.modifier.span.clone());
                        let func = self.add_function(id, sig, inverted);
                        self.push_instr(Instr::PushFunc(func));
                    }
                } else {
                    return Err(self.fatal_error(span, "No inverse found"));
                }
            }
            Under => {
                let mut operands = modified.code_operands().cloned();
                let f = operands.next().unwrap();
                let f_span = f.span.clone();
                let (f_instrs, _) = self.compile_operand_words(vec![f])?;
                let (g_instrs, g_sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                if let Some((f_before, f_after)) = under_instrs(&f_instrs, g_sig, self) {
                    let before_sig = instrs_signature(&f_before).map_err(|e| {
                        self.fatal_error(
                            f_span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    let after_sig = instrs_signature(&f_after).map_err(|e| {
                        self.fatal_error(
                            f_span.clone(),
                            format!("Cannot infer function signature: {e}"),
                        )
                    })?;
                    let mut instrs = if call {
                        eco_vec![Instr::PushSig(before_sig)]
                    } else {
                        EcoVec::new()
                    };
                    instrs.extend(f_before);
                    if call {
                        instrs.push(Instr::PopSig);
                    }
                    instrs.extend(g_instrs);
                    if call {
                        instrs.push(Instr::PushSig(after_sig));
                    }
                    instrs.extend(f_after);
                    if call {
                        instrs.push(Instr::PopSig);
                    }
                    if call {
                        self.push_all_instrs(instrs);
                    } else {
                        let sig = Signature::new(
                            (before_sig.args + g_sig.args + after_sig.args)
                                .saturating_sub(before_sig.outputs + g_sig.outputs)
                                .max(before_sig.args),
                            (before_sig.outputs + g_sig.outputs)
                                .saturating_sub(g_sig.args + after_sig.args)
                                + after_sig.outputs,
                        );
                        let func = self.add_function(
                            FunctionId::Anonymous(modified.modifier.span.clone()),
                            sig,
                            instrs,
                        );
                        self.push_instr(Instr::PushFunc(func));
                    }
                } else {
                    return Err(self.fatal_error(f_span, "No inverse found"));
                }
            }
            Both => {
                let mut operands = modified.code_operands().cloned();
                let (mut instrs, sig) =
                    self.compile_operand_words(vec![operands.next().unwrap()])?;
                let span = self.add_span(modified.modifier.span.clone());
                instrs.insert(
                    0,
                    Instr::PushTemp {
                        stack: TempStack::Inline,
                        count: sig.args,
                        span,
                    },
                );
                instrs.push(Instr::PopTemp {
                    stack: TempStack::Inline,
                    count: sig.args,
                    span,
                });
                for i in 1..instrs.len() - 1 {
                    instrs.push(instrs[i].clone());
                }
                let sig = Signature::new(sig.args * 2, sig.outputs * 2);
                if call {
                    self.push_instr(Instr::PushSig(sig));
                    self.push_all_instrs(instrs);
                    self.push_instr(Instr::PopSig);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Bind => {
                let operand = modified.code_operands().next().cloned().unwrap();
                let operand_span = operand.span.clone();
                self.scope.locals.push(HashSet::new());
                let (mut instrs, mut sig) = self.compile_operand_words(vec![operand])?;
                let locals = self.scope.locals.pop().unwrap();
                let local_count = locals.into_iter().max().map_or(0, |i| i + 1);
                let span = self.add_span(modified.modifier.span.clone());
                sig.args += local_count;
                if sig.args < 3 {
                    self.emit_diagnostic(
                        format!(
                            "{} should be reserved for functions with at least 3 arguments, \
                            but this function has {} arguments",
                            Bind.format(),
                            sig.args
                        ),
                        DiagnosticKind::Advice,
                        operand_span,
                    );
                }
                instrs.insert(
                    0,
                    Instr::PushLocals {
                        count: sig.args,
                        span,
                    },
                );
                instrs.push(Instr::PopLocals);
                if call {
                    self.push_all_instrs(instrs);
                } else {
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Comptime => {
                let mut operands = modified.code_operands().cloned();
                let (instrs, sig) = self.compile_operand_words(vec![operands.next().unwrap()])?;
                if sig.args > 0 {
                    self.add_error(
                        modified.modifier.span.clone(),
                        format!(
                            "{}'s function must have no arguments, but it has {}",
                            Comptime.format(),
                            sig.args
                        ),
                    );
                    return Ok(false);
                }
                let instrs = optimize_instrs(instrs, true);
                let mut asm = self.asm.clone();
                let start = asm.instrs.len();
                let len = instrs.len();
                asm.instrs.extend(instrs);
                asm.top_slices.push(FuncSlice { start, len });
                let mut env = Uiua::with_backend(self.backend.clone());
                env.run_asm(&asm)?;
                let values = env.take_stack();
                if !call {
                    self.new_functions.push(EcoVec::new());
                }
                let val_count = sig.outputs;
                for value in values.into_iter().rev().take(val_count).rev() {
                    self.push_instr(Instr::push(value));
                }
                if !call {
                    let instrs = self.new_functions.pop().unwrap();
                    let sig = Signature::new(0, val_count);
                    let func = self.add_function(
                        FunctionId::Anonymous(modified.modifier.span.clone()),
                        sig,
                        instrs,
                    );
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Reduce => {
                let operand = modified.code_operands().next().cloned().unwrap();
                let mut instrs = self.compile_operand_words(vec![operand])?.0;
                if let [Instr::PushFunc(_), Instr::Prim(Content, span)] = instrs.as_slice() {
                    let span = *span;
                    *instrs.make_mut().last_mut().unwrap() =
                        Instr::ImplPrim(ImplPrimitive::ReduceContent, span);
                    let sig = instrs_signature(&instrs).unwrap_or(Signature::new(1, 1));
                    if call {
                        self.push_all_instrs(instrs);
                    } else {
                        let func = self.add_function(
                            FunctionId::Anonymous(modified.modifier.span.clone()),
                            sig,
                            instrs,
                        );
                        self.push_instr(Instr::PushFunc(func));
                    }
                } else {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        }
        self.handle_primitive_experimental(prim, &modified.modifier.span);
        self.handle_primitive_deprecation(prim, &modified.modifier.span);
        Ok(true)
    }
    fn handle_primitive_deprecation(&mut self, prim: Primitive, span: &CodeSpan) {
        if let Some(suggestion) = prim.deprecation_suggestion() {
            let suggestion = if suggestion.is_empty() {
                String::new()
            } else {
                format!(", {suggestion}")
            };
            self.emit_diagnostic(
                format!(
                    "{} is deprecated and will be removed in a future version{}",
                    prim.format(),
                    suggestion
                ),
                DiagnosticKind::Warning,
                span.clone(),
            );
        }
    }
    fn handle_primitive_experimental(&mut self, prim: Primitive, span: &CodeSpan) {
        if prim.is_experimental() && !self.scope.experimental {
            self.add_error(
                span.clone(),
                format!(
                    "{} is experimental. To use it, add \
                    `# Experimental!` to the top of the file.",
                    prim.format()
                ),
            );
        }
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) {
        self.handle_primitive_experimental(prim, &span);
        self.handle_primitive_deprecation(prim, &span);
        let span_i = self.add_span(span.clone());
        if call {
            self.push_instr(Instr::Prim(prim, span_i));
        } else {
            let instrs = [Instr::Prim(prim, span_i)];
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    let func = self.add_function(FunctionId::Primitive(prim), sig, instrs);
                    self.push_instr(Instr::PushFunc(func))
                }
                Err(e) => {
                    self.add_error(span, format!("Cannot infer function signature: {e}"));
                }
            }
        }
    }

    fn increment_placeholders(
        &mut self,
        f: &Function,
        curr: &mut usize,
        map: &mut HashMap<usize, usize>,
    ) {
        let len = f.instrs(self).len();
        for i in 0..len {
            match &mut f.instrs_mut(self)[i] {
                Instr::GetTempFunction { offset, span, .. } => {
                    *offset = *map.entry(*span).or_insert_with(|| {
                        let new = *curr;
                        *curr += 1;
                        new
                    });
                }
                Instr::PushFunc(f) => {
                    let f = f.clone();
                    self.increment_placeholders(&f, curr, map);
                }
                _ => (),
            }
        }
    }
    fn count_temp_functions(&self, instrs: &[Instr], counted: &mut HashSet<usize>) -> usize {
        let mut count = 0;
        for instr in instrs {
            match instr {
                Instr::GetTempFunction { span, .. } => count += counted.insert(*span) as usize,
                Instr::PushFunc(f) if matches!(f.id, FunctionId::Anonymous(_)) => {
                    count += self.count_temp_functions(f.instrs(self), counted);
                }
                _ => {}
            }
        }
        count
    }
    fn has_tracing(&self, instrs: &[Instr]) -> bool {
        for instr in instrs {
            match instr {
                Instr::Prim(
                    Primitive::Trace | Primitive::Dump | Primitive::Stack | Primitive::Assert,
                    _,
                ) => return true,
                Instr::ImplPrim(
                    ImplPrimitive::InvTrace | ImplPrimitive::InvDump | ImplPrimitive::InvStack,
                    _,
                ) => return true,
                Instr::PushFunc(f) if self.has_tracing(f.instrs(self)) => return true,
                _ => {}
            }
        }
        false
    }
    /// Get all diagnostics
    pub fn diagnostics(&self) -> &BTreeSet<Diagnostic> {
        &self.diagnostics
    }
    /// Get all diagnostics mutably
    pub fn diagnostics_mut(&mut self) -> &mut BTreeSet<Diagnostic> {
        &mut self.diagnostics
    }
    /// Take all diagnostics
    ///
    /// These are only available if `print_diagnostics` is `false`
    pub fn take_diagnostics(&mut self) -> BTreeSet<Diagnostic> {
        take(&mut self.diagnostics)
    }
    /// Construct and add a diagnostic with a custom span
    pub fn emit_diagnostic(
        &mut self,
        message: impl Into<String>,
        kind: DiagnosticKind,
        span: impl Into<Span>,
    ) {
        self.diagnostics.insert(Diagnostic::new(
            message.into(),
            span,
            kind,
            self.asm.inputs.clone(),
        ));
    }
    fn add_error(&mut self, span: impl Into<Span>, message: impl ToString) {
        let e = UiuaError::Run(
            span.into().sp(message.to_string()),
            self.asm.inputs.clone().into(),
        );
        self.errors.push(e);
    }
    fn fatal_error(&self, span: impl Into<Span>, message: impl ToString) -> UiuaError {
        UiuaError::Run(
            span.into().sp(message.to_string()),
            self.asm.inputs.clone().into(),
        )
    }
    /// Get a span by its index
    pub fn get_span(&self, span: usize) -> Span {
        self.asm.spans[span].clone()
    }
    /// Register a span
    pub fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let idx = self.asm.spans.len();
        self.asm.spans.push(span.into());
        idx
    }
    /// Create a function
    pub fn create_function(
        &mut self,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static,
    ) -> Function {
        let signature = signature.into();
        let index = self.asm.dynamic_functions.len();
        self.asm.dynamic_functions.push(Arc::new(f));
        self.add_function(
            FunctionId::Unnamed,
            signature,
            vec![Instr::Dynamic(DynamicFunction { index, signature })],
        )
    }
    /// Bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error in the binding name is not valid
    pub fn bind_function(&mut self, name: impl Into<EcoString>, function: Function) -> UiuaResult {
        let index = self.next_global;
        let name = name.into();
        self.compile_bind_function(&name, index, function, 0, None)?;
        self.next_global += 1;
        self.scope.names.insert(name, index);
        Ok(())
    }
    /// Create and bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error in the binding name is not valid
    pub fn create_bind_function(
        &mut self,
        name: impl Into<EcoString>,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static,
    ) -> UiuaResult {
        let function = self.create_function(signature, f);
        self.bind_function(name, function)
    }
}

fn words_look_pervasive(words: &[Sp<Word>]) -> bool {
    use Primitive::*;
    words.iter().all(|word| match &word.value {
        Word::Primitive(p) if p.class().is_pervasive() => true,
        Word::Primitive(
            Dup | Flip | Over | Dip | Identity | Fork | Both | Bracket | Under | Each,
        ) => true,
        Word::Func(func) if func.lines.iter().all(|line| words_look_pervasive(line)) => true,
        Word::Number(..) | Word::Char(..) => true,
        Word::Modified(m) if m.modifier.value == Modifier::Primitive(Primitive::Each) => true,
        _ => false,
    })
}
