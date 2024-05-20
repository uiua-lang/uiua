mod binding;
mod modifier;

use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet},
    fmt, fs,
    hash::{Hash, Hasher},
    iter::repeat,
    mem::{replace, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Arc,
};

use ecow::{eco_vec, EcoString, EcoVec};
use indexmap::IndexMap;
use instant::Duration;

use crate::{
    algorithm::invert::{invert_instrs, under_instrs},
    ast::*,
    check::{instrs_all_signatures, instrs_signature, SigCheckError, SigCheckErrorKind},
    example_ua,
    format::format_word,
    function::*,
    ident_modifier_args,
    lex::{CodeSpan, Sp, Span},
    lsp::{CodeMeta, SigDecl},
    optimize::{optimize_instrs, optimize_instrs_mut},
    parse::{count_placeholders, parse, split_words, unsplit_words},
    Array, Assembly, BindingKind, Boxed, Diagnostic, DiagnosticKind, DocComment, Ident,
    ImplPrimitive, InputSrc, IntoInputSrc, IntoSysBackend, Primitive, RunMode, SemanticComment,
    SysBackend, Uiua, UiuaError, UiuaResult, Value, CONSTANTS, VERSION,
};

/// The Uiua compiler
#[derive(Clone)]
pub struct Compiler {
    pub(crate) asm: Assembly,
    pub(crate) code_meta: CodeMeta,
    /// Functions which are under construction
    new_functions: Vec<EcoVec<Instr>>,
    /// The name of the current binding
    current_binding: Option<CurrentBinding>,
    /// The index of the next global binding
    next_global: usize,
    /// The current scope
    scope: Scope,
    /// Ancestor scopes of the current one
    higher_scopes: Vec<Scope>,
    /// Determines which How test scopes are run
    mode: RunMode,
    /// The paths of files currently being imported (used to detect import cycles)
    current_imports: Vec<PathBuf>,
    /// The bindings of imported files
    imports: HashMap<PathBuf, Import>,
    /// Unexpanded stack macros
    stack_macros: HashMap<usize, StackMacro>,
    /// Unexpanded array macros
    array_macros: HashMap<usize, ArrayMacro>,
    /// The depth of macro expansion
    macro_depth: usize,
    /// Whether the compiler is in an inverse
    in_inverse: bool,
    /// Accumulated errors
    errors: Vec<UiuaError>,
    /// Primitives that have emitted errors because they are experimental
    experimental_prim_errors: HashSet<Primitive>,
    /// Primitives that have emitted errors because they are deprecated
    deprecated_prim_errors: HashSet<Primitive>,
    /// Whether an error has been emitted for experimental function strand
    experimental_function_strand_error: bool,
    /// Accumulated diagnostics
    diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    print_diagnostics: bool,
    /// Whether to evaluate comptime code
    comptime: bool,
    /// The comptime mode
    pre_eval_mode: PreEvalMode,
    /// The interpreter used for comptime code
    macro_env: Uiua,
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler {
            asm: Assembly::default(),
            code_meta: CodeMeta::default(),
            new_functions: Vec::new(),
            current_binding: None,
            next_global: 0,
            scope: Scope::default(),
            higher_scopes: Vec::new(),
            mode: RunMode::All,
            current_imports: Vec::new(),
            imports: HashMap::new(),
            stack_macros: HashMap::new(),
            array_macros: HashMap::new(),
            macro_depth: 0,
            in_inverse: false,
            errors: Vec::new(),
            experimental_prim_errors: HashSet::new(),
            deprecated_prim_errors: HashSet::new(),
            experimental_function_strand_error: false,
            diagnostics: BTreeSet::new(),
            print_diagnostics: false,
            comptime: true,
            pre_eval_mode: PreEvalMode::default(),
            macro_env: Uiua::default(),
        }
    }
}

/// An imported module
#[derive(Clone)]
pub struct Import {
    /// The top level comment
    pub comment: Option<EcoString>,
    /// Map module-local names to global indices
    names: IndexMap<Ident, LocalName>,
    /// Whether the import uses experimental features
    experimental: bool,
}

#[derive(Clone)]
struct StackMacro {
    words: Vec<Sp<Word>>,
    names: IndexMap<Ident, LocalName>,
}

#[derive(Clone)]
struct ArrayMacro {
    function: Function,
    names: IndexMap<Ident, LocalName>,
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
struct CurrentBinding {
    name: Ident,
    signature: Option<Signature>,
    referenced: bool,
    global_index: usize,
}

/// A scope where names are defined
#[derive(Clone)]
pub(crate) struct Scope {
    kind: ScopeKind,
    /// The name of the current file, if any
    file_path: Option<PathBuf>,
    /// The top level comment
    comment: Option<EcoString>,
    /// Map local names to global indices
    names: IndexMap<Ident, LocalName>,
    /// Whether to allow experimental features
    experimental: bool,
    /// The stack height between top-level statements
    stack_height: Result<usize, Sp<SigCheckError>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ScopeKind {
    /// A scope at the top level of a file
    File,
    /// A temporary scope, probably for a macro
    Temp,
    /// A test scope between `---`s
    Test,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            kind: ScopeKind::File,
            file_path: None,
            comment: None,
            names: IndexMap::new(),
            experimental: false,
            stack_height: Ok(0),
        }
    }
}

/// The index of a named local in the bindings, and whether it is public
#[derive(Clone, Copy)]
pub(crate) struct LocalName {
    pub index: usize,
    pub public: bool,
}

/// The mode that dictates how much code to pre-evaluate at compile time
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PreEvalMode {
    /// The normal mode. Tries to evaluate pure, time-bounded constants and expressions at comptime
    #[default]
    Normal,
    /// Does not evalute pure constants and expressions at comptime, but still evaluates `comptime`
    Lazy,
    /// Evaluate as much as possible at compile time, even impure expressions
    ///
    /// Recursive functions and certain system functions are not evaluated
    Lsp,
}

impl PreEvalMode {
    fn matches_instrs(&self, instrs: &[Instr], asm: &Assembly) -> bool {
        match self {
            PreEvalMode::Normal => {
                instrs_are_pure(instrs, asm, Purity::Pure) && instrs_are_limit_bounded(instrs, asm)
            }
            PreEvalMode::Lazy => false,
            PreEvalMode::Lsp => {
                instrs_are_pure(instrs, asm, Purity::Impure)
                    && instrs_are_limit_bounded(instrs, asm)
            }
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
            macro_env: Uiua::with_backend(backend.into_sys_backend()),
            ..Self::default()
        }
    }
    /// Set the compiler's assembly
    pub fn with_assembly(self, asm: Assembly) -> Self {
        Self { asm, ..self }
    }
    /// Get a reference to the assembly
    pub fn assembly(&self) -> &Assembly {
        &self.asm
    }
    /// Get a mutable reference to the assembly
    pub fn assembly_mut(&mut self) -> &mut Assembly {
        &mut self.asm
    }
    /// Get a reference to the code metadata
    pub fn code_meta(&self) -> &CodeMeta {
        &self.code_meta
    }
    /// Get a mutable reference to the code metadata
    pub fn code_meta_mut(&mut self) -> &mut CodeMeta {
        &mut self.code_meta
    }
    /// Take a completed assembly from the compiler
    pub fn finish(&mut self) -> Assembly {
        take(&mut self.asm)
    }
    /// Set whether to evaluate `comptime`
    pub fn comptime(&mut self, comptime: bool) -> &mut Self {
        self.comptime = comptime;
        self
    }
    /// Set the [`PreEvalMode`]
    pub fn pre_eval_mode(&mut self, mode: PreEvalMode) -> &mut Self {
        self.pre_eval_mode = mode;
        self
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
    /// Get the backend
    pub fn backend(&self) -> Arc<dyn SysBackend> {
        self.macro_env.rt.backend.clone()
    }
    /// Attempt to downcast the system backend to a concrete reference type
    pub fn downcast_backend<T: SysBackend>(&self) -> Option<&T> {
        self.macro_env.downcast_backend()
    }
    /// Attempt to downcast the system backend to a concrete mutable type
    pub fn downcast_backend_mut<T: SysBackend>(&mut self) -> Option<&mut T> {
        self.macro_env.downcast_backend_mut()
    }
    /// Take the system backend
    pub fn take_backend<T: SysBackend>(&mut self) -> Option<T>
    where
        T: Default,
    {
        self.macro_env.take_backend()
    }
    /// Compile a Uiua file from a file at a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input: EcoString = fs::read_to_string(path)
            .map_err(|e| UiuaError::Load(path.into(), e.into()))?
            .into();
        // _ = crate::lsp::spans(&input);
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
    fn in_scope<T>(
        &mut self,
        kind: ScopeKind,
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<Import> {
        let experimental = self.scope.experimental;
        self.higher_scopes.push(take(&mut self.scope));
        self.scope.kind = kind;
        self.scope.experimental = experimental;
        let res = f(self);
        let scope = replace(&mut self.scope, self.higher_scopes.pop().unwrap());
        res?;
        Ok(Import {
            comment: scope.comment,
            names: scope.names,
            experimental: scope.experimental,
        })
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
            self.scope.file_path = Some(path.to_path_buf());
        }

        let res = self.catching_crash(input, |env| env.items(items, false));

        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.report());
            }
        }

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
            _ => Err(UiuaError::Multi(take(&mut self.errors))),
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
Please report this at http://github.com/uiua-lang/uiua/issues/new \
or on Discord at https://discord.gg/9CU2ME4kmn.

Uiua version {VERSION}

code:
{input}"
            ))),
        }
    }
    pub(crate) fn items(&mut self, items: Vec<Item>, in_test: bool) -> UiuaResult {
        // Set scope comment
        if let Some(Item::Words(lines)) = items.first() {
            let mut started = false;
            let mut comment = String::new();
            for line in lines {
                for word in line {
                    match &word.value {
                        Word::Comment(c) => {
                            let mut c = c.as_str();
                            if c.starts_with(' ') {
                                c = &c[1..];
                            }
                            comment.push_str(c);
                            started = true;
                        }
                        Word::Spaces => {}
                        _ => {
                            comment.clear();
                            break;
                        }
                    }
                }
                if line.is_empty() && started {
                    break;
                }
                comment.push('\n');
            }
            if !comment.trim().is_empty() {
                self.scope.comment = Some(comment.trim().into());
            }
        }

        let mut prev_comment = None;
        for item in items {
            if let Err(e) = self.item(item, in_test, &mut prev_comment) {
                if self.errors.is_empty() {
                    self.errors.push(e);
                }
            }
        }
        Ok(())
    }
    fn item(
        &mut self,
        item: Item,
        in_test: bool,
        prev_comment: &mut Option<EcoString>,
    ) -> UiuaResult {
        fn words_should_run_anyway(words: &[Sp<Word>]) -> bool {
            (words.iter()).any(|w| matches!(&w.value, Word::SemanticComment(_)))
        }
        let mut lines = match item {
            Item::TestScope(items) => {
                prev_comment.take();
                self.in_scope(ScopeKind::Test, |env| env.items(items.value, true))?;
                return Ok(());
            }
            Item::Words(lines) => lines,
            Item::Binding(binding) => {
                let can_run = match self.mode {
                    RunMode::Normal => !in_test,
                    RunMode::All | RunMode::Test => true,
                };
                let prev_com = prev_comment.take();
                if can_run || words_should_run_anyway(&binding.words) {
                    self.binding(binding, prev_com)?;
                }
                return Ok(());
            }
            Item::Import(import) => return self.import(import, prev_comment.take()),
        };

        // Compile top-level words

        // Get top-level comments
        for line in &lines {
            if let [Sp {
                value: Word::Comment(com),
                ..
            }] = line.as_slice()
            {
                if let Some(curr_com) = prev_comment {
                    curr_com.push('\n');
                    curr_com.push_str(com);
                } else {
                    *prev_comment = Some(com.as_str().into());
                }
            } else {
                *prev_comment = None;
            }
        }
        let can_run = match self.mode {
            RunMode::Normal => !in_test,
            RunMode::Test => in_test,
            RunMode::All => true,
        };
        lines = unsplit_words(lines.into_iter().flat_map(split_words));
        for line in lines {
            if line.is_empty() || !can_run && !words_should_run_anyway(&line) {
                continue;
            }
            let span =
                (line.first().unwrap().span.clone()).merge(line.last().unwrap().span.clone());
            if count_placeholders(&line) > 0 {
                self.add_error(span.clone(), "Cannot use placeholder outside of function");
            }
            let all_literal = line.iter().filter(|w| w.value.is_code()).all(|w| {
                matches!(
                    w.value,
                    Word::Char(_) | Word::Number(..) | Word::String(_) | Word::MultilineString(_)
                )
            });
            // Compile the words
            let instr_count_before = self.asm.instrs.len();
            let instrs = self.compile_words(line, true)?;
            let (mut instrs, pre_eval_errors) = self.pre_eval_instrs(instrs);
            let mut line_eval_errored = false;
            match instrs_signature(&instrs) {
                Ok(sig) => {
                    // Update scope stack height
                    if let Ok(height) = &mut self.scope.stack_height {
                        *height = (*height + sig.outputs).saturating_sub(sig.args);
                    }
                    // Try to evaluate at comptime
                    // This can be done when there are at least as many push instructions
                    // preceding the current line as there are arguments to the line
                    if !instrs.is_empty()
                        && instr_count_before >= sig.args
                        && (self.asm.instrs.iter().take(instr_count_before).rev())
                            .take(sig.args)
                            .all(|instr| matches!(instr, Instr::Push(_)))
                    {
                        // The instructions for evaluation are the preceding push
                        // instructions, followed by the current line
                        let mut comp_instrs = EcoVec::from(
                            &self.asm.instrs
                                [instr_count_before.saturating_sub(sig.args)..instr_count_before],
                        );
                        comp_instrs.extend(instrs.iter().cloned());
                        match self.comptime_instrs(comp_instrs) {
                            Ok(Some(vals)) => {
                                // Track top level values
                                if !all_literal {
                                    self.code_meta.top_level_values.insert(span, vals.clone());
                                }
                                // Truncate instrs
                                self.asm.instrs.truncate(instr_count_before - sig.args);
                                // Truncate top slices
                                let mut remaining = sig.args;
                                while let Some(slice) = self.asm.top_slices.last_mut() {
                                    let to_sub = slice.len.min(remaining);
                                    slice.len -= to_sub;
                                    remaining -= to_sub;
                                    if remaining == 0 {
                                        break;
                                    }
                                    if slice.len == 0 {
                                        self.asm.top_slices.pop();
                                    }
                                }
                                // Set instrs
                                instrs = vals.into_iter().map(Instr::push).collect();
                            }
                            Ok(None) => {}
                            Err(e) => {
                                self.errors.push(e);
                                line_eval_errored = true;
                            }
                        }
                    }
                }
                Err(e) => self.scope.stack_height = Err(span.sp(e)),
            }
            if !line_eval_errored {
                self.errors.extend(pre_eval_errors);
            }
            let start = self.asm.instrs.len();
            (self.asm.instrs).extend(instrs);
            let end = self.asm.instrs.len();
            self.asm.top_slices.push(FuncSlice {
                start,
                len: end - start,
            });
        }
        Ok(())
    }
    #[must_use]
    pub(crate) fn make_function<I>(&mut self, id: FunctionId, sig: Signature, instrs: I) -> Function
    where
        I: IntoIterator<Item = Instr> + fmt::Debug,
        I::IntoIter: ExactSizeIterator,
    {
        let (instrs, errors) = self.pre_eval_instrs(instrs.into_iter().collect());
        self.errors.extend(errors);
        let len = instrs.len();
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("({id}").into()));
        }
        let start = self.asm.instrs.len();
        let mut hasher = DefaultHasher::new();
        instrs.hash(&mut hasher);
        let hash = hasher.finish();
        self.asm.instrs.extend(instrs);
        if len > 1 {
            (self.asm.instrs).push(Instr::Comment(format!("{id})").into()));
        }
        Function::new(id, sig, FuncSlice { start, len }, hash)
    }
    fn compile_bind_function(
        &mut self,
        name: &Ident,
        local: LocalName,
        function: Function,
        span: usize,
        comment: Option<&str>,
    ) -> UiuaResult {
        let comment = comment.map(|text| {
            let comment = DocComment::from(text);
            if let Some(sig) = &comment.sig {
                if !sig.matches_sig(function.signature()) {
                    self.emit_diagnostic(
                        format!(
                            "{}'s comment describes {}, \
                            but its code has signature {}",
                            name,
                            sig.sig_string(),
                            function.signature(),
                        ),
                        DiagnosticKind::Warning,
                        self.get_span(span).clone().code().unwrap(),
                    );
                }
            }
            comment
        });
        self.scope.names.insert(name.clone(), local);
        self.asm.bind_function(local, function, span, comment);
        Ok(())
    }
    fn compile_bind_const(
        &mut self,
        name: &Ident,
        local: LocalName,
        value: Option<Value>,
        span: usize,
        comment: Option<&str>,
    ) {
        let span = self.get_span(span).clone().code();
        let comment = comment.map(|text| {
            let comment = DocComment::from(text);
            if let Some(sig) = &comment.sig {
                self.emit_diagnostic(
                    format!(
                        "{}'s comment describes {}, \
                        but it is a constant",
                        name,
                        sig.sig_string(),
                    ),
                    DiagnosticKind::Warning,
                    span.clone().unwrap(),
                );
            }
            comment
        });
        self.asm
            .add_global_at(local, BindingKind::Const(value), span, comment);
    }
    /// Import a module
    pub(crate) fn import_module(&mut self, path_str: &str, span: &CodeSpan) -> UiuaResult<PathBuf> {
        // Resolve path
        let path = if let Some(mut url) = path_str.strip_prefix("git:") {
            let mut branch = None;
            if let Some((a, b)) = url.split_once("branch:") {
                url = a;
                branch = Some(b.trim());
            }
            // Git import
            let mut url = url.trim().trim_end_matches(".git").to_string();
            if ![".com", ".net", ".org", ".io", ".dev"]
                .iter()
                .any(|s| url.contains(s))
            {
                if !url.starts_with('/') {
                    url = format!("/{url}");
                }
                url = format!("github.com{url}");
            }
            if !(url.starts_with("https://") || url.starts_with("http://")) {
                url = format!("https://{url}");
            }
            self.backend()
                .load_git_module(&url, branch)
                .map_err(|e| self.fatal_error(span.clone(), e))?
        } else {
            // Normal import
            self.resolve_import_path(Path::new(path_str))
        };
        if self.imports.get(&path).is_none() {
            let bytes = self
                .backend()
                .file_read_all(&path)
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
            if self.current_imports.iter().any(|p| p == &path) {
                return Err(self.fatal_error(
                    span.clone(),
                    format!("Cycle detected importing {}", path.to_string_lossy()),
                ));
            }
            let import = self.in_scope(ScopeKind::File, |env| {
                env.load_str_src(&input, &path).map(drop)
            })?;
            self.imports.insert(path.clone(), import);
        }
        let import = self.imports.get(&path).unwrap();
        if import.experimental && !self.scope.experimental {
            self.add_error(
                span.clone(),
                format!(
                    "Module `{path_str}` is experimental. \
                    To use it, add `# Experimental!` to the top of this file."
                ),
            );
        }
        Ok(path)
    }
    /// Resolve a declared import path relative to the path of the file that is being executed
    pub(crate) fn resolve_import_path(&self, path: &Path) -> PathBuf {
        let mut target = if let Some(parent) = self.current_imports.last().and_then(|p| p.parent())
        {
            parent.join(path)
        } else {
            path.to_path_buf()
        };
        if !target.exists() && target.extension().is_none() {
            target = target.with_extension("ua");
        }
        let base = Path::new(".");
        if let (Ok(canon_target), Ok(canon_base)) = (target.canonicalize(), base.canonicalize()) {
            pathdiff::diff_paths(canon_target, canon_base).unwrap_or(target)
        } else {
            pathdiff::diff_paths(&target, base).unwrap_or(target)
        }
    }
    fn compile_words(
        &mut self,
        words: impl IntoIterator<Item = Sp<Word>>,
        call: bool,
    ) -> UiuaResult<EcoVec<Instr>> {
        let words = unsplit_words(split_words(words))
            .into_iter()
            .flatten()
            .collect();

        self.new_functions.push(EcoVec::new());
        self.words(words, call)?;
        Ok(self.new_functions.pop().unwrap())
    }
    fn compile_operand_word(&mut self, word: Sp<Word>) -> UiuaResult<(EcoVec<Instr>, Signature)> {
        let span = word.span.clone();
        let mut instrs = self.compile_words([word], true)?;
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
            self.sig_of(&instrs, &span)?
        };
        let instrs = optimize_instrs(instrs, false, &self.asm);
        Ok((instrs, sig))
    }
    fn words(&mut self, mut words: Vec<Sp<Word>>, call: bool) -> UiuaResult {
        words.retain(|word| word.value.is_code());
        words.reverse();
        #[derive(Debug, Clone)]
        struct PrevWord(Option<Primitive>, Option<Signature>, CodeSpan);
        let mut a: Option<PrevWord> = None;
        let mut b: Option<PrevWord> = None;
        for word in words {
            let span = word.span.clone();
            let prim = match word.value {
                Word::Primitive(prim) => Some(prim),
                _ => None,
            };

            // First select diagnostic
            if let (Some(PrevWord(Some(Primitive::Select), _, b_span)), Some(Primitive::First)) =
                (&b, prim)
            {
                self.emit_diagnostic(
                    format!(
                        "Flip the order of {} and {} to improve performance",
                        Primitive::First.format(),
                        Primitive::Select.format()
                    ),
                    DiagnosticKind::Advice,
                    b_span.clone().merge(span.clone()),
                );
            }
            match (a, &b, prim) {
                // Flip monadic dup diagnostic
                (
                    Some(PrevWord(Some(Primitive::Dup), _, a_span)),
                    Some(PrevWord(
                        _,
                        Some(Signature {
                            args: 1,
                            outputs: 1,
                        }),
                        _,
                    )),
                    Some(Primitive::Flip),
                ) => {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {} {} here",
                            Primitive::On,
                            Primitive::Flip,
                            Primitive::Dup
                        ),
                        DiagnosticKind::Style,
                        a_span.merge(span.clone()),
                    );
                }
                // Keep unique dup diagnostic
                (
                    Some(PrevWord(Some(Primitive::Dup), _, a_span)),
                    Some(PrevWord(Some(Primitive::Unique), _, _)),
                    Some(Primitive::Keep),
                ) => {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {}{}{}",
                            Primitive::Deduplicate.format(),
                            Primitive::Keep,
                            Primitive::Unique,
                            Primitive::Dup
                        ),
                        DiagnosticKind::Advice,
                        a_span.merge(span.clone()),
                    );
                }
                _ => {}
            }

            let start = self.new_functions.last().unwrap().len();

            // Compile the word
            self.word(word, call)?;

            let new_functions = self.new_functions.last().unwrap();
            let sig = if new_functions.len() >= start {
                instrs_signature(&new_functions[start..]).ok()
            } else {
                None
            };
            a = b;
            b = Some(PrevWord(prim, sig, span));
        }
        Ok(())
    }
    /// Push an instruction to the current function being compiled
    ///
    /// Also performs some optimizations if the instruction and the previous
    /// instruction form some known pattern
    fn push_instr(&mut self, instr: Instr) {
        let instrs = self.new_functions.last_mut().unwrap();
        optimize_instrs_mut(instrs, instr, false, &self.asm);
    }
    fn push_all_instrs(&mut self, instrs: impl IntoIterator<Item = Instr>) {
        for instr in instrs {
            self.push_instr(instr);
        }
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> UiuaResult {
        match word.value {
            Word::Number(_, n) => {
                let mut instr = Instr::push(n);
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::Char(c) => {
                let val: Value = if c.chars().count() == 1 {
                    c.chars().next().unwrap().into()
                } else {
                    c.into()
                };
                let mut instr = Instr::push(val);
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::String(s) | Word::MultilineString(s) => {
                let mut instr = Instr::push(s);
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::Label(label) => {
                if !self.scope.experimental {
                    self.add_error(
                        word.span.clone(),
                        "Labels are experimental. To use them, add \
                        `# Experimental!` to the top of the file.",
                    );
                }
                let mut instr = Instr::Label {
                    label: label.into(),
                    span: self.add_span(word.span.clone()),
                };
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::FormatString(frags) => {
                let signature = Signature::new(frags.len() - 1, 1);
                let parts = frags.into_iter().map(Into::into).collect();
                let span = self.add_span(word.span.clone());
                let mut instr = Instr::Format { parts, span };
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        signature,
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::MultilineFormatString(lines) => {
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
                let mut instr = Instr::Format { parts, span };
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        signature,
                        vec![instr],
                    ));
                }
                self.push_instr(instr);
            }
            Word::Ref(r) => self.reference(r, call)?,
            Word::IncompleteRef { path, in_macro_arg } => {
                if let Some((_, locals)) = self.ref_path(&path, in_macro_arg)? {
                    self.add_error(
                        path.last().unwrap().tilde_span.clone(),
                        "Incomplete module reference",
                    );
                    for (local, comp) in locals.iter().zip(path) {
                        self.validate_local(&comp.module.value, *local, &comp.module.span);
                        self.code_meta
                            .global_references
                            .insert(comp.module, local.index);
                    }
                    self.code_meta
                        .incomplete_refs
                        .insert(word.span.clone(), locals.last().unwrap().index);
                }
            }
            Word::Strand(items) => {
                // Track span for LSP
                let just_spans: Vec<_> = items.iter().map(|w| w.span.clone()).collect();
                // Compile individual items
                let op_instrs = items
                    .into_iter()
                    .rev()
                    .map(|word| self.compile_operand_word(word))
                    .collect::<UiuaResult<Vec<_>>>()?;
                // Check item sigs
                let has_functions = op_instrs.iter().any(|(_, sig)| sig.args > 0);
                if has_functions {
                    return Err(self.fatal_error(
                        word.span.clone(),
                        "Functions are not allowed in strands. \n\
                        You can use the ‿ character (which formats from __) \
                        to make a function strand.",
                    ));
                }
                self.code_meta.strands.insert(word.span.clone(), just_spans);
                // Flatten instrs
                let inner: Vec<Instr> = op_instrs
                    .into_iter()
                    .flat_map(|(instrs, _)| instrs)
                    .collect();

                // Normal strand
                if !call {
                    self.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
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

                let span_index = self.add_span(word.span.clone());
                let instrs = self.new_functions.last_mut().unwrap();
                // Inline constant arrays
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    let values = inner.iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v.clone(),
                        _ => unreachable!(),
                    });
                    match Value::from_row_values(values, &(&word.span, &self.asm.inputs)) {
                        Ok(val) => {
                            instrs.pop();
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
                    let sig = instrs_signature(&instrs).unwrap();
                    let func = self.make_function(FunctionId::Anonymous(word.span), sig, instrs);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Word::Array(arr) => {
                // Track span for LSP
                if !arr.boxes
                    && (arr.lines.iter().flatten())
                        .filter(|w| w.value.is_code())
                        .all(|w| w.value.is_literal() && !matches!(w.value, Word::Strand(_)))
                {
                    let just_spans: Vec<_> = (arr.lines.iter().rev().flatten())
                        .filter(|w| w.value.is_code())
                        .map(|w| w.span.clone())
                        .collect();
                    self.code_meta.arrays.insert(word.span.clone(), just_spans);
                }

                if !call {
                    self.new_functions.push(EcoVec::new());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                let line_count = arr.lines.len();
                for lines in arr.lines.into_iter().rev() {
                    inner.extend(self.compile_words(lines, true)?);
                }
                // Validate inner loop correctness
                let inner_sig = self.validate_array_loop_sig(&inner, &word.span);
                // Validate signature
                if let Some((declared_sig, inner_sig)) = arr.signature.zip(inner_sig) {
                    if inner_sig != declared_sig.value {
                        self.add_error(
                            declared_sig.span.clone(),
                            format!(
                                "Array signature mismatch: declared {} but inferred {}",
                                declared_sig.value, inner_sig
                            ),
                        );
                    }
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
                            instrs.pop();
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
                    let func = self.make_function(FunctionId::Anonymous(word.span), sig, instrs);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Word::Undertied(words) => {
                if words.len() > 2 {
                    self.add_error(
                        word.span.clone(),
                        "Function strands cannot contain more than two items",
                    );
                }
                if !self.scope.experimental && !self.experimental_function_strand_error {
                    self.experimental_function_strand_error = true;
                    self.add_error(
                        word.span.clone(),
                        "Function strands are experimental. To use them, add \
                        `# Experimental!` to the top of the file.",
                    );
                }
                let instrs = self.compile_words(words, true)?;
                if call {
                    self.push_all_instrs(instrs);
                } else {
                    match instrs_signature(&instrs) {
                        Ok(sig) => {
                            let func =
                                self.make_function(FunctionId::Anonymous(word.span), sig, instrs);
                            self.push_instr(Instr::PushFunc(func));
                        }
                        Err(e) => {
                            return Err(self.fatal_error(
                                word.span,
                                format!(
                                    "Cannot infer function signature: {e}{}",
                                    if e.kind == SigCheckErrorKind::Ambiguous {
                                        ". A signature can be declared after the opening `(`."
                                    } else {
                                        ""
                                    }
                                ),
                            ));
                        }
                    }
                }
            }
            Word::Func(func) => self.func(func, word.span, call)?,
            Word::Switch(sw) => self.switch(sw, word.span, call)?,
            Word::Primitive(p) => self.primitive(p, word.span, call)?,
            Word::SemicolonPop => {
                self.emit_diagnostic(
                    format!(
                        "Using `;` for {} is deprecated and will be \
                        removed in the future. Type `pop` or `po` instead.",
                        Primitive::Pop.format()
                    ),
                    DiagnosticKind::Warning,
                    word.span.clone(),
                );
                self.primitive(Primitive::Pop, word.span, call)?
            }
            Word::Modified(m) => self.modified(*m, call)?,
            Word::Placeholder(_) => {
                // We could error here, but it's easier to handle it higher up
            }
            Word::StackSwizzle(sw) => self.swizzle(sw, word.span, call),
            Word::SemanticComment(sc) => match sc {
                SemanticComment::Experimental => self.scope.experimental = true,
                SemanticComment::NoInline => {
                    let mut instr = Instr::NoInline;
                    if !call {
                        let f = self.make_function(
                            FunctionId::Anonymous(word.span.clone()),
                            Signature::new(0, 0),
                            vec![Instr::NoInline],
                        );
                        instr = Instr::PushFunc(f);
                    }
                    self.push_instr(instr);
                }
                SemanticComment::Boo => {
                    self.add_error(word.span.clone(), "The compiler is scared!")
                }
            },
            Word::OutputComment { i, n } => self.push_instr(Instr::SetOutputComment { i, n }),
            Word::Comment(_) | Word::Spaces | Word::BreakLine | Word::UnbreakLine => {}
        }
        Ok(())
    }
    fn validate_array_loop_sig(&mut self, instrs: &[Instr], span: &CodeSpan) -> Option<Signature> {
        let inner_sig = instrs_signature(instrs);
        if self.current_binding.is_none() {
            return inner_sig.ok();
        }
        let Err(e) = &inner_sig else {
            return inner_sig.ok();
        };
        let before_sig = (0..instrs.len())
            .rev()
            .find_map(|i| instrs_signature(&instrs[..i]).ok())
            .unwrap();
        let after_sig = (0..=instrs.len())
            .find_map(|i| instrs_signature(&instrs[i..]).ok())
            .unwrap();
        match e.kind {
            SigCheckErrorKind::LoopExcess { sig: body_sig, inf } => {
                let positive_body_sig = body_sig.outputs > body_sig.args;
                let balanced_after_args = body_sig.args.saturating_sub(before_sig.outputs);
                if body_sig.args > 0
                    && (positive_body_sig && after_sig.args != balanced_after_args && !inf)
                    || body_sig.args == 0 && after_sig.args > 0
                {
                    let mut message = format!(
                        "This array contains a loop that has a variable \
                        number of outputs. The code left of the loop has \
                        signature {after_sig}, which may result in a variable \
                        number of values being pulled into the array."
                    );
                    if after_sig.args > body_sig.args {
                    } else {
                        let replacement: String =
                            repeat('⊙').take(body_sig.args - 1).chain(['∘']).collect();
                        message.push_str(&format!(
                            " To fix this, insert `{replacement}` to the left of the loop."
                        ));
                    }
                    self.emit_diagnostic(message, DiagnosticKind::Warning, span.clone())
                }
            }
            SigCheckErrorKind::LoopOverreach => self.emit_diagnostic(
                "This array contains a loop that has a variable \
                number of inputs. This may result in a variable \
                number of values being pulled into the array.",
                DiagnosticKind::Warning,
                span.clone(),
            ),
            _ => {}
        }
        inner_sig.ok()
    }
    fn ref_local(&self, r: &Ref) -> UiuaResult<(Vec<LocalName>, LocalName)> {
        if let Some((module, path_locals)) = self.ref_path(&r.path, r.in_macro_arg)? {
            if let Some(local) = self.imports[&module].names.get(&r.name.value).copied() {
                Ok((path_locals, local))
            } else {
                Err(self.fatal_error(
                    r.name.span.clone(),
                    format!(
                        "Item `{}` not found in module `{}`",
                        r.name.value,
                        module.display()
                    ),
                ))
            }
        } else if let Some(local) = self.find_name(&r.name.value, r.in_macro_arg) {
            Ok((Vec::new(), local))
        } else {
            Err(self.fatal_error(
                r.name.span.clone(),
                format!("Unknown identifier `{}`", r.name.value),
            ))
        }
    }
    fn find_name(&self, name: &str, skip_local: bool) -> Option<LocalName> {
        if !skip_local {
            if let Some(local) = self.scope.names.get(name).copied() {
                return Some(local);
            }
        }
        let mut hit_file = false;
        for scope in self.higher_scopes.iter().rev() {
            if scope.kind == ScopeKind::File {
                if hit_file || self.scope.kind == ScopeKind::File {
                    break;
                }
                hit_file = true;
            }
            if let Some(local) = scope.names.get(name).copied() {
                return Some(local);
            }
        }
        None
    }
    fn ref_path(
        &self,
        path: &[RefComponent],
        skip_local: bool,
    ) -> UiuaResult<Option<(PathBuf, Vec<LocalName>)>> {
        let Some(first) = path.first() else {
            return Ok(None);
        };
        let mut path_locals = Vec::new();
        let module_local = self
            .find_name(&first.module.value, skip_local)
            .ok_or_else(|| {
                self.fatal_error(
                    first.module.span.clone(),
                    format!("Unknown import `{}`", first.module.value),
                )
            })?;
        path_locals.push(module_local);
        let global = &self.asm.bindings[module_local.index].kind;
        let mut module = match global {
            BindingKind::Module(module) => module,
            BindingKind::Func(_) => {
                return Err(self.fatal_error(
                    first.module.span.clone(),
                    format!("`{}` is a function, not a module", first.module.value),
                ))
            }
            BindingKind::Const(_) => {
                return Err(self.fatal_error(
                    first.module.span.clone(),
                    format!("`{}` is a constant, not a module", first.module.value),
                ))
            }
            BindingKind::Macro => {
                return Err(self.fatal_error(
                    first.module.span.clone(),
                    format!("`{}` is a modifier, not a module", first.module.value),
                ))
            }
        };
        for comp in path.iter().skip(1) {
            let submod_local = self.imports[module]
                .names
                .get(&comp.module.value)
                .copied()
                .ok_or_else(|| {
                    self.fatal_error(
                        comp.module.span.clone(),
                        format!(
                            "Module `{}` not found in module `{}`",
                            comp.module.value,
                            module.display()
                        ),
                    )
                })?;
            path_locals.push(submod_local);
            let global = &self.asm.bindings[submod_local.index].kind;
            module = match global {
                BindingKind::Module(module) => module,
                BindingKind::Func(_) => {
                    return Err(self.fatal_error(
                        comp.module.span.clone(),
                        format!("`{}` is a function, not a module", comp.module.value),
                    ))
                }
                BindingKind::Const(_) => {
                    return Err(self.fatal_error(
                        comp.module.span.clone(),
                        format!("`{}` is a constant, not a module", comp.module.value),
                    ))
                }
                BindingKind::Macro => {
                    return Err(self.fatal_error(
                        comp.module.span.clone(),
                        format!("`{}` is a modifier, not a module", comp.module.value),
                    ))
                }
            };
        }

        Ok(Some((module.clone(), path_locals)))
    }
    fn reference(&mut self, r: Ref, call: bool) -> UiuaResult {
        if r.path.is_empty() {
            self.ident(r.name.value, r.name.span, call, r.in_macro_arg)
        } else {
            let (path_locals, local) = self.ref_local(&r)?;
            self.validate_local(&r.name.value, local, &r.name.span);
            for (local, comp) in path_locals.into_iter().zip(&r.path) {
                self.validate_local(&comp.module.value, local, &comp.module.span);
                (self.code_meta.global_references).insert(comp.module.clone(), local.index);
            }
            self.code_meta
                .global_references
                .insert(r.name.clone(), local.index);
            self.global_index(local.index, r.name.span, call);
            Ok(())
        }
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool, skip_local: bool) -> UiuaResult {
        if let Some(curr) = (self.current_binding.as_mut()).filter(|curr| curr.name == ident) {
            // Name is a recursive call
            let Some(sig) = curr.signature else {
                return Err(self.fatal_error(
                    span,
                    format!(
                        "Recursive function `{ident}` must have a \
                        signature declared after the `←`."
                    ),
                ));
            };
            curr.referenced = true;
            (self.code_meta.global_references).insert(span.clone().sp(ident), curr.global_index);
            let instr = Instr::Recur(self.add_span(span.clone()));
            if call {
                self.push_all_instrs([Instr::PushSig(sig), instr, Instr::PopSig]);
            } else {
                let f = self.make_function(FunctionId::Anonymous(span), sig, [instr]);
                self.push_instr(Instr::PushFunc(f));
            }
        } else if let Some(local) = self.find_name(&ident, skip_local) {
            // Name exists in scope
            (self.code_meta.global_references).insert(span.clone().sp(ident), local.index);
            self.global_index(local.index, span, call);
        } else if let Some(constant) = CONSTANTS.iter().find(|c| c.name == ident) {
            // Name is a built-in constant
            let instr = Instr::push(constant.value.resolve(self.scope.file_path.as_deref()));
            self.code_meta
                .constant_references
                .insert(span.clone().sp(ident));
            if call {
                self.push_instr(instr);
            } else {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
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
        let global = self.asm.bindings[index].kind.clone();
        match global {
            BindingKind::Const(Some(val)) if call => self.push_instr(Instr::push(val)),
            BindingKind::Const(Some(val)) => {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    vec![Instr::push(val)],
                );
                self.push_instr(Instr::PushFunc(f));
            }
            BindingKind::Const(None) if call => self.push_instr(Instr::CallGlobal { index, call }),
            BindingKind::Const(None) => {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    vec![Instr::CallGlobal { index, call }],
                );
                self.push_instr(Instr::PushFunc(f));
            }
            BindingKind::Func(f) if self.inlinable(f.instrs(self)) => {
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
            BindingKind::Func(f) => {
                self.push_instr(Instr::PushFunc(f));
                if call {
                    let span = self.add_span(span);
                    self.push_instr(Instr::Call(span));
                }
            }
            BindingKind::Module { .. } => self.add_error(span, "Cannot import module item here."),
            BindingKind::Macro => {
                // We could error here, but it's easier to handle it higher up
            }
        }
    }
    fn func(&mut self, func: Func, span: CodeSpan, call: bool) -> UiuaResult {
        if (func.lines.iter().flatten().filter(|w| w.value.is_code())).count() == 1 {
            // Inline single ident
            if let Some(
                word @ Sp {
                    value: Word::Ref(_),
                    ..
                },
            ) = func.lines.iter().flatten().find(|w| w.value.is_code())
            {
                return self.word(word.clone(), call);
            }
        }

        if call {
            let (_, _, instrs) = self.compile_func_instrs(func, span)?;
            self.push_all_instrs(instrs);
        } else {
            let function = self.compile_func(func, span.clone())?;
            self.push_instr(Instr::PushFunc(function));
        }
        Ok(())
    }
    fn compile_func(&mut self, func: Func, span: CodeSpan) -> UiuaResult<Function> {
        let (id, sig, instrs) = self.compile_func_instrs(func, span)?;

        if let [Instr::PushFunc(f), Instr::Call(_)] = instrs.as_slice() {
            return Ok(Function::clone(f));
        }

        Ok(self.make_function(id, sig, instrs))
    }
    fn compile_func_instrs(
        &mut self,
        func: Func,
        span: CodeSpan,
    ) -> UiuaResult<(FunctionId, Signature, Vec<Instr>)> {
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
                            if e.kind == SigCheckErrorKind::Ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            }
        };
        self.code_meta.function_sigs.insert(
            span.clone(),
            SigDecl {
                sig,
                explicit: func.signature.is_some(),
                inline: true,
            },
        );
        Ok((func.id, sig, instrs))
    }
    fn switch(&mut self, sw: Switch, span: CodeSpan, call: bool) -> UiuaResult {
        let count = sw.branches.len();
        if !call {
            self.new_functions.push(EcoVec::new());
        }
        let mut branches = sw.branches.into_iter();
        // Compile first branch
        let first_branch = branches.next().expect("switch cannot have no branches");
        let f = self.compile_func(first_branch.value, first_branch.span)?;
        let mut sig = f.signature();
        let mut branch_funcs = vec![f];
        // Compile remaining branches
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
            branch_funcs.push(f);
        }

        // Maybe use `repeat` diagnostic
        if branch_funcs.len() == 2
            && matches!(
                branch_funcs[0].instrs(&self.asm),
                [] | [Instr::Prim(Primitive::Identity, _)]
            )
            && branch_funcs[1].signature().args == branch_funcs[1].signature().outputs
        {
            self.emit_diagnostic(
                format!(
                    "Prefer {} over this switch function",
                    Primitive::Repeat.format(),
                ),
                DiagnosticKind::Style,
                span.clone(),
            );
        }

        self.push_all_instrs(branch_funcs.into_iter().map(Instr::PushFunc));

        let span_idx = self.add_span(span.clone());
        self.push_instr(Instr::Switch {
            count,
            sig,
            span: span_idx,
            under_cond: false,
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
                            if e.kind == SigCheckErrorKind::Ambiguous {
                                ". A signature can be declared after the opening `(`."
                            } else {
                                ""
                            }
                        ),
                    ));
                }
            };
            let function = self.make_function(FunctionId::Anonymous(span), sig, instrs);
            self.push_instr(Instr::PushFunc(function));
        }
        Ok(())
    }
    fn handle_primitive_deprecation(&mut self, prim: Primitive, span: &CodeSpan) {
        if let Some(suggestion) = prim.deprecation_suggestion() {
            if !self.deprecated_prim_errors.insert(prim) {
                return;
            }
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
        if prim.is_experimental()
            && !self.scope.experimental
            && self.experimental_prim_errors.insert(prim)
        {
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
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        self.handle_primitive_experimental(prim, &span);
        self.handle_primitive_deprecation(prim, &span);
        let span_i = self.add_span(span.clone());
        if call {
            self.push_instr(Instr::Prim(prim, span_i));
        } else {
            let instrs = [Instr::Prim(prim, span_i)];
            let sig = self.sig_of(&instrs, &span)?;
            let func = self.make_function(FunctionId::Primitive(prim), sig, instrs);
            self.push_instr(Instr::PushFunc(func));
        }
        Ok(())
    }
    fn swizzle(&mut self, sw: StackSwizzle, span: CodeSpan, call: bool) {
        if !self.scope.experimental {
            self.add_error(
                span.clone(),
                "Swizzles are experimental. To use them, add \
                `# Experimental!` to the top of the file.",
            );
        }
        let sig = sw.signature();
        let spandex = self.add_span(span.clone());
        let equivalent = match sw.indices.as_slice() {
            [0] => Some(Primitive::Identity),
            [1, 0] => Some(Primitive::Flip),
            [0, 0] => Some(Primitive::Dup),
            [1, 0, 1] => Some(Primitive::Over),
            _ => None,
        };
        let mut instr = if let Some(prim) = equivalent {
            self.emit_diagnostic(
                format!(
                    "This swizzle is equivalent to {}. \
                    Use that instead.",
                    prim.format()
                ),
                DiagnosticKind::Style,
                span.clone(),
            );
            Instr::Prim(prim, spandex)
        } else {
            Instr::StackSwizzle(sw, spandex)
        };
        if !call {
            instr =
                Instr::PushFunc(self.make_function(FunctionId::Anonymous(span), sig, vec![instr]));
        }
        self.push_instr(instr);
    }
    fn inlinable(&self, instrs: &[Instr]) -> bool {
        use ImplPrimitive::*;
        use Primitive::*;
        if instrs.len() > 10 {
            return false;
        }
        for instr in instrs {
            match instr {
                Instr::Prim(Trace | Dump | Stack | Assert, _) => return false,
                Instr::ImplPrim(UnTrace | UnDump | UnStack | BothTrace | UnBothTrace, _) => {
                    return false
                }
                Instr::PushFunc(f) if !self.inlinable(f.instrs(self)) => return false,
                Instr::NoInline => return false,
                _ => {}
            }
        }
        true
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
        span: CodeSpan,
    ) {
        let inputs = self.asm.inputs.clone();
        self.diagnostics
            .insert(Diagnostic::new(message.into(), span, kind, inputs));
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
    fn validate_local(&mut self, name: &str, local: LocalName, span: &CodeSpan) {
        if local.public {
            return;
        }
        if !local.public
            && (self.scope.names.get(name))
                .or_else(|| {
                    self.higher_scopes
                        .last()
                        .filter(|_| self.scope.kind != ScopeKind::File)
                        .and_then(|scope| scope.names.get(name))
                })
                .map_or(true, |l| l.index != local.index)
        {
            self.add_error(span.clone(), format!("`{}` is private", name));
        }
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
        self.make_function(
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
        let local = LocalName {
            index,
            public: true,
        };
        self.compile_bind_function(&name, local, function, 0, None)?;
        self.next_global += 1;
        self.scope.names.insert(name, local);
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
    #[must_use]
    fn pre_eval_instrs(&mut self, instrs: EcoVec<Instr>) -> (EcoVec<Instr>, Vec<UiuaError>) {
        let mut errors = Vec::new();
        let instrs = optimize_instrs(instrs, true, &self.asm);
        if self.pre_eval_mode == PreEvalMode::Lazy
            || instrs.iter().all(|instr| matches!(instr, Instr::Push(_)))
            || instrs.iter().any(|instr| matches!(instr, Instr::NoInline))
        {
            return (instrs, errors);
        }
        let mut start = 0;
        let mut new_instrs: Option<EcoVec<Instr>> = None;
        'start: while start < instrs.len() {
            for end in (start + 1..=instrs.len()).rev() {
                let section = &instrs[start..end];
                if !instrs_can_pre_eval(section, &self.asm) {
                    continue;
                }
                if instrs_are_pure(section, &self.asm, Purity::Pure)
                    && instrs_all_signatures(section).is_ok_and(|(sig, temps)| {
                        sig.args == 0 && sig.outputs > 0 && temps.iter().all(|&sig| sig == (0, 0))
                    })
                {
                    // println!("section: {section:?}");
                    let mut success = false;
                    match self.comptime_instrs(section.into()) {
                        Ok(Some(values)) => {
                            for val in &values {
                                val.validate_shape();
                            }
                            let new_instrs =
                                new_instrs.get_or_insert_with(|| instrs[..start].into());
                            new_instrs.extend(values.into_iter().map(Instr::Push));
                            success = true;
                        }
                        Ok(None) => {}
                        Err(e) if e.is_fill() => {}
                        Err(e) if e.message().contains("No locals to get") => {}
                        Err(e) => errors.push(e),
                    }
                    if !success {
                        if let Some(new_instrs) = &mut new_instrs {
                            new_instrs.extend(section.iter().cloned());
                        }
                    }
                    start = end;
                    continue 'start;
                }
            }
            if let Some(new_instrs) = &mut new_instrs {
                new_instrs.push(instrs[start].clone())
            }
            start += 1;
        }
        // if let Some(new_instrs) = &new_instrs {
        //     println!("eval: {new_instrs:?}")
        // }
        (new_instrs.unwrap_or(instrs), errors)
    }
    fn comptime_instrs(&mut self, instrs: EcoVec<Instr>) -> UiuaResult<Option<Vec<Value>>> {
        if !self.pre_eval_mode.matches_instrs(&instrs, &self.asm) {
            return Ok(None);
        }
        if instrs.iter().all(|instr| matches!(instr, Instr::Push(_))) {
            return Ok(Some(
                (instrs.into_iter())
                    .map(|instr| match instr {
                        Instr::Push(val) => val,
                        _ => unreachable!(),
                    })
                    .collect(),
            ));
        }
        thread_local! {
            static CACHE: RefCell<HashMap<EcoVec<Instr>, Option<Vec<Value>>>> = RefCell::new(HashMap::new());
        }
        CACHE.with(|cache| {
            let instrs = optimize_instrs(instrs, true, &self.asm);
            if let Some(stack) = cache.borrow().get(&instrs) {
                return Ok(stack.clone());
            }
            let mut asm = self.asm.clone();
            asm.top_slices.clear();
            let start = asm.instrs.len();
            let len = instrs.len();
            asm.instrs.extend(instrs.iter().cloned());
            asm.top_slices.push(FuncSlice { start, len });
            let mut env = if self.pre_eval_mode == PreEvalMode::Lsp {
                #[cfg(feature = "native_sys")]
                {
                    Uiua::with_native_sys()
                }
                #[cfg(not(feature = "native_sys"))]
                Uiua::with_safe_sys()
            } else {
                Uiua::with_safe_sys()
            }
            .with_execution_limit(Duration::from_millis(40));
            match env.run_asm(asm) {
                Ok(()) => {
                    let stack = env.take_stack();
                    let res = if stack.iter().any(|v| v.element_count() > 1000) {
                        None
                    } else {
                        Some(stack)
                    };
                    cache.borrow_mut().insert(instrs, res.clone());
                    Ok(res)
                }
                Err(e) if matches!(e.inner(), UiuaError::Timeout(..)) => {
                    cache.borrow_mut().insert(instrs, None);
                    Ok(None)
                }
                Err(e) => Err(e),
            }
        })
    }
    fn sig_of(&self, instrs: &[Instr], span: &CodeSpan) -> UiuaResult<Signature> {
        instrs_signature(instrs).map_err(|e| {
            self.fatal_error(
                span.clone(),
                format!("Cannot infer function signature: {e}"),
            )
        })
    }
}

fn instrs_can_pre_eval(instrs: &[Instr], asm: &Assembly) -> bool {
    use Primitive::*;
    if instrs.is_empty() {
        return true;
    }
    // Begin and end array instructions must be balanced
    let begin_array_pos = (instrs.iter()).position(|instr| matches!(instr, Instr::BeginArray));
    let begin_array_count = (instrs.iter())
        .filter(|instr| matches!(instr, Instr::BeginArray))
        .count();
    let end_array_pos = (instrs.iter()).position(|instr| matches!(instr, Instr::EndArray { .. }));
    let end_array_count = (instrs.iter())
        .filter(|instr| matches!(instr, Instr::EndArray { .. }))
        .count();
    let array_allowed = begin_array_count == end_array_count
        && match (begin_array_pos, end_array_pos) {
            (Some(0), Some(end)) => end == instrs.len() - 1,
            (None, None) => true,
            _ => false,
        };
    if !array_allowed
        || matches!(
            instrs.last().unwrap(),
            Instr::PushFunc(_) | Instr::BeginArray
        )
        || instrs.iter().all(|instr| matches!(instr, Instr::Push(_)))
        || instrs.iter().any(|instr| {
            matches!(
                instr,
                Instr::Prim(SetInverse | SetUnder, _) | Instr::ImplPrim(ImplPrimitive::UnPop, _)
            )
        })
    {
        return false;
    }
    for instr in instrs {
        if let Instr::PushFunc(f) = instr {
            if !instrs_can_pre_eval(f.instrs(asm), asm) {
                return false;
            }
        }
    }
    true
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

fn collect_placeholder(words: &[Sp<Word>]) -> Vec<Sp<PlaceholderOp>> {
    let mut ops = Vec::new();
    for word in words {
        match &word.value {
            Word::Placeholder(op) => ops.push(word.span.clone().sp(*op)),
            Word::Strand(items) => ops.extend(collect_placeholder(items)),
            Word::Array(arr) => arr.lines.iter().for_each(|line| {
                ops.extend(collect_placeholder(line));
            }),
            Word::Func(func) => func.lines.iter().for_each(|line| {
                ops.extend(collect_placeholder(line));
            }),
            Word::Modified(m) => ops.extend(collect_placeholder(&m.operands)),
            Word::Switch(sw) => sw.branches.iter().for_each(|branch| {
                (branch.value.lines.iter()).for_each(|line| ops.extend(collect_placeholder(line)))
            }),
            _ => {}
        }
    }
    ops
}

fn replace_placeholders(words: &mut Vec<Sp<Word>>, next: &mut dyn FnMut() -> Sp<Word>) {
    recurse_words(words, &mut |word| match &mut word.value {
        Word::Placeholder(PlaceholderOp::Call) => *word = next(),
        _ => {}
    });
    words.retain(|word| !matches!(word.value, Word::Placeholder(_)))
}

fn set_in_macro_arg(words: &mut Vec<Sp<Word>>) {
    recurse_words(words, &mut |word| match &mut word.value {
        Word::Ref(r) => r.in_macro_arg = true,
        Word::IncompleteRef { in_macro_arg, .. } => *in_macro_arg = true,
        _ => {}
    });
}

fn recurse_words(words: &mut Vec<Sp<Word>>, f: &mut dyn FnMut(&mut Sp<Word>)) {
    for word in words {
        f(word);
        match &mut word.value {
            Word::Strand(items) => recurse_words(items, f),
            Word::Array(arr) => arr.lines.iter_mut().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Func(func) => func.lines.iter_mut().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Modified(m) => recurse_words(&mut m.operands, f),
            Word::Switch(sw) => sw.branches.iter_mut().for_each(|branch| {
                (branch.value.lines.iter_mut()).for_each(|line| recurse_words(line, f))
            }),
            _ => {}
        }
    }
}
