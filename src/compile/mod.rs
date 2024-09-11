mod binding;
mod modifier;

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{hash_map::DefaultHasher, BTreeSet, HashMap, HashSet, VecDeque},
    env::current_dir,
    fmt, fs,
    hash::{Hash, Hasher},
    iter::{once, repeat},
    mem::{replace, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    slice,
    sync::Arc,
    time::Duration,
};

use ecow::{eco_vec, EcoString, EcoVec};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{
    algorithm::{
        invert::{invert_instrs, under_instrs},
        IgnoreError,
    },
    ast::*,
    check::{instrs_clean_signature, instrs_signature, SigCheckError, SigCheckErrorKind},
    format::{format_word, format_words},
    function::*,
    ident_modifier_args,
    lex::{CodeSpan, Sp, Span},
    lsp::{CodeMeta, ImportSrc, SigDecl},
    optimize::{optimize_instrs, optimize_instrs_mut},
    parse::{count_placeholders, flip_unsplit_lines, parse, split_words},
    Array, Assembly, BindingKind, Boxed, Diagnostic, DiagnosticKind, DocComment, DocCommentSig,
    GitTarget, Ident, ImplPrimitive, InputSrc, IntoInputSrc, IntoSysBackend, Primitive, RunMode,
    SemanticComment, SysBackend, Uiua, UiuaError, UiuaErrorKind, UiuaResult, Value, CONSTANTS,
    EXAMPLE_UA, SUBSCRIPT_NUMS, VERSION,
};

/// The Uiua compiler
#[derive(Clone)]
pub struct Compiler {
    pub(crate) asm: Assembly,
    pub(crate) code_meta: CodeMeta,
    /// Functions which are under construction
    new_functions: Vec<NewFunction>,
    /// The name of the current bindings
    current_bindings: Vec<CurrentBinding>,
    /// The index of the next global binding
    next_global: usize,
    /// The current scope
    pub(crate) scope: Scope,
    /// Ancestor scopes of the current one
    higher_scopes: Vec<Scope>,
    /// Determines which How test scopes are run
    mode: RunMode,
    /// The paths of files currently being imported (used to detect import cycles)
    current_imports: Vec<PathBuf>,
    /// The bindings of imported files
    imports: HashMap<PathBuf, Module>,
    /// Unexpanded index macros
    index_macros: HashMap<usize, IndexMacro>,
    /// Unexpanded code macros
    code_macros: HashMap<usize, CodeMacro>,
    /// The depth of macro expansion
    macro_depth: usize,
    /// Whether the compiler is in an inverse
    in_inverse: bool,
    /// Whether the compiler is in a try
    in_try: bool,
    /// Map of instructions to undered functions that created them
    ///
    /// This is used to under functions that have already been inlined
    pub(crate) undered_funcs: HashMap<EcoVec<Instr>, UnderedFunctions>,
    /// Accumulated errors
    errors: Vec<UiuaError>,
    /// Primitives that have emitted errors because they are deprecated
    deprecated_prim_errors: HashSet<Primitive>,
    /// Accumulated diagnostics
    diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    pub(crate) print_diagnostics: bool,
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
            current_bindings: Vec::new(),
            next_global: 0,
            scope: Scope::default(),
            higher_scopes: Vec::new(),
            mode: RunMode::All,
            current_imports: Vec::new(),
            imports: HashMap::new(),
            index_macros: HashMap::new(),
            code_macros: HashMap::new(),
            macro_depth: 0,
            in_inverse: false,
            in_try: false,
            undered_funcs: HashMap::new(),
            errors: Vec::new(),
            deprecated_prim_errors: HashSet::new(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: false,
            comptime: true,
            pre_eval_mode: PreEvalMode::default(),
            macro_env: Uiua::default(),
        }
    }
}

/// A function that is under construction
///
/// Has a list of instructions but also tracks flags
#[derive(Debug, Clone, Default)]
pub(crate) struct NewFunction {
    pub instrs: EcoVec<Instr>,
    pub flags: FunctionFlags,
}

impl From<EcoVec<Instr>> for NewFunction {
    fn from(instrs: EcoVec<Instr>) -> Self {
        Self {
            instrs,
            flags: FunctionFlags::default(),
        }
    }
}

impl From<FunctionFlags> for NewFunction {
    fn from(flags: FunctionFlags) -> Self {
        Self {
            flags,
            instrs: EcoVec::new(),
        }
    }
}

#[derive(Default)]
struct BindingPrelude {
    flags: FunctionFlags,
    comment: Option<EcoString>,
}

/// A Uiua module
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    /// The top level comment
    pub comment: Option<EcoString>,
    /// Map module-local names to global indices
    pub names: IndexMap<Ident, LocalName>,
    /// Whether the module uses experimental features
    experimental: bool,
}

/// An index macro
#[derive(Clone)]
struct IndexMacro {
    words: Vec<Sp<Word>>,
    names: IndexMap<Ident, LocalName>,
    sig: Option<Signature>,
    hygenic: bool,
    recursive: bool,
    flags: FunctionFlags,
}

/// A code macro
#[derive(Clone)]
struct CodeMacro {
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

#[derive(Debug, Clone)]
pub(crate) struct UnderedFunctions {
    pub f: EcoVec<Instr>,
    pub f_sig: Signature,
    pub g: EcoVec<Instr>,
    pub g_sig: Signature,
    pub span: usize,
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
    pub experimental: bool,
    /// Whether an error has been emitted for experimental features
    experimental_error: bool,
    /// Whether an error has been emitted for fill function signatures
    fill_sig_error: bool,
    /// The stack height between top-level statements
    stack_height: Result<usize, Sp<SigCheckError>>,
}

#[derive(Clone, PartialEq, Eq)]
enum ScopeKind {
    /// A scope at the top level of a file
    File,
    /// A scope in a named module
    Module(Ident),
    /// A scope that includes all bindings in a module
    AllInModule,
    /// A temporary scope, probably for a macro
    Temp(Option<MacroLocal>),
    /// A test scope between `---`s
    Test,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct MacroLocal {
    macro_index: usize,
    expansion_index: usize,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            kind: ScopeKind::File,
            file_path: None,
            comment: None,
            names: IndexMap::new(),
            experimental: false,
            experimental_error: false,
            fill_sig_error: false,
            stack_height: Ok(0),
        }
    }
}

/// The index of a named local in the bindings, and whether it is public
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct LocalName {
    /// The index of the binding in assembly's bindings
    pub index: usize,
    /// Whether the binding is public
    pub public: bool,
}

/// The mode that dictates how much code to pre-evaluate at compile time
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum PreEvalMode {
    /// Evaluate as much as possible at compile time, even impure expressions
    ///
    /// Recursive functions and certain system functions are not evaluated
    Lsp,
    /// Does not evalute pure constants and expressions at comptime, but still evaluates `comptime`
    Lazy,
    /// Pre-evaluate each line, but not multiple lines together
    Line,
    /// The normal mode. Tries to evaluate pure, time-bounded constants and expressions at comptime
    #[default]
    Normal,
}

const MAX_PRE_EVAL_ELEMS: usize = 1000;
const MAX_PRE_EVAL_RANK: usize = 4;

impl PreEvalMode {
    fn matches_instrs(&self, instrs: &[Instr], asm: &Assembly) -> bool {
        if instrs.iter().any(|instr| {
            matches!(
                instr,
                Instr::Push(val)
                    if val.element_count() > MAX_PRE_EVAL_ELEMS
                    || val.rank() > MAX_PRE_EVAL_RANK
            )
        }) {
            return false;
        }
        match self {
            PreEvalMode::Normal | PreEvalMode::Line => {
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
    pub fn take_backend<T: SysBackend + Default>(&mut self) -> Option<T> {
        self.macro_env.take_backend()
    }
    /// Compile a Uiua file from a file at a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input: EcoString = fs::read_to_string(path)
            .map_err(|e| UiuaErrorKind::Load(path.into(), e.into()))?
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
    ) -> UiuaResult<Module> {
        let experimental = self.scope.experimental;
        self.higher_scopes.push(take(&mut self.scope));
        self.scope.kind = kind;
        self.scope.experimental = experimental;
        let res = f(self);
        let scope = replace(&mut self.scope, self.higher_scopes.pop().unwrap());
        res?;
        Ok(Module {
            comment: scope.comment,
            names: scope.names,
            experimental: scope.experimental,
        })
    }
    fn load_impl(&mut self, input: &str, src: InputSrc) -> UiuaResult<&mut Self> {
        let instrs_start = self.asm.instrs.len();
        let top_slices_start = self.asm.top_slices.len();
        let (items, errors, diagnostics) = parse(input, src.clone(), &mut self.asm.inputs);
        for diagnostic in diagnostics {
            self.emit_diagnostic_impl(diagnostic);
        }
        if !errors.is_empty() {
            return Err(UiuaErrorKind::Parse(errors, self.asm.inputs.clone().into()).into());
        }
        if let InputSrc::File(path) = &src {
            self.current_imports.push(path.to_path_buf());
            self.scope.file_path = Some(if path.is_absolute() {
                current_dir()
                    .ok()
                    .and_then(|dir| pathdiff::diff_paths(path, dir))
                    .unwrap_or_else(|| path.to_path_buf())
            } else {
                path.to_path_buf()
            });
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
            _ => Err(UiuaError::from_multi(take(&mut self.errors))),
        }
    }
    fn catching_crash<T>(
        &mut self,
        input: impl fmt::Display,
        f: impl FnOnce(&mut Self) -> T,
    ) -> UiuaResult<T> {
        match catch_unwind(AssertUnwindSafe(|| f(self))) {
            Ok(res) => Ok(res),
            Err(_) => Err(UiuaErrorKind::CompilerPanic(format!(
                "\
The compiler has crashed!
Hooray! You found a bug!
Please report this at http://github.com/uiua-lang/uiua/issues/new \
or on Discord at https://discord.gg/9CU2ME4kmn.

Uiua version {VERSION}

code:
{input}"
            ))
            .into()),
        }
    }
    pub(crate) fn items(&mut self, items: Vec<Item>, must_run: bool) -> UiuaResult {
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

        let mut prelude = BindingPrelude::default();
        let mut item_errored = false;
        let mut items = VecDeque::from(items);
        while let Some(item) = items.pop_front() {
            let must_run = must_run
                || matches!(&item, Item::Words(_))
                    && items.iter().any(|item| match item {
                        Item::Binding(binding)
                            if (binding.words.iter())
                                .filter(|word| word.value.is_code())
                                .count()
                                == 0 =>
                        {
                            true
                        }
                        Item::Words(lines) => lines.iter().any(|line| {
                            line.iter().find(|w| w.value.is_code()).is_some_and(|w| {
                                matches!(w.value, Word::Primitive(Primitive::Assert))
                            })
                        }),
                        _ => false,
                    });
            if let Err(e) = self.item(item, must_run, &mut prelude) {
                if !item_errored {
                    self.errors.push(e);
                }
                item_errored = true;
            }
        }
        Ok(())
    }
    fn item(&mut self, item: Item, must_run: bool, prelude: &mut BindingPrelude) -> UiuaResult {
        fn words_should_run_anyway(words: &[Sp<Word>]) -> bool {
            let mut anyway = false;
            recurse_words(words, &mut |w| {
                anyway = anyway
                    || matches!(&w.value, Word::SemanticComment(_))
                    || matches!(&w.value, Word::Modified(m)
                        if matches!(m.modifier.value, Modifier::Ref(_)))
            });
            anyway
        }
        let in_test = self.scope.kind == ScopeKind::Test
            || self.higher_scopes.iter().any(|s| s.kind == ScopeKind::Test);
        let mut lines = match item {
            Item::Module(m) => return self.module(m, take(prelude).comment),
            Item::Words(lines) => lines,
            Item::Binding(binding) => {
                let prelude = take(prelude);
                self.binding(binding, prelude)?;
                return Ok(());
            }
            Item::Import(import) => return self.import(import, take(prelude).comment),
        };

        // Compile top-level words

        // Populate prelude
        for line in &mut lines {
            let mut words = line.iter().filter(|w| !matches!(w.value, Word::Spaces));
            if words.clone().count() == 1 {
                let word = words.next().unwrap();
                match &word.value {
                    Word::Comment(c) => {
                        if let Some(curr_com) = &mut prelude.comment {
                            curr_com.push('\n');
                            curr_com.push_str(c);
                        } else {
                            prelude.comment = Some(c.as_str().into());
                        }
                        line.clear();
                    }
                    Word::SemanticComment(SemanticComment::NoInline) => {
                        prelude.flags |= FunctionFlags::NO_INLINE;
                    }
                    Word::SemanticComment(SemanticComment::TrackCaller) => {
                        prelude.flags |= FunctionFlags::TRACK_CALLER;
                    }
                    _ => *prelude = BindingPrelude::default(),
                }
            } else {
                *prelude = BindingPrelude::default();
            }
        }
        let can_run = match self.mode {
            RunMode::Normal => !in_test,
            RunMode::Test => in_test,
            RunMode::All => true,
        };
        let mut lines = VecDeque::from(flip_unsplit_lines(
            lines.into_iter().flat_map(split_words).collect(),
        ));
        while let Some(line) = lines.pop_front() {
            let assert_later = || {
                lines.iter().any(|line| {
                    line.iter()
                        .find(|w| w.value.is_code())
                        .is_some_and(|w| matches!(w.value, Word::Primitive(Primitive::Assert)))
                })
            };
            if line.is_empty()
                || !(can_run || must_run || assert_later() || words_should_run_anyway(&line))
            {
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
            let line_sig_comment = line_sig(&line);
            // Compile the words
            let instr_count_before = self.asm.instrs.len();
            let binding_count_before = self.asm.bindings.len();
            let new_func = self.compile_words(line, true)?;
            let instr_count_after = self.asm.instrs.len();
            let binding_count_after = self.asm.bindings.len();
            let (mut new_func, pre_eval_errors) = self.pre_eval_instrs(new_func);
            let mut line_eval_errored = false;
            match instrs_signature(&new_func.instrs) {
                Ok(sig) => {
                    // Check doc comment sig
                    if let Some(comment_sig) = line_sig_comment {
                        if !comment_sig.value.matches_sig(sig) {
                            self.emit_diagnostic(
                                format!("Line signature {sig} does not match comment"),
                                DiagnosticKind::Warning,
                                comment_sig.span.clone(),
                            );
                        }
                    }
                    // Update scope stack height
                    if let Ok(height) = &mut self.scope.stack_height {
                        *height = (*height + sig.outputs).saturating_sub(sig.args);
                        // Compile test assert
                        if self.mode != RunMode::Normal && sig.outputs == 0 {
                            if let Some(Instr::Prim(Primitive::Assert, span)) =
                                new_func.instrs.last()
                            {
                                let span = *span;
                                new_func.instrs.pop();
                                new_func
                                    .instrs
                                    .push(Instr::ImplPrim(ImplPrimitive::TestAssert, span));
                            }
                        }
                    }
                    // Try to evaluate at comptime
                    // This can be done when:
                    // - the pre-eval mode is not `Line`
                    // - there are at least as many push instructions preceding the current line as there are arguments to the line
                    // - the words create no bindings
                    if self.pre_eval_mode != PreEvalMode::Line
                        && !new_func.instrs.is_empty()
                        && instr_count_before >= sig.args
                        && instr_count_after >= instr_count_before
                        && binding_count_before == binding_count_after
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
                        comp_instrs.extend(new_func.instrs.iter().cloned());
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
                                new_func.instrs = vals.into_iter().map(Instr::push).collect();
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
            self.asm.instrs.extend(new_func.instrs);
            let end = self.asm.instrs.len();
            if end != start {
                self.asm.top_slices.push(FuncSlice {
                    start,
                    len: end - start,
                });
            }
        }
        Ok(())
    }
    #[must_use]
    pub(crate) fn make_function(
        &mut self,
        id: FunctionId,
        sig: Signature,
        new_func: NewFunction,
    ) -> Function {
        let (new_func, errors) = self.pre_eval_instrs(new_func);
        self.errors.extend(errors);
        let len = new_func.instrs.len();
        (self.asm.instrs).push(Instr::Comment(format!("({id}").into()));
        let start = if len == 0 { 0 } else { self.asm.instrs.len() };
        let mut hasher = DefaultHasher::new();
        new_func.instrs.hash(&mut hasher);
        let hash = hasher.finish();
        self.asm.instrs.extend(new_func.instrs);
        (self.asm.instrs).push(Instr::Comment(format!("{id})").into()));
        let slice = FuncSlice { start, len };
        // println!(
        //     "make function: {id} {sig} {slice:?} {:?}",
        //     self.asm.instrs(slice)
        // );
        Function::new(id, sig, slice, hash).with_flags(new_func.flags)
    }
    fn compile_bind_function(
        &mut self,
        name: Ident,
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
        self.scope.names.insert(name, local);
        self.asm.bind_function(local, function, span, comment);
        Ok(())
    }
    fn compile_bind_const(
        &mut self,
        name: Ident,
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
            .add_binding_at(local, BindingKind::Const(value), span, comment);
        self.scope.names.insert(name, local);
    }
    /// Import a module
    pub(crate) fn import_module(&mut self, path_str: &str, span: &CodeSpan) -> UiuaResult<PathBuf> {
        // Resolve path
        let path = if let Some(mut url) = path_str.trim().strip_prefix("git:") {
            if url.contains("branch:") && url.contains("commit:") {
                return Err(self.fatal_error(
                    span.clone(),
                    "Cannot specify both branch and commit in git import",
                ));
            }
            let target = if let Some((a, b)) = url.split_once("branch:") {
                url = a;
                GitTarget::Branch(b.trim().into())
            } else if let Some((a, b)) = url.split_once("commit:") {
                url = a;
                GitTarget::Commit(b.trim().into())
            } else {
                GitTarget::Default
            };
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
                self.emit_diagnostic(
                    "Implicit GitHub URLs are deprecated and \
                    will be removed in the future. Prefix the \
                    URL with `github.com/` or `https://github.com/`.",
                    DiagnosticKind::Warning,
                    span.clone(),
                );
            }
            if !(url.starts_with("https://") || url.starts_with("http://")) {
                url = format!("https://{url}");
            }
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::Git(url.clone()));
            self.backend()
                .load_git_module(&url, target)
                .map_err(|e| self.fatal_error(span.clone(), e))?
        } else {
            // Normal import
            let path = self.resolve_import_path(Path::new(path_str));
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::File(path.clone()));
            path
        };
        if !self.imports.contains_key(&path) {
            let bytes = self
                .backend()
                .file_read_all(&path)
                .or_else(|e| {
                    if path.ends_with(Path::new("example.ua")) {
                        Ok(EXAMPLE_UA.as_bytes().to_vec())
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
        let module = self.imports.get(&path).unwrap();
        if module.experimental {
            self.experimental_error(span, || {
                format!(
                    "Module `{path_str}` is experimental. \
                    To use it, add `# Experimental!` to the top of this file."
                )
            });
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
    fn compile_words(&mut self, words: Vec<Sp<Word>>, call: bool) -> UiuaResult<NewFunction> {
        self.new_functions.push(NewFunction::default());
        for line in flip_unsplit_lines(split_words(words)) {
            self.words(line, call)?;
        }
        Ok(self.new_functions.pop().unwrap())
    }
    // Compile a line, checking an end-of-line signature comment
    fn compile_line(&mut self, line: Vec<Sp<Word>>, call: bool) -> UiuaResult<NewFunction> {
        let comment_sig = line_sig(&line);
        let new_func = self.compile_words(line, call)?;
        if let Some(comment_sig) = comment_sig {
            if let Ok(sig) = instrs_signature(&new_func.instrs) {
                if !comment_sig.value.matches_sig(sig) {
                    self.emit_diagnostic(
                        format!("Line signature {sig} does not match comment"),
                        DiagnosticKind::Warning,
                        comment_sig.span.clone(),
                    );
                }
            }
        }
        Ok(new_func)
    }
    fn compile_operand_word(&mut self, word: Sp<Word>) -> UiuaResult<(NewFunction, Signature)> {
        let span = word.span.clone();
        let mut new_func = self.compile_words(vec![word], true)?;
        let mut sig = None;
        // Extract function instrs if possible
        if let [Instr::PushFunc(f)] = new_func.instrs.as_slice() {
            sig = Some(f.signature());
            let slice = f.slice;
            new_func.flags = f.flags;
            new_func.instrs = f.instrs(&self.asm).into();
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
            self.sig_of(&new_func.instrs, &span)?
        };
        new_func.instrs = optimize_instrs(new_func.instrs, false, &self.asm);
        Ok((new_func, sig))
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

            let start = self.new_functions.last().unwrap().instrs.len();

            // Compile the word
            self.word(word, call)?;

            let new_function = self.new_functions.last().unwrap();
            let sig = if new_function.instrs.len() >= start {
                instrs_signature(&new_function.instrs[start..]).ok()
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
        let new_func = self.new_functions.last_mut().unwrap();
        optimize_instrs_mut(&mut new_func.instrs, instr, false, &self.asm);
    }
    fn push_all_instrs(&mut self, new_func: impl Into<NewFunction>) {
        let new_func = new_func.into();
        let curr = self.new_functions.last_mut().unwrap();
        curr.flags |= new_func.flags;
        for instr in new_func.instrs {
            optimize_instrs_mut(&mut curr.instrs, instr, false, &self.asm);
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
                        eco_vec![instr].into(),
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
                        eco_vec![instr].into(),
                    ));
                }
                self.push_instr(instr);
            }
            Word::String(s) => {
                let mut instr = Instr::push(s);
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        eco_vec![instr].into(),
                    ));
                }
                self.push_instr(instr);
            }
            Word::MultilineString(lines) => {
                let mut s = EcoVec::new();
                for (i, line) in lines.into_iter().enumerate() {
                    if i > 0 {
                        s.push('\n');
                    }
                    s.extend(line.value.chars());
                }
                let mut instr = Instr::push(s);
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        eco_vec![instr].into(),
                    ));
                }
                self.push_instr(instr);
            }
            Word::Label(label) => {
                let mut instr = Instr::Label {
                    label: label.into(),
                    span: self.add_span(word.span.clone()),
                    remove: false,
                };
                if !call {
                    instr = Instr::PushFunc(self.make_function(
                        FunctionId::Anonymous(word.span.clone()),
                        Signature::new(0, 1),
                        eco_vec![instr].into(),
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
                        eco_vec![instr].into(),
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
                        eco_vec![instr].into(),
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
                            .insert(comp.module.span, local.index);
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
                    return Err(
                        self.fatal_error(word.span.clone(), "Functions are not allowed in strands")
                    );
                }
                self.code_meta.strands.insert(word.span.clone(), just_spans);
                // Flatten instrs
                let (inner, flags): (Vec<Instr>, FunctionFlags) = op_instrs.into_iter().fold(
                    (Vec::new(), FunctionFlags::default()),
                    |(mut inner, mut flags), (nf, _)| {
                        inner.extend(nf.instrs);
                        flags |= nf.flags;
                        (inner, flags)
                    },
                );

                // Normal strand
                if !call {
                    self.new_functions.push(NewFunction::from(flags));
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
                let new_func = self.new_functions.last_mut().unwrap();
                // Inline constant arrays
                if call && inner.iter().all(|instr| matches!(instr, Instr::Push(_))) {
                    let values = inner.iter().rev().map(|instr| match instr {
                        Instr::Push(v) => v.clone(),
                        _ => unreachable!(),
                    });
                    match Value::from_row_values(values, &(&word.span, &self.asm.inputs)) {
                        Ok(val) => {
                            new_func.instrs.pop();
                            self.push_instr(Instr::push(val));
                            return Ok(());
                        }
                        Err(e) if e.is_fill => {}
                        Err(e) => return Err(e),
                    }
                }
                // Normal case
                new_func.instrs.extend(inner);
                self.push_instr(Instr::EndArray {
                    span: span_index,
                    boxed: false,
                });
                if !call {
                    let new_func = self.new_functions.pop().unwrap();
                    let sig = instrs_signature(&new_func.instrs).unwrap();
                    let func = self.make_function(FunctionId::Anonymous(word.span), sig, new_func);
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
                    self.code_meta
                        .array_inner_spans
                        .insert(word.span.clone(), just_spans);
                }

                if !call {
                    self.new_functions.push(NewFunction::default());
                }
                self.push_instr(Instr::BeginArray);
                let mut inner = Vec::new();
                let mut flags = FunctionFlags::default();
                let line_count = arr.lines.len();
                for line in arr.lines.into_iter().rev() {
                    let nf = self.compile_line(line, true)?;
                    inner.extend(nf.instrs);
                    flags |= nf.flags;
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
                let new_func = self.new_functions.last_mut().unwrap();
                new_func.flags |= flags;
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
                            self.code_meta
                                .array_shapes
                                .insert(word.span.clone(), val.shape().clone());
                            new_func.instrs.pop();
                            self.push_instr(Instr::push(val));
                            return Ok(());
                        }
                        Err(e) if e.is_fill => {}
                        Err(e) => return Err(e),
                    }
                }
                // Normal case
                new_func.instrs.extend(inner);
                self.push_instr(Instr::EndArray {
                    span,
                    boxed: arr.boxes,
                });
                if !call {
                    let new_func = self.new_functions.pop().unwrap();
                    let sig = instrs_signature(&new_func.instrs).unwrap_or(Signature::new(0, 0));
                    let func = self.make_function(FunctionId::Anonymous(word.span), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            Word::Func(func) => self.func(func, word.span, call)?,
            Word::Pack(pack) => {
                if pack.angled {
                    self.switch(
                        pack.branches
                            .into_iter()
                            .map(|sp| sp.map(Word::Func))
                            .collect(),
                        word.span,
                        call,
                    )?
                } else {
                    self.add_error(
                        word.span.clone(),
                        "Function packs are not allowed without a modifier",
                    );
                    if let Some(first) = pack.branches.into_iter().next() {
                        self.word(first.map(Word::Func), call)?;
                    }
                }
            }
            Word::Primitive(p) => self.primitive(p, word.span, call)?,
            Word::Modified(m) => self.modified(*m, None, call)?,
            Word::Placeholder(_) => {
                // We could error here, but it's easier to handle it higher up
            }
            Word::SemanticComment(sc) => match sc {
                SemanticComment::Experimental => self.scope.experimental = true,
                SemanticComment::NoInline => {
                    if let Some(new_func) = self.new_functions.last_mut() {
                        new_func.flags |= FunctionFlags::NO_INLINE;
                    }
                }
                SemanticComment::TrackCaller => {
                    if let Some(new_func) = self.new_functions.last_mut() {
                        new_func.flags |= FunctionFlags::TRACK_CALLER;
                    }
                }
                SemanticComment::Boo => {
                    self.add_error(word.span.clone(), "The compiler is scared!")
                }
            },
            Word::OutputComment { i, n } => self.push_instr(Instr::SetOutputComment { i, n }),
            Word::Subscript(sub) => self.subscript(*sub, word.span, call)?,
            Word::Comment(_) | Word::Spaces | Word::BreakLine | Word::FlipLine => {}
        }
        Ok(())
    }
    /// Emit a warning if a loop inside an array could
    /// potentially pull in a variable number of values
    fn validate_array_loop_sig(&mut self, instrs: &[Instr], span: &CodeSpan) -> Option<Signature> {
        let inner_sig = instrs_signature(instrs);
        if self.current_bindings.is_empty() && !matches!(self.scope.kind, ScopeKind::Temp(_)) {
            return inner_sig.ok();
        }
        let Err(e) = &inner_sig else {
            // Case where repeat's function has a balanced signature
            // This is fine in other contexts, so an error is not returned
            // from the signature check, but it is not okay in an array.
            if let Some(i) = instrs
                .iter()
                .position(|instr| {
                    matches!(
                        instr,
                        Instr::Prim(Primitive::Repeat, _)
                            | Instr::ImplPrim(ImplPrimitive::RepeatWithInverse, _)
                    )
                })
                .filter(|&i| i > 0)
            {
                if let Instr::PushFunc(f) = &instrs[i - 1] {
                    let body_sig = f.signature();
                    let before_sig = instrs_signature(&instrs[..i - 1]).ok()?;
                    let after_sig = instrs_signature(&instrs[i + 1..]).ok()?;
                    if body_sig.args == body_sig.outputs
                        && before_sig.args < body_sig.args
                        && before_sig.outputs <= body_sig.args
                        && after_sig.args.saturating_sub(body_sig.outputs) < body_sig.args
                    {
                        let replacement: String = repeat('⊙')
                            .take(body_sig.args.saturating_sub(1))
                            .chain(['∘'])
                            .collect();
                        let message = format!(
                            "This array contains a loop with an equal number \
                            of arguments and outputs. This may result in a \
                            variable number of values being pulled into the \
                            array. To fix this, insert `{replacement}` on the \
                            right side of the array.",
                        );
                        self.emit_diagnostic(message, DiagnosticKind::Warning, span.clone());
                    }
                }
            }
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
            SigCheckErrorKind::LoopVariable { sig: body_sig, inf } => {
                let positive_body_sig = body_sig.outputs >= body_sig.args;
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
                    let max_after_args_required_by_before = before_sig
                        .outputs
                        .saturating_sub(before_sig.args)
                        .max(before_sig.args);
                    if after_sig.args > max_after_args_required_by_before {
                        if after_sig.args > body_sig.args {
                            let replacement: String = repeat('⊙')
                                .take(after_sig.args - body_sig.args)
                                .chain(['∘'])
                                .collect();
                            message.push_str(&format!(
                                " To fix this, insert `{replacement}` on the \
                                right side of the array."
                            ));
                        } else {
                            let replacement: String =
                                repeat('⊙').take(body_sig.args - 1).chain(['∘']).collect();
                            message.push_str(&format!(
                                " To fix this, insert `{replacement}` to the \
                                left of the loop."
                            ));
                        }
                        self.emit_diagnostic(message, DiagnosticKind::Warning, span.clone())
                    }
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
            if let Some(local) = module.names.get(&r.name.value).copied().or_else(|| {
                (r.name.value.strip_suffix('!')).and_then(|name| {
                    module.names.get(name).copied().filter(|local| {
                        matches!(&self.asm.bindings[local.index].kind, BindingKind::Module(_))
                    })
                })
            }) {
                Ok((path_locals, local))
            } else {
                Err(self.fatal_error(
                    r.name.span.clone(),
                    format!("Item `{}` not found", r.name.value,),
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
        // Attempt to look up the identifier as a non-macro
        let as_non_macro = self.find_name(name.strip_suffix('!')?, skip_local)?;
        if let BindingKind::Module(_) = self.asm.bindings[as_non_macro.index].kind {
            // Only allow it if it is a module
            Some(as_non_macro)
        } else {
            None
        }
    }
    fn ref_path(
        &self,
        path: &[RefComponent],
        skip_local: bool,
    ) -> UiuaResult<Option<(&Module, Vec<LocalName>)>> {
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
            BindingKind::Import(path) => &self.imports[path],
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
            BindingKind::IndexMacro(_) => {
                return Err(self.fatal_error(
                    first.module.span.clone(),
                    format!("`{}` is an index macro, not a module", first.module.value),
                ))
            }
            BindingKind::CodeMacro(_) => {
                return Err(self.fatal_error(
                    first.module.span.clone(),
                    format!("`{}` is a code macro, not a module", first.module.value),
                ))
            }
        };
        for comp in path.iter().skip(1) {
            let submod_local = module
                .names
                .get(&comp.module.value)
                .copied()
                .ok_or_else(|| {
                    self.fatal_error(
                        comp.module.span.clone(),
                        format!("Module `{}` not found", comp.module.value),
                    )
                })?;
            path_locals.push(submod_local);
            let global = &self.asm.bindings[submod_local.index].kind;
            module = match global {
                BindingKind::Import(path) => &self.imports[path],
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
                BindingKind::IndexMacro(_) => {
                    return Err(self.fatal_error(
                        comp.module.span.clone(),
                        format!(
                            "`{}` is a positional macro, not a module",
                            comp.module.value
                        ),
                    ))
                }
                BindingKind::CodeMacro(_) => {
                    return Err(self.fatal_error(
                        comp.module.span.clone(),
                        format!("`{}` is a code macro, not a module", comp.module.value),
                    ))
                }
            };
        }

        Ok(Some((module, path_locals)))
    }
    fn reference(&mut self, r: Ref, call: bool) -> UiuaResult {
        if r.path.is_empty() {
            self.ident(r.name.value, r.name.span, call, r.in_macro_arg)
        } else {
            let (path_locals, local) = self.ref_local(&r)?;
            self.validate_local(&r.name.value, local, &r.name.span);
            for (local, comp) in path_locals.into_iter().zip(&r.path) {
                self.validate_local(&comp.module.value, local, &comp.module.span);
                (self.code_meta.global_references).insert(comp.module.span.clone(), local.index);
            }
            self.code_meta
                .global_references
                .insert(r.name.span.clone(), local.index);
            self.global_index(local.index, r.name.span, call);
            Ok(())
        }
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan, call: bool, skip_local: bool) -> UiuaResult {
        if let Some(curr) = (self.current_bindings.last_mut()).filter(|curr| curr.name == ident) {
            // Name is a recursive call
            let Some(sig) = curr.signature else {
                return Err(self.fatal_error(
                    span,
                    format!(
                        "Recursive function `{ident}` must have a \
                        signature declared after the ←."
                    ),
                ));
            };
            curr.referenced = true;
            (self.code_meta.global_references).insert(span.clone(), curr.global_index);
            let instr = Instr::Recur(self.add_span(span.clone()));
            if call {
                self.push_all_instrs(eco_vec![Instr::PushSig(sig), instr, Instr::PopSig]);
            } else {
                let f =
                    self.make_function(FunctionId::Anonymous(span), sig, eco_vec![instr].into());
                self.push_instr(Instr::PushFunc(f));
            }
        } else if let Some(local) = self.find_name(&ident, skip_local) {
            // Name exists in scope
            (self.code_meta.global_references).insert(span.clone(), local.index);
            self.global_index(local.index, span, call);
        } else if let Some(constant) = CONSTANTS.iter().find(|c| c.name == ident) {
            // Name is a built-in constant
            let instr = Instr::push(
                constant
                    .value
                    .resolve(self.scope_file_path(), &*self.backend()),
            );
            self.code_meta
                .constant_references
                .insert(span.clone().sp(ident));
            if call {
                self.push_instr(instr);
            } else {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    eco_vec![instr].into(),
                );
                self.push_instr(Instr::PushFunc(f));
            }
        } else {
            return Err(self.fatal_error(span, format!("Unknown identifier `{ident}`")));
        }
        Ok(())
    }
    fn scope_file_path(&self) -> Option<&Path> {
        for scope in once(&self.scope).chain(self.higher_scopes.iter().rev()) {
            if let Some(file_path) = &scope.file_path {
                return Some(file_path);
            }
        }
        None
    }
    fn global_index(&mut self, index: usize, span: CodeSpan, call: bool) {
        let global = self.asm.bindings[index].kind.clone();
        match global {
            BindingKind::Const(Some(val)) if call => self.push_instr(Instr::push(val)),
            BindingKind::Const(Some(val)) => {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    eco_vec![Instr::push(val)].into(),
                );
                self.push_instr(Instr::PushFunc(f));
            }
            BindingKind::Const(None) if call => self.push_instr(Instr::CallGlobal {
                index,
                call: true,
                sig: Signature::new(0, 1),
            }),
            BindingKind::Const(None) => {
                let f = self.make_function(
                    FunctionId::Anonymous(span),
                    Signature::new(0, 1),
                    eco_vec![Instr::CallGlobal {
                        index,
                        call: true,
                        sig: Signature::new(0, 1)
                    }]
                    .into(),
                );
                self.push_instr(Instr::PushFunc(f));
            }
            BindingKind::Func(f) if self.inlinable(f.instrs(&self.asm), f.flags) => {
                if call {
                    // Inline instructions
                    self.push_all_instrs(EcoVec::from(f.instrs(&self.asm)));
                } else {
                    self.push_instr(Instr::PushFunc(f));
                }
            }
            BindingKind::Func(f) => {
                if let Some(new_func) = self.new_functions.last_mut() {
                    if f.flags.track_caller() {
                        new_func.flags |= FunctionFlags::NO_PRE_EVAL;
                    }
                }
                self.push_instr(Instr::PushFunc(f));
                if call {
                    let span = self.add_span(span);
                    self.push_instr(Instr::Call(span));
                }
            }
            BindingKind::Import { .. } => self.add_error(span, "Cannot import module item here."),
            BindingKind::Module(m) => {
                if let Some(local) = m.names.get("Call").or_else(|| m.names.get("New")) {
                    self.code_meta.global_references.remove(&span);
                    self.code_meta
                        .global_references
                        .insert(span.clone(), local.index);
                    self.global_index(local.index, span, call);
                } else {
                    self.add_error(span, "Cannot import module item here.");
                }
            }
            BindingKind::IndexMacro(_) | BindingKind::CodeMacro(_) => {
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
            let (_, _, instrs) = self.compile_func_instrs(func, span, false)?;
            self.push_all_instrs(instrs);
        } else {
            let function = self.compile_func(func, span.clone())?;
            self.push_instr(Instr::PushFunc(function));
        }
        Ok(())
    }
    fn compile_func(&mut self, func: Func, span: CodeSpan) -> UiuaResult<Function> {
        let (id, sig, new_func) = self.compile_func_instrs(func, span, true)?;
        let sig = sig.unwrap();

        if let [Instr::PushFunc(f), Instr::Call(_)] = new_func.instrs.as_slice() {
            return Ok(Function::clone(f));
        }

        Ok(self.make_function(id, sig, new_func))
    }
    fn compile_func_instrs(
        &mut self,
        func: Func,
        span: CodeSpan,
        require_valid_sig: bool,
    ) -> UiuaResult<(FunctionId, Option<Signature>, NewFunction)> {
        let mut new_func = NewFunction::default();
        for line in func.lines {
            let nf = self.compile_line(line, true)?;
            new_func.instrs.extend(nf.instrs);
            new_func.flags |= nf.flags;
        }

        // Validate signature
        let sig = match instrs_signature(&new_func.instrs) {
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
                Some(sig)
            }
            Err(e) => {
                if let Some(declared_sig) = &func.signature {
                    if e.kind == SigCheckErrorKind::Ambiguous {
                        Some(declared_sig.value)
                    } else {
                        return Err(self.fatal_error(
                            declared_sig.span.clone(),
                            format!(
                                "Cannot infer function signature: {e}. \
                                An explicit signature can only be used \
                                with ambiguous functions."
                            ),
                        ));
                    }
                } else if require_valid_sig {
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
                } else {
                    None
                }
            }
        };
        if let Some(sig) = sig {
            self.code_meta.function_sigs.insert(
                span.clone(),
                SigDecl {
                    sig,
                    explicit: func.signature.is_some(),
                    inline: true,
                },
            );
        }
        Ok((func.id, sig, new_func))
    }
    fn switch(&mut self, branches: Vec<Sp<Word>>, span: CodeSpan, call: bool) -> UiuaResult {
        let count = branches.len();
        if !call {
            self.new_functions.push(NewFunction::default());
        }
        // Compile branches
        let mut functions = Vec::with_capacity(count);
        let mut rigid_indices = Vec::new();
        let mut flex_indices = Vec::new();
        for (i, branch) in branches.into_iter().enumerate() {
            let span = branch.span.clone();
            let (new_func, sig) = self.compile_operand_word(branch)?;
            let is_flex = (new_func.instrs.iter())
                .rposition(|instr| matches!(instr, Instr::Prim(Primitive::Assert, _)))
                .is_some_and(|end| {
                    (0..end).rev().any(|start| {
                        let sub = &new_func.instrs[start..end];
                        match sub {
                            [Instr::Push(val), Instr::Prim(Primitive::Dup, _)]
                            | [Instr::Push(val), Instr::Push(..)]
                                if val != &Value::from(1) =>
                            {
                                return true;
                            }
                            [Instr::Format { .. }, Instr::Prim(Primitive::Dup, _)] => return true,
                            _ => (),
                        }
                        if !(instrs_are_pure(sub, &self.asm, Purity::Pure)
                            && instrs_signature(sub).is_ok_and(|sig| sig == (0, 2)))
                        {
                            return false;
                        }
                        let mut comp = self.clone();
                        let func = comp.make_function(
                            FunctionId::Anonymous(span.clone()),
                            Signature::new(0, 2),
                            EcoVec::from(sub).into(),
                        );
                        comp.macro_env.asm = comp.asm.clone();
                        comp.macro_env
                            .call(func)
                            .and_then(|_| {
                                let _message = comp.macro_env.pop(1)?;
                                let flag = comp.macro_env.pop(2)?;
                                Ok(flag != Value::from(1))
                            })
                            .unwrap_or(false)
                    })
                });
            functions.push((new_func, sig, span));
            if is_flex {
                flex_indices.push(i);
            } else {
                rigid_indices.push(i);
            }
        }
        let mut rigid_funcs = rigid_indices.into_iter().map(|i| &functions[i]);
        let mut sig = None;
        if let Some((_, f_sig, _)) = rigid_funcs.next() {
            sig = Some(*f_sig);
            let sig = sig.as_mut().unwrap();
            // Compile remaining branches
            for (_, f_sig, span) in rigid_funcs {
                if f_sig.is_compatible_with(*sig) {
                    *sig = sig.max_with(*f_sig);
                } else if f_sig.outputs == sig.outputs {
                    sig.args = sig.args.max(f_sig.args)
                } else {
                    self.add_error(
                        span.clone(),
                        format!(
                            "Switch branch's signature {f_sig} is \
                            incompatible with previous branches {sig}",
                        ),
                    );
                }
            }
        }
        let mut flex_funcs = flex_indices.into_iter().map(|i| &functions[i]);
        let mut sig = sig.unwrap_or_else(|| flex_funcs.next().unwrap().1);
        for (_, f_sig, _) in flex_funcs {
            sig.args = sig.args.max(f_sig.args);
        }

        for (new_func, sig, span) in functions {
            let id = FunctionId::Anonymous(span);
            let function = self.make_function(id, sig, new_func);
            self.push_instr(Instr::PushFunc(function));
        }

        let span_idx = self.add_span(span.clone());
        self.push_instr(Instr::Switch {
            count,
            sig,
            span: span_idx,
            under_cond: false,
        });
        if !call {
            let new_func = self.new_functions.pop().unwrap();
            let sig = match instrs_signature(&new_func.instrs) {
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
            let function = self.make_function(FunctionId::Anonymous(span), sig, new_func);
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
        if prim.is_experimental() {
            self.experimental_error(span, || {
                format!(
                    "{} is experimental. To use it, add \
                    `# Experimental!` to the top of the file.",
                    prim.format()
                )
            });
        }
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan, call: bool) -> UiuaResult {
        self.handle_primitive_experimental(prim, &span);
        self.handle_primitive_deprecation(prim, &span);
        let spandex = self.add_span(span.clone());
        self.instructions(
            FunctionId::Primitive(prim),
            eco_vec![Instr::Prim(prim, spandex)],
            &span,
            call,
        )
    }
    fn instructions(
        &mut self,
        function_id: FunctionId,
        instrs: EcoVec<Instr>,
        span: &CodeSpan,
        call: bool,
    ) -> UiuaResult {
        if call {
            self.push_all_instrs(instrs);
        } else {
            let sig = self.sig_of(&instrs, span)?;
            let func = self.make_function(function_id, sig, instrs.into());
            self.push_instr(Instr::PushFunc(func));
        }
        Ok(())
    }
    #[allow(clippy::match_single_binding)]
    fn subscript(&mut self, sub: Subscript, span: CodeSpan, call: bool) -> UiuaResult {
        if !matches!(sub.word.value, Word::Primitive(Primitive::Utf8)) {
            self.experimental_error(&span, || {
                "Subscripts are experimental. To use them, add \
                `# Experimental!` to the top of the file."
            });
        }
        let n = sub.n.value;
        match sub.word.value {
            Word::Modified(m) => match m.modifier.value {
                Modifier::Ref(_) => {
                    self.add_error(span, "Subscripts are not implemented for macros");
                    self.modified(*m, Some(n), call)?;
                }
                Modifier::Primitive(prim) => match prim {
                    _ => {
                        if !matches!(
                            prim,
                            Primitive::Both
                                | Primitive::Repeat
                                | Primitive::Rows
                                | Primitive::Each
                                | Primitive::Tuples
                        ) {
                            self.add_error(
                                span,
                                format!("Subscripts are not implemented for {}", prim.format()),
                            );
                        }
                        self.modified(*m, Some(n), call)?;
                    }
                },
            },
            Word::Primitive(prim) => {
                if !call {
                    self.new_functions.push(NewFunction::default());
                }
                let sp = span.clone();
                match prim {
                    prim if prim.signature().is_some_and(|sig| sig == (2, 1))
                        && prim.subscript_sig(2).is_some_and(|sig| sig == (1, 1)) =>
                    {
                        self.word(sub.n.map(|n| Word::Number(n.to_string(), n as f64)), true)?;
                        self.primitive(prim, span, true)?;
                    }
                    Primitive::Transpose => {
                        if n > 100 {
                            self.add_error(span.clone(), "Too many subscript repetitions");
                        }
                        for _ in 0..n.min(100) {
                            self.primitive(prim, span.clone(), true)?;
                        }
                    }
                    Primitive::Sqrt => {
                        if n == 0 {
                            self.add_error(span.clone(), "Cannot take 0th root");
                        }
                        self.push_instr(Instr::push(1.0 / n.max(1) as f64));
                        self.primitive(Primitive::Pow, span, true)?;
                    }
                    Primitive::Round | Primitive::Floor | Primitive::Ceil => {
                        let mul = 10f64.powi(n as i32);
                        self.push_instr(Instr::push(mul));
                        self.primitive(Primitive::Mul, span.clone(), true)?;
                        self.primitive(prim, span.clone(), true)?;
                        self.push_instr(Instr::push(mul));
                        self.primitive(Primitive::Div, span, true)?;
                    }
                    Primitive::Rand => {
                        self.primitive(Primitive::Rand, span.clone(), true)?;
                        self.push_instr(Instr::push(n));
                        self.primitive(Primitive::Mul, span.clone(), true)?;
                        self.primitive(Primitive::Floor, span, true)?;
                    }
                    Primitive::Utf8 => {
                        if n != 8 {
                            self.add_error(span.clone(), "Only UTF-8 is supported");
                        }
                        self.primitive(prim, span, true)?
                    }
                    Primitive::Couple => match n {
                        1 => self.primitive(Primitive::Fix, span, true)?,
                        2 => self.primitive(Primitive::Couple, span, true)?,
                        n => {
                            let span = self.add_span(span.clone());
                            self.push_instr(Instr::BeginArray);
                            if n > 0 {
                                self.push_instr(Instr::TouchStack { count: n, span });
                            }
                            self.push_instr(Instr::EndArray { boxed: false, span });
                        }
                    },
                    Primitive::Box => {
                        let span = self.add_span(span.clone());
                        self.push_instr(Instr::BeginArray);
                        if n > 0 {
                            self.push_instr(Instr::TouchStack { count: n, span });
                        }
                        self.push_instr(Instr::EndArray { boxed: true, span });
                    }
                    Primitive::Stack => {
                        let span = self.add_span(span.clone());
                        self.push_instr(Instr::ImplPrim(
                            ImplPrimitive::TraceN {
                                n,
                                inverse: false,
                                stack_sub: true,
                            },
                            span,
                        ));
                    }
                    _ => {
                        self.add_error(
                            span.clone(),
                            format!("Subscripts are not implemented for {}", prim.format()),
                        );
                        self.primitive(prim, span, true)?;
                    }
                }
                if !call {
                    let new_func = self.new_functions.pop().unwrap();
                    let sig = self.sig_of(&new_func.instrs, &sp)?;
                    let func = self.make_function(FunctionId::Anonymous(sp), sig, new_func);
                    self.push_instr(Instr::PushFunc(func));
                }
            }
            _ => {
                self.word(sub.word, call)?;
                self.add_error(span.clone(), "Subscripts are not allowed in this context");
            }
        }
        Ok(())
    }
    pub(crate) fn inlinable(&self, instrs: &[Instr], flags: FunctionFlags) -> bool {
        use ImplPrimitive::*;
        use Primitive::*;
        if flags.track_caller() || flags.no_inline() || flags.no_pre_eval() {
            return false;
        }
        if instrs.len() > 10 {
            return false;
        }
        for instr in instrs {
            match instr {
                Instr::Prim(Trace | Dump | Stack | Assert, _) => return false,
                Instr::ImplPrim(UnDump | UnStack | TraceN { .. }, _) => return false,
                Instr::PushFunc(f)
                    if !self.inlinable(f.instrs(&self.asm), FunctionFlags::default()) =>
                {
                    return false
                }
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
        span: impl Into<Span>,
    ) {
        let inputs = self.asm.inputs.clone();
        self.emit_diagnostic_impl(Diagnostic::new(message.into(), span, kind, inputs));
    }
    fn emit_diagnostic_impl(&mut self, diagnostic: Diagnostic) {
        if self.print_diagnostics {
            println!("{}", diagnostic.report()); // Allow println
        } else {
            self.diagnostics.insert(diagnostic);
        }
    }
    fn add_error(&mut self, span: impl Into<Span>, message: impl ToString) {
        let e = UiuaErrorKind::Run(
            span.into().sp(message.to_string()),
            self.asm.inputs.clone().into(),
        )
        .into();
        self.errors.push(e);
    }
    fn experimental_error<S>(&mut self, span: &CodeSpan, message: impl FnOnce() -> S)
    where
        S: ToString,
    {
        if !self.scope.experimental && !self.scope.experimental_error {
            self.scope.experimental_error = true;
            self.add_error(span.clone(), message().to_string());
        }
    }
    fn fatal_error(&self, span: impl Into<Span>, message: impl ToString) -> UiuaError {
        UiuaErrorKind::Run(
            span.into().sp(message.to_string()),
            self.asm.inputs.clone().into(),
        )
        .into()
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
                        .filter(|_| !matches!(self.scope.kind, ScopeKind::Module(_)))
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
            eco_vec![Instr::Dynamic(DynamicFunction { index, signature })].into(),
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
        self.next_global += 1;
        self.compile_bind_function(name.clone(), local, function, 0, None)?;
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
    fn pre_eval_instrs(&mut self, mut new_func: NewFunction) -> (NewFunction, Vec<UiuaError>) {
        let mut errors = Vec::new();
        new_func.instrs = optimize_instrs(new_func.instrs, true, &self.asm);
        if self.in_inverse
            || self.pre_eval_mode == PreEvalMode::Lazy
            || (new_func.instrs.iter()).all(|instr| matches!(instr, Instr::Push(_)))
            || new_func.flags.no_inline()
            || new_func.flags.track_caller()
            || new_func.flags.no_pre_eval()
        {
            return (new_func, errors);
        }
        let allow_error = instrs_are_pure(&new_func.instrs, &self.asm, Purity::Pure);
        // println!("pre eval {:?}", new_func);
        let mut start = 0;
        let mut new_instrs: Option<EcoVec<Instr>> = None;
        'start: while start < new_func.instrs.len() {
            for end in (start + 1..=new_func.instrs.len()).rev() {
                let section = &new_func.instrs[start..end];
                if !instrs_can_pre_eval(section, &self.asm) {
                    continue;
                }
                if instrs_are_pure(section, &self.asm, Purity::Pure)
                    && instrs_clean_signature(section)
                        .is_some_and(|sig| sig.args == 0 && sig.outputs > 0)
                {
                    let mut success = false;
                    match self.comptime_instrs(section.into()) {
                        Ok(Some(values)) => {
                            // println!("section: {section:?}");
                            // println!("values: {values:?}");
                            for val in &values {
                                val.validate_shape();
                            }
                            let new_instrs =
                                new_instrs.get_or_insert_with(|| new_func.instrs[..start].into());
                            new_instrs.extend(values.into_iter().map(Instr::Push));
                            success = true;
                        }
                        Ok(None) => {}
                        Err(e) if !allow_error || e.is_fill || self.in_try => {}
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
                new_instrs.push(new_func.instrs[start].clone())
            }
            start += 1;
        }
        // if let Some(new_instrs) = &new_instrs {
        //     println!("eval: {new_instrs:?}")
        // }
        new_func.instrs = new_instrs.unwrap_or(new_func.instrs);
        (new_func, errors)
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
            if len > 0 {
                asm.top_slices.push(FuncSlice { start, len });
            }
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
                    let res = if stack.iter().any(|v| {
                        v.element_count() > MAX_PRE_EVAL_ELEMS || v.rank() > MAX_PRE_EVAL_RANK
                    }) {
                        None
                    } else {
                        Some(stack)
                    };
                    cache.borrow_mut().insert(instrs, res.clone());
                    Ok(res)
                }
                Err(e) if matches!(e.kind, UiuaErrorKind::Timeout(..)) => {
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
                Instr::Prim(Obverse | SetInverse | SetUnder, _)
                    | Instr::ImplPrim(ImplPrimitive::UnPop, _)
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
        Word::Primitive(Dup | Dip | Identity | Fork | Under | Each) => true,
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
            Word::Func(func) => func.lines.iter().rev().for_each(|line| {
                ops.extend(collect_placeholder(line));
            }),
            Word::Modified(m) => ops.extend(collect_placeholder(&m.operands)),
            Word::Pack(pack) => pack.branches.iter().for_each(|branch| {
                (branch.value.lines.iter()).for_each(|line| ops.extend(collect_placeholder(line)))
            }),
            _ => {}
        }
    }
    ops
}

fn set_in_macro_arg(words: &mut Vec<Sp<Word>>) {
    recurse_words_mut(words, &mut |word| match &mut word.value {
        Word::Ref(r) => r.in_macro_arg = true,
        Word::IncompleteRef { in_macro_arg, .. } => *in_macro_arg = true,
        _ => {}
    });
}

fn recurse_words(words: &[Sp<Word>], f: &mut dyn FnMut(&Sp<Word>)) {
    for word in words {
        f(word);
        match &word.value {
            Word::Strand(items) => recurse_words(items, f),
            Word::Array(arr) => arr.lines.iter().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Func(func) => func.lines.iter().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Modified(m) => recurse_words(&m.operands, f),
            Word::Pack(pack) => pack.branches.iter().for_each(|branch| {
                (branch.value.lines.iter()).for_each(|line| recurse_words(line, f))
            }),
            _ => {}
        }
    }
}

fn recurse_words_mut(words: &mut Vec<Sp<Word>>, f: &mut dyn FnMut(&mut Sp<Word>)) {
    for word in words {
        f(word);
        match &mut word.value {
            Word::Strand(items) => recurse_words_mut(items, f),
            Word::Array(arr) => arr.lines.iter_mut().for_each(|line| {
                recurse_words_mut(line, f);
            }),
            Word::Func(func) => func.lines.iter_mut().for_each(|line| {
                recurse_words_mut(line, f);
            }),
            Word::Modified(m) => recurse_words_mut(&mut m.operands, f),
            Word::Pack(pack) => pack.branches.iter_mut().for_each(|branch| {
                (branch.value.lines.iter_mut()).for_each(|line| recurse_words_mut(line, f))
            }),
            _ => {}
        }
    }
}

fn line_sig(line: &[Sp<Word>]) -> Option<Sp<DocCommentSig>> {
    line.split_last()
        .and_then(|(last, mut rest)| match &last.value {
            Word::Comment(c) => c.parse::<DocCommentSig>().ok().map(|sig| {
                while rest.last().is_some_and(|w| matches!(w.value, Word::Spaces)) {
                    rest = &rest[..rest.len() - 1];
                }
                let span = (rest.first())
                    .zip(rest.last())
                    .map(|(f, l)| f.span.clone().merge(l.span.clone()))
                    .unwrap_or_else(|| last.span.clone());
                span.sp(sig)
            }),
            _ => None,
        })
}
