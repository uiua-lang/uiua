pub(crate) mod algebra;
mod binding;
mod data;
pub(crate) mod invert;
mod modifier;
pub(crate) mod optimize;
mod pre_eval;

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    env::current_dir,
    fmt, fs,
    iter::{once, repeat_n},
    mem::{replace, swap, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    slice,
    sync::Arc,
};

use ecow::{eco_vec, EcoString, EcoVec};
use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};

use crate::{
    algorithm::ga::Spec,
    ast::*,
    check::nodes_sig,
    format::{format_word, format_words},
    function::DynamicFunction,
    lsp::{CodeMeta, Completion, ImportSrc, SetInverses, SigDecl},
    parse::{
        flip_unsplit_items, flip_unsplit_lines, ident_modifier_args, max_placeholder, parse,
        split_items, split_words,
    },
    Array, ArrayValue, Assembly, BindingKind, BindingMeta, Boxed, CodeSpan, CustomInverse,
    Diagnostic, DiagnosticKind, DocComment, DocCommentSig, ExactDoubleIterator, Function,
    FunctionId, GitTarget, Ident, ImplPrimitive, InputSrc, IntoInputSrc, IntoSysBackend, Node,
    NumericSubscript, PrimClass, Primitive, Purity, RunMode, SemanticComment, SigNode, Signature,
    Sp, Span, SubSide, Subscript, SysBackend, Uiua, UiuaError, UiuaErrorKind, UiuaResult, Value,
    CONSTANTS, EXAMPLE_UA, SUBSCRIPT_DIGITS, VERSION,
};
pub(crate) use modifier::*;
pub use pre_eval::PreEvalMode;

/// The Uiua compiler
#[derive(Clone)]
pub struct Compiler {
    pub(crate) asm: Assembly,
    pub(crate) code_meta: CodeMeta,
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
    /// Indices of named external functions
    externals: HashMap<Ident, usize>,
    /// The depth of compile-time evaluation
    comptime_depth: usize,
    /// Whether the compiler is in a try
    in_try: bool,
    /// Accumulated errors
    errors: Vec<UiuaError>,
    /// Primitives that have emitted errors because they are deprecated
    deprecated_prim_errors: HashSet<Primitive>,
    /// Constants that have emitted errors because they are deprecated
    deprecated_const_errors: HashSet<&'static str>,
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
    /// Start addresses
    start_addrs: Vec<usize>,
    /// Primitive optional arg getter bindings
    prim_arg_bindings: HashMap<(Primitive, Ident), usize>,
}

impl Default for Compiler {
    fn default() -> Self {
        Compiler {
            asm: Assembly::default(),
            code_meta: CodeMeta::default(),
            current_bindings: Vec::new(),
            next_global: 0,
            scope: Scope::default(),
            higher_scopes: Vec::new(),
            mode: RunMode::All,
            current_imports: Vec::new(),
            imports: HashMap::new(),
            index_macros: HashMap::new(),
            code_macros: HashMap::new(),
            externals: HashMap::new(),
            comptime_depth: 0,
            in_try: false,
            errors: Vec::new(),
            deprecated_prim_errors: HashSet::new(),
            deprecated_const_errors: HashSet::new(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: false,
            comptime: true,
            pre_eval_mode: PreEvalMode::default(),
            macro_env: Uiua::default(),
            start_addrs: Vec::new(),
            prim_arg_bindings: HashMap::new(),
        }
    }
}

#[derive(Debug, Default)]
struct BindingPrelude {
    comment: Option<EcoString>,
    track_caller: bool,
    no_inline: bool,
    external: bool,
    deprecation: Option<EcoString>,
}

type LocalNames = IndexMap<Ident, LocalName>;

/// A Uiua module
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Module {
    /// The top level comment
    pub comment: Option<EcoString>,
    /// Map module-local names to global indices
    pub names: LocalNames,
    /// Whether the mode is a data function
    data_func: bool,
    /// Whether the module uses experimental features
    experimental: bool,
}

/// An index macro
#[derive(Clone)]
struct IndexMacro {
    words: Vec<Sp<Word>>,
    /// Map of spans of identifiers used in the macro that were in scope
    /// when the macro was declared to their local indices. This is used
    /// for name resolution. It is keyed by span rather than by name so
    /// that names in both the declaration and invocation's scope can
    /// be disambiguated.
    locals: HashMap<CodeSpan, usize>,
    sig: Option<Signature>,
    recursive: bool,
}

/// A code macro
#[derive(Clone)]
struct CodeMacro {
    root: SigNode,
    names: LocalNames,
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

#[derive(Debug, Clone)]
struct CurrentBinding {
    name: Ident,
    signature: Option<Signature>,
    recurses: usize,
    global_index: usize,
}

/// A scope where names are defined
#[derive(Debug, Clone)]
pub(crate) struct Scope {
    kind: ScopeKind,
    /// The name of the current file, if any
    file_path: Option<PathBuf>,
    /// The top level comment
    comment: Option<EcoString>,
    /// Map local names to global indices
    names: LocalNames,
    /// Whether the scope has a data def defined
    has_data_def: bool,
    /// Whether the scope's data def is a data function
    is_data_func: bool,
    /// Names of data variants
    data_variants: IndexSet<EcoString>,
    /// Whether to allow experimental features
    pub experimental: bool,
    /// Whether an error has been emitted for experimental features
    experimental_error: bool,
    /// Whether an error has been emitted for fill function signatures
    fill_sig_error: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ScopeKind {
    /// A scope at the top level of a file
    File(FileScopeKind),
    /// A scope in a named module
    Module(Ident),
    /// A scope that includes all bindings in a module
    AllInModule,
    /// A temporary scope, probably for a macro
    Macro(Option<MacroLocal>),
    /// A binding scope
    Binding,
    /// A function scope between some delimiters
    Function,
    /// A test scope between `---`s
    Test,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FileScopeKind {
    Source,
    Git,
}

/// Indices of an index macro's locals
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MacroLocal {
    macro_index: usize,
    expansion_index: Option<usize>,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            kind: ScopeKind::File(FileScopeKind::Source),
            file_path: None,
            comment: None,
            names: IndexMap::new(),
            has_data_def: false,
            is_data_func: false,
            data_variants: IndexSet::new(),
            experimental: false,
            experimental_error: false,
            fill_sig_error: false,
        }
    }
}

impl Scope {
    pub(crate) fn add_module_name(&mut self, name: EcoString, local: LocalName) {
        self.names.swap_remove(format!("{name}!").as_str());
        self.names.insert(name, local);
    }
}

/// The index of a named local in the bindings, and whether it is public
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct LocalName {
    /// The index of the binding in assembly's bindings
    pub index: usize,
    /// Whether the binding is public
    pub public: bool,
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
    /// Enable experimental features
    pub fn experimental(&mut self, experimental: bool) -> &mut Self {
        self.scope.experimental = experimental;
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
    /// Set the system backend
    pub fn set_backend<T: SysBackend>(&mut self, backend: T) {
        self.macro_env.rt.backend = Arc::new(backend);
    }
    /// Compile a Uiua file from a file at a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<&mut Self> {
        let path = path.as_ref();
        let input: EcoString = fs::read_to_string(path)
            .map_err(|e| UiuaErrorKind::Load(path.into(), e.into()))?
            .into();
        // _ = crate::lsp::Spans::from_input(&input);
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
    fn scopes(&self) -> impl ExactDoubleIterator<Item = &Scope> {
        once(&self.scope)
            .chain(self.higher_scopes.iter().rev())
            .collect::<Vec<_>>()
            .into_iter()
    }
    #[allow(dead_code)]
    fn scopes_mut(&mut self) -> impl Iterator<Item = &mut Scope> {
        once(&mut self.scope).chain(self.higher_scopes.iter_mut().rev())
    }
    fn scopes_to_file(&self) -> impl Iterator<Item = &Scope> {
        let already_file = matches!(self.scope.kind, ScopeKind::File(_));
        once(&self.scope).chain(
            (!already_file)
                .then(|| {
                    let file_index = self
                        .higher_scopes
                        .iter()
                        .rposition(|s| matches!(s.kind, ScopeKind::File(_)))?;
                    Some(self.higher_scopes[file_index..].iter().rev())
                })
                .flatten()
                .into_iter()
                .flatten(),
        )
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
    ) -> UiuaResult<(Module, T)> {
        self.higher_scopes.push(take(&mut self.scope));
        self.scope.kind = kind;

        let res = f(self);

        let scope = replace(&mut self.scope, self.higher_scopes.pop().unwrap());

        let res = res?;
        let module = Module {
            comment: scope.comment,
            names: scope.names,
            data_func: scope.is_data_func,
            experimental: scope.experimental,
        };
        Ok((module, res))
    }
    fn load_impl(&mut self, input: &str, src: InputSrc) -> UiuaResult<&mut Self> {
        let node_start = self.asm.root.len();
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

        let base = 0u8;
        self.start_addrs.push(&base as *const u8 as usize);
        let res = self.catching_crash(input, |env| env.items(items, ItemCompMode::TopLevel));
        self.start_addrs.pop();

        // Optimize root
        // We only optimize if this is not an import
        let do_optimize = match src {
            InputSrc::File(_) => self.current_imports.len() <= 1,
            _ => self.current_imports.is_empty(),
        };
        if do_optimize {
            self.asm.root.optimize_full();
            // Optimize and pre-eval functions
            for i in 0..self.asm.functions.len() {
                self.asm.functions.make_mut()[i].optimize_full();
                if let Some((root, errs)) = self.pre_eval(&self.asm.functions[i]) {
                    self.asm.functions.make_mut()[i] = root;
                    self.errors.extend(errs);
                    self.asm.functions.make_mut()[i].optimize_full();
                }
            }
        }
        // dbg!(&self.asm.root);

        // Print diagnostics
        if self.print_diagnostics {
            for diagnostic in self.take_diagnostics() {
                eprintln!("{}", diagnostic.report());
            }
        }

        // Update top-level bindings
        self.code_meta.top_level_names = (self.scope.names.iter())
            .map(|(name, local)| (name.clone(), *local))
            .collect();

        if let InputSrc::File(_) = &src {
            self.current_imports.pop();
        }
        // Collect errors
        match res {
            Err(e) | Ok(Err(e)) => {
                self.asm.root.truncate(node_start);
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
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ItemCompMode {
    TopLevel,
    Function,
    CodeMacro,
}

impl Compiler {
    fn items(&mut self, items: Vec<Item>, mode: ItemCompMode) -> UiuaResult {
        // Set scope comment
        let mut started = false;
        let mut comment = String::new();
        let mut items_iter = items.iter();
        while let Some(Item::Words(line)) = items_iter.next() {
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
        }
        comment.push('\n');
        if !comment.trim().is_empty() {
            self.scope.comment = Some(comment.trim().into());
        }

        let mut prelude = BindingPrelude::default();
        let mut item_errored = false;
        let mut item_queue = VecDeque::from(flip_unsplit_items(split_items(items)));
        while let Some(item) = item_queue.pop_front() {
            let must_run = mode == ItemCompMode::CodeMacro
                || matches!(&item, Item::Words(_))
                    && item_queue.iter().any(|item| match item {
                        Item::Binding(binding)
                            if (binding.words.iter())
                                .filter(|word| word.value.is_code())
                                .count()
                                == 0 =>
                        {
                            true
                        }
                        Item::Words(line) => line
                            .iter()
                            .find(|w| w.value.is_code())
                            .is_some_and(|w| matches!(w.value, Word::Primitive(Primitive::Assert))),
                        _ => false,
                    });
            if let Err(e) = self.item(item, must_run, mode, &mut prelude) {
                if !item_errored || self.errors.is_empty() {
                    self.errors.push(e);
                }
                item_errored = true;
            }
        }
        Ok(())
    }
    fn item(
        &mut self,
        item: Item,
        must_run: bool,
        mode: ItemCompMode,
        prelude: &mut BindingPrelude,
    ) -> UiuaResult {
        match item {
            Item::Module(m) => self.module(m, take(prelude)),
            Item::Words(line) => self.top_level_words(line, must_run, mode, prelude),
            Item::Binding(binding) => self.binding(binding, take(prelude)),
            Item::Import(import) => self.import(import, take(prelude).comment),
            Item::Data(defs) => {
                for data in defs {
                    self.data_def(data, true, take(prelude))?
                }
                Ok(())
            }
        }
    }
    fn top_level_words(
        &mut self,
        mut line: Vec<Sp<Word>>,
        must_run: bool,
        mode: ItemCompMode,
        prelude: &mut BindingPrelude,
    ) -> UiuaResult {
        // Populate prelude
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
                Word::SemanticComment(SemanticComment::NoInline) => prelude.no_inline = true,
                Word::SemanticComment(SemanticComment::TrackCaller) => prelude.track_caller = true,
                Word::SemanticComment(SemanticComment::External) => prelude.external = true,
                Word::SemanticComment(SemanticComment::Deprecated(s)) => {
                    prelude.deprecation = Some(s.clone())
                }
                _ => *prelude = BindingPrelude::default(),
            }
        } else {
            *prelude = BindingPrelude::default();
        }

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
        let in_test = self.scopes().any(|sc| sc.kind == ScopeKind::Test);
        let can_run = mode != ItemCompMode::TopLevel
            || match self.mode {
                RunMode::Normal => !in_test,
                RunMode::Test => in_test,
                RunMode::All => true,
            };
        if line.is_empty() || !(can_run || must_run || words_should_run_anyway(&line)) {
            return Ok(());
        }
        let span = (line.first().unwrap().span.clone()).merge(line.last().unwrap().span.clone());
        if max_placeholder(&line).is_some() {
            self.add_error(
                span.clone(),
                "Cannot use placeholder outside of an index macro",
            );
        }
        // Compile the words
        let binding_count_before = self.asm.bindings.len();
        let root_len_before = self.asm.root.len();
        let error_count_before = self.errors.len();

        let mut line_node = self.line(line, true)?;

        let binding_count_after = self.asm.bindings.len();
        let error_count_after = self.errors.len();

        if mode != ItemCompMode::Function {
            line_node.optimize_full();
        }
        match line_node.sig() {
            Ok(sig) => {
                // Compile test assert
                if self.mode != RunMode::Normal
                    && mode != ItemCompMode::Function
                    && !self
                        .scopes()
                        .any(|sc| sc.kind == ScopeKind::File(FileScopeKind::Git))
                {
                    let test_assert = line_node
                        .last_mut_recursive(&mut self.asm, |node| {
                            if let &mut Node::Prim(Primitive::Assert, span) = node {
                                *node = Node::ImplPrim(ImplPrimitive::TestAssert, span);
                                true
                            } else {
                                false
                            }
                        })
                        .unwrap_or(false);
                    if test_assert {
                        self.asm.test_assert_count += 1;
                    }
                }
                // Try to evaluate at comptime
                // This can be done when:
                // - the line is not in a () function
                // - the pre-eval mode is greater that `Line`
                // - there are at least as many push nodes preceding the current line as there are arguments to the line
                // - the words create no bindings
                if mode != ItemCompMode::Function
                    && self.pre_eval_mode > PreEvalMode::Line
                    && error_count_after == error_count_before
                    && binding_count_before == binding_count_after
                    && root_len_before == self.asm.root.len()
                    && !line_node.is_empty()
                    && self.asm.root.len() >= sig.args()
                    && (self.asm.root.iter().rev().take(sig.args()))
                        .all(|node| matches!(node, Node::Push(_)))
                {
                    // The nodes for evaluation are the preceding
                    // push nodes, followed by the current line
                    let mut node = Node::from(&self.asm.root[self.asm.root.len() - sig.args()..]);
                    node.extend(line_node.iter().cloned());
                    if let Some((node, errs)) = self.pre_eval(&node) {
                        self.errors.extend(errs);
                        // Track top-level values
                        if !matches!(line_node.last().unwrap(), Node::SetOutputComment { .. })
                            && node.iter().all(|node| matches!(node, Node::Push(_)))
                        {
                            let vals: Vec<_> = node
                                .iter()
                                .map(|node| match node {
                                    Node::Push(val) => val.clone(),
                                    _ => unreachable!(),
                                })
                                .collect();
                            self.code_meta.top_level_values.insert(span, vals);
                        }
                        // Truncate root
                        self.asm.root.truncate(self.asm.root.len() - sig.args());
                        // Set line node to the pre-evaluated node
                        line_node = node;
                    }
                }
            }
            Err(e) => self.add_error(span, e),
        }
        self.asm.root.push(line_node);
        Ok(())
    }
    fn compile_bind_function(
        &mut self,
        name: Ident,
        local: LocalName,
        function: Function,
        span: usize,
        meta: BindingMeta,
    ) -> UiuaResult {
        self.scope.names.insert(name.clone(), local);
        let span = if span == 0 {
            Some(CodeSpan::literal(name))
        } else {
            self.get_span(span).clone().code()
        };
        self.asm
            .add_binding_at(local, BindingKind::Func(function), span, meta);
        Ok(())
    }
    fn compile_bind_const(
        &mut self,
        name: Ident,
        local: LocalName,
        value: Option<Value>,
        span: usize,
        meta: BindingMeta,
    ) {
        let span = self.get_span(span).clone().code();
        self.asm
            .add_binding_at(local, BindingKind::Const(value), span, meta);
        self.scope.names.insert(name, local);
    }
    /// Import a module
    pub(crate) fn import_module(&mut self, path_str: &str, span: &CodeSpan) -> UiuaResult<PathBuf> {
        // Resolve path
        let (path, file_kind) = if let Some(mut url) = path_str.trim().strip_prefix("git:") {
            if url.contains("branch:") && url.contains("commit:") {
                return Err(self.error(
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
            if url.ends_with("/uiua") {
                return Err(self.error(span.clone(), "Cannot import what looks like a Uiua fork"));
            }
            if !(url.starts_with("https://") || url.starts_with("http://")) {
                url = format!("https://{url}");
            }
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::Git(url.clone()));
            let path = self
                .backend()
                .load_git_module(&url, target)
                .map_err(|e| self.error(span.clone(), e))?;
            (path, FileScopeKind::Git)
        } else {
            // Normal import
            let path = self.resolve_import_path(Path::new(path_str));
            self.code_meta
                .import_srcs
                .insert(span.clone(), ImportSrc::File(path.clone()));
            (path, FileScopeKind::Source)
        };
        if !self.imports.contains_key(&path) {
            // We cache Git modules on WASM so that the pad doesn't have to recompile big modules constantly
            thread_local! {
                static GIT_CACHE: RefCell<HashMap<PathBuf, Compiler>> = RefCell::new(HashMap::new());
            }
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
                .map_err(|e| self.error(span.clone(), e))?;
            if let Some(mut comp) = (bytes.len() > 1000)
                .then(|| GIT_CACHE.with(|cache| cache.borrow().get(&path).cloned()))
                .flatten()
            {
                swap(self, &mut comp);
                self.macro_env.rt.backend = comp.macro_env.rt.backend;
                self.asm.inputs.strings = comp.asm.inputs.strings;
                self.asm.inputs.files.extend(comp.asm.inputs.files);
                self.scope.experimental = comp.scope.experimental;
                self.diagnostics.extend(comp.diagnostics);
            } else {
                let input: EcoString = String::from_utf8(bytes)
                    .map_err(|e| self.error(span.clone(), format!("Failed to read file: {e}")))?
                    .into();
                if self.current_imports.iter().any(|p| p == &path) {
                    return Err(self.error(
                        span.clone(),
                        format!("Cycle detected importing {}", path.to_string_lossy()),
                    ));
                }
                let (module, ()) = self.in_scope(ScopeKind::File(file_kind), |comp| {
                    comp.load_str_src(&input, &path).map(drop)
                })?;
                self.imports.insert(path.clone(), module);
                #[cfg(target_arch = "wasm32")]
                if file_kind == FileScopeKind::Git {
                    GIT_CACHE.with(|cache| {
                        let mut clone = self.clone();
                        clone.macro_env.rt.backend = Arc::new(crate::SafeSys::default());
                        cache.borrow_mut().insert(path.clone(), clone);
                    });
                }
            };
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
    // Compile a line, checking an end-of-line signature comment
    fn line(&mut self, mut line: Vec<Sp<Word>>, must_be_callable: bool) -> UiuaResult<Node> {
        let comment_sig = line_sig(&line);
        let output_comment = if line
            .last()
            .is_some_and(|w| matches!(w.value, Word::OutputComment { .. }))
        {
            let word = line.pop().unwrap();
            Some(self.word(word)?)
        } else {
            None
        };
        // Actually compile the line
        let mut node = self.words(line)?;
        node.extend(output_comment);
        // Validate line signature
        if let Some(comment_sig) = comment_sig {
            self.apply_node_comment(&mut node, &comment_sig.value, "Line", &comment_sig.span);
        }
        // Validate callability
        if must_be_callable {
            if let Err((e, f, mut spans)) = node.check_callability(&self.asm) {
                let e = e.clone();
                if let Some(f_id) = f.map(|f| f.id.clone()) {
                    let src_span = self.get_span(spans.remove(0));
                    let call_span = self.get_span(spans.pop().unwrap());
                    let error = self.error_with_info(
                        call_span,
                        format!("Cannot call {f_id} because of an inversion error"),
                        [(src_span, e)],
                    );
                    self.errors.push(error);
                } else {
                    let span = self.get_span(spans.pop().unwrap());
                    self.add_error(span, e);
                }
            }
        }
        Ok(node)
    }
    fn apply_node_comment(
        &mut self,
        node: &mut Node,
        comment_sig: &DocCommentSig,
        name: &str,
        span: &CodeSpan,
    ) {
        let mut spandex: Option<usize> = None;
        // Validate comment signature
        if let Ok(sig) = node.sig() {
            if !comment_sig.matches_sig(sig) {
                let span = *spandex.get_or_insert_with(|| self.add_span(span.clone()));
                self.emit_diagnostic(
                    format!(
                        "{name} comment describes {}, \
                        but its code has signature {sig}",
                        comment_sig.sig_string()
                    ),
                    DiagnosticKind::Warning,
                    self.get_span(span).clone().code().unwrap(),
                );
            }
        }
        if comment_sig.label {
            // Add argument labels
            if let Some(args) = &comment_sig.args {
                let span = *spandex.get_or_insert_with(|| self.add_span(span.clone()));
                let labels = Node::bracket(
                    args.iter()
                        .map(|a| Node::Label(a.name.clone(), span).sig_node().unwrap()),
                    span,
                );
                node.prepend(labels);
            }
            // Add output labels
            if let Some(outputs) = &comment_sig.outputs {
                let span = *spandex.get_or_insert_with(|| self.add_span(span.clone()));
                let labels = Node::bracket(
                    outputs
                        .iter()
                        .map(|o| Node::Label(o.name.clone(), span).sig_node().unwrap()),
                    span,
                );
                node.push(labels);
            }
        }
    }
    fn args(&mut self, words: Vec<Sp<Word>>) -> UiuaResult<EcoVec<SigNode>> {
        (words.into_iter())
            .filter(|w| w.value.is_code())
            .map(|w| self.word_sig(w))
            .collect()
    }
    fn words_sig(&mut self, words: Vec<Sp<Word>>) -> UiuaResult<SigNode> {
        let span = words
            .first()
            .zip(words.last())
            .map(|(f, l)| f.span.clone().merge(l.span.clone()));
        let node = self.words(words)?;
        let sig = if let Some(span) = span {
            self.sig_of(&node, &span)?
        } else {
            Signature::new(0, 0)
        };
        Ok(SigNode::new(sig, node))
    }
    fn words(&mut self, mut words: Vec<Sp<Word>>) -> UiuaResult<Node> {
        // Filter out non-code words
        words.retain(|word| word.value.is_code());
        // Extract semantic comment
        let mut sem = None;
        if let Some(word) = words.last() {
            if let Word::SemanticComment(com) = &word.value {
                let com = com.clone();
                sem = Some(words.pop().unwrap().span.sp(com));
            }
        }
        // Right-to-left
        words.reverse();

        // We keep track of some data from previous words so we can emit
        // diagnostics about suggested changes
        #[derive(Debug, Clone)]
        struct PrevWord(
            Option<Primitive>,
            Option<Primitive>,
            Option<Signature>,
            CodeSpan,
        );
        let mut a: Option<PrevWord> = None;
        let mut b: Option<PrevWord> = None;
        let mut nodes = Node::empty();
        for word in flip_unsplit_lines(split_words(words)).into_iter().flatten() {
            let span = word.span.clone();
            let (mut modif, mut prim) = (None, None);
            match &word.value {
                Word::Primitive(p) => prim = Some(*p),
                Word::Modified(m) => {
                    if let Modifier::Primitive(mprim) = &m.modifier.value {
                        if let Some(first) = m.operands.first() {
                            if let Word::Primitive(p) = first.value {
                                modif = Some(*mprim);
                                prim = Some(p);
                            }
                        }
                    }
                }
                _ => {}
            }

            match (a, &b, prim.filter(|_| modif.is_none())) {
                // Flip monadic dup diagnostic
                (
                    Some(PrevWord(None, Some(Primitive::Dup), _, a_span)),
                    Some(PrevWord(_, _, Some(sig), _)),
                    Some(Primitive::Flip),
                ) if *sig == (1, 1) => {
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
                    Some(PrevWord(None, Some(Primitive::Dup), _, a_span)),
                    Some(PrevWord(None, Some(Primitive::Unique), _, _)),
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
                // First reverse diagnostic
                (
                    _,
                    Some(PrevWord(None, Some(Primitive::Reverse), _, b_span)),
                    Some(Primitive::First),
                ) => {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {}{}",
                            Primitive::Last.format(),
                            Primitive::First,
                            Primitive::Reverse
                        ),
                        DiagnosticKind::Advice,
                        b_span.clone().merge(span.clone()),
                    );
                }
                // Select by rise and select rise dup diagnostics
                (
                    Some(PrevWord(None, Some(Primitive::Dup), _, a_span)),
                    Some(PrevWord(None, Some(Primitive::Rise), _, _)),
                    Some(Primitive::Select),
                ) => {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {}{}{}",
                            Primitive::Sort.format(),
                            Primitive::Select,
                            Primitive::Rise,
                            Primitive::Dup
                        ),
                        DiagnosticKind::Advice,
                        a_span.merge(span.clone()),
                    );
                }
                (
                    _,
                    Some(PrevWord(Some(Primitive::By), Some(Primitive::Rise), _, b_span)),
                    Some(Primitive::Select),
                ) => {
                    self.emit_diagnostic(
                        format!(
                            "Prefer {} over {}{}{}",
                            Primitive::Sort.format(),
                            Primitive::Select,
                            Primitive::By,
                            Primitive::Rise
                        ),
                        DiagnosticKind::Advice,
                        b_span.clone().merge(span.clone()),
                    );
                }
                _ => {}
            }

            // Compile the word
            let node = self.word(word)?;
            let sig = node.sig().ok();
            nodes.push(node);
            a = b;
            b = Some(PrevWord(modif, prim, sig, span));
        }
        if let Some(sem) = sem {
            nodes = self.semantic_comment(sem.value, sem.span, nodes);
        }
        Ok(nodes)
    }
    fn word_sig(&mut self, word: Sp<Word>) -> UiuaResult<SigNode> {
        let span = word.span.clone();
        let node = self.word(word)?;
        let sig = self.sig_of(&node, &span)?;
        Ok(SigNode::new(sig, node))
    }
    fn check_depth(&mut self, span: &CodeSpan) -> UiuaResult {
        const MAX_RECURSION_DEPTH: usize =
            match (cfg!(target_arch = "wasm32"), cfg!(debug_assertions)) {
                (false, false) => (512 + 256 + 64) * 1024 * 2,
                (false, true) => (512 + 256 + 64) * 1024,
                (true, false) => 640 * 1024,
                (true, true) => 640 * 1024,
            };
        let start_addr = *self.start_addrs.first().unwrap();
        let curr = 0u8;
        let curr_addr = &curr as *const u8 as usize;
        let diff = curr_addr.abs_diff(start_addr);
        if diff > MAX_RECURSION_DEPTH {
            return Err(self.error(span.clone(), "Compilation recursion limit reached"));
        }
        Ok(())
    }
    fn word(&mut self, word: Sp<Word>) -> UiuaResult<Node> {
        self.check_depth(&word.span)?;
        Ok(match word.value {
            Word::Number(NumWord::Real(n), _) => Node::new_push(n),
            Word::Number(NumWord::Complex(c), _) => Node::new_push(c),
            Word::Number(NumWord::Err(s), _) => {
                self.add_error(word.span.clone(), format!("Invalid number `{s}`"));
                Node::new_push(0.0)
            }
            Word::Char(c) => {
                let val: Value = if c.chars().count() == 1 {
                    c.chars().next().unwrap().into()
                } else {
                    c.into()
                };
                Node::new_push(val)
            }
            Word::String(s) => Node::new_push(s),
            Word::MultilineString(lines) => {
                let mut s = EcoVec::new();
                for (i, line) in lines.into_iter().enumerate() {
                    if i > 0 {
                        s.push('\n');
                    }
                    s.extend(line.value.chars());
                }
                Node::new_push(s)
            }
            Word::Label(label) => Node::Label(label.into(), self.add_span(word.span.clone())),
            Word::FormatString(frags) => {
                let parts = frags.into_iter().map(Into::into).collect();
                let span = self.add_span(word.span.clone());
                Node::Format(parts, span)
            }
            Word::MultilineFormatString(lines) => {
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
                Node::Format(parts, span)
            }
            Word::Ref(r) => self.reference(r),
            Word::IncompleteRef { path, .. } => 'blk: {
                if let Some((_, locals)) = self.ref_path(&path)? {
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
                    let index = locals.last().unwrap().index;
                    let names = match &self.asm.bindings[index].kind {
                        BindingKind::Import(path) => &self.imports[path].names,
                        BindingKind::Module(module) => &module.names,
                        _ => break 'blk Node::empty(),
                    };
                    let mut completions = Vec::new();
                    for (name, local) in names {
                        if !local.public {
                            continue;
                        }
                        completions.push(Completion {
                            text: name.into(),
                            index: local.index,
                            replace: false,
                        });
                    }
                    if !completions.is_empty() {
                        self.code_meta
                            .completions
                            .insert(word.span.clone(), completions);
                    }
                }
                Node::empty()
            }
            Word::Strand(items) => {
                // Track span for LSP
                let just_spans: Vec<_> = items.iter().map(|w| w.span.clone()).collect();
                // Compile individual items
                let op_nodes = items
                    .into_iter()
                    .rev()
                    .map(|word| self.word_sig(word))
                    .collect::<UiuaResult<Vec<_>>>()?;
                // Check item sigs
                let has_functions = op_nodes.iter().any(|sn| sn.sig.args() > 0);
                if has_functions {
                    return Err(
                        self.error(word.span.clone(), "Functions are not allowed in strands")
                    );
                }
                self.code_meta.strands.insert(word.span.clone(), just_spans);
                // Flatten instrs
                let inner = Node::from_iter(op_nodes.into_iter().map(|sn| sn.node));

                // Normal strand

                // Diagnostic for strand of characters
                if !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Node::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.emit_diagnostic(
                        "Stranded characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }

                let span = self.add_span(word.span.clone());
                // Inline constant arrays
                if inner.iter().all(|instr| matches!(instr, Node::Push(_))) {
                    let values: Vec<_> = inner
                        .iter()
                        .rev()
                        .map(|instr| match instr {
                            Node::Push(v) => v.clone(),
                            _ => unreachable!(),
                        })
                        .collect();
                    match Value::from_row_values(values, &(&word.span, &self.asm.inputs)) {
                        Ok(val) => return Ok(Node::new_push(val)),
                        Err(e) if e.meta.is_fill => {}
                        Err(e) => return Err(e),
                    }
                }
                let sig = self.sig_of(&inner, &word.span)?;
                Node::Array {
                    len: sig.outputs(),
                    inner: inner.into(),
                    boxed: false,
                    allow_ext: false,
                    prim: None,
                    span,
                }
            }
            Word::Array(arr) => {
                if let Some(down_span) = &arr.down_span {
                    self.experimental_error(down_span, || {
                        "Lexical ordering is experimental. To use it, \
                        add `# Experimental!` to the top of the file."
                    });
                }
                // Track span for LSP
                if !arr.boxes
                    && (arr.word_lines().flatten())
                        .filter(|w| w.value.is_code())
                        .all(|w| w.value.is_literal() && !matches!(w.value, Word::Strand(_)))
                {
                    let just_spans: Vec<_> = (arr.word_lines().rev().flatten())
                        .filter(|w| w.value.is_code())
                        .map(|w| w.span.clone())
                        .collect();
                    self.code_meta
                        .array_inner_spans
                        .insert(word.span.clone(), just_spans);
                }
                let line_count = arr.lines.len();
                let any_contents = arr.word_lines().flatten().any(|w| w.value.is_code());
                // Compile lines
                let (mut word_lines, item_lines): (Vec<_>, Vec<_>) = arr
                    .lines
                    .into_iter()
                    .partition(|item| matches!(item, Item::Words(_)));
                if arr.down_span.is_none() {
                    word_lines.reverse();
                }
                let root_start = self.asm.root.len();
                self.in_scope(ScopeKind::Function, |comp| {
                    comp.items(item_lines, ItemCompMode::Function)?;
                    comp.items(word_lines, ItemCompMode::Function)
                })?;
                let inner = self.asm.root.split_off(root_start);
                // Calculate length
                let len = match inner.sig() {
                    Ok(sig) => {
                        if sig.outputs() == 0 && any_contents {
                            self.emit_diagnostic(
                                "Array wraps function with no outputs. This is probably not what you want.",
                                DiagnosticKind::Advice,
                                word.span.clone(),
                            )
                        }
                        sig.outputs()
                    }
                    Err(e) => {
                        return Err(self.error(
                            word.span.clone(),
                            format!("Cannot infer array signature: {e}"),
                        ))
                    }
                };
                // Diagnostic for array of characters
                if line_count <= 1
                    && !arr.boxes
                    && !inner.is_empty()
                    && inner.iter().all(
                        |instr| matches!(instr, Node::Push(Value::Char(arr)) if arr.rank() == 0),
                    )
                {
                    self.emit_diagnostic(
                        "An array of characters should instead be written as a string",
                        DiagnosticKind::Advice,
                        word.span.clone(),
                    );
                }
                let span = self.add_span(word.span.clone());
                // Inline constant arrays
                if inner.iter().all(|instr| matches!(instr, Node::Push(_))) {
                    let empty = inner.is_empty();
                    let values = inner.iter().rev().map(|instr| match instr {
                        Node::Push(v) => v.clone(),
                        _ => unreachable!(),
                    });
                    let res = if arr.boxes {
                        if empty {
                            Ok(Array::<Boxed>::default().into())
                        } else {
                            Value::from_row_values(
                                values
                                    .map(|v| Value::Box(Boxed(v).into()))
                                    .collect::<Vec<_>>(),
                                &(&word.span, &self.asm.inputs),
                            )
                        }
                    } else {
                        Value::from_row_values(
                            values.collect::<Vec<_>>(),
                            &(&word.span, &self.asm.inputs),
                        )
                    };
                    match res {
                        Ok(val) => {
                            self.code_meta
                                .array_shapes
                                .insert(word.span.clone(), val.shape.clone());
                            return Ok(Node::new_push(val));
                        }
                        Err(e) if e.meta.is_fill => {}
                        Err(e) => return Err(e),
                    }
                }
                // Normal case
                Node::Array {
                    len,
                    inner: inner.into(),
                    boxed: arr.boxes,
                    allow_ext: false,
                    prim: None,
                    span,
                }
            }
            Word::Func(func) => self.func(func, word.span)?,
            Word::Pack(pack) => {
                self.add_error(
                    word.span.clone(),
                    "Function packs are not allowed without a modifier",
                );
                if let Some(first) = pack.into_lexical_order().next() {
                    self.func(first.value, first.span)?
                } else {
                    Node::empty()
                }
            }
            Word::Primitive(p) => self.primitive(p, word.span),
            Word::Modified(m) => self.modified(*m, None)?,
            Word::Placeholder(_) => {
                // We could error here, but it's easier to handle it higher up
                Node::empty()
            }
            Word::SemanticComment(_) => {
                // Semantic comments are handled higher up
                Node::empty()
            }
            Word::OutputComment { i, n } => Node::SetOutputComment { i, n },
            Word::Subscripted(sub) => self.subscript(*sub, word.span)?,
            Word::Comment(_) | Word::Spaces | Word::BreakLine | Word::FlipLine => Node::empty(),
            Word::InlineMacro(_) => {
                self.add_error(
                    word.span.clone(),
                    "Inline macro was not parsed as a modifier. \
                    This is a bug in the interpreter",
                );
                Node::empty()
            }
            Word::ArgSetter(_) => {
                self.add_error(
                    word.span.clone(),
                    "Argument setters are no longer supported",
                );
                Node::Prim(Primitive::Pop, self.add_span(word.span))
            }
        })
    }
    fn force_sig(
        &mut self,
        mut node: Node,
        new_sig: Signature,
        span: &CodeSpan,
    ) -> UiuaResult<Node> {
        let Ok(sig) = node.sig() else {
            return Ok(node);
        };
        if new_sig == sig {
            return Ok(node);
        }
        let delta = sig.outputs() as isize - sig.args() as isize;
        let new_delta = new_sig.outputs() as isize - new_sig.args() as isize;
        match delta.cmp(&new_delta) {
            Ordering::Equal => {
                if sig.args() < new_sig.args() {
                    let spandex = self.add_span(span.clone());
                    let mut dip = Node::empty();
                    for _ in 0..new_sig.args() {
                        dip = Node::Mod(Primitive::Dip, eco_vec![dip.sig_node().unwrap()], spandex);
                    }
                    node.prepend(dip);
                }
                self.emit_diagnostic(
                    format!("Signature mismatch: declared {new_sig} but inferred {sig}"),
                    DiagnosticKind::Warning,
                    span.clone(),
                );
            }
            Ordering::Less => {
                if new_sig.outputs() > 10 {
                    return Err(self.error(
                        span.clone(),
                        format!(
                            "Signature mismatch: declared {new_sig} but inferred {sig}. \
                            Signatures with more than 10 outputs cannot be forced."
                        ),
                    ));
                }
                let diff = (new_delta - delta).unsigned_abs();
                let spandex = self.add_span(span.clone());
                let mut extra = Node::from_iter(
                    (0..diff).map(|i| Node::new_push(Boxed(Value::from(format!("dbg-{}", i + 1))))),
                );
                for _ in 0..sig.outputs() {
                    extra = Node::Mod(Primitive::Dip, eco_vec![extra.sig_node().unwrap()], spandex);
                }
                node.push(extra);
                self.emit_diagnostic(
                    format!(
                        "Signature mismatch: declared {new_sig} but inferred {sig}. \
                        {diff} debug output{} will be generated.",
                        if diff == 1 { "" } else { "s" }
                    ),
                    DiagnosticKind::Warning,
                    span.clone(),
                );
            }
            Ordering::Greater => {
                if new_sig.args() > 10 {
                    return Err(self.error(
                        span.clone(),
                        format!(
                            "Signature mismatch: declared {new_sig} but inferred {sig}. \
                            Signatures with more than 10 arguments cannot be forced."
                        ),
                    ));
                }
                let diff = (delta - new_delta).unsigned_abs();
                let spandex = self.add_span(span.clone());
                let mut pops =
                    Node::from_iter((0..diff).map(|_| Node::Prim(Primitive::Pop, spandex)));
                for _ in 0..new_sig.outputs() {
                    pops = Node::Mod(Primitive::Dip, eco_vec![pops.sig_node().unwrap()], spandex);
                }
                node.push(pops);
                self.emit_diagnostic(
                    format!(
                        "Signature mismatch: declared {new_sig} but inferred {sig}. \
                        Additional arguments will be popped."
                    ),
                    DiagnosticKind::Warning,
                    span.clone(),
                );
            }
        }
        Ok(node)
    }
    #[must_use]
    fn semantic_comment(&mut self, comment: SemanticComment, span: CodeSpan, inner: Node) -> Node {
        match comment {
            SemanticComment::Experimental => {
                self.scope.experimental = true;
                inner
            }
            SemanticComment::NoInline => Node::NoInline(inner.into()),
            SemanticComment::TrackCaller => Node::TrackCaller(inner.into()),
            SemanticComment::External => inner,
            SemanticComment::Deprecated(_) => inner,
            SemanticComment::Boo => {
                self.add_error(span, "The compiler is scared!");
                inner
            }
        }
    }
    /// Find the [`LocalName`]s of both the name and all parts of the path of a [`Ref`]
    ///
    /// Returns [`None`] if the reference is to a constant
    fn ref_local(&self, r: &Ref) -> UiuaResult<Option<(Vec<LocalName>, LocalName)>> {
        if let Some((names, path_locals)) = self.ref_path(&r.path)? {
            if let Some(local) = names.get(&r.name.value).copied().or_else(|| {
                (r.name.value.strip_suffix('!')).and_then(|name| {
                    names.get(name).copied().filter(|local| {
                        matches!(&self.asm.bindings[local.index].kind, BindingKind::Module(_))
                    })
                })
            }) {
                if local.public {
                    Ok(Some((path_locals, local)))
                } else {
                    Err(self.error(
                        r.name.span.clone(),
                        format!("`{}` is private", r.name.value),
                    ))
                }
            } else {
                Err(self.error(
                    r.name.span.clone(),
                    format!("Item `{}` not found", r.name.value),
                ))
            }
        } else if let Some(local) = self.find_name(&r.name.value, &r.name.span) {
            Ok(Some((Vec::new(), local)))
        } else if r.path.is_empty() && CONSTANTS.iter().any(|def| def.name == r.name.value) {
            Ok(None)
        } else {
            Err(self.error(
                r.name.span.clone(),
                format!("Unknown identifier `{}`", r.name.value),
            ))
        }
    }
    fn find_name(&self, name: &str, span: &CodeSpan) -> Option<LocalName> {
        self.find_name_impl(name, span, false)
    }
    fn find_name_impl(
        &self,
        name: &str,
        span: &CodeSpan,
        stop_at_binding: bool,
    ) -> Option<LocalName> {
        // println!("name: {name:?} @ {}", span);
        // for scope in self.scopes() {
        //     println!("  {:?} {:?}", scope.kind, scope.names);
        // }
        let mut hit_stop = false;
        for scope in self.scopes() {
            if matches!(scope.kind, ScopeKind::File(_))
                || stop_at_binding && matches!(scope.kind, ScopeKind::Binding)
            {
                if hit_stop {
                    break;
                }
                hit_stop = true;
            }
            // Look in the scope's names
            if let Some(local) = scope.names.get(name).copied() {
                return Some(local);
            }
            // Look in the macro's locals. We look up by span rather than
            // name to disambiguate the macro declaration's locals from
            // the current ones.
            if let ScopeKind::Macro(Some(mac_local)) = &scope.kind {
                let mac = &self.index_macros[&mac_local.macro_index];
                if let Some(index) = mac.locals.get(span).copied() {
                    return Some(LocalName {
                        index,
                        public: true,
                    });
                }
            }
        }
        // Attempt to look up the identifier as a non-macro
        let as_non_macro = self.find_name_impl(name.strip_suffix('!')?, span, stop_at_binding)?;
        if let BindingKind::Module(_) | BindingKind::Scope(_) | BindingKind::Import(_) =
            self.asm.bindings[as_non_macro.index].kind
        {
            // Only allow it if it is a module
            Some(as_non_macro)
        } else {
            None
        }
    }
    fn ref_path(&self, path: &[RefComponent]) -> UiuaResult<Option<(&LocalNames, Vec<LocalName>)>> {
        let Some(first) = path.first() else {
            return Ok(None);
        };
        let mut path_locals = Vec::new();
        let module_local = self
            .find_name(&first.module.value, &first.module.span)
            .ok_or_else(|| {
                self.error(
                    first.module.span.clone(),
                    format!("Unknown import `{}`", first.module.value),
                )
            })?;
        path_locals.push(module_local);
        let global = &self.asm.bindings[module_local.index].kind;
        let mut names = match global {
            BindingKind::Import(path) => &self.imports[path].names,
            BindingKind::Module(module) => &module.names,
            BindingKind::Scope(i) => &self.higher_scopes.get(*i).unwrap_or(&self.scope).names,
            BindingKind::Func(_) => {
                return Err(self.error(
                    first.module.span.clone(),
                    format!("`{}` is a function, not a module", first.module.value),
                ))
            }
            BindingKind::Const(_) => {
                return Err(self.error(
                    first.module.span.clone(),
                    format!("`{}` is a constant, not a module", first.module.value),
                ))
            }
            BindingKind::IndexMacro(_) => {
                return Err(self.error(
                    first.module.span.clone(),
                    format!("`{}` is an index macro, not a module", first.module.value),
                ))
            }
            BindingKind::CodeMacro(_) => {
                return Err(self.error(
                    first.module.span.clone(),
                    format!("`{}` is a code macro, not a module", first.module.value),
                ))
            }
            BindingKind::Error => return Ok(None),
        };
        for comp in path.iter().skip(1) {
            let submod_local = names.get(&comp.module.value).copied().ok_or_else(|| {
                self.error(
                    comp.module.span.clone(),
                    format!("Module `{}` not found", comp.module.value),
                )
            })?;
            path_locals.push(submod_local);
            let global = &self.asm.bindings[submod_local.index].kind;
            names = match global {
                BindingKind::Import(path) => &self.imports[path].names,
                BindingKind::Module(module) => &module.names,
                BindingKind::Scope(i) => &self.higher_scopes.get(*i).unwrap_or(&self.scope).names,
                BindingKind::Func(_) => {
                    return Err(self.error(
                        comp.module.span.clone(),
                        format!("`{}` is a function, not a module", comp.module.value),
                    ))
                }
                BindingKind::Const(_) => {
                    return Err(self.error(
                        comp.module.span.clone(),
                        format!("`{}` is a constant, not a module", comp.module.value),
                    ))
                }
                BindingKind::IndexMacro(_) => {
                    return Err(self.error(
                        comp.module.span.clone(),
                        format!("`{}` is an index macro, not a module", comp.module.value),
                    ))
                }
                BindingKind::CodeMacro(_) => {
                    return Err(self.error(
                        comp.module.span.clone(),
                        format!("`{}` is a code macro, not a module", comp.module.value),
                    ))
                }
                BindingKind::Error => return Ok(None),
            };
        }

        Ok(Some((names, path_locals)))
    }
    fn reference(&mut self, r: Ref) -> Node {
        if r.path.is_empty() {
            self.ident(r.name.value, r.name.span)
        } else {
            match self.ref_local(&r) {
                Ok(Some((path_locals, local))) => {
                    self.validate_local(&r.name.value, local, &r.name.span);
                    for (local, comp) in path_locals.into_iter().zip(&r.path) {
                        self.validate_local(&comp.module.value, local, &comp.module.span);
                        (self.code_meta.global_references)
                            .insert(comp.module.span.clone(), local.index);
                    }
                    self.code_meta
                        .global_references
                        .insert(r.name.span.clone(), local.index);
                    self.global_index(local.index, r.name.span)
                }
                Ok(None) => self.ident(r.name.value, r.name.span),
                Err(e) => {
                    self.errors.push(e);
                    Node::new_push(Value::default())
                }
            }
        }
    }
    fn completions(&self, prefix: &str, names: &LocalNames, public_only: bool) -> Vec<Completion> {
        let mut completions = Vec::new();
        for (name, local) in names {
            if public_only && !local.public {
                continue;
            }
            if name.starts_with(prefix) {
                completions.push(Completion {
                    text: name.into(),
                    index: local.index,
                    replace: true,
                });
            }
            let subnames = match &self.asm.bindings[local.index].kind {
                BindingKind::Module(m) => &m.names,
                BindingKind::Import(path) => &self.imports[path].names,
                _ => continue,
            };
            let subcompletions = self.completions(prefix, subnames, true);
            completions.extend(subcompletions.into_iter().map(|c| Completion {
                text: format!("{name}~{}", c.text),
                ..c
            }));
        }
        completions
    }
    fn ident(&mut self, ident: Ident, span: CodeSpan) -> Node {
        // Add completions
        let completions = self.completions(&ident, &self.scope.names, false);
        if !completions.is_empty() {
            self.code_meta.completions.insert(span.clone(), completions);
        }

        if let Some(local) = self.find_name_impl(&ident, &span, true) {
            // Name exists in binding scope
            self.validate_local(&ident, local, &span);
            (self.code_meta.global_references).insert(span.clone(), local.index);
            self.global_index(local.index, span)
        } else if let Some(curr) =
            (self.current_bindings.last_mut()).filter(|curr| curr.name == ident)
        {
            // Name is a recursive call
            curr.recurses += 1;
            let global_index = curr.global_index;
            (self.code_meta.global_references).insert(span.clone(), global_index);
            if let Some(sig) = curr.signature.filter(|sig| sig.outputs() <= 10) {
                Node::CallGlobal(global_index, sig)
            } else {
                Node::empty()
            }
        } else if let Some(local) = self.find_name(&ident, &span) {
            // Name exists in scope
            self.validate_local(&ident, local, &span);
            (self.code_meta.global_references).insert(span.clone(), local.index);
            self.global_index(local.index, span)
        } else if let Some(constant) = CONSTANTS.iter().find(|c| c.name == ident) {
            // Name is a built-in constant
            if let Some(suggestion) = constant.deprecation {
                if self.deprecated_const_errors.insert(constant.name) {
                    let suggestion = if suggestion.is_empty() {
                        String::new()
                    } else {
                        format!(". {suggestion}")
                    };
                    let message = format!(
                        "{} is deprecated and will be removed in a future version{}",
                        constant.name, suggestion
                    );
                    self.emit_diagnostic(message, DiagnosticKind::Warning, span.clone());
                }
            }
            self.code_meta
                .constant_references
                .insert(span.clone().sp(ident));
            Node::Push((constant.value).resolve(self.scope_file_path(), &*self.backend()))
        } else {
            self.add_error(span, format!("Unknown identifier `{ident}`"));
            Node::new_push(Value::default())
        }
    }
    fn scope_file_path(&self) -> Option<&Path> {
        for scope in self.scopes() {
            if let Some(file_path) = &scope.file_path {
                return Some(file_path);
            }
        }
        None
    }
    fn global_index(&mut self, index: usize, span: CodeSpan) -> Node {
        let binfo = &mut self.asm.bindings.make_mut()[index];
        binfo.used = true;
        let bkind = binfo.kind.clone();
        match bkind {
            BindingKind::Const(Some(val)) => Node::new_push(val),
            BindingKind::Const(None) => Node::CallGlobal(index, Signature::new(0, 1)),
            BindingKind::Func(f) => Node::Call(f, self.add_span(span.clone())),
            BindingKind::Import(path) => {
                if let Some(local) = self.imports.get(&path).and_then(|m| m.names.get("Call")) {
                    self.code_meta.global_references.remove(&span);
                    (self.code_meta.global_references).insert(span.clone(), local.index);
                    self.global_index(local.index, span)
                } else {
                    self.add_error(
                        span,
                        "Module cannot be called here as \
                        it has no `Call` function.",
                    );
                    Node::empty()
                }
            }
            global @ (BindingKind::Module(_) | BindingKind::Scope(_)) => {
                // Handle called modules
                let names = match &global {
                    BindingKind::Module(m) => &m.names,
                    BindingKind::Scope(i) => {
                        &self.higher_scopes.get(*i).unwrap_or(&self.scope).names
                    }
                    _ => unreachable!(),
                };
                if let Some(&local) = names.get("Call").or_else(|| names.get("New")) {
                    self.code_meta.global_references.remove(&span);
                    (self.code_meta.global_references).insert(span.clone(), local.index);
                    self.global_index(local.index, span.clone())
                } else {
                    self.add_error(
                        span,
                        "Module cannot be called here as \
                        it has no `Call` or `New` function.",
                    );
                    Node::empty()
                }
            }
            BindingKind::IndexMacro(_) | BindingKind::CodeMacro(_) => {
                // We could error here, but it's easier to handle it higher up
                Node::empty()
            }
            BindingKind::Error => Node::empty(),
        }
    }
    fn func(&mut self, func: Func, span: CodeSpan) -> UiuaResult<Node> {
        let root_start = self.asm.root.len();
        self.in_scope(ScopeKind::Function, |comp| {
            comp.items(func.lines, ItemCompMode::Function)
        })?;
        let mut root = self.asm.root.split_off(root_start);

        // Validate signature
        let sig = match root.sig() {
            Ok(mut sig) => {
                if let Some(declared_sig) = &func.signature {
                    root = self.force_sig(root, declared_sig.value, &declared_sig.span)?;
                    sig = declared_sig.value;
                }
                Some(sig)
            }
            Err(e) => return Err(self.error(span, format!("Cannot infer function signature: {e}"))),
        };
        if let Some(sig) = sig {
            self.code_meta.function_sigs.insert(
                span.clone(),
                SigDecl {
                    sig,
                    explicit: func.signature.is_some(),
                    inline: true,
                    set_inverses: Default::default(),
                },
            );
        }
        Ok(root)
    }
    fn switch(&mut self, branches: Vec<Sp<Word>>, span: CodeSpan) -> UiuaResult<Node> {
        let count = branches.len();
        // Compile branches
        let mut br = EcoVec::with_capacity(count);
        let mut rigid_indices = Vec::new();
        let mut flex_indices = Vec::new();
        for (i, branch) in branches.into_iter().enumerate() {
            let span = branch.span.clone();
            let SigNode { node, sig } = self.word_sig(branch)?;
            let is_flex = node
                .iter()
                .rposition(|node| matches!(node, Node::Prim(Primitive::Assert, _)))
                .is_some_and(|end| {
                    (0..end).rev().any(|start| {
                        let sub = node.slice(start..end);
                        match sub.as_slice() {
                            [Node::Push(val), Node::Prim(Primitive::Dup, _)]
                            | [Node::Push(val), Node::Push(..)]
                                if val != &Value::from(1) =>
                            {
                                return true;
                            }
                            [Node::Format(..), Node::Prim(Primitive::Dup, _)] => return true,
                            _ => (),
                        }
                        sub.is_pure(&self.asm) || !sub.sig().is_ok_and(|sig| sig == (0, 2))
                    })
                });
            br.push((SigNode::new(sig, node), span));
            if is_flex {
                flex_indices.push(i);
            } else {
                rigid_indices.push(i);
            }
        }
        let mut rigid_funcs = rigid_indices.into_iter().map(|i| &br[i]);
        let mut sig = None;
        if let Some((arg, _)) = rigid_funcs.next() {
            sig = Some(arg.sig);
            let sig = sig.as_mut().unwrap();
            // Compile remaining branches
            for (arg, span) in rigid_funcs {
                if arg.sig.is_compatible_with(*sig) {
                    *sig = sig.max_with(arg.sig);
                } else if arg.sig.outputs() == sig.outputs() {
                    sig.update_args(|a| a.max(arg.sig.args()));
                    sig.update_under_args(|a| a.max(arg.sig.under_args()));
                } else {
                    self.add_error(
                        span.clone(),
                        format!(
                            "Switch branch's signature {} is \
                            incompatible with previous branches {sig}",
                            arg.sig
                        ),
                    );
                }
            }
        }
        let mut flex_funcs = flex_indices.into_iter().map(|i| &br[i]);
        let mut sig = sig.unwrap_or_else(|| flex_funcs.next().unwrap().0.sig);
        for (arg, _) in flex_funcs {
            sig.update_args(|a| a.max(arg.sig.args()));
            sig.update_under_args(|a| a.max(arg.sig.under_args()));
        }

        let span = self.add_span(span.clone());
        Ok(Node::Switch {
            branches: br.into_iter().map(|(arg, _)| arg).collect(),
            sig,
            span,
            under_cond: false,
        })
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
    fn validate_primitive(&mut self, prim: Primitive, span: &CodeSpan) {
        self.handle_primitive_experimental(prim, span);
        self.handle_primitive_deprecation(prim, span);
    }
    fn primitive(&mut self, prim: Primitive, span: CodeSpan) -> Node {
        self.validate_primitive(prim, &span);
        let spandex = self.add_span(span.clone());
        Node::Prim(prim, spandex)
    }
    #[allow(clippy::match_single_binding, unused_parens)]
    fn subscript(&mut self, sub: Subscripted, span: CodeSpan) -> UiuaResult<Node> {
        use Primitive::*;
        let scr = sub.script;
        Ok(match sub.word.value {
            Word::Modified(m) => match m.modifier.value {
                Modifier::Ref(_) | Modifier::Macro(..) => {
                    self.add_error(
                        m.modifier.span.clone().merge(scr.span.clone()),
                        "Subscripts are not implemented for macros",
                    );
                    self.modified(*m, Some(scr.map(Into::into)))?
                }
                Modifier::Primitive(prim) => {
                    if !matches!(
                        prim,
                        (Both | Bracket)
                            | (Slf | Backward | Reach | On | By | With | Off)
                            | (Rows | Each | Inventory)
                            | (Repeat | Tuples | Stencil)
                            | (Fill | Geometric)
                    ) {
                        self.add_error(
                            m.modifier.span.clone().merge(scr.span.clone()),
                            format!("Subscripts are not implemented for {}", prim.format()),
                        );
                    }
                    self.modified(*m, Some(scr.map(Into::into)))?
                }
            },
            Word::Primitive(prim) => self.subscript_prim(prim, sub.word.span, scr)?,
            _ => {
                self.add_error(span.clone(), "Subscripts are not allowed in this context");
                self.word(sub.word)?
            }
        })
    }
    fn subscript_prim(
        &mut self,
        prim: Primitive,
        span: CodeSpan,
        scr: Sp<Subscript>,
    ) -> UiuaResult<Node> {
        use Primitive::*;
        Ok(match prim {
            prim if prim.class() == PrimClass::DyadicPervasive => {
                let Some(nos) = self.subscript_n_or_side(&scr, prim.format()) else {
                    return Ok(self.primitive(prim, span));
                };
                self.validate_primitive(prim, &span);
                match nos {
                    SubNOrSide::N(n) => {
                        Node::from_iter([Node::new_push(n), self.primitive(prim, span)])
                    }
                    SubNOrSide::Side(side) => {
                        self.experimental_error_it(&scr.span, || {
                            format!("Sided {}", prim.format())
                        });
                        let sub_span = self.add_span(scr.span);
                        let mut node = Node::Prim(Primitive::Fix, sub_span);
                        if side == SubSide::Right {
                            node = Node::Mod(
                                Primitive::Dip,
                                eco_vec![node.sig_node().unwrap()],
                                sub_span,
                            );
                        }
                        node.push(self.primitive(prim, span));
                        node
                    }
                }
            }
            EncodeBytes => {
                let Some(side) = self.subscript_side_only(&scr, EncodeBytes.format()) else {
                    return Ok(self.primitive(EncodeBytes, span));
                };
                self.validate_primitive(prim, &span);
                let span = self.add_span(span);
                Node::ImplPrim(ImplPrimitive::SidedEncodeBytes(side), span)
            }
            prim => {
                let Some(n) = self.subscript_n_only(&scr, prim.format()) else {
                    return Ok(self.primitive(prim, span));
                };
                self.validate_primitive(prim, &span);
                match prim {
                    prim if prim.sig().is_some_and(|sig| sig == (2, 1))
                        && prim
                            .subscript_sig(Some(&Subscript::numeric(2)))
                            .is_some_and(|sig| sig == (1, 1)) =>
                    {
                        Node::from_iter([Node::new_push(n), self.primitive(prim, span)])
                    }
                    Deshape => Node::ImplPrim(ImplPrimitive::DeshapeSub(n), self.add_span(span)),
                    Transpose => {
                        self.subscript_experimental(prim, &span);
                        if n > 100 {
                            self.add_error(span.clone(), "Too many subscript repetitions");
                        }
                        (0..n.min(100))
                            .map(|_| self.primitive(prim, span.clone()))
                            .collect()
                    }
                    Neg => {
                        use crate::Complex;
                        let rotation = match n {
                            // Ensure that common cases are exact
                            -1..=1 => Complex::ONE,
                            2 | -2 => -Complex::ONE,
                            4 => Complex::I,
                            -4 => -Complex::I,
                            _ => Complex::from_polar(1.0, std::f64::consts::TAU / n as f64),
                        };
                        Node::from_iter([Node::new_push(rotation), self.primitive(Mul, span)])
                    }
                    Sqrt => {
                        if n == 0 {
                            self.add_error(span.clone(), "Cannot take 0th root");
                        }
                        Node::from_iter([Node::new_push(1.0 / n as f64), self.primitive(Pow, span)])
                    }
                    Exp => Node::from_iter([
                        Node::new_push(n as f64),
                        self.primitive(Flip, span.clone()),
                        self.primitive(Pow, span),
                    ]),
                    Floor | Ceil => {
                        self.subscript_experimental(prim, &span);
                        let mul = 10f64.powi(n);
                        Node::from_iter([
                            Node::new_push(mul),
                            self.primitive(Mul, span.clone()),
                            self.primitive(prim, span.clone()),
                            Node::new_push(mul),
                            self.primitive(Div, span),
                        ])
                    }
                    Round => {
                        let mul = 10f64.powi(n);
                        Node::from_iter([
                            Node::new_push(mul),
                            self.primitive(Mul, span.clone()),
                            self.primitive(prim, span.clone()),
                            Node::new_push(mul),
                            self.primitive(Div, span),
                        ])
                    }
                    Rand => Node::from_iter([
                        self.primitive(Rand, span.clone()),
                        Node::new_push(n),
                        self.primitive(Mul, span.clone()),
                        self.primitive(Floor, span),
                    ]),
                    Utf8 => match n {
                        8 => self.primitive(Utf8, span),
                        16 => Node::ImplPrim(ImplPrimitive::Utf16, self.add_span(span)),
                        _ => {
                            self.add_error(span.clone(), "Only UTF-8 and UTF-16 are supported");
                            self.primitive(Utf8, span)
                        }
                    },
                    Couple => match n {
                        1 => self.primitive(Fix, span),
                        2 => self.primitive(Couple, span),
                        n => Node::Array {
                            len: self.positive_subscript(n, Couple, &span),
                            inner: Node::empty().into(),
                            boxed: false,
                            allow_ext: true,
                            prim: Some(Couple),
                            span: self.add_span(span),
                        },
                    },
                    Join => match self.positive_subscript(n, Join, &span) {
                        0 => Node::new_push(Value::default()),
                        1 => Node::Prim(Identity, self.add_span(span)),
                        n => {
                            Node::from_iter(repeat_n(Node::Prim(Join, self.add_span(span)), n - 1))
                        }
                    },
                    Box => Node::Array {
                        len: self.positive_subscript(n, Box, &span),
                        inner: Node::empty().into(),
                        boxed: true,
                        allow_ext: false,
                        prim: Some(Box),
                        span: self.add_span(span),
                    },
                    Parse => {
                        if !matches!(n, 1..=36 | 64) {
                            self.add_error(span.clone(), format!("Cannot parse base {n}"));
                        }
                        Node::ImplPrim(ImplPrimitive::ParseSub(n as usize), self.add_span(span))
                    }
                    Stack => Node::ImplPrim(
                        ImplPrimitive::StackN {
                            n: self.positive_subscript(n, Stack, &span),
                            inverse: false,
                        },
                        self.add_span(span),
                    ),
                    First | Last => {
                        let n = self.positive_subscript(n, prim, &span);
                        let span = self.add_span(span);
                        match n {
                            0 => Node::Prim(Pop, span),
                            1 => Node::Prim(prim, span),
                            n if prim == First => Node::from_iter([
                                Node::new_push(n),
                                Node::Prim(Take, span),
                                Node::Unpack {
                                    count: n,
                                    unbox: false,
                                    allow_ext: false,
                                    prim: Some(First),
                                    span,
                                },
                            ]),
                            n => Node::from_iter([
                                Node::new_push(-(n as i32)),
                                Node::Prim(Take, span),
                                Node::Prim(Reverse, span),
                                Node::Unpack {
                                    count: n,
                                    unbox: false,
                                    allow_ext: false,
                                    prim: Some(Last),
                                    span,
                                },
                            ]),
                        }
                    }
                    Bits => {
                        let n = self.positive_subscript(n, Bits, &span);
                        let span = self.add_span(span);
                        Node::ImplPrim(ImplPrimitive::NBits(n), span)
                    }
                    Len => {
                        let span = self.add_span(span);
                        Node::from_iter([
                            Node::Prim(Shape, span),
                            Node::new_push(n),
                            Node::Prim(Select, span),
                        ])
                    }
                    Shape => {
                        let span = self.add_span(span);
                        Node::from_iter([
                            Node::Prim(Shape, span),
                            Node::new_push(n),
                            Node::Prim(Take, span),
                        ])
                    }
                    Range => {
                        let span = self.add_span(span);
                        Node::from_iter([
                            Node::Prim(Range, span),
                            Node::new_push(n),
                            Node::Prim(Add, span),
                        ])
                    }
                    Keep => {
                        let n = self.positive_subscript(n, Keep, &span);
                        let span = self.add_span(span);
                        Node::ImplPrim(ImplPrimitive::MultiKeep(n), span)
                    }
                    Occurrences => {
                        let n = self.positive_subscript(n, Occurrences, &span);
                        let span = self.add_span(span);
                        Node::from([
                            Node::Prim(Occurrences, span),
                            Node::new_push(n),
                            Node::Prim(Lt, span),
                        ])
                    }
                    _ => {
                        self.add_error(
                            span.clone(),
                            format!("Subscripts are not implemented for {}", prim.format()),
                        );
                        self.primitive(prim, span)
                    }
                }
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubNOrSide<N = i32> {
    N(N),
    Side(SubSide),
}

impl<N: PartialEq> PartialEq<N> for SubNOrSide<N> {
    fn eq(&self, other: &N) -> bool {
        match self {
            SubNOrSide::N(n) => *n == *other,
            SubNOrSide::Side(_) => false,
        }
    }
}

impl Compiler {
    fn validate_subscript(&mut self, sub: Sp<Subscript>) -> Sp<Subscript<i32>> {
        let side = sub.value.side;
        let num = (sub.value.num.as_ref()).and_then(|num| self.numeric_subscript_n(num, &sub.span));
        if num.is_none() && side.is_none() {
            self.add_error(sub.span.clone(), "Subscript is incomplete");
        }
        sub.span.sp(Subscript { num, side })
    }
    fn numeric_subscript_n(&mut self, num: &NumericSubscript, span: &CodeSpan) -> Option<i32> {
        match num {
            NumericSubscript::N(n) => Some(*n),
            NumericSubscript::NegOnly => {
                self.add_error(span.clone(), "Subscript is incomplete");
                None
            }
            NumericSubscript::TooLarge(_) => {
                self.add_error(span.clone(), "Subscript is too large");
                None
            }
        }
    }
    fn subscript_n_or_side(
        &mut self,
        sub: &Sp<Subscript>,
        for_what: impl fmt::Display,
    ) -> Option<SubNOrSide> {
        match (&sub.value.num, sub.value.side) {
            (None, None) => {
                self.add_error(sub.span.clone(), "Subscript is incomplete");
                None
            }
            (Some(num), None) => self.numeric_subscript_n(num, &sub.span).map(SubNOrSide::N),
            (None, Some(side)) => {
                if side.n.is_some() {
                    self.add_error(
                        sub.span.clone(),
                        format!("Sided subscript quantifiers are not allowed for {for_what}"),
                    );
                }
                Some(SubNOrSide::Side(side.side))
            }
            (Some(_), Some(_)) => {
                self.add_error(
                    sub.span.clone(),
                    format!("Mixed subscripts are not allowed for {for_what}"),
                );
                None
            }
        }
    }
    fn subscript_n_only(
        &mut self,
        sub: &Sp<Subscript>,
        for_what: impl fmt::Display + Copy,
    ) -> Option<i32> {
        let nos = self.subscript_n_or_side(sub, for_what)?;
        match nos {
            SubNOrSide::N(n) => Some(n),
            SubNOrSide::Side(_) => {
                self.add_error(
                    sub.span.clone(),
                    format!("Sided subscripts are not allowed for {for_what}"),
                );
                None
            }
        }
    }
    fn subscript_side_only(
        &mut self,
        sub: &Sp<Subscript>,
        for_what: impl fmt::Display + Copy,
    ) -> Option<SubSide> {
        let nos = self.subscript_n_or_side(sub, for_what)?;
        match nos {
            SubNOrSide::N(_) => {
                self.add_error(
                    sub.span.clone(),
                    format!("Numeric subscripts are not allowed for {for_what}"),
                );
                None
            }
            SubNOrSide::Side(side) => Some(side),
        }
    }
    fn positive_subscript(&mut self, n: i32, prim: Primitive, span: &CodeSpan) -> usize {
        if n < 0 {
            self.add_error(
                span.clone(),
                format!("Subscript for {} must be positive", prim.format()),
            );
        }
        n.unsigned_abs() as usize
    }
    fn subscript_experimental(&mut self, prim: Primitive, span: &CodeSpan) {
        self.experimental_error(span, || {
            format!(
                "Subcripted {} is experimental. To use it, \
                add `# Experimental!` to the top of the file.",
                prim.format()
            )
        });
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
    #[allow(clippy::print_stdout)]
    fn emit_diagnostic_impl(&mut self, diagnostic: Diagnostic) {
        if self.print_diagnostics {
            eprintln!("{}", diagnostic.report());
        } else {
            self.diagnostics.insert(diagnostic);
        }
    }
    fn add_error(&mut self, span: impl Into<Span>, message: impl ToString) {
        let e = self.error(span, message);
        self.errors.push(e);
    }
    fn experimental_error<S>(&mut self, span: &CodeSpan, message: impl FnOnce() -> S)
    where
        S: fmt::Display,
    {
        if !self.allow_experimental() {
            self.scope.experimental_error = true;
            self.add_error(span.clone(), message().to_string());
        }
    }
    fn experimental_error_it<S>(&mut self, span: &CodeSpan, thing: impl FnOnce() -> S)
    where
        S: fmt::Display,
    {
        self.experimental_error(span, || {
            format!(
                "{} is experimental. Add `# Experimental!` \
                to the top of the file to use it.",
                thing()
            )
        })
    }
    fn allow_experimental(&self) -> bool {
        let take = self
            .scopes()
            .position(|sc| matches!(sc.kind, ScopeKind::File(_)))
            .map(|i| i + 1)
            .unwrap_or(usize::MAX);
        self.scopes()
            .take(take)
            .any(|sc| sc.experimental || sc.experimental_error)
    }
    fn error(&self, span: impl Into<Span>, message: impl ToString) -> UiuaError {
        UiuaErrorKind::Run {
            message: span.into().sp(message.to_string()),
            info: Vec::new(),
            inputs: self.asm.inputs.clone().into(),
        }
        .into()
    }
    fn error_with_info<S, M>(
        &self,
        span: impl Into<Span>,
        message: impl ToString,
        info: impl IntoIterator<Item = (S, M)>,
    ) -> UiuaError
    where
        S: Into<Span>,
        M: ToString,
    {
        UiuaErrorKind::Run {
            message: span.into().sp(message.to_string()),
            info: info
                .into_iter()
                .map(|(s, m)| s.into().sp(m.to_string()))
                .collect(),
            inputs: self.asm.inputs.clone().into(),
        }
        .into()
    }
    fn validate_local(&mut self, name: &str, local: LocalName, span: &CodeSpan) {
        // Emit deprecation warning
        if let Some(suggestion) = &self.asm.bindings[local.index].meta.deprecation {
            let mut message = format!("{name} is deprecated");
            if !suggestion.is_empty() {
                message.push_str(". ");
                message.push_str(suggestion);
                if message.ends_with(char::is_alphanumeric) {
                    message.push('.');
                }
            }
            self.emit_diagnostic(message, DiagnosticKind::Warning, span.clone());
        }
        // Validate public
        if local.public {
            return;
        }
        let get = |scope: &Scope| {
            (scope.names.get(name))
                .or_else(|| scope.names.get(name.strip_suffix('!')?))
                .copied()
        };
        if !local.public
            && get(&self.scope)
                .filter(|l| l.public || !matches!(self.scope.kind, ScopeKind::AllInModule))
                .or_else(|| self.scopes_to_file().skip(1).find_map(get))
                .is_none_or(|l| l.index != local.index)
        {
            self.add_error(span.clone(), format!("`{name}` is private"));
        }
    }
    /// Get a span by its index
    pub fn get_span(&self, span: usize) -> Span {
        self.asm.spans[span].clone()
    }
    /// Register a span
    pub fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let span = span.into();
        if let Some(i) = self.asm.spans.iter().position(|s| *s == span) {
            return i;
        }
        let idx = self.asm.spans.len();
        self.asm.spans.push(span);
        idx
    }
    /// Create a function
    pub fn create_function(
        &mut self,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + SendSyncNative + 'static,
    ) -> Function {
        let sig = signature.into();
        let df = self.create_dynamic_function(sig, f);
        self.asm
            .add_function(FunctionId::Unnamed, sig, Node::Dynamic(df))
    }
    fn create_dynamic_function(
        &mut self,
        sig: Signature,
        f: impl Fn(&mut Uiua) -> UiuaResult + SendSyncNative + 'static,
    ) -> DynamicFunction {
        let index = self.asm.dynamic_functions.len();
        self.asm.dynamic_functions.push(Arc::new(f));
        DynamicFunction { index, sig }
    }
    /// Bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error if the binding name is not valid
    pub fn bind_function(&mut self, name: impl Into<EcoString>, function: Function) -> UiuaResult {
        self.bind_function_with_meta(name, function, BindingMeta::default())
    }
    fn bind_function_with_meta(
        &mut self,
        name: impl Into<EcoString>,
        function: Function,
        meta: BindingMeta,
    ) -> UiuaResult {
        let name = name.into();
        let local = LocalName {
            index: self.next_global,
            public: true,
        };
        self.next_global += 1;
        self.compile_bind_function(name, local, function, 0, meta)?;
        Ok(())
    }
    /// Create and bind a function in the current scope
    ///
    /// This function is the only way to bind `# External!` functions.
    ///
    /// # Errors
    /// Returns an error if the binding name is not valid
    pub fn create_bind_function(
        &mut self,
        name: impl Into<EcoString>,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + SendSyncNative + 'static,
    ) -> UiuaResult {
        let name = name.into();
        if let Some(index) = self.externals.get(&name).copied() {
            let df = self.create_dynamic_function(signature.into(), f);
            self.asm.functions.make_mut()[index] = Node::Dynamic(df);
            Ok(())
        } else {
            let function = self.create_function(signature, f);
            let meta = BindingMeta {
                external: true,
                ..Default::default()
            };
            self.bind_function_with_meta(name, function, meta)
        }
    }
    fn sig_of(&self, node: &Node, span: &CodeSpan) -> UiuaResult<Signature> {
        node.sig().map_err(|e| {
            self.error(
                span.clone(),
                format!("Cannot infer function signature: {e}"),
            )
        })
    }
}

fn words_look_pervasive(words: &[Sp<Word>]) -> bool {
    use Primitive::*;
    words.iter().all(|word| match &word.value {
        Word::Primitive(p) if p.class().is_pervasive() => true,
        Word::Primitive(Dup | Dip | Identity | Fork | Under | Each) => true,
        Word::Func(func) if func.word_lines().all(words_look_pervasive) => true,
        Word::Number(..) | Word::Char(..) => true,
        Word::Modified(m) if m.modifier.value == Modifier::Primitive(Primitive::Each) => true,
        _ => false,
    })
}

fn set_in_macro_arg(words: &mut [Sp<Word>]) {
    recurse_words_mut_impl(
        words,
        &|word| match &word.value {
            Word::Modified(m) => matches!(m.modifier.value, Modifier::Primitive(_)),
            _ => true,
        },
        &mut |word| match &mut word.value {
            Word::Ref(r) => r.in_macro_arg = true,
            Word::IncompleteRef { in_macro_arg, .. } => *in_macro_arg = true,
            _ => {}
        },
    );
}

fn recurse_words(words: &[Sp<Word>], f: &mut dyn FnMut(&Sp<Word>)) {
    for word in words {
        f(word);
        match &word.value {
            Word::Strand(items) => recurse_words(items, f),
            Word::Array(arr) => arr.word_lines().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Func(func) => func.word_lines().for_each(|line| {
                recurse_words(line, f);
            }),
            Word::Modified(m) => recurse_words(&m.operands, f),
            Word::Pack(pack) => pack.branches.iter().for_each(|branch| {
                (branch.value.word_lines()).for_each(|line| recurse_words(line, f))
            }),
            _ => {}
        }
    }
}

fn recurse_words_mut(words: &mut [Sp<Word>], f: &mut dyn FnMut(&mut Sp<Word>)) {
    recurse_words_mut_impl(words, &|_| true, f);
}

fn recurse_words_mut_impl(
    words: &mut [Sp<Word>],
    recurse_word: &dyn Fn(&Sp<Word>) -> bool,
    f: &mut dyn FnMut(&mut Sp<Word>),
) {
    for word in words {
        if !recurse_word(word) {
            continue;
        }
        match &mut word.value {
            Word::Strand(items) => recurse_words_mut(items, f),
            Word::Array(arr) => arr.word_lines_mut().for_each(|line| {
                recurse_words_mut(line, f);
            }),
            Word::Func(func) => func.word_lines_mut().for_each(|line| {
                recurse_words_mut(line, f);
            }),
            Word::Modified(m) => recurse_words_mut(&mut m.operands, f),
            Word::Pack(pack) => pack.branches.iter_mut().for_each(|branch| {
                (branch.value.word_lines_mut()).for_each(|line| recurse_words_mut(line, f))
            }),
            Word::Subscripted(sub) => recurse_words_mut(slice::from_mut(&mut sub.word), f),
            _ => {}
        }
        f(word);
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

/// Supertrait for `Send` and `Sync` on native targets, but not on wasm32
#[cfg(not(target_arch = "wasm32"))]
pub trait SendSyncNative: Send + Sync {}
#[cfg(not(target_arch = "wasm32"))]
impl<T: Send + Sync> SendSyncNative for T {}

/// Supertrait for `Send` and `Sync` on native targets, but not on wasm32
#[cfg(target_arch = "wasm32")]
pub trait SendSyncNative {}
#[cfg(target_arch = "wasm32")]
impl<T> SendSyncNative for T {}
