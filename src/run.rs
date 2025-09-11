//! The Uiua interpreter/runtime

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    hash::Hash,
    mem::{size_of, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::Duration,
};

use crossbeam_channel::{Receiver, Sender, TryRecvError};
use ecow::EcoVec;
use parking_lot::Mutex;
use thread_local::ThreadLocal;
use threadpool::ThreadPool;

use crate::{
    algorithm::{self, validate_size_impl},
    fill::{Fill, FillValue},
    invert::match_format_pattern,
    run_prim_func, run_prim_mod, Array, Assembly, BindingKind, BindingMeta, Boxed, CodeSpan,
    Compiler, Function, FunctionId, Ident, Inputs, IntoSysBackend, LocalName, Node, Primitive,
    Report, SafeSys, SigNode, Signature, Span, SubSide, SysBackend, TraceFrame, UiuaError,
    UiuaErrorKind, UiuaResult, Value, VERSION,
};

/// The Uiua interpreter
#[derive(Clone)]
pub struct Uiua {
    pub(crate) rt: Runtime,
    /// The compiled assembly
    pub asm: Assembly,
}

/// Runtime-only data
#[derive(Clone)]
pub(crate) struct Runtime {
    /// The thread's stack
    pub(crate) stack: Vec<Value>,
    /// The thread's under stack
    pub(crate) under_stack: Vec<Value>,
    /// The call stack
    pub(crate) call_stack: Vec<StackFrame>,
    /// The local stack
    pub(crate) local_stack: EcoVec<(usize, Value)>,
    /// The stack for tracking recursion points
    recur_stack: Vec<usize>,
    /// The fill stack
    fill_stack: Vec<FillValue>,
    /// The unfill stack
    unfill_stack: Vec<FillValue>,
    /// The fill boundary stack
    fill_boundary_stack: Vec<(usize, usize)>,
    /// A limit on the execution duration in milliseconds
    pub(crate) execution_limit: Option<f64>,
    /// The time at which execution started
    pub(crate) execution_start: f64,
    /// The recursion limit
    recursion_limit: usize,
    /// Whether the program was interrupted
    #[cfg(not(target_arch = "wasm32"))]
    pub(crate) interrupted: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    #[cfg(target_arch = "wasm32")]
    pub(crate) interrupted: Option<Arc<dyn Fn() -> bool>>,
    /// Whether to print the time taken to execute each instruction
    time_instrs: bool,
    /// The time at which the last instruction was executed
    last_time: f64,
    /// Arguments passed from the command line
    cli_arguments: Vec<String>,
    /// File that was passed to the interpreter for execution
    cli_file_path: PathBuf,
    /// Code for unevaluated pure constants, in case they are needed for macros
    ///
    /// This should only be used in the compile-time environment
    pub(crate) unevaluated_constants: HashMap<usize, Node>,
    /// The system backend
    pub(crate) backend: Arc<dyn SysBackend>,
    /// The thread pool
    thread_pool: Arc<Mutex<Option<ThreadPool>>>,
    /// The thread interface
    thread: ThisThread,
    /// Values for output comments
    pub(crate) output_comments: HashMap<usize, Vec<Vec<Value>>>,
    /// Memoized values
    pub(crate) memo: Arc<ThreadLocal<RefCell<MemoMap>>>,
    /// The results of tests
    pub(crate) test_results: Vec<UiuaResult>,
    /// Reports to print
    pub(crate) reports: Vec<Report>,
}

type MemoMap = HashMap<Node, HashMap<Vec<Value>, Vec<Value>>>;

impl AsRef<Assembly> for Uiua {
    fn as_ref(&self) -> &Assembly {
        &self.asm
    }
}

impl AsMut<Assembly> for Uiua {
    fn as_mut(&mut self) -> &mut Assembly {
        &mut self.asm
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct StackFrame {
    pub(crate) sig: Signature,
    pub(crate) id: Option<FunctionId>,
    track_caller: bool,
    /// The span at which the function was called
    pub(crate) call_span: usize,
    /// Additional spans for error reporting
    spans: Vec<(usize, Option<Primitive>)>,
    /// The stack height at the start of the function
    pub(crate) start_height: usize,
}

#[derive(Debug, Clone)]
struct Channel {
    pub send: Sender<Value>,
    pub recv: Receiver<Value>,
}

#[derive(Debug, Clone)]
struct ThisThread {
    pub parent: Option<Channel>,
    pub children: HashMap<usize, Thread>,
    pub next_child_id: usize,
}

impl Default for ThisThread {
    fn default() -> Self {
        Self {
            parent: Default::default(),
            children: Default::default(),
            next_child_id: 1,
        }
    }
}

#[derive(Debug, Clone)]
struct Thread {
    #[cfg(not(target_arch = "wasm32"))]
    pub recv: Receiver<UiuaResult<Vec<Value>>>,
    #[cfg(target_arch = "wasm32")]
    pub result: UiuaResult<Vec<Value>>,
    pub channel: Channel,
}

impl Default for Uiua {
    fn default() -> Self {
        Self::with_safe_sys()
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
            _ => Err(format!("unknown run mode `{s}`")),
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime {
            stack: Vec::new(),
            under_stack: Vec::new(),
            call_stack: vec![StackFrame {
                id: Some(FunctionId::Main),
                ..Default::default()
            }],
            local_stack: EcoVec::new(),
            recur_stack: Vec::new(),
            fill_stack: Vec::new(),
            fill_boundary_stack: Vec::new(),
            unfill_stack: Vec::new(),
            backend: Arc::new(SafeSys::default()),
            time_instrs: false,
            last_time: 0.0,
            cli_arguments: Vec::new(),
            cli_file_path: PathBuf::new(),
            execution_limit: None,
            execution_start: 0.0,
            #[cfg(debug_assertions)]
            recursion_limit: 20,
            #[cfg(not(debug_assertions))]
            recursion_limit: std::env::var("UIUA_RECURSION_LIMIT")
                .ok()
                .and_then(|s| s.parse().ok())
                .unwrap_or(100),
            interrupted: None,
            thread_pool: Arc::new(Mutex::new(None)),
            thread: ThisThread::default(),
            output_comments: HashMap::new(),
            memo: Arc::new(ThreadLocal::new()),
            unevaluated_constants: HashMap::new(),
            test_results: Vec::new(),
            reports: Vec::new(),
        }
    }
}

impl Uiua {
    /// Create a new Uiua runtime with the standard IO backend
    #[cfg(feature = "native_sys")]
    pub fn with_native_sys() -> Self {
        Self::with_backend(crate::NativeSys)
    }
    /// Create a new Uiua runtime with no IO capabilities
    pub fn with_safe_sys() -> Self {
        Self::with_backend(SafeSys::default())
    }
    /// Create a new Uiua runtime with a custom IO backend
    pub fn with_backend(backend: impl IntoSysBackend) -> Self {
        Uiua {
            rt: Runtime {
                backend: backend.into_sys_backend(),
                ..Runtime::default()
            },
            asm: Assembly::default(),
        }
    }
    /// Build an assembly
    pub fn build(self) -> Assembly {
        self.asm
    }
    /// Get a reference to the system backend
    pub fn backend(&self) -> &dyn SysBackend {
        &*self.rt.backend
    }
    /// Attempt to downcast the system backend to a concrete reference type
    pub fn downcast_backend<T: SysBackend>(&self) -> Option<&T> {
        self.rt.backend.any().downcast_ref()
    }
    /// Attempt to downcast the system backend to a concrete mutable type
    pub fn downcast_backend_mut<T: SysBackend>(&mut self) -> Option<&mut T> {
        Arc::get_mut(&mut self.rt.backend).and_then(|b| b.any_mut().downcast_mut())
    }
    /// Take the system backend
    pub fn take_backend<T: SysBackend + Default>(&mut self) -> Option<T> {
        self.downcast_backend_mut::<T>().map(take)
    }
    /// Take all pending reports
    pub fn take_reports(&mut self) -> Vec<Report> {
        take(&mut self.rt.reports)
    }
    /// Print all pending reports
    #[allow(clippy::print_stdout)]
    pub fn print_reports(&mut self) {
        for report in self.take_reports() {
            eprintln!("{report}");
        }
    }
    /// Take the assembly
    pub fn take_asm(&mut self) -> Assembly {
        take(&mut self.asm)
    }
    /// Set whether to emit the time taken to execute each instruction
    pub fn time_instrs(mut self, time_instrs: bool) -> Self {
        self.rt.time_instrs = time_instrs;
        self
    }
    /// Limit the execution duration
    pub fn with_execution_limit(mut self, limit: Duration) -> Self {
        self.rt.execution_limit = Some(limit.as_secs_f64());
        self
    }
    /// Limit the execution duration
    pub fn maybe_with_execution_limit(mut self, limit: Option<Duration>) -> Self {
        self.rt.execution_limit = limit.map(|limit| limit.as_secs_f64());
        self
    }
    /// Set the recursion limit
    ///
    /// Default is 100 for release builds and 20 for debug builds
    pub fn with_recursion_limit(mut self, limit: usize) -> Self {
        self.rt.recursion_limit = limit;
        self
    }
    /// Set the interrupted hook
    #[cfg(not(target_arch = "wasm32"))]
    pub fn with_interrupt_hook(mut self, hook: impl Fn() -> bool + Send + Sync + 'static) -> Self {
        self.rt.interrupted = Some(Arc::new(hook));
        self
    }
    #[cfg(target_arch = "wasm32")]
    /// Set the interrupted hook
    pub fn with_interrupt_hook(mut self, hook: impl Fn() -> bool + 'static) -> Self {
        self.rt.interrupted = Some(Arc::new(hook));
        self
    }
    /// Set the command line arguments
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.rt.cli_arguments = args;
        self
    }
    /// Get the command line arguments
    pub fn args(&self) -> &[String] {
        self.rt.cli_arguments.as_slice()
    }
    /// Set the path of the file that is being executed
    pub fn with_file_path(mut self, file_path: impl Into<PathBuf>) -> Self {
        self.rt.cli_file_path = file_path.into();
        self
    }
    /// Get the path of the file that is being executed
    pub fn file_path(&self) -> &Path {
        self.rt.cli_file_path.as_path()
    }
    /// Get the input code
    pub fn inputs(&self) -> &Inputs {
        &self.asm.inputs
    }
    /// Configure the compiler, compile, and run
    pub fn compile_run(
        &mut self,
        compile: impl FnOnce(&mut Compiler) -> UiuaResult<&mut Compiler>,
    ) -> UiuaResult<Compiler> {
        let mut comp = Compiler::with_backend(self.rt.backend.clone());
        let asm = compile(&mut comp)?.finish();
        self.run_asm(asm)?;
        comp.set_backend(SafeSys::default());
        Ok(comp)
    }
    /// Run a string as Uiua code
    pub fn run_str(&mut self, input: &str) -> UiuaResult<Compiler> {
        self.compile_run(|comp| comp.load_str(input))
    }
    /// Run a file as Uiua code
    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<Compiler> {
        self.compile_run(|comp| comp.load_file(path))
    }
    /// Run from a compiler
    ///
    /// The runtime will inherit the system backend from the compiler
    pub fn run_compiler(&mut self, compiler: &mut Compiler) -> UiuaResult {
        let backup = compiler.clone();
        self.rt.backend = compiler.backend();
        let res = self.run_asm(compiler.finish());
        let asm = self.take_asm();
        match res {
            Ok(()) => {
                *compiler.assembly_mut() = asm;
                Ok(())
            }
            Err(e) => {
                *compiler = backup;
                Err(e)
            }
        }
    }
    /// Run a Uiua assembly
    pub fn run_asm(&mut self, asm: Assembly) -> UiuaResult {
        self.asm = asm;
        self.rt.execution_start = self.rt.backend.now();
        let mut res = self
            .catching_crash(|env| env.exec(env.asm.root.clone()))
            .unwrap_or_else(Err);
        let mut push_error = |te: UiuaError| match &mut res {
            Ok(()) => res = Err(te),
            Err(e) => e.meta.multi.push(te),
        };
        if self.asm.test_assert_count > 0 {
            let total_run = self.rt.test_results.len();
            let not_run = self.asm.test_assert_count.saturating_sub(total_run);
            let mut successes = 0;
            for res in self.rt.test_results.drain(..) {
                match res {
                    Ok(()) => successes += 1,
                    Err(e) => push_error(e),
                }
            }
            (self.rt.reports).push(Report::tests(successes, total_run - successes, not_run));
        }
        #[cfg(debug_assertions)]
        if res.is_ok() && !self.rt.under_stack.is_empty() {
            res = Err(self.error(format!(
                "Execution ended with {} values left on the under stack. \
                This is a bug in the interpreter.",
                self.rt.under_stack.len()
            )))
        }
        if res.is_err() {
            self.rt = Runtime {
                backend: self.rt.backend.clone(),
                execution_limit: self.rt.execution_limit,
                time_instrs: self.rt.time_instrs,
                output_comments: take(&mut self.rt.output_comments),
                reports: take(&mut self.rt.reports),
                stack: take(&mut self.rt.stack),
                cli_arguments: take(&mut self.rt.cli_arguments),
                ..Runtime::default()
            };
        }
        res
    }
    fn catching_crash<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> UiuaResult<T> {
        match catch_unwind(AssertUnwindSafe(|| f(self))) {
            Ok(res) => Ok(res),
            Err(_) => Err(self.error(format!(
                "\
The interpreter has crashed!
Hooray! You found a bug!
Please report this at http://github.com/uiua-lang/uiua/issues/new \
or on Discord at https://discord.gg/9CU2ME4kmn.

Uiua version {VERSION}

at {}",
                self.span()
            ))),
        }
    }
}

/// Things that can be executed
pub trait Exec {
    /// Execute
    fn exec(self, uiua: &mut Uiua) -> UiuaResult;
}

impl Exec for Node {
    fn exec(self, uiua: &mut Uiua) -> UiuaResult {
        uiua.exec_impl(self)
    }
}

impl Exec for SigNode {
    fn exec(self, uiua: &mut Uiua) -> UiuaResult {
        uiua.exec_with_span(self, 0)
    }
}

impl<T: Exec + Clone> Exec for Arc<T> {
    fn exec(self, uiua: &mut Uiua) -> UiuaResult {
        Arc::unwrap_or_clone(self).exec(uiua)
    }
}

impl Uiua {
    /// Execute an [`Exec`]
    pub fn exec(&mut self, node: impl Exec) -> UiuaResult {
        node.exec(self)
    }
    fn exec_impl(&mut self, node: Node) -> UiuaResult {
        let mut formatted_node = String::new();

        // Uncomment to debug
        // for val in self.rt.stack.iter().rev() {
        //     print!("{:?} ", val);
        // }
        // if self.rt.stack.is_empty() {
        //     print!("(empty) ");
        // }
        // println!();
        // if !self.rt.under_stack.is_empty() {
        //     print!("under: ");
        //     for val in self.rt.under_stack.iter().rev() {
        //         print!("{:?} ", val);
        //     }
        //     println!();
        // }
        // println!("\n    {node:?}");

        if self.rt.time_instrs {
            formatted_node = format!("{node:?}");
            self.rt.last_time = self.rt.backend.now();
        }
        let res = match node {
            Node::Run(nodes) => nodes.into_iter().try_for_each(|node| self.exec(node)),
            Node::Prim(prim, span) => {
                self.with_prim_span(span, Some(prim), |env| run_prim_func(&prim, env))
            }
            Node::ImplPrim(prim, span) => self.with_span(span, |env| prim.run(env)),
            Node::Mod(prim, args, span) => {
                self.with_prim_span(span, Some(prim), |env| run_prim_mod(&prim, args, env))
            }
            Node::ImplMod(prim, args, span) => self.with_span(span, |env| prim.run_mod(args, env)),
            Node::Push(val) => {
                self.rt.stack.push(val);
                Ok(())
            }
            Node::CallGlobal(index, _) => {
                let binding = self.asm.bindings.get(index).ok_or_else(|| {
                    self.error(
                        "Called out-of-bounds binding. \
                        This is a bug in the interpreter.",
                    )
                })?;
                match binding.kind.clone() {
                    BindingKind::Const(Some(val)) => {
                        self.rt.stack.push(val);
                        Ok(())
                    }
                    BindingKind::Const(None) => {
                        if let Some(node) = self.rt.unevaluated_constants.remove(&index) {
                            (|| -> UiuaResult {
                                self.exec(node)?;
                                let val = self.pop("constant")?;
                                self.push(val.clone());
                                self.asm.bindings.make_mut()[index].kind =
                                    BindingKind::Const(Some(val));
                                Ok(())
                            })()
                        } else {
                            Err(self.error(
                                "Called unbound constant. \
                                This is a bug in the interpreter.",
                            ))
                        }
                    }
                    BindingKind::Func(f) => {
                        self.respect_recursion_limit().and_then(|_| self.call(&f))
                    }
                    BindingKind::Import { .. } | BindingKind::Module(_) | BindingKind::Scope(_) => {
                        Err(self.error(
                            "Called module global. \
                        This is a bug in the interpreter.",
                        ))
                    }
                    BindingKind::IndexMacro(_) => Err(self.error(
                        "Called index macro global. \
                        This is a bug in the interpreter.",
                    )),
                    BindingKind::CodeMacro(_) => Err(self.error(
                        "Called code macro global. \
                        This is a bug in the interpreter.",
                    )),
                    BindingKind::Error => Ok(()),
                }
            }
            Node::CallMacro { index, span, .. } => self.with_span(span, |env| {
                let binding = env.asm.bindings.get(index).ok_or_else(|| {
                    env.error(
                        "Called out-of-bounds binding. \
                        This is a bug in the interpreter.",
                    )
                })?;
                let func = match &binding.kind {
                    BindingKind::Func(f) => f.clone(),
                    _ => {
                        return Err(env.error(
                            "Recursive macro is not bound as a function. \
                            This is a bug in the interpreter.",
                        ))
                    }
                };
                env.call(&func)
            }),
            Node::BindGlobal { span, index } => {
                let local = LocalName {
                    index,
                    public: false,
                };
                let Some(mut value) = self.rt.stack.pop() else {
                    return Err(self.error_with_span(
                        self.get_span(span),
                        "No values on the stack for binding. \
                        This is a bug in the interpreter",
                    ));
                };
                value.compress();
                // Binding is a constant
                self.asm
                    .bind_const(local, Some(value), span, BindingMeta::default());
                Ok(())
            }
            Node::Array {
                len,
                inner,
                boxed,
                allow_ext,
                span,
                ..
            } => self.with_span(span, |env| {
                env.make_array(len, inner.into(), boxed, allow_ext)
            }),
            Node::Call(f, span) => self.call_with_span(&f, span),
            Node::CustomInverse(cust, span) => match &cust.normal {
                Ok(normal) => self.exec_with_span(normal.clone(), span),
                Err(e) => self.with_span(span, |env| Err(env.error(e))),
            },
            Node::Switch {
                branches,
                sig,
                span,
                under_cond,
            } => self.with_span(span, |env| {
                algorithm::switch(branches, sig, under_cond, env)
            }),
            Node::Format(parts, span) => self.with_span(span, |env| algorithm::format(&parts, env)),
            Node::MatchFormatPattern(parts, span) => {
                self.with_span(span, |env| match_format_pattern(parts, env))
            }
            Node::Label(label, span) => self.with_span(span, |env| {
                env.monadic_mut(|val| {
                    val.meta
                        .set_label(if label.is_empty() { None } else { Some(label) });
                })
            }),
            Node::RemoveLabel(_, span) => {
                self.with_span(span, |env| env.monadic_mut(|val| val.meta.set_label(None)))
            }
            Node::ValidateType {
                index,
                name,
                type_num,
                span,
            } => {
                let name = name.clone();
                self.with_span(span, |env| {
                    let val = env.pop(index)?;
                    if val.type_id() != type_num {
                        let found = if val.shape.elements() == 1 {
                            val.type_name()
                        } else {
                            val.type_name_plural()
                        };
                        let expected = match type_num {
                            0 => "numbers",
                            1 => "complex numbers",
                            2 => "characters",
                            3 => "boxes",
                            _ => {
                                return Err(env.error(format!(
                                    "Invalid type number {type_num}. \
                                        This is a bug in the interpreter."
                                )));
                            }
                        };
                        return Err(env.error(format!(
                            "Field `{name}` should be {expected} but found {found}"
                        )));
                    }
                    env.push(val);
                    Ok(())
                })
            }
            Node::Dynamic(df) => (|| {
                self.asm
                    .dynamic_functions
                    .get(df.index)
                    .ok_or_else(|| {
                        self.error(format!("Dynamic function index {} out of range", df.index))
                    })?
                    .clone()(self)
            })(),
            Node::Unpack {
                count,
                span,
                prim,
                unbox,
                ..
            } => self.with_span(span, |env| {
                let arr = env.pop(1)?;
                if arr.row_count() != count || arr.shape.is_empty() {
                    let prim_text;
                    let prim_text = if let Some(prim) = prim {
                        prim_text = prim.to_string();
                        prim_text.as_str()
                    } else if unbox {
                        "{}"
                    } else {
                        "[]"
                    };
                    return Err(env.error(if arr.shape.is_empty() {
                        format!(
                            "This °{prim_text} expects an array with {count} rows, \
                            but the array is a scalar"
                        )
                    } else {
                        format!(
                            "This °{prim_text} expects an array with {count} rows, \
                            but the array has {}",
                            arr.row_count()
                        )
                    }));
                }
                if unbox {
                    for val in arr.into_rows().rev() {
                        env.push(val.unboxed());
                    }
                } else {
                    for val in arr.into_rows().rev() {
                        env.push(val);
                    }
                }
                Ok(())
            }),
            Node::SetOutputComment { i, n } => {
                let values = self.stack()[self.stack().len().saturating_sub(n)..].to_vec();
                let stack_values = self.rt.output_comments.entry(i).or_default();
                if stack_values.is_empty() {
                    *stack_values = values.into_iter().map(|v| vec![v]).collect();
                } else {
                    for (stack_values, value) in stack_values.iter_mut().zip(values) {
                        stack_values.push(value);
                    }
                }
                Ok(())
            }
            Node::PushUnder(n, span) => self.with_span(span, |env| {
                env.require_height(n)?;
                let start = env.rt.stack.len() - n;
                env.rt.under_stack.extend(env.rt.stack.drain(start..).rev());
                Ok(())
            }),
            Node::CopyToUnder(n, span) => self.with_span(span, |env| {
                env.require_height(n)?;
                env.rt
                    .under_stack
                    .extend(env.rt.stack.iter().rev().take(n).cloned());
                Ok(())
            }),
            Node::PopUnder(n, span) => self.with_span(span, |env| {
                if env.under_stack_height() < n {
                    return Err(env.error(
                        "Stack was empty when getting context value. \
                        This is a bug in the interpreter.",
                    ));
                }
                let start = env.under_stack_height() - n;
                env.rt.stack.extend(env.rt.under_stack.drain(start..).rev());
                Ok(())
            }),
            Node::NoInline(inner) => self.exec(inner),
            Node::TrackCaller(inner) => {
                self.rt.call_stack.last_mut().unwrap().track_caller = true;
                self.exec(inner)
            }
        };
        #[allow(clippy::print_stdout)]
        if self.rt.time_instrs {
            let end_time = self.rt.backend.now();
            let padding = self.rt.call_stack.len().saturating_sub(1) * 2;
            eprintln!(
                "  ⏲{:padding$}{:.2}ms - {}",
                "",
                end_time - self.rt.last_time,
                formatted_node
            );
            self.rt.last_time = self.rt.backend.now();
        }
        self.respect_execution_limit()?;
        res
    }
    /// Timeout if an execution limit is set and has been exceeded
    pub fn respect_execution_limit(&self) -> UiuaResult {
        if let Some(limit) = self.rt.execution_limit {
            let elapsed = self.rt.backend.now() - self.rt.execution_start;
            if elapsed > limit {
                return Err(
                    UiuaErrorKind::Timeout(self.span(), self.inputs().clone().into()).into(),
                );
            }
        }
        if let Some(hook) = &self.rt.interrupted {
            if hook() {
                return Err(UiuaErrorKind::Interrupted.into());
            }
        }
        Ok(())
    }
    pub(crate) fn with_span<T>(
        &mut self,
        span: usize,
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<T> {
        self.with_prim_span(span, None, f)
    }
    fn with_prim_span<T>(
        &mut self,
        span: usize,
        prim: Option<Primitive>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.rt
            .call_stack
            .last_mut()
            .unwrap()
            .spans
            .push((span, prim));
        let res = f(self);
        self.rt.call_stack.last_mut().unwrap().spans.pop();
        res
    }
    /// Call a function
    #[inline]
    pub fn call(&mut self, f: &Function) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(f, call_span)
    }
    /// Call and truncate the stack to before the args were pushed if the call fails
    pub(crate) fn exec_clean_stack(&mut self, sn: SigNode) -> UiuaResult {
        let sig = sn.sig;
        let bottom = self.stack_height().saturating_sub(sig.args());
        let under_bottom = self
            .rt
            .under_stack
            .len()
            .saturating_sub(sn.sig.under_args());
        let res = self.exec(sn.node);
        if res.is_err() {
            self.truncate_stack(bottom);
            self.rt.under_stack.truncate(under_bottom);
        }
        res
    }
    /// Call and maintain the stack delta if the call fails
    pub(crate) fn exec_maintain_sig(&mut self, sn: SigNode) -> UiuaResult {
        if !sn.node.is_pure(&self.asm) {
            for i in 0..sn.sig.args() {
                self.pop(i + 1)?;
            }
            for _ in 0..sn.sig.outputs() {
                self.push(Value::default());
            }
            return Ok(());
        }

        let mut args = self.stack()[self.stack().len().saturating_sub(sn.sig.args())..].to_vec();
        args.reverse();
        let target_height = (self.stack_height() + sn.sig.outputs()).saturating_sub(sn.sig.args());
        let under_target_height = (self.rt.under_stack.len() + sn.sig.under_outputs())
            .saturating_sub(sn.sig.under_args());
        let res = self.exec(sn);
        match self.stack_height().cmp(&target_height) {
            Ordering::Equal => {}
            Ordering::Greater => {
                self.truncate_stack(target_height);
            }
            Ordering::Less => {
                let diff = target_height - self.stack_height();
                for _ in 0..diff {
                    self.push(args.pop().unwrap_or_default());
                }
            }
        }
        match self.rt.under_stack.len().cmp(&under_target_height) {
            Ordering::Equal => {}
            Ordering::Greater => self.truncate_under_stack(under_target_height),
            Ordering::Less => {
                let diff = under_target_height - self.rt.under_stack.len();
                for _ in 0..diff {
                    self.push_under(args.pop().unwrap_or_default());
                }
            }
        }
        res
    }
    fn call_with_span(&mut self, f: &Function, call_span: usize) -> UiuaResult {
        self.without_fill(|env| {
            env.exec_with_frame_span(
                env.asm[f].clone(),
                StackFrame {
                    sig: f.sig,
                    id: Some(f.id.clone()),
                    call_span,
                    start_height: env.stack_height(),
                    ..Default::default()
                },
                call_span,
            )
        })
    }
    fn exec_with_span(&mut self, sn: SigNode, call_span: usize) -> UiuaResult {
        self.exec_with_frame_span(
            sn.node,
            StackFrame {
                sig: sn.sig,
                call_span,
                start_height: self.stack_height(),
                ..Default::default()
            },
            call_span,
        )
    }
    fn exec_with_frame_span(
        &mut self,
        node: Node,
        frame: StackFrame,
        _call_span: usize,
    ) -> UiuaResult {
        let start_height = self.rt.stack.len();
        let sig = frame.sig;
        self.rt.call_stack.push(frame);
        let res = self.exec(node);
        let frame = self.rt.call_stack.pop().unwrap();
        if let Err(mut err) = res {
            // Trace errors
            let span = self.asm.spans[frame.call_span].clone();
            if frame.track_caller {
                err.track_caller(span);
            } else {
                err.meta.trace.push(TraceFrame { id: frame.id, span });
            }
            return Err(err);
        }
        let height_diff = self.rt.stack.len() as isize - start_height as isize;
        let sig_diff = sig.outputs() as isize - sig.args() as isize;
        if height_diff != sig_diff {
            let message = format!(
                "Function modified the stack by {height_diff} values, but its \
                signature of {sig} implies a change of {sig_diff}"
            );
            #[cfg(debug_assertions)]
            panic!("{message}");
            #[cfg(not(debug_assertions))]
            return Err(self.error_with_span(self.asm.spans[_call_span].clone(), message));
        }
        Ok(())
    }
    pub(crate) fn span_index(&self) -> usize {
        self.rt.call_stack.last().map_or(0, |frame| {
            (frame.spans.last())
                .map(|(i, _)| *i)
                .unwrap_or(frame.call_span)
        })
    }
    /// Get the span of the current function call
    #[track_caller]
    pub fn span(&self) -> Span {
        self.get_span(self.span_index())
    }
    /// Get a span by its index
    #[track_caller]
    pub fn get_span(&self, span: usize) -> Span {
        self.asm.spans[span].clone()
    }
    /// Register a span
    pub fn add_span(&mut self, span: Span) -> usize {
        let idx = self.asm.spans.len();
        self.asm.spans.push(span);
        idx
    }
    /// Construct an error with the current span
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaErrorKind::Run {
            message: self.span().clone().sp(message.to_string()),
            info: Vec::new(),
            inputs: self.inputs().clone().into(),
        }
        .into()
    }
    /// Construct an error with a custom span
    pub fn error_with_span(&self, span: Span, message: impl ToString) -> UiuaError {
        UiuaErrorKind::Run {
            message: span.sp(message.to_string()),
            info: Vec::new(),
            inputs: self.inputs().clone().into(),
        }
        .into()
    }
    #[allow(dead_code)]
    pub(crate) fn error_maybe_span(
        &self,
        span: Option<&CodeSpan>,
        message: impl ToString,
    ) -> UiuaError {
        if let Some(span) = span {
            self.error_with_span(span.clone().into(), message)
        } else {
            self.error(message)
        }
    }
    /// Pop a value from the stack
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        self.rt
            .stack
            .pop()
            .ok_or_else(|| self.error(format!("Stack was empty when getting {}", arg.arg_name())))
    }
    /// Pop a value and try to convert it
    pub fn pop_convert<T>(
        &mut self,
        f: impl FnOnce(&Value, &Uiua, &'static str) -> UiuaResult<T>,
    ) -> UiuaResult<T> {
        f(&self.pop(())?, self, "")
    }
    /// Attempt to pop a value and convert it to a boolean
    pub fn pop_bool(&mut self) -> UiuaResult<bool> {
        self.pop_convert(Value::as_bool)
    }
    /// Attempt to pop a value and convert it to an integer
    pub fn pop_int(&mut self) -> UiuaResult<isize> {
        self.pop_convert(Value::as_int)
    }
    /// Attempt to pop a value and convert it to a natural number
    pub fn pop_nat(&mut self) -> UiuaResult<usize> {
        self.pop_convert(Value::as_nat)
    }
    /// Attempt to pop a value and convert it to a number
    pub fn pop_num(&mut self) -> UiuaResult<f64> {
        self.pop_convert(Value::as_num)
    }
    /// Attempt to pop a value and convert it to a list of natural numbers
    pub fn pop_nats(&mut self) -> UiuaResult<Vec<usize>> {
        self.pop_convert(Value::as_nats)
    }
    /// Attempt to pop a value and convert it to a list of integers
    pub fn pop_ints(&mut self) -> UiuaResult<Vec<isize>> {
        self.pop_convert(Value::as_ints)
    }
    /// Attempt to pop a value and convert it to a list of numbers
    pub fn pop_nums(&mut self) -> UiuaResult<Vec<f64>> {
        Ok(self.pop(())?.as_nums(self, "")?.into_owned())
    }
    /// Attempt to pop a value and convert it to a string
    pub fn pop_string(&mut self) -> UiuaResult<String> {
        self.pop_convert(Value::as_string)
    }
    pub(crate) fn make_array(
        &mut self,
        len: usize,
        inner: Node,
        boxed: bool,
        allow_ext: bool,
    ) -> UiuaResult {
        self.exec(inner)?;
        let start = self.require_height(len)?;
        let values = self.rt.stack.drain(start..).rev();
        let values: Vec<Value> = if boxed {
            values.map(Boxed).map(Value::from).collect()
        } else {
            values.collect()
        };
        let val = if values.is_empty() && boxed {
            Array::<Boxed>::default().into()
        } else {
            let elems: usize = values.iter().map(|v| v.shape.elements()).sum();
            let elem_size = values.first().map_or(size_of::<f64>(), Value::elem_size);
            validate_size_impl(elem_size, [elems]).map_err(|e| self.error(e))?;
            Value::from_row_values_impl(values, self, allow_ext)?
        };
        self.push(val);
        Ok(())
    }
    /// Push a value onto the stack
    pub fn push<V: Into<Value>>(&mut self, val: V) {
        self.rt.stack.push(val.into());
    }
    pub(crate) fn push_under(&mut self, val: Value) {
        self.rt.under_stack.push(val);
    }
    /// Push several values onto the stack
    pub fn push_all<V: Into<Value>>(&mut self, vals: impl IntoIterator<Item = V>) {
        self.rt.stack.extend(vals.into_iter().map(Into::into));
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        self.rt.under_stack.clear();
        take(&mut self.rt.stack)
    }
    /// Take the main stack and under stack
    pub fn take_stacks(&mut self) -> (Vec<Value>, Vec<Value>) {
        let stack = take(&mut self.rt.stack);
        let under = take(&mut self.rt.under_stack);
        (stack, under)
    }
    /// Copy some values from the stack
    pub fn copy_n(&self, n: usize) -> UiuaResult<Vec<Value>> {
        let height = self.require_height(n)?;
        Ok(self.rt.stack[height..].to_vec())
    }
    /// Copy some values down the stack
    ///
    /// `depth` must be greater than or equal to `n`
    pub fn copy_n_down(&self, n: usize, depth: usize) -> UiuaResult<Vec<Value>> {
        debug_assert!(depth >= n);
        let height = self.require_height(depth)?;
        Ok(self.rt.stack[height..][..n].to_vec())
    }
    /// Prepare to fork and return the arguments to f
    pub fn prepare_fork(&mut self, f_args: usize, g_args: usize) -> UiuaResult<Vec<Value>> {
        if f_args > g_args {
            self.require_height(f_args)?;
            let mut vals = Vec::with_capacity(f_args);
            let len = self.rt.stack.len();
            vals.extend(self.rt.stack.drain(len - f_args..(len - g_args)));
            vals.extend(
                self.rt.stack[self.rt.stack.len() - g_args..]
                    .iter()
                    .cloned(),
            );
            debug_assert_eq!(vals.len(), f_args);
            Ok(vals)
        } else {
            self.copy_n(f_args)
        }
    }
    /// Get a value some amount from the top of the stack
    pub fn copy_nth(&self, n: usize) -> UiuaResult<Value> {
        let height = self.require_height(n + 1)?;
        Ok(self.rt.stack[height].clone())
    }
    /// Duplicate some values down the stack
    ///
    /// `depth` must be greater than or equal to `n`
    pub fn dup_values(&mut self, n: usize, depth: usize) -> UiuaResult {
        debug_assert!(depth >= n);
        let start = self.require_height(depth)?;
        for i in 0..n {
            self.rt.stack.push(self.rt.stack[start + i].clone());
        }
        if n != depth {
            self.rt.stack[start..].rotate_right(n);
        }
        Ok(())
    }
    /// Insert some values into the stack
    pub fn insert_stack(
        &mut self,
        depth: usize,
        values: impl IntoIterator<Item = Value>,
    ) -> UiuaResult {
        let start = self.require_height(depth)?;
        self.rt.stack.extend(values);
        self.rt.stack[start..].rotate_left(depth);
        Ok(())
    }
    /// Remove some values from the stack
    ///
    /// `depth` must be greater than or equal to `n`
    pub fn remove_n(
        &mut self,
        n: usize,
        depth: usize,
    ) -> UiuaResult<impl DoubleEndedIterator<Item = Value> + '_> {
        debug_assert!(depth >= n);
        let start = self.require_height(depth)?;
        Ok(self.rt.stack.drain(start..start + n).rev())
    }
    /// Rotate the stack up at some depth
    pub fn rotate_up(&mut self, n: usize, depth: usize) -> UiuaResult {
        let start = self.require_height(depth)?;
        self.rt.stack[start..].rotate_right(n);
        Ok(())
    }
    /// Rotate the stack down at some depth
    pub fn rotate_down(&mut self, n: usize, depth: usize) -> UiuaResult {
        let start = self.require_height(depth)?;
        self.rt.stack[start..].rotate_left(n);
        Ok(())
    }
    /// Access n stack values mutably
    pub fn n_mut(&mut self, n: usize) -> UiuaResult<&mut [Value]> {
        let start = self.require_height(n)?;
        Ok(&mut self.rt.stack[start..])
    }
    pub(crate) fn require_height(&self, n: usize) -> UiuaResult<usize> {
        if self.rt.stack.len() < n {
            return Err(self.error(format!(
                "Stack was empty when getting argument {}",
                self.rt.stack.len() + 1
            )));
        }
        Ok(self.rt.stack.len() - n)
    }
    /// Get a reference to the stack
    pub fn stack(&self) -> &[Value] {
        &self.rt.stack
    }
    /// Get a mutable reference to the stack data
    pub fn stack_mut(&mut self) -> &mut [Value] {
        &mut self.rt.stack
    }
    /// Get all bound values in the assembly
    ///
    /// Bindings are only given values once the assembly has been run successfully
    pub fn bound_values(&self) -> HashMap<Ident, Value> {
        let mut bindings = HashMap::new();
        for binding in &self.asm.bindings {
            if let BindingKind::Const(Some(val)) = &binding.kind {
                let name = binding.span.as_str(self.inputs(), |s| s.into());
                bindings.insert(name, val.clone());
            }
        }
        bindings
    }
    /// Get all bound functions in the assembly
    pub fn bound_functions(&self) -> HashMap<Ident, Function> {
        let mut bindings = HashMap::new();
        for binding in &self.asm.bindings {
            if let BindingKind::Func(f) = &binding.kind {
                let name = binding.span.as_str(self.inputs(), |s| s.into());
                bindings.insert(name, f.clone());
            }
        }
        bindings
    }
    /// Clone `n` values from the top of the stack
    ///
    /// Values are cloned in the order they were pushed
    pub fn clone_stack_top(&self, n: usize) -> UiuaResult<Vec<Value>> {
        if self.rt.stack.len() < n {
            return Err(self.error(format!(
                "Stack was empty getting argument {}",
                n - self.rt.stack.len()
            )));
        }
        Ok(self.rt.stack.iter().rev().take(n).rev().cloned().collect())
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
    pub(crate) fn monadic_env_with<T, V: Into<Value>>(
        &mut self,
        with: T,
        f: fn(T, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let value = self.pop(1)?;
        self.push(f(with, value, self)?);
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
    pub(crate) fn monadic_mut(&mut self, f: impl FnOnce(&mut Value)) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a);
        self.push(a);
        Ok(())
    }
    pub(crate) fn monadic_mut_env(
        &mut self,
        f: impl FnOnce(&mut Value, &Self) -> UiuaResult,
    ) -> UiuaResult {
        let mut a = self.pop(1)?;
        f(&mut a, self)?;
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
    pub(crate) fn dyadic_oo_env_with<V: Into<Value>, T>(
        &mut self,
        with: T,
        f: fn(T, Value, Value, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(with, a, b, self)?);
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
    #[inline]
    pub(crate) fn stack_height(&self) -> usize {
        self.rt.stack.len()
    }
    #[inline]
    pub(crate) fn under_stack_height(&self) -> usize {
        self.rt.under_stack.len()
    }
    #[inline]
    pub(crate) fn truncate_stack(&mut self, size: usize) -> Vec<Value> {
        self.rt.stack.split_off(size)
    }
    #[inline]
    pub(crate) fn truncate_under_stack(&mut self, size: usize) {
        self.rt.under_stack.truncate(size);
    }
    /// Pop `n` values from the stack
    pub fn pop_n(&mut self, n: usize) -> UiuaResult<Vec<Value>> {
        let height = self.require_height(n)?;
        Ok(self.rt.stack.split_off(height))
    }
    /// Get a mutable slice of the top `n` stack values
    pub fn top_n_mut(&mut self, n: usize) -> UiuaResult<&mut [Value]> {
        let height = self.require_height(n)?;
        Ok(&mut self.rt.stack[height..])
    }
    pub(crate) fn value_fill(&self) -> Option<&FillValue> {
        if (self.rt.fill_boundary_stack.last()).is_some_and(|&(i, _)| i >= self.rt.fill_stack.len())
        {
            None
        } else {
            self.last_fill()
        }
    }
    pub(crate) fn value_unfill(&self) -> Option<&FillValue> {
        if (self.rt.fill_boundary_stack.last())
            .is_some_and(|&(_, i)| i >= self.rt.unfill_stack.len())
        {
            None
        } else {
            self.last_unfill()
        }
    }
    pub(crate) fn last_fill(&self) -> Option<&FillValue> {
        self.rt.fill_stack.last()
    }
    pub(crate) fn last_unfill(&self) -> Option<&FillValue> {
        self.rt.unfill_stack.last()
    }
    pub(crate) fn fill(&self) -> Fill {
        Fill::new(self)
    }
    pub(crate) fn unfill(&self) -> Fill {
        Fill::new_un(self)
    }
    /// Do something with the fill context set
    pub(crate) fn with_fill<T>(
        &mut self,
        val: Value,
        side: Option<SubSide>,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<T> {
        self.rt.fill_stack.push(FillValue { value: val, side });
        let res = in_ctx(self);
        self.rt.fill_stack.pop();
        res
    }
    /// Do something with the unfill context set
    pub(crate) fn with_unfill<T>(
        &mut self,
        val: Value,
        side: Option<SubSide>,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<T> {
        self.rt.unfill_stack.push(FillValue { value: val, side });
        let res = in_ctx(self);
        self.rt.unfill_stack.pop();
        res
    }
    /// Do something with the top fill context unset
    pub(crate) fn without_fill<T>(&mut self, in_ctx: impl FnOnce(&mut Self) -> T) -> T {
        let fill_boundary = self.rt.fill_stack.len();
        let unfill_boundary = self.rt.unfill_stack.len();
        (self.rt.fill_boundary_stack).push((fill_boundary, unfill_boundary));
        let res = in_ctx(self);
        self.rt.fill_boundary_stack.pop();
        res
    }
    pub(crate) fn without_fill_but(
        &mut self,
        n: usize,
        but: impl FnOnce(&mut Self) -> UiuaResult,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult,
    ) -> UiuaResult {
        let fills = self.rt.fill_stack[self.rt.fill_stack.len().max(n) - n..].to_vec();
        if fills.len() < n {
            for _ in 0..n - fills.len() {
                self.push(Value::default());
            }
        }
        for fv in fills.into_iter().rev() {
            self.push(fv.value);
        }
        but(self)?;
        self.without_fill(|env| in_ctx(env))
    }
    pub(crate) fn without_unfill_but(
        &mut self,
        n: usize,
        but: impl FnOnce(&mut Self) -> UiuaResult,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult,
    ) -> UiuaResult {
        let fills = self.rt.unfill_stack[self.rt.unfill_stack.len().max(n) - n..].to_vec();
        if fills.len() < n {
            for _ in 0..n - fills.len() {
                self.push(Value::default());
            }
        }
        for fv in fills.into_iter().rev() {
            self.push(fv.value);
        }
        but(self)?;
        self.without_fill(|env| in_ctx(env))
    }
    pub(crate) fn call_frames(&self) -> impl DoubleEndedIterator<Item = &StackFrame> {
        self.rt.call_stack.iter()
    }
    pub(crate) fn respect_recursion_limit(&mut self) -> UiuaResult {
        if self.rt.call_stack.len() > self.rt.recursion_limit {
            Err(
                self.error(if cfg!(target_arch = "wasm32") || cfg!(debug_assertions) {
                    "Recursion limit reached".into()
                } else {
                    format!(
                        "Recursion limit reached. \
                        You can try setting UIUA_RECURSION_LIMIT to a higher value. \
                        The current limit is {}.",
                        self.rt.recursion_limit
                    )
                }),
            )
        } else {
            Ok(())
        }
    }
    /// Spawn a thread
    pub(crate) fn spawn(&mut self, _pool: bool, f: SigNode) -> UiuaResult {
        if !self.rt.backend.allow_thread_spawning() {
            return Err(self.error("Thread spawning is not allowed in this environment"));
        }
        if self.rt.stack.len() < f.sig.args() {
            return Err(self.error(format!(
                "Expected at least {} value(s) on the stack, but there are {}",
                f.sig.args(),
                self.rt.stack.len()
            )))?;
        }
        let (this_send, child_recv) = crossbeam_channel::unbounded();
        let (child_send, this_recv) = crossbeam_channel::unbounded();
        let thread = ThisThread {
            parent: Some(Channel {
                send: child_send,
                recv: child_recv,
            }),
            ..ThisThread::default()
        };
        let make_env = || Uiua {
            asm: self.asm.clone(),
            rt: Runtime {
                stack: (self.rt.stack)
                    .drain(self.rt.stack.len() - f.sig.args()..)
                    .collect(),
                under_stack: Vec::new(),
                local_stack: self.rt.local_stack.clone(),
                fill_stack: Vec::new(),
                fill_boundary_stack: Vec::new(),
                unfill_stack: Vec::new(),
                recur_stack: self.rt.recur_stack.clone(),
                call_stack: Vec::from_iter(self.rt.call_stack.last().cloned()),
                time_instrs: self.rt.time_instrs,
                last_time: self.rt.last_time,
                cli_arguments: self.rt.cli_arguments.clone(),
                cli_file_path: self.rt.cli_file_path.clone(),
                backend: self.rt.backend.clone(),
                execution_limit: self.rt.execution_limit,
                execution_start: self.rt.execution_start,
                recursion_limit: self.rt.recursion_limit,
                interrupted: self.rt.interrupted.clone(),
                output_comments: HashMap::new(),
                memo: self.rt.memo.clone(),
                unevaluated_constants: HashMap::new(),
                test_results: Vec::new(),
                reports: Vec::new(),
                thread_pool: self.rt.thread_pool.clone(),
                thread,
            },
        };
        #[cfg(not(target_arch = "wasm32"))]
        let recv = {
            let (send, recv) = crossbeam_channel::unbounded();
            if _pool {
                let max_threads = std::thread::available_parallelism()
                    .map(|p| p.get())
                    .unwrap_or(1);
                let mut pool = self.rt.thread_pool.lock();
                if pool.is_none() {
                    *pool = Some(ThreadPool::new(max_threads));
                }
                let pool = pool.as_mut().unwrap();
                while pool.active_count() >= max_threads {
                    std::thread::yield_now();
                }
                let mut env = make_env();
                pool.execute(move || _ = send.send(env.exec(f).map(|_| env.take_stack())));
            } else {
                let mut env = make_env();
                std::thread::Builder::new()
                    .spawn(move || _ = send.send(env.exec(f).map(|_| env.take_stack())))
                    .map_err(|e| self.error(format!("Error spawning thread: {e}")))?;
            }
            recv
        };
        #[cfg(target_arch = "wasm32")]
        let result = {
            let mut env = make_env();
            env.exec(f).map(|_| env.take_stack())
        };

        let id = self.rt.thread.next_child_id;
        self.rt.thread.next_child_id += 1;
        self.rt.thread.children.insert(
            id,
            Thread {
                #[cfg(not(target_arch = "wasm32"))]
                recv,
                #[cfg(target_arch = "wasm32")]
                result,
                channel: Channel {
                    send: this_send,
                    recv: this_recv,
                },
            },
        );
        self.push(id);
        Ok(())
    }
    /// Wait for a thread to finish
    pub(crate) fn wait(&mut self, id: Value) -> UiuaResult {
        let ids = id.as_natural_array(self, "Thread id must be an array of natural numbers")?;
        if ids.shape.is_empty() {
            let handle = ids.data[0];
            #[cfg(not(target_arch = "wasm32"))]
            let mut thread_stack = self
                .rt
                .thread
                .children
                .remove(&handle)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .recv
                .recv()
                .unwrap()?;
            #[cfg(target_arch = "wasm32")]
            let mut thread_stack = self
                .rt
                .thread
                .children
                .remove(&handle)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .result?;
            match thread_stack.len() {
                0 => self.push(Value::default()),
                1 => self.push(thread_stack.into_iter().next().unwrap()),
                _ => {
                    thread_stack.reverse();
                    self.push(Value::from_row_values(thread_stack, self)?)
                }
            }
        } else {
            let mut rows = Vec::new();
            for handle in ids.data {
                #[cfg(not(target_arch = "wasm32"))]
                let mut thread_stack = self
                    .rt
                    .thread
                    .children
                    .remove(&handle)
                    .ok_or_else(|| self.error("Invalid thread id"))?
                    .recv
                    .recv()
                    .unwrap()?;
                #[cfg(target_arch = "wasm32")]
                let mut thread_stack = self
                    .rt
                    .thread
                    .children
                    .remove(&handle)
                    .ok_or_else(|| self.error("Invalid thread id"))?
                    .result?;
                let row = if thread_stack.len() == 1 {
                    thread_stack.into_iter().next().unwrap()
                } else {
                    thread_stack.reverse();
                    Value::from_row_values(thread_stack, self)?
                };
                rows.push(row);
            }
            let mut val = Value::from_row_values(rows, self)?;
            let mut shape = ids.shape;
            shape.extend_from_slice(&val.shape[1..]);
            val.shape = shape;
            self.push(val);
        }
        Ok(())
    }
    pub(crate) fn send(&self, id: Value, value: Value) -> UiuaResult {
        if cfg!(target_arch = "wasm32") {
            return Err(self.error("send is not supported in this environment"));
        }
        let ids = id.as_natural_array(self, "Thread id must be an array of natural numbers")?;
        for id in ids.data {
            self.channel(id)?
                .send
                .send(value.clone())
                .map_err(|_| self.error("Thread channel closed"))?;
        }
        Ok(())
    }
    pub(crate) fn recv(&mut self, id: Value) -> UiuaResult {
        if cfg!(target_arch = "wasm32") {
            return Err(self.error("recv is not supported in this environment"));
        }
        let ids = id.as_natural_array(self, "Thread id must be an array of natural numbers")?;
        let mut values = Vec::with_capacity(ids.data.len());
        for id in ids.data {
            values.push(self.channel(id)?.recv.recv().map_err(|_| {
                if let Err(e) = self.wait(id.into()) {
                    e
                } else {
                    self.error("Thread channel closed")
                }
            })?);
        }
        let mut val = Value::from_row_values(values, self)?;
        let mut shape = ids.shape;
        shape.extend_from_slice(&val.shape[1..]);
        val.shape = shape;
        self.push(val);
        Ok(())
    }
    pub(crate) fn try_recv(&mut self, id: Value) -> UiuaResult {
        if cfg!(target_arch = "wasm32") {
            return Err(self.error("try_recv is not supported in this environment"));
        }
        let id = id.as_nat(self, "Thread id must be a natural number")?;
        let value = match self.channel(id)?.recv.try_recv() {
            Ok(value) => value,
            Err(TryRecvError::Empty) => return Err(self.error("No value available")),
            Err(_) => {
                return Err(if let Err(e) = self.wait(id.into()) {
                    e
                } else {
                    self.error("Thread channel closed")
                })
            }
        };
        self.push(value);
        Ok(())
    }
    fn channel(&self, id: usize) -> UiuaResult<&Channel> {
        Ok(if id == 0 {
            self.rt
                .thread
                .parent
                .as_ref()
                .ok_or_else(|| self.error("Thread has no parent"))?
        } else {
            &self
                .rt
                .thread
                .children
                .get(&id)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .channel
        })
    }
}

/// A trait for types that can be used as argument specifiers for [`Uiua::pop`]
///
/// If the stack is empty, the error message will be "Stack was empty when getting {arg_name}"
pub trait StackArg {
    /// Get the name of the argument
    fn arg_name(self) -> String;
}

impl StackArg for () {
    fn arg_name(self) -> String {
        "value".to_string()
    }
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
impl StackArg for &str {
    fn arg_name(self) -> String {
        self.to_string()
    }
}

impl StackArg for String {
    fn arg_name(self) -> String {
        self
    }
}

impl StackArg for (&'static str, usize) {
    fn arg_name(self) -> String {
        format!("{} {}", self.0, self.1)
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
