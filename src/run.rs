//! The Uiua interpreter/runtime

use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt,
    hash::Hash,
    mem::{size_of, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
    time::Duration,
};

use crossbeam_channel::{Receiver, Sender, TryRecvError};
use enum_iterator::{all, Sequence};
use thread_local::ThreadLocal;

use crate::{
    algorithm::{self, invert, validate_size_impl},
    array::Array,
    boxed::Boxed,
    check::instrs_temp_signatures,
    function::*,
    lex::Span,
    value::Value,
    Assembly, BindingKind, CodeSpan, Compiler, Complex, Ident, Inputs, IntoSysBackend, LocalName,
    Primitive, SafeSys, SysBackend, SysOp, TraceFrame, UiuaError, UiuaErrorKind, UiuaResult,
    VERSION,
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
    /// The thread's function stack
    pub(crate) function_stack: Vec<Function>,
    /// The thread's temp stack for inlining
    temp_stacks: [Vec<Value>; TempStack::CARDINALITY],
    /// The stack height at the start of each array currently being built
    pub(crate) array_stack: Vec<usize>,
    /// The call stack
    pub(crate) call_stack: Vec<StackFrame>,
    /// The stack for tracking recursion points
    recur_stack: Vec<usize>,
    /// The fill stack
    fill_stack: Vec<Value>,
    /// The fill boundary stack
    fill_boundary_stack: Vec<usize>,
    /// A limit on the execution duration in milliseconds
    pub(crate) execution_limit: Option<f64>,
    /// The time at which execution started
    pub(crate) execution_start: f64,
    /// Whether the program was interrupted
    pub(crate) interrupted: Option<Arc<dyn Fn() -> bool + Send + Sync>>,
    /// Whether to print the time taken to execute each instruction
    time_instrs: bool,
    /// The time at which the last instruction was executed
    last_time: f64,
    /// Arguments passed from the command line
    cli_arguments: Vec<String>,
    /// File that was passed to the interpreter for execution
    cli_file_path: PathBuf,
    /// The system backend
    pub(crate) backend: Arc<dyn SysBackend>,
    /// The thread interface
    thread: ThisThread,
    /// Values for output comments
    pub(crate) output_comments: HashMap<usize, Vec<Vec<Value>>>,
    /// Memoized values
    pub(crate) memo: Arc<ThreadLocal<RefCell<MemoMap>>>,
}

type MemoMap = HashMap<FunctionId, HashMap<Vec<Value>, Vec<Value>>>;

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

#[derive(Debug, Clone)]
pub(crate) struct StackFrame {
    /// The function being executed
    pub(crate) slice: FuncSlice,
    pub(crate) id: FunctionId,
    pub(crate) sig: Signature,
    track_caller: bool,
    /// The span at which the function was called
    pub(crate) call_span: usize,
    /// The program counter for the function
    pub(crate) pc: usize,
    /// Additional spans for error reporting
    spans: Vec<(usize, Option<Primitive>)>,
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
            _ => Err(format!("unknown run mode `{}`", s)),
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Runtime {
            stack: Vec::new(),
            function_stack: Vec::new(),
            temp_stacks: [Vec::new(), Vec::new()],
            array_stack: Vec::new(),
            call_stack: vec![StackFrame {
                slice: FuncSlice::default(),
                id: FunctionId::Main,
                sig: Signature::new(0, 0),
                track_caller: false,
                call_span: 0,
                pc: 0,
                spans: Vec::new(),
            }],
            recur_stack: Vec::new(),
            fill_stack: Vec::new(),
            fill_boundary_stack: Vec::new(),
            backend: Arc::new(SafeSys::default()),
            time_instrs: false,
            last_time: 0.0,
            cli_arguments: Vec::new(),
            cli_file_path: PathBuf::new(),
            execution_limit: None,
            execution_start: 0.0,
            interrupted: None,
            thread: ThisThread::default(),
            output_comments: HashMap::new(),
            memo: Arc::new(ThreadLocal::new()),
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
    /// Set the interrupted hook
    pub fn with_interrupt_hook(mut self, hook: impl Fn() -> bool + Send + Sync + 'static) -> Self {
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
        self.run_asm(&asm)?;
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
        let mut asm = self.take_asm();
        match res {
            Ok(()) => {
                asm.top_slices.clear();
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
    pub fn run_asm(&mut self, asm: impl Into<Assembly>) -> UiuaResult {
        fn run_asm(env: &mut Uiua, asm: Assembly) -> UiuaResult {
            env.asm = asm;
            env.rt.execution_start = env.rt.backend.now();
            let res = env.run_top_slices();
            if res.is_err() {
                env.rt = Runtime {
                    backend: env.rt.backend.clone(),
                    execution_limit: env.rt.execution_limit,
                    time_instrs: env.rt.time_instrs,
                    output_comments: env.rt.output_comments.clone(),
                    ..Runtime::default()
                };
            }
            res
        }
        run_asm(self, asm.into())
    }
    pub(crate) fn run_top_slices(&mut self) -> UiuaResult {
        let top_slices = take(&mut self.asm.top_slices);
        let mut res = Ok(());
        if let Err(e) = self.catching_crash("", |env| {
            for &slice in &top_slices {
                res = env.call_slice(slice);
                if res.is_err() {
                    break;
                }
            }
        }) {
            res = Err(e);
        }
        self.asm.top_slices = top_slices;
        res
    }
    fn catching_crash<T>(
        &mut self,
        input: impl fmt::Display,
        f: impl FnOnce(&mut Self) -> T,
    ) -> UiuaResult<T> {
        match catch_unwind(AssertUnwindSafe(|| f(self))) {
            Ok(res) => Ok(res),
            Err(_) => Err(self.error(format!(
                "\
The interpreter has crashed!
Hooray! You found a bug!
Please report this at http://github.com/uiua-lang/uiua/issues/new \
or on Discord at https://discord.gg/9CU2ME4kmn.

Uiua version {VERSION}

code:
{}
{}",
                self.span(),
                input
            ))),
        }
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        let slice = frame.slice;
        self.rt.call_stack.push(frame);
        let mut formatted_instr = String::new();
        for i in slice.start..slice.end() {
            let instr = &self.asm.instrs[i];

            // Uncomment to debug
            // for val in &self.rt.stack {
            //     print!("{:?} ", val);
            // }
            // if self.rt.stack.is_empty() {
            //     print!("(empty) ");
            // }
            // println!();
            // if !self.rt.array_stack.is_empty() {
            //     print!("array: ");
            //     for val in &self.rt.array_stack {
            //         print!("{:?} ", val);
            //     }
            //     println!();
            // }
            // if !self.rt.function_stack.is_empty() {
            //     println!("{} function(s)", self.rt.function_stack.len());
            // }
            // for temp in enum_iterator::all::<TempStack>() {
            //     if !self.rt.temp_stacks[temp as usize].is_empty() {
            //         print!("{temp}: ");
            //         for val in &self.rt.temp_stacks[temp as usize] {
            //             print!("{:?} ", val);
            //         }
            //         println!();
            //     }
            // }
            // println!("\n    {:?}", instr);

            if self.rt.time_instrs {
                formatted_instr = format!("{instr:?}");
                self.rt.last_time = self.rt.backend.now();
            }
            let res = match instr {
                Instr::Comment(_) => Ok(()),
                // Pause execution timer during &sc
                &Instr::Prim(prim @ Primitive::Sys(SysOp::ScanLine), span) => {
                    self.with_prim_span(span, Some(prim), |env| {
                        let start = env.rt.backend.now();
                        let res = prim.run(env);
                        env.rt.execution_start += env.rt.backend.now() - start;
                        res
                    })
                }
                &Instr::Prim(prim, span) => {
                    self.with_prim_span(span, Some(prim), |env| prim.run(env))
                }
                &Instr::ImplPrim(prim, span) => self.with_span(span, |env| prim.run(env)),
                Instr::Push(val) => {
                    self.rt.stack.push(Value::clone(val));
                    Ok(())
                }
                &Instr::CallGlobal { index, call, .. } => {
                    match self.asm.bindings[index].kind.clone() {
                        BindingKind::Const(Some(val)) => {
                            self.rt.stack.push(val);
                            Ok(())
                        }
                        BindingKind::Const(None) => Err(self.error(
                            "Called unbound constant. \
                            This is a bug in the interpreter.",
                        )),
                        BindingKind::Func(f) if call => {
                            self.respect_recursion_limit().and_then(|_| self.call(f))
                        }
                        BindingKind::Func(f) => self
                            .respect_recursion_limit()
                            .map(|_| self.rt.function_stack.push(f)),
                        BindingKind::Import { .. } | BindingKind::Module(_) => Err(self.error(
                            "Called module global. \
                            This is a bug in the interpreter.",
                        )),
                        BindingKind::IndexMacro(_) => Err(self.error(
                            "Called index macro global. \
                            This is a bug in the interpreter.",
                        )),
                        BindingKind::CodeMacro(_) => Err(self.error(
                            "Called code macro global. \
                            This is a bug in the interpreter.",
                        )),
                    }
                }
                &Instr::BindGlobal { span, index } => {
                    let local = LocalName {
                        index,
                        public: false,
                    };
                    if let Some(f) = self.rt.function_stack.pop() {
                        // Binding is an imported function
                        self.asm.bind_function(local, f, span, None);
                    } else if let Some(mut value) = self.rt.stack.pop() {
                        value.compress();
                        // Binding is a constant
                        self.asm.bind_const(local, Some(value), span, None);
                    } else {
                        // Binding is an empty function
                        let id = match self.get_span(span) {
                            Span::Code(span) => FunctionId::Anonymous(span),
                            Span::Builtin => FunctionId::Unnamed,
                        };
                        let func = Function::new(id, Signature::new(0, 0), FuncSlice::default(), 0);
                        self.asm.bind_function(local, func, span, None);
                    }
                    Ok(())
                }
                Instr::BeginArray => {
                    self.rt.array_stack.push(self.rt.stack.len());
                    Ok(())
                }
                &Instr::EndArray { span, boxed } => {
                    self.with_span(span, |env| env.end_array(boxed, None))
                }
                &Instr::Call(span) => self
                    .pop_function()
                    .and_then(|f| self.call_with_span(f, span)),
                &Instr::CallRecursive(span) => self.with_span(span, |env| {
                    let f = env.pop_function()?;
                    env.call_recursive(f)
                }),
                &Instr::Recur(span) => self.with_span(span, |env| env.recur()),
                Instr::PushFunc(f) => {
                    self.rt.function_stack.push(f.clone());
                    Ok(())
                }
                &Instr::Switch {
                    count,
                    sig,
                    span,
                    under_cond,
                } => self.with_span(span, |env| algorithm::switch(count, sig, under_cond, env)),
                Instr::Format { parts, span } => {
                    let parts = parts.clone();
                    self.with_span(*span, |env| {
                        let mut s = String::new();
                        for (i, part) in parts.into_iter().enumerate() {
                            if i > 0 {
                                s.push_str(&env.pop(("format argument", i))?.format());
                            }
                            s.push_str(&part);
                        }
                        env.push(s);
                        Ok(())
                    })
                }
                Instr::MatchFormatPattern { parts, span } => {
                    let parts = parts.clone();
                    self.with_span(*span, |env| invert::match_format_pattern(parts, env))
                }
                &Instr::Label {
                    ref label,
                    span,
                    remove,
                } => {
                    let label = label.clone();
                    self.with_span(span, |env| {
                        env.monadic_mut(|val| {
                            let label = if label.is_empty() {
                                None
                            } else if remove {
                                if val.meta().label.as_ref().map_or(true, |l| l == &label) {
                                    None
                                } else {
                                    Some(label)
                                }
                            } else {
                                Some(label)
                            };
                            val.set_label(label);
                        })
                    })
                }
                &Instr::ValidateType {
                    index,
                    ref name,
                    type_num,
                    span,
                } => {
                    let name = name.clone();
                    self.with_span(span, |env| {
                        let val = env.pop(index)?;
                        if val.type_id() != type_num {
                            let found = if val.element_count() == 1 {
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
                &Instr::Dynamic(df) => (|| {
                    self.asm
                        .dynamic_functions
                        .get(df.index)
                        .ok_or_else(|| {
                            self.error(format!("Dynamic function index {} out of range", df.index))
                        })?
                        .clone()(self)
                })(),
                &Instr::Unpack { count, span, unbox } => self.with_span(span, |env| {
                    let arr = env.pop(1)?;
                    if arr.row_count() != count {
                        return Err(env.error(format!(
                            "This °{} expects an array with {} rows, \
                            but the array has {}",
                            if unbox { "{}" } else { "[]" },
                            count,
                            arr.row_count()
                        )));
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
                &Instr::TouchStack { count, span } => {
                    self.with_span(span, |env| env.touch_array_stack(count))
                }
                &Instr::PushTemp { stack, count, span } => self.with_span(span, |env| {
                    for i in 0..count {
                        let value = env.pop(i + 1)?;
                        env.rt.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                &Instr::PopTemp {
                    stack, count, span, ..
                } => self.with_span(span, |env| {
                    for _ in 0..count {
                        let value = env.rt.temp_stacks[stack as usize].pop().ok_or_else(|| {
                            env.error(format!(
                                "Stack was empty when getting saved {} value",
                                format!("{stack:?}").to_lowercase()
                            ))
                        })?;
                        env.push(value);
                    }

                    Ok(())
                }),
                &Instr::CopyToTemp { stack, count, span } => self.with_span(span, |env| {
                    env.touch_array_stack(count)?;
                    for i in 0..count {
                        let value = env.rt.stack[env.rt.stack.len() - i - 1].clone();
                        env.rt.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                Instr::PushSig(_) => Err(self.error(
                    "PushSig should have been removed before running. \
                    This is a bug in the interpreter.",
                )),
                Instr::PopSig => Err(self.error(
                    "PopSig should have been removed before running. \
                    This is a bug in the interpreter.",
                )),
                &Instr::SetOutputComment { i, n } => {
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
            };
            if self.rt.time_instrs {
                let end_time = self.rt.backend.now();
                let padding = self.rt.call_stack.len().saturating_sub(1) * 2;
                #[rustfmt::skip]
                println!( // Allow println
                    "  ⏲{:padding$}{:.2}ms - {}",
                    "",
                    end_time - self.rt.last_time,
                    formatted_instr
                );
                self.rt.last_time = self.rt.backend.now();
            }
            if let Err(mut err) = res {
                // Trace errors
                let frame = self.rt.call_stack.pop().unwrap();
                let span = self.asm.spans[frame.call_span].clone();
                if frame.track_caller {
                    err.track_caller(span);
                } else {
                    err.trace.push(TraceFrame { id: frame.id, span });
                }
                return Err(err);
            }
            self.rt.call_stack.last_mut().unwrap().pc += 1;
            self.respect_execution_limit()?;
        }
        self.rt.call_stack.pop();
        Ok(())
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
    pub fn call(&mut self, f: Function) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(f, call_span)
    }
    #[inline]
    fn call_slice(&mut self, slice: FuncSlice) -> UiuaResult {
        let call_span = self.span_index();
        let frame = StackFrame {
            slice,
            sig: Signature::new(0, 0),
            track_caller: false,
            id: FunctionId::Main,
            call_span,
            spans: Vec::new(),
            pc: 0,
        };
        self.exec(frame)
    }
    #[inline]
    fn call_frame(&mut self, frame: StackFrame) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_frame_span(frame, call_span)
    }
    /// Call and truncate the stack to before the args were pushed if the call fails
    pub(crate) fn call_clean_stack(&mut self, f: Function) -> UiuaResult {
        let sig = f.signature();
        let temp_sigs = instrs_temp_signatures(f.instrs(&self.asm))
            .unwrap_or([Signature::new(0, 0); TempStack::CARDINALITY]);
        let bottom = self.stack_height().saturating_sub(sig.args);
        let mut temp_bottoms: [usize; TempStack::CARDINALITY] = [0; TempStack::CARDINALITY];
        for (i, stack) in self.rt.temp_stacks.iter().enumerate() {
            temp_bottoms[i] = stack.len().saturating_sub(temp_sigs[i].args);
        }
        let array_stack_height = self.rt.array_stack.len();
        let res = self.call(f);
        if res.is_err() {
            self.truncate_stack(bottom);
            for (stack, bottom) in self.rt.temp_stacks.iter_mut().zip(temp_bottoms) {
                stack.truncate(bottom);
            }
            self.rt.array_stack.truncate(array_stack_height);
        }
        res
    }
    /// Call and maintaint the stack delta if the call fails
    pub(crate) fn call_maintain_sig(&mut self, f: Function) -> UiuaResult {
        let sig = f.signature();
        let mut args = self.stack()[self.stack().len().saturating_sub(sig.args)..].to_vec();
        args.reverse();
        let temp_sigs = instrs_temp_signatures(f.instrs(&self.asm))
            .unwrap_or([Signature::new(0, 0); TempStack::CARDINALITY]);
        let target_height = (self.stack_height() + sig.outputs).saturating_sub(sig.args);
        let mut temp_target_heights: [usize; TempStack::CARDINALITY] = [0; TempStack::CARDINALITY];
        for (temp, temp_sig) in all::<TempStack>().zip(&temp_sigs) {
            temp_target_heights[temp as usize] =
                (self.temp_stack_height(temp) + temp_sig.outputs).saturating_sub(temp_sig.args);
        }
        let res = self.call(f);
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
        for (temp, target_height) in all::<TempStack>().zip(&temp_target_heights) {
            match self.temp_stack_height(temp).cmp(target_height) {
                Ordering::Equal => {}
                Ordering::Greater => self.truncate_temp_stack(temp, *target_height),
                Ordering::Less => {
                    let diff = target_height - self.temp_stack_height(temp);
                    for _ in 0..diff {
                        self.push_temp(temp, args.pop().unwrap_or_default());
                    }
                }
            }
        }
        res
    }
    #[inline]
    fn call_with_span(&mut self, f: Function, call_span: usize) -> UiuaResult {
        self.call_with_frame_span(
            StackFrame {
                slice: f.slice(),
                sig: f.signature(),
                id: f.id,
                track_caller: f.flags.track_caller(),
                call_span,
                spans: Vec::new(),
                pc: 0,
            },
            call_span,
        )
    }
    #[inline]
    fn call_with_frame_span(&mut self, frame: StackFrame, call_span: usize) -> UiuaResult {
        let start_height = self.rt.stack.len();
        let sig = frame.sig;
        self.exec(frame)?;
        let height_diff = self.rt.stack.len() as isize - start_height as isize;
        let sig_diff = sig.outputs as isize - sig.args as isize;
        if height_diff != sig_diff {
            return Err(self.error_with_span(
                self.asm.spans[call_span].clone(),
                format!(
                    "Function modified the stack by {height_diff} values, but its \
                    signature of {sig} implies a change of {sig_diff}"
                ),
            ));
        }
        Ok(())
    }
    pub(crate) fn span_index(&self) -> usize {
        self.rt.call_stack.last().map_or(0, |frame| {
            frame
                .spans
                .last()
                .map(|(i, _)| *i)
                .unwrap_or(frame.call_span)
        })
    }
    /// Get the span of the current function call
    pub fn span(&self) -> Span {
        self.get_span(self.span_index())
    }
    /// Get a span by its index
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
        UiuaErrorKind::Run(
            self.span().clone().sp(message.to_string()),
            self.inputs().clone().into(),
        )
        .into()
    }
    /// Construct an error with a custom span
    pub fn error_with_span(&self, span: Span, message: impl ToString) -> UiuaError {
        UiuaErrorKind::Run(span.sp(message.to_string()), self.inputs().clone().into()).into()
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
    pub(crate) fn pattern_match_error(&self) -> UiuaError {
        UiuaErrorKind::Run(
            self.span().sp("Pattern match failed".into()),
            self.inputs().clone().into(),
        )
        .into()
    }
    /// Pop a value from the stack
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        let res = self.rt.stack.pop().ok_or_else(|| {
            self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))
        });
        for bottom in &mut self.rt.array_stack {
            *bottom = (*bottom).min(self.rt.stack.len());
        }
        res
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
        self.pop_convert(Value::as_nums)
    }
    /// Attempt to pop a value and convert it to a string
    pub fn pop_string(&mut self) -> UiuaResult<String> {
        self.pop_convert(Value::as_string)
    }
    /// Simulates popping a value and immediately pushing it back
    pub(crate) fn touch_array_stack(&mut self, n: usize) -> UiuaResult {
        if self.rt.stack.len() < n {
            return Err(self.error(format!(
                "Stack was empty evaluating argument {}",
                self.rt.stack.len() + 1
            )));
        }
        for bottom in &mut self.rt.array_stack {
            *bottom = (*bottom).min(self.rt.stack.len().saturating_sub(n));
        }
        Ok(())
    }
    pub(crate) fn end_array(&mut self, boxed: bool, initial_value: Option<Value>) -> UiuaResult {
        let start = self.rt.array_stack.pop().unwrap();
        let values = self.rt.stack.drain(start..).rev();
        let values: Vec<Value> = if boxed {
            values.map(Boxed).map(Value::from).collect()
        } else {
            values.collect()
        };
        let mut val = if values.is_empty() && boxed {
            Array::<Boxed>::default().into()
        } else {
            let elems: usize = values.iter().map(Value::element_count).sum();
            let elem_size = values.first().map_or(size_of::<f64>(), Value::elem_size);
            validate_size_impl(elem_size, [elems]).map_err(|e| self.error(e))?;
            Value::from_row_values(values, self)?
        };
        if let Some(init) = initial_value {
            if val.shape() == [0] {
                val = init;
            } else {
                val = init.join(val, false, self)?;
            }
        }
        self.push(val);
        Ok(())
    }
    /// Push a value onto the stack
    pub fn push<V: Into<Value>>(&mut self, val: V) {
        self.rt.stack.push(val.into());
    }
    pub(crate) fn push_temp(&mut self, temp: TempStack, val: Value) {
        self.rt.temp_stacks[temp as usize].push(val);
    }
    /// Push a function onto the function stack
    pub fn push_func(&mut self, f: Function) {
        self.rt.function_stack.push(f);
    }
    /// Get a slice of instructions
    pub fn instrs(&self, slice: FuncSlice) -> &[Instr] {
        &self.asm.instrs[slice.start..][..slice.len]
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        for stack in &mut self.rt.temp_stacks {
            stack.clear();
        }
        self.rt.function_stack.clear();
        take(&mut self.rt.stack)
    }
    /// Take all stacks
    pub fn take_stacks(&mut self) -> (Vec<Value>, [Vec<Value>; TempStack::CARDINALITY]) {
        let temp_stacks = take(&mut self.rt.temp_stacks);
        let stack = take(&mut self.rt.stack);
        (stack, temp_stacks)
    }
    /// Get a reference to the stack
    pub fn stack(&self) -> &[Value] {
        &self.rt.stack
    }
    /// Get a mutable reference to the stack data
    pub fn stack_mut(&mut self) -> &mut [Value] {
        &mut self.rt.stack
    }
    /// Pop a function from the function stack
    pub fn pop_function(&mut self) -> UiuaResult<Function> {
        self.rt.function_stack.pop().ok_or_else(|| {
            self.error(
                "Function stack was empty when popping. \
                This is a bug in the interpreter.",
            )
        })
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
                "Stack was empty evaluating argument {}",
                n - self.rt.stack.len()
            )));
        }
        Ok(self.rt.stack.iter().rev().take(n).rev().cloned().collect())
    }
    pub(crate) fn dup_n(&mut self, n: usize) -> UiuaResult {
        if self.rt.stack.len() < n {
            return Err(self.error(format!(
                "Stack was empty evaluating argument {}",
                n - self.rt.stack.len()
            )));
        }
        let start = self.rt.stack.len() - n;
        for bottom in &mut self.rt.array_stack {
            *bottom = (*bottom).min(start);
        }
        for i in 0..n {
            self.push(self.rt.stack[start + i].clone());
        }
        Ok(())
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
    pub(crate) fn dyadic_oo_00_env<V: Into<Value>>(
        &mut self,
        f: fn(Value, Value, usize, usize, &Self) -> UiuaResult<V>,
    ) -> UiuaResult {
        let a = self.pop(1)?;
        let b = self.pop(2)?;
        self.push(f(a, b, 0, 0, self)?);
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
    pub(crate) fn stack_height(&self) -> usize {
        self.rt.stack.len()
    }
    pub(crate) fn temp_stack_height(&self, stack: TempStack) -> usize {
        self.rt.temp_stacks[stack as usize].len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) -> Vec<Value> {
        self.rt.stack.split_off(size)
    }
    pub(crate) fn truncate_temp_stack(&mut self, stack: TempStack, size: usize) {
        self.rt.temp_stacks[stack as usize].truncate(size);
    }
    pub(crate) fn remove_nth_back(&mut self, n: usize) -> UiuaResult<Value> {
        let len = self.rt.stack.len();
        if n >= len {
            return Err(self.error(format!("Stack was empty evaluating argument {}", n + 1)));
        }
        Ok(self.rt.stack.remove(len - n - 1))
    }
    pub(crate) fn pop_n(&mut self, n: usize) -> UiuaResult<Vec<Value>> {
        let len = self.rt.stack.len();
        if n > len {
            return Err(self.error(format!("Stack was empty evaluating argument {}", n + 1)));
        }
        Ok(self.rt.stack.split_off(len - n))
    }
    pub(crate) fn num_scalar_fill(&self) -> Result<f64, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n)) if n.rank() == 0 => Ok(n.data[0]),
            Some(Value::Num(_)) => Err(self.fill_error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(n.data[0] as f64),
            Some(Value::Byte(_)) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn num_array_fill(&self) -> Result<Array<f64>, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n)) => Ok(n.clone()),
            Some(Value::Byte(n)) => Ok(n.convert_ref()),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn byte_scalar_fill(&self) -> Result<u8, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n))
                if n.rank() == 0
                    && n.data[0].fract() == 0.0
                    && (0.0..=255.0).contains(&n.data[0]) =>
            {
                Ok(n.data[0] as u8)
            }
            Some(Value::Num(n)) if n.rank() == 0 => Err(self.fill_error(false)),
            Some(Value::Num(_)) => Err(self.fill_error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(n.data[0]),
            Some(Value::Byte(_)) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn byte_array_fill(&self) -> Result<Array<u8>, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n))
                if n.data
                    .iter()
                    .all(|&n| n.fract() == 0.0 && (0.0..=255.0).contains(&n)) =>
            {
                Ok(n.data.iter().copied().map(|n| n as u8).collect())
            }
            Some(Value::Byte(n)) => Ok(n.clone()),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn char_scalar_fill(&self) -> Result<char, &'static str> {
        match self.value_fill() {
            Some(Value::Char(c)) if c.rank() == 0 => Ok(c.data[0]),
            Some(Value::Char(_)) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn char_array_fill(&self) -> Result<Array<char>, &'static str> {
        match self.value_fill() {
            Some(Value::Char(c)) => Ok(c.clone()),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn box_scalar_fill(&self) -> Result<Boxed, &'static str> {
        match self.value_fill() {
            Some(Value::Box(b)) if b.rank() == 0 => Ok(b.data[0].clone()),
            Some(Value::Box(_)) => Err(self.fill_error(true)),
            Some(val) => Ok(Boxed(val.clone())),
            None => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn box_array_fill(&self) -> Result<Array<Boxed>, &'static str> {
        match self.value_fill() {
            Some(Value::Box(b)) => Ok(b.clone()),
            Some(val) => Ok(Array::new([], [Boxed(val.clone())])),
            None => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn complex_scalar_fill(&self) -> Result<Complex, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n)) if n.rank() == 0 => Ok(Complex::new(n.data[0], 0.0)),
            Some(Value::Num(_)) => Err(self.fill_error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(Complex::new(n.data[0] as f64, 0.0)),
            Some(Value::Byte(_)) => Err(self.fill_error(true)),
            Some(Value::Complex(c)) if c.rank() == 0 => Ok(c.data[0]),
            Some(Value::Complex(_)) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn complex_array_fill(&self) -> Result<Array<Complex>, &'static str> {
        match self.value_fill() {
            Some(Value::Num(n)) => Ok(n.convert_ref()),
            Some(Value::Byte(n)) => Ok(n.convert_ref()),
            Some(Value::Complex(c)) => Ok(c.clone()),
            _ => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn value_fill(&self) -> Option<&Value> {
        if (self.rt.fill_boundary_stack.last()).is_some_and(|&i| i >= self.rt.fill_stack.len()) {
            None
        } else {
            self.last_fill()
        }
    }
    pub(crate) fn last_fill(&self) -> Option<&Value> {
        self.rt.fill_stack.last()
    }
    fn fill_error(&self, scalar: bool) -> &'static str {
        if scalar {
            match self.value_fill() {
                Some(Value::Num(_)) => ". A number fill is set, but is is not a scalar.",
                Some(Value::Byte(_)) => ". A number fill is set, but is is not a scalar.",
                Some(Value::Char(_)) => ". A character fill is set, but is is not a scalar.",
                Some(Value::Complex(_)) => ". A complex fill is set, but is is not a scalar.",
                Some(Value::Box(_)) => ". A box fill is set, but is is not a scalar.",
                None => "",
            }
        } else {
            match self.value_fill() {
                Some(Value::Num(_)) => ". A number fill is set, but the array is not numbers.",
                Some(Value::Byte(_)) => ". A number fill is set, but the array is not numbers.",
                Some(Value::Char(_)) => {
                    ". A character fill is set, but the array is not characters."
                }
                Some(Value::Complex(_)) => {
                    ". A complex fill is set, but the array is not complex numbers."
                }
                Some(Value::Box(_)) => ". A box fill is set, but the array is not boxed values.",
                None => "",
            }
        }
    }
    /// Do something with the fill context set
    pub(crate) fn with_fill<T>(
        &mut self,
        value: Value,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<T> {
        self.rt.fill_stack.push(value);
        let res = in_ctx(self);
        self.rt.fill_stack.pop();
        res
    }
    /// Do something with the top fill context unset
    pub(crate) fn without_fill<T>(&mut self, in_ctx: impl FnOnce(&mut Self) -> T) -> T {
        self.rt.fill_boundary_stack.push(self.rt.fill_stack.len());
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
        let fills = self
            .rt
            .fill_stack
            .split_off(self.rt.fill_stack.len().max(n) - n);
        if fills.len() < n {
            for _ in 0..n - fills.len() {
                self.push(Value::default());
            }
        }
        for value in fills.iter().rev().cloned() {
            self.push(value);
        }
        let res1 = but(self);
        let res2 = in_ctx(self);
        self.rt.fill_stack.extend(fills.into_iter().rev());
        res1?;
        res2
    }
    pub(crate) fn call_frames(&self) -> impl DoubleEndedIterator<Item = &StackFrame> {
        self.rt.call_stack.iter()
    }
    pub(crate) fn call_recursive(&mut self, f: Function) -> UiuaResult {
        let call_height = self.rt.call_stack.len();
        let with_height = self.rt.recur_stack.len();
        self.rt.recur_stack.push(self.rt.call_stack.len());
        let res = self.call(f);
        self.rt.call_stack.truncate(call_height);
        self.rt.recur_stack.truncate(with_height);
        res
    }
    pub(crate) fn respect_recursion_limit(&mut self) -> UiuaResult {
        #[cfg(debug_assertions)]
        const RECURSION_LIMIT: usize = 22;
        #[cfg(not(debug_assertions))]
        const RECURSION_LIMIT: usize = 130;
        if self.rt.call_stack.len() > RECURSION_LIMIT {
            Err(self.error("Recursion limit reached"))
        } else {
            Ok(())
        }
    }
    pub(crate) fn recur(&mut self) -> UiuaResult {
        let Some(i) = self.rt.recur_stack.last().copied() else {
            return Err(self.error(
                "No recursion context set. This \
                is a bug in the interpreter.",
            ));
        };
        self.respect_recursion_limit()?;
        let mut frame = self.rt.call_stack[i].clone();
        frame.pc = 0;
        self.respect_execution_limit()?;
        self.call_frame(frame)
    }
    /// Spawn a thread
    pub(crate) fn spawn(
        &mut self,
        capture_count: usize,
        _pool: bool,
        f: impl FnOnce(&mut Self) -> UiuaResult + Send + 'static,
    ) -> UiuaResult {
        if self.rt.stack.len() < capture_count {
            return Err(self.error(format!(
                "Expected at least {} value(s) on the stack, but there are {}",
                capture_count,
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
        let mut env = Uiua {
            asm: self.asm.clone(),
            rt: Runtime {
                stack: (self.rt.stack)
                    .drain(self.rt.stack.len() - capture_count..)
                    .collect(),
                function_stack: Vec::new(),
                temp_stacks: [Vec::new(), Vec::new()],
                array_stack: Vec::new(),
                fill_stack: Vec::new(),
                fill_boundary_stack: Vec::new(),
                recur_stack: self.rt.recur_stack.clone(),
                call_stack: Vec::new(),
                time_instrs: self.rt.time_instrs,
                last_time: self.rt.last_time,
                cli_arguments: self.rt.cli_arguments.clone(),
                cli_file_path: self.rt.cli_file_path.clone(),
                backend: self.rt.backend.clone(),
                execution_limit: self.rt.execution_limit,
                execution_start: self.rt.execution_start,
                interrupted: self.rt.interrupted.clone(),
                output_comments: HashMap::new(),
                memo: self.rt.memo.clone(),
                thread,
            },
        };
        #[cfg(not(target_arch = "wasm32"))]
        let recv = {
            let (send, recv) = crossbeam_channel::unbounded();
            if _pool {
                rayon::spawn(move || _ = send.send(f(&mut env).map(|_| env.take_stack())));
            } else {
                std::thread::Builder::new()
                    .spawn(move || _ = send.send(f(&mut env).map(|_| env.take_stack())))
                    .map_err(|e| self.error(format!("Error spawning thread: {e}")))?;
            }
            recv
        };
        #[cfg(target_arch = "wasm32")]
        let result = f(&mut env).map(|_| env.take_stack());

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
            let thread_stack = self
                .rt
                .thread
                .children
                .remove(&handle)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .recv
                .recv()
                .unwrap()?;
            #[cfg(target_arch = "wasm32")]
            let thread_stack = self
                .rt
                .thread
                .children
                .remove(&handle)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .result?;
            self.rt.stack.extend(thread_stack);
        } else {
            let mut rows = Vec::new();
            for handle in ids.data {
                #[cfg(not(target_arch = "wasm32"))]
                let thread_stack = self
                    .rt
                    .thread
                    .children
                    .remove(&handle)
                    .ok_or_else(|| self.error("Invalid thread id"))?
                    .recv
                    .recv()
                    .unwrap()?;
                #[cfg(target_arch = "wasm32")]
                let thread_stack = self
                    .rt
                    .thread
                    .children
                    .remove(&handle)
                    .ok_or_else(|| self.error("Invalid thread id"))?
                    .result?;
                let row = if thread_stack.len() == 1 {
                    thread_stack.into_iter().next().unwrap()
                } else {
                    Value::from_row_values(thread_stack, self)?
                };
                rows.push(row);
            }
            let mut val = Value::from_row_values(rows, self)?;
            let mut shape = ids.shape;
            shape.extend_from_slice(&val.shape()[1..]);
            *val.shape_mut() = shape;
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
        shape.extend_from_slice(&val.shape()[1..]);
        *val.shape_mut() = shape;
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
/// If the stack is empty, the error message will be "Stack was empty when evaluating {arg_name}"
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
