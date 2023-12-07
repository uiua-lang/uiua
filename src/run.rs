use std::{
    collections::{BTreeSet, HashMap},
    fs,
    hash::Hash,
    mem::{replace, size_of, take},
    panic::{catch_unwind, AssertUnwindSafe},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use crossbeam_channel::{Receiver, Sender, TryRecvError};
use ecow::EcoVec;
use enum_iterator::Sequence;
use instant::Duration;
use parking_lot::Mutex;
use rand::prelude::*;

use crate::{
    algorithm, array::Array, boxed::Boxed, constants, function::*, lex::Span, parse::parse,
    value::Value, Complex, Diagnostic, DiagnosticKind, Ident, NativeSys, Primitive, SysBackend,
    SysOp, TraceFrame, UiuaError, UiuaResult,
};

/// The Uiua runtime
#[derive(Clone)]
pub struct Uiua {
    pub(crate) instrs: EcoVec<Instr>,
    /// Functions which are under construction
    pub(crate) new_functions: Vec<EcoVec<Instr>>,
    /// Global values
    pub(crate) globals: Arc<Mutex<Vec<Global>>>,
    /// Indexable spans
    spans: Arc<Mutex<Vec<Span>>>,
    /// The thread's stack
    pub(crate) stack: Vec<Value>,
    /// The thread's function stack
    pub(crate) function_stack: Vec<Function>,
    /// The thread's temp stack for inlining
    temp_stacks: [Vec<Value>; TempStack::CARDINALITY],
    /// The thread's temp stack for functions
    temp_function_stack: Vec<Function>,
    /// The current scope
    pub(crate) scope: Scope,
    /// Ancestor scopes of the current one
    pub(crate) higher_scopes: Vec<Scope>,
    /// Determines which How test scopes are run
    pub(crate) mode: RunMode,
    /// A limit on the execution duration in milliseconds
    execution_limit: Option<f64>,
    /// The time at which execution started
    execution_start: f64,
    /// The paths of files currently being imported (used to detect import cycles)
    current_imports: Arc<Mutex<Vec<PathBuf>>>,
    /// The bindings of imported files
    imports: Arc<Mutex<HashMap<PathBuf, HashMap<Ident, usize>>>>,
    /// Accumulated diagnostics
    pub(crate) diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    pub(crate) print_diagnostics: bool,
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
}

#[derive(Clone)]
pub(crate) enum Global {
    Val(Value),
    Func { f: Function, sig_declared: bool },
}

#[derive(Clone)]
pub(crate) struct Scope {
    /// The stack height at the start of each array currently being built
    array: Vec<usize>,
    /// The call stack
    call: Vec<StackFrame>,
    /// The recur stack
    this: Vec<usize>,
    /// Map local names to global indices
    pub names: HashMap<Ident, usize>,
    /// The shape fix stack
    fills: Vec<Fill>,
    /// Whether to unpack boxed values
    pub unpack_boxes: bool,
    /// Whether to allow experimental features
    pub experimental: bool,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            array: Vec::new(),
            call: vec![StackFrame {
                slice: FuncSlice::default(),
                id: FunctionId::Main,
                sig: Signature::new(0, 0),
                call_span: 0,
                pc: 0,
                spans: Vec::new(),
            }],
            this: Vec::new(),
            names: HashMap::new(),
            fills: Vec::new(),
            unpack_boxes: false,
            experimental: false,
        }
    }
}

#[derive(Clone)]
enum Fill {
    Num(f64),
    Complex(Complex),
    Char(char),
    Box(Boxed),
    None,
}

#[derive(Clone)]
pub(crate) struct StackFrame {
    /// The function being executed
    pub(crate) slice: FuncSlice,
    pub(crate) id: FunctionId,
    pub(crate) sig: Signature,
    /// The span at which the function was called
    call_span: usize,
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
    pub handle: Arc<std::thread::JoinHandle<UiuaResult<Vec<Value>>>>,
    #[cfg(target_arch = "wasm32")]
    pub result: UiuaResult<Vec<Value>>,
    pub channel: Channel,
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
        for def in constants() {
            scope.names.insert(def.name.into(), globals.len());
            globals.push(Global::Val(def.value.clone()));
        }
        Uiua {
            instrs: EcoVec::new(),
            spans: Arc::new(Mutex::new(vec![Span::Builtin])),
            stack: Vec::new(),
            function_stack: Vec::new(),
            temp_stacks: [Vec::new(), Vec::new()],
            temp_function_stack: Vec::new(),
            scope,
            higher_scopes: Vec::new(),
            globals: Arc::new(Mutex::new(globals)),
            new_functions: Vec::new(),
            current_imports: Arc::new(Mutex::new(Vec::new())),
            imports: Arc::new(Mutex::new(HashMap::new())),
            mode: RunMode::Normal,
            diagnostics: BTreeSet::new(),
            backend: Arc::new(NativeSys),
            print_diagnostics: false,
            time_instrs: false,
            last_time: 0.0,
            cli_arguments: Vec::new(),
            cli_file_path: PathBuf::new(),
            execution_limit: None,
            execution_start: 0.0,
            thread: ThisThread::default(),
        }
    }
    /// Create a new Uiua runtime with a custom IO backend
    pub fn with_backend(backend: impl SysBackend) -> Self {
        Uiua {
            backend: Arc::new(backend),
            ..Default::default()
        }
    }
    /// Get a reference to the system backend
    pub fn backend(&self) -> &dyn SysBackend {
        &*self.backend
    }
    /// Attempt to downcast the system backend to a concrete reference type
    pub fn downcast_backend<T: SysBackend>(&self) -> Option<&T> {
        self.backend.any().downcast_ref()
    }
    /// Attempt to downcast the system backend to a concrete mutable type
    pub fn downcast_backend_mut<T: SysBackend>(&mut self) -> Option<&mut T> {
        Arc::get_mut(&mut self.backend).and_then(|b| b.any_mut().downcast_mut())
    }
    /// Take the system backend
    pub fn take_backend<T: SysBackend>(&mut self) -> Option<T>
    where
        T: Default,
    {
        self.downcast_backend_mut::<T>().map(take)
    }
    /// Set whether to consume print diagnostics as they are encountered
    pub fn print_diagnostics(mut self, print_diagnostics: bool) -> Self {
        self.print_diagnostics = print_diagnostics;
        self
    }
    /// Set whether to emit the time taken to execute each instruction
    pub fn time_instrs(mut self, time_instrs: bool) -> Self {
        self.time_instrs = time_instrs;
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
        f: impl FnOnce(&mut Self) -> UiuaResult<T>,
    ) -> UiuaResult<HashMap<Ident, usize>> {
        self.higher_scopes.push(take(&mut self.scope));
        let start_height = self.stack.len();
        let res = f(self);
        let scope = replace(&mut self.scope, self.higher_scopes.pop().unwrap());
        res?;
        let mut names = HashMap::new();
        for (name, idx) in scope.names {
            if idx >= constants().len() {
                names.insert(name, idx);
            }
        }
        self.stack.truncate(start_height);
        Ok(names)
    }
    fn load_impl(&mut self, input: &str, path: Option<&Path>) -> UiuaResult {
        self.execution_start = instant::now();
        let (items, errors, diagnostics) = parse(input, path);
        if self.print_diagnostics {
            for diagnostic in diagnostics {
                println!("{}", diagnostic.report());
            }
        } else {
            self.diagnostics.extend(diagnostics);
        }
        if !errors.is_empty() {
            return Err(errors.into());
        }
        if let Some(path) = path {
            self.current_imports.lock().push(path.into());
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
        if path.is_some() {
            self.current_imports.lock().pop();
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
            id: frame.id.clone(),
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
    pub(crate) fn import(&mut self, input: &str, path: &Path, item: &str) -> UiuaResult {
        if self.current_imports.lock().iter().any(|p| p == path) {
            return Err(self.error(format!(
                "Cycle detected importing {}",
                path.to_string_lossy()
            )));
        }
        if !self.imports.lock().contains_key(path) {
            let import = self.in_scope(|env| env.load_str_path(input, path).map(drop))?;
            self.imports.lock().insert(path.into(), import);
        }
        let imports_gaurd = self.imports.lock();
        let imports = &imports_gaurd[path];
        let idx = imports.get(item).ok_or_else(|| {
            self.error(format!("Item `{}` not found in {}", item, path.display()))
        })?;
        let global = self.globals.lock()[*idx].clone();
        drop(imports_gaurd);
        match global {
            Global::Val(val) => self.push(val),
            Global::Func { f, .. } => self.function_stack.push(f),
        }
        Ok(())
    }
    /// Resolve a declared import path relative to the path of the file that is being executed
    pub(crate) fn resolve_import_path(&self, path: &Path) -> PathBuf {
        let target =
            if let Some(parent) = self.current_imports.lock().last().and_then(|p| p.parent()) {
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
    pub(crate) fn exec_global_instrs(&mut self, instrs: EcoVec<Instr>) -> UiuaResult {
        let start = self.instrs.len();
        self.instrs.extend(instrs);
        let end = self.instrs.len();
        self.exec(StackFrame {
            slice: FuncSlice {
                address: start,
                len: end - start,
            },
            id: FunctionId::Main,
            sig: Signature::new(0, 0),
            call_span: 0,
            pc: 0,
            spans: Vec::new(),
        })
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        self.scope.call.push(frame);
        let mut formatted_instr = String::new();
        loop {
            let frame = self.scope.call.last().unwrap();
            let Some(instr) = self.instrs.get(frame.slice.address + frame.pc) else {
                break;
            };
            // Uncomment to debug
            // for val in &self.stack {
            //     print!("{:?} ", val);
            // }
            // println!();
            // if !self.scope.array.is_empty() {
            //     print!("array: ");
            //     for val in &self.scope.array {
            //         print!("{:?} ", val);
            //     }
            //     println!();
            // }
            // if !self.function_stack.is_empty() {
            //     println!("{} function(s)", self.function_stack.len());
            // }
            // for temp in enum_iterator::all::<TempStack>() {
            //     if !self.temp_stacks[temp as usize].is_empty() {
            //         print!("{temp}: ");
            //         for val in &self.temp_stacks[temp as usize] {
            //             print!("{:?} ", val);
            //         }
            //         println!();
            //     }
            // }
            // println!("  {:?}", instr);

            if self.time_instrs {
                formatted_instr = format!("{instr:?}");
                self.last_time = instant::now();
            }
            let res = match instr {
                &Instr::Prim(prim, span) => {
                    self.with_prim_span(span, Some(prim), |env| prim.run(env))
                }
                &Instr::ImplPrim(prim, span) => self.with_span(span, |env| prim.run(env)),
                Instr::Push(val) => {
                    self.stack.push(Value::clone(val));
                    Ok(())
                }
                Instr::BeginArray => {
                    self.scope.array.push(self.stack.len());
                    Ok(())
                }
                &Instr::EndArray { span, boxed } => self.with_span(span, |env| {
                    let start = env.scope.array.pop().unwrap();
                    let values = env.stack.drain(start..).rev();
                    let values: Vec<Value> = if boxed {
                        values.map(Boxed).map(Value::from).collect()
                    } else {
                        values.collect()
                    };
                    let val = if values.is_empty() && boxed {
                        Array::<Boxed>::default().into()
                    } else {
                        let elems: usize = values.iter().map(Value::element_count).sum();
                        let elem_size = values.get(0).map_or(size_of::<f64>(), Value::elem_size);
                        let max_mega = if cfg!(target_arch = "wasm32") {
                            256
                        } else {
                            2048
                        };
                        if elems * elem_size > max_mega * 1024usize.pow(2) {
                            return Err(
                                env.error(format!("Array of {elems} elements would be too large",))
                            );
                        }
                        Value::from_row_values(values, env)?
                    };
                    env.push(val);
                    Ok(())
                }),
                &Instr::Call(span) => self
                    .pop_function()
                    .and_then(|f| self.call_with_span(f, span)),
                Instr::PushFunc(f) => {
                    self.function_stack.push(f.clone());
                    Ok(())
                }
                &Instr::Switch { count, sig, span } => {
                    self.with_span(span, |env| algorithm::switch(count, sig, env))
                }
                &Instr::PushTempFunctions(n) => (|| {
                    for _ in 0..n {
                        let f = self.pop_function()?;
                        self.temp_function_stack.push(f);
                    }
                    Ok(())
                })(),
                &Instr::PopTempFunctions(n) => {
                    self.temp_function_stack
                        .truncate(self.temp_function_stack.len() - n);
                    Ok(())
                }
                &Instr::GetTempFunction { offset, sig, span } => self.with_span(span, |env| {
                    let f = env
                        .temp_function_stack
                        .get(env.temp_function_stack.len() - 1 - offset)
                        .ok_or_else(|| {
                            env.error(
                                "Error getting placeholder function. \
                                This is a bug in the interpreter.",
                            )
                        })?;
                    let f_sig = f.signature();
                    if f_sig != sig {
                        return Err(env.error(format!(
                            "Function signature {f_sig} does not match \
                            placeholder signature {sig}"
                        )));
                    }
                    env.function_stack.push(f.clone());
                    Ok(())
                }),
                Instr::Dynamic(df) => df.f.clone()(self),
                &Instr::Unpack { count, span, unbox } => self.with_span(span, |env| {
                    let arr = env.pop(1)?;
                    if arr.row_count() != count {
                        return Err(env.error(format!(
                            "This °[] expects an array with {} rows, \
                            but the array has {}",
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
                &Instr::TouchStack { count, span } => self.with_span(span, |env| {
                    if env.stack.len() < count {
                        return Err(env.error(format!(
                            "Stack was empty evaluating argument {}",
                            count - env.stack.len()
                        )));
                    }
                    env.touch_array_stack(count);
                    Ok(())
                }),
                &Instr::PushTemp { stack, count, span } => self.with_span(span, |env| {
                    for i in 0..count {
                        let value = env.pop(i + 1)?;
                        env.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                &Instr::PopTemp { stack, count, span } => self.with_span(span, |env| {
                    for _ in 0..count {
                        let value = env.temp_stacks[stack as usize]
                            .pop()
                            .ok_or_else(|| env.error("Stack was empty when getting saved value"))?;
                        env.push(value);
                    }

                    Ok(())
                }),
                &Instr::CopyToTemp { stack, count, span } => self.with_span(span, |env| {
                    if env.stack.len() < count {
                        return Err(env.error(format!(
                            "Stack was empty evaluating argument {}",
                            count - env.stack.len()
                        )));
                    }
                    for i in 0..count {
                        let value = env.stack[env.stack.len() - i - 1].clone();
                        env.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                &Instr::CopyFromTemp {
                    stack,
                    offset,
                    count,
                    span,
                } => self.with_span(span, |env| {
                    if env.temp_stacks[stack as usize].len() < offset + count {
                        return Err(env.error("Stack was empty when copying saved value"));
                    }
                    let start = env.temp_stacks[stack as usize].len() - offset;
                    for i in 0..count {
                        let value = env.temp_stacks[stack as usize][start - i - 1].clone();
                        env.push(value);
                    }
                    Ok(())
                }),
                &Instr::DropTemp { stack, count, span } => self.with_span(span, |env| {
                    let stack = &mut env.temp_stacks[stack as usize];
                    if stack.len() < count {
                        return Err(env.error("Stack was empty when dropping saved value"));
                    }
                    stack.truncate(stack.len() - count);
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
            };
            if self.time_instrs {
                let end_time = instant::now();
                let padding = self.scope.call.len().saturating_sub(1) * 2;
                println!(
                    "  ⏲{:padding$}{:.2}ms - {}",
                    "",
                    end_time - self.last_time,
                    formatted_instr
                );
                self.last_time = instant::now();
            }
            if let Err(err) = res {
                // Trace errors
                let frame = self.scope.call.pop().unwrap();
                return Err(self.trace_error(err, frame));
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
    pub(crate) fn with_span<T>(&mut self, span: usize, f: impl FnOnce(&mut Self) -> T) -> T {
        self.with_prim_span(span, None, f)
    }
    fn with_prim_span<T>(
        &mut self,
        span: usize,
        prim: Option<Primitive>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        self.scope.call.last_mut().unwrap().spans.push((span, prim));
        let res = f(self);
        self.scope.call.last_mut().unwrap().spans.pop();
        res
    }
    /// Call a function
    #[inline]
    pub fn call(&mut self, f: Function) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_span(f, call_span)
    }
    #[inline]
    fn call_frame(&mut self, frame: StackFrame) -> UiuaResult {
        let call_span = self.span_index();
        self.call_with_frame_span(frame, call_span)
    }
    #[inline]
    pub(crate) fn call_restore(&mut self, f: Function) -> UiuaResult {
        let f_args = f.signature().args;
        let bottom = self.stack_height().saturating_sub(f_args);
        let res = self.call(f);
        if res.is_err() {
            self.truncate_stack(bottom);
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
                call_span,
                spans: Vec::new(),
                pc: 0,
            },
            call_span,
        )
    }
    #[inline]
    fn call_with_frame_span(&mut self, frame: StackFrame, call_span: usize) -> UiuaResult {
        let start_height = self.stack.len();
        let sig = frame.sig;
        let slice = frame.slice;
        self.exec(frame)?;
        let height_diff = self.stack.len() as isize - start_height as isize;
        let sig_diff = sig.outputs as isize - sig.args as isize;
        if height_diff != sig_diff
            && !self
                .instrs(slice)
                .iter()
                .any(|instr| matches!(instr, Instr::Prim(Primitive::Sys(SysOp::Import), _)))
        {
            return Err(self.spans.lock()[call_span]
                .clone()
                .sp(format!(
                    "Function modified the stack by {height_diff} values, but its \
                    signature of {sig} implies a change of {sig_diff}"
                ))
                .into());
        }
        Ok(())
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
        self.get_span(self.span_index())
    }
    /// Get a span by its index
    pub fn get_span(&self, span: usize) -> Span {
        self.spans.lock()[span].clone()
    }
    /// Register a span
    pub fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let mut spans = self.spans.lock();
        let idx = spans.len();
        spans.push(span.into());
        idx
    }
    /// Construct an error with the current span
    pub fn error(&self, message: impl ToString) -> UiuaError {
        UiuaError::Run(self.span().clone().sp(message.to_string()))
    }
    /// Construct and add a diagnostic with the current span
    pub fn diagnostic(&mut self, message: impl Into<String>, kind: DiagnosticKind) {
        self.diagnostic_with_span(message, kind, self.span());
    }
    /// Construct and add a diagnostic with a custom span
    pub fn diagnostic_with_span(
        &mut self,
        message: impl Into<String>,
        kind: DiagnosticKind,
        span: impl Into<Span>,
    ) {
        self.diagnostics
            .insert(Diagnostic::new(message.into(), span, kind));
    }
    /// Pop a value from the stack
    pub fn pop(&mut self, arg: impl StackArg) -> UiuaResult<Value> {
        let res = match self.stack.pop() {
            Some(mut val) => {
                if self.unpack_boxes() {
                    val.unpack();
                }
                Ok(val)
            }
            None => Err(self.error(format!(
                "Stack was empty when evaluating {}",
                arg.arg_name()
            ))),
        };
        for bottom in &mut self.scope.array {
            *bottom = (*bottom).min(self.stack.len());
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
    /// Simulates popping a value and imediately pushing it back
    pub(crate) fn touch_array_stack(&mut self, n: usize) {
        for bottom in &mut self.scope.array {
            *bottom = (*bottom).min(self.stack.len().saturating_sub(n));
        }
    }
    /// Push a value onto the stack
    pub fn push(&mut self, val: impl Into<Value>) {
        self.stack.push(val.into());
    }
    /// Push a function onto the function stack
    pub fn push_func(&mut self, f: Function) {
        self.function_stack.push(f.into());
    }
    /// Create a function
    pub fn create_function(
        &mut self,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static,
    ) -> Function {
        let signature = signature.into();
        self.new_function(
            FunctionId::Unnamed,
            signature,
            vec![Instr::Dynamic(DynamicFunction {
                id: SmallRng::seed_from_u64(instant::now().to_bits()).gen(),
                f: Arc::new(f),
                signature,
            })],
        )
    }
    pub fn instrs(&self, slice: FuncSlice) -> &[Instr] {
        &self.instrs[slice.address..][..slice.len]
    }
    /// Bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error in the binding name is not valid
    pub fn bind_function(&mut self, name: impl Into<Arc<str>>, function: Function) -> UiuaResult {
        self.compile_bind_function(name.into(), function.into(), true, Span::Builtin)
    }
    /// Create and bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error in the binding name is not valid
    pub fn create_bind_function(
        &mut self,
        name: impl Into<Arc<str>>,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static,
    ) -> UiuaResult {
        let function = self.create_function(signature, f);
        self.bind_function(name, function)
    }
    /// Take the entire stack
    pub fn take_stack(&mut self) -> Vec<Value> {
        for stack in &mut self.temp_stacks {
            stack.clear();
        }
        self.temp_function_stack.clear();
        self.function_stack.clear();
        take(&mut self.stack)
    }
    /// Pop a function from the function stack
    pub fn pop_function(&mut self) -> UiuaResult<Function> {
        self.function_stack.pop().ok_or_else(|| {
            self.error(
                "Function stack was empty when popping. \
                This is a bug in the interpreter.",
            )
        })
    }
    /// Get the values for all bindings in the current scope
    pub fn all_values_is_scope(&self) -> HashMap<Ident, Value> {
        let mut bindings = HashMap::new();
        let globals = self.globals.lock();
        for (name, idx) in &self.scope.names {
            if !constants().iter().any(|c| c.name == name.as_ref()) {
                if let Global::Val(val) = &globals[*idx] {
                    bindings.insert(name.clone(), val.clone());
                }
            }
        }
        bindings
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
    /// Clone `n` values from the top of the stack
    pub fn clone_stack_top(&self, n: usize) -> Vec<Value> {
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
        self.stack.len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) {
        self.stack.truncate(size);
    }
    pub(crate) fn num_fill(&self) -> Result<f64, &'static str> {
        match self.scope.fills.last() {
            Some(Fill::Num(n)) => Ok(*n),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn byte_fill(&self) -> Result<u8, &'static str> {
        match self.scope.fills.last() {
            Some(Fill::Num(n)) if (n.fract() == 0.0 && (0.0..=255.0).contains(n)) => Ok(*n as u8),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn char_fill(&self) -> Result<char, &'static str> {
        match self.scope.fills.last() {
            Some(Fill::Char(c)) => Ok(*c),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn box_fill(&self) -> Result<Boxed, &'static str> {
        match self.scope.fills.last().cloned() {
            Some(Fill::Num(n)) => Ok(Value::from(n).into()),
            Some(Fill::Char(c)) => Ok(Value::from(c).into()),
            Some(Fill::Complex(c)) => Ok(Value::from(c).into()),
            Some(Fill::Box(b)) => Ok(b),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn complex_fill(&self) -> Result<Complex, &'static str> {
        match self.scope.fills.last() {
            Some(Fill::Num(n)) => Ok(Complex::new(*n, 0.0)),
            Some(Fill::Complex(c)) => Ok(*c),
            _ => Err(self.fill_error()),
        }
    }
    fn fill_error(&self) -> &'static str {
        match self.scope.fills.last() {
            Some(Fill::Num(_)) => ". A number fill is set, but the array is not numbers.",
            Some(Fill::Char(_)) => ". A character fill is set, but the array is not characters.",
            Some(Fill::Complex(_)) => {
                ". A complex fill is set, but the array is not complex numbers."
            }
            Some(Fill::Box(_)) => ". A box fill is set, but the array is not boxed values.",
            _ => "",
        }
    }
    /// Do something with the fill context set
    pub(crate) fn with_fill(
        &mut self,
        fill: Value,
        in_ctx: impl FnOnce(&mut Self) -> UiuaResult,
    ) -> UiuaResult {
        if fill.shape() == [0] {
            self.scope.fills.push(Fill::None)
        } else {
            if !fill.shape().is_empty() {
                return Err(self.error(format!(
                    "Fill values must be scalar or an empty list, but its shape is {}",
                    fill.format_shape()
                )));
            }
            self.scope.fills.push(match fill {
                Value::Num(n) => Fill::Num(n.data.into_iter().next().unwrap()),
                #[cfg(feature = "bytes")]
                Value::Byte(b) => Fill::Num(b.data.into_iter().next().unwrap() as f64),
                Value::Char(c) => Fill::Char(c.data.into_iter().next().unwrap()),
                Value::Box(b) => Fill::Box(b.data.into_iter().next().unwrap()),
                Value::Complex(c) => Fill::Complex(c.data.into_iter().next().unwrap()),
            });
        }
        let res = in_ctx(self);
        self.scope.fills.pop();
        res
    }
    pub(crate) fn with_pack(&mut self, in_ctx: impl FnOnce(&mut Self) -> UiuaResult) -> UiuaResult {
        let upper = replace(&mut self.scope.unpack_boxes, true);
        let res = in_ctx(self);
        self.scope.unpack_boxes = upper;
        res
    }
    pub(crate) fn unpack_boxes(&self) -> bool {
        self.scope.unpack_boxes
    }
    pub(crate) fn call_frames(&self) -> impl DoubleEndedIterator<Item = &StackFrame> {
        self.scope.call.iter()
    }
    pub(crate) fn call_with_this(&mut self, f: Function) -> UiuaResult {
        let call_height = self.scope.call.len();
        let with_height = self.scope.this.len();
        self.scope.this.push(self.scope.call.len());
        let res = self.call(f.into());
        self.scope.call.truncate(call_height);
        self.scope.this.truncate(with_height);
        res
    }
    pub(crate) fn recur(&mut self) -> UiuaResult {
        let Some(i) = self.scope.this.last().copied() else {
            return Err(self.error("No recursion context set"));
        };
        let frame = self.scope.call[i].clone();
        self.call_frame(frame)
    }
    /// Spawn a thread
    pub(crate) fn spawn(
        &mut self,
        capture_count: usize,
        f: impl FnOnce(&mut Self) -> UiuaResult + Send + 'static,
    ) -> UiuaResult {
        if self.stack.len() < capture_count {
            return Err(self.error(format!(
                "Expected at least {} value(s) on the stack, but there are {}",
                capture_count,
                self.stack.len()
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
            instrs: EcoVec::new(),
            new_functions: Vec::new(),
            globals: self.globals.clone(),
            spans: self.spans.clone(),
            stack: self
                .stack
                .drain(self.stack.len() - capture_count..)
                .collect(),
            function_stack: Vec::new(),
            temp_stacks: [Vec::new(), Vec::new()],
            temp_function_stack: Vec::new(),
            scope: self.scope.clone(),
            higher_scopes: self.higher_scopes.last().cloned().into_iter().collect(),
            mode: self.mode,
            current_imports: self.current_imports.clone(),
            imports: self.imports.clone(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: self.print_diagnostics,
            time_instrs: self.time_instrs,
            last_time: self.last_time,
            cli_arguments: self.cli_arguments.clone(),
            cli_file_path: self.cli_file_path.clone(),
            backend: self.backend.clone(),
            execution_limit: self.execution_limit,
            execution_start: self.execution_start,
            thread,
        };
        #[cfg(not(target_arch = "wasm32"))]
        let handle = std::thread::Builder::new()
            .spawn(move || {
                f(&mut env)?;
                Ok(env.take_stack())
            })
            .map_err(|e| self.error(format!("Error spawning thread: {e}")))?;
        #[cfg(target_arch = "wasm32")]
        let result = f(&mut env).map(|_| env.take_stack());

        let id = self.thread.next_child_id;
        self.thread.next_child_id += 1;
        self.thread.children.insert(
            id,
            Thread {
                #[cfg(not(target_arch = "wasm32"))]
                handle: handle.into(),
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
            let handle = ids.data.into_iter().next().unwrap();
            #[cfg(not(target_arch = "wasm32"))]
            let thread_stack = Arc::into_inner(
                self.thread
                    .children
                    .remove(&handle)
                    .ok_or_else(|| self.error("Invalid thread id"))?
                    .handle,
            )
            .ok_or_else(|| self.error("Cannot wait on thread spawned in cloned environment"))?
            .join()
            .unwrap()?;
            #[cfg(target_arch = "wasm32")]
            let thread_stack = self
                .thread
                .children
                .remove(&handle)
                .ok_or_else(|| self.error("Invalid thread id"))?
                .result?;
            self.stack.extend(thread_stack);
        } else {
            let mut rows = Vec::new();
            for handle in ids.data {
                #[cfg(not(target_arch = "wasm32"))]
                let thread_stack = Arc::into_inner(
                    self.thread
                        .children
                        .remove(&handle)
                        .ok_or_else(|| self.error("Invalid thread id"))?
                        .handle,
                )
                .ok_or_else(|| self.error("Cannot wait on thread spawned in cloned environment"))?
                .join()
                .unwrap()?;
                #[cfg(target_arch = "wasm32")]
                let thread_stack = self
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
        let ids = id.as_natural_array(self, "Thread id must be an array of natural numbers")?;
        let mut values = Vec::with_capacity(ids.data.len());
        for id in ids.data {
            values.push(
                self.channel(id)?
                    .recv
                    .recv()
                    .map_err(|_| self.error("Thread channel closed"))?,
            );
        }
        let mut val = Value::from_row_values(values, self)?;
        let mut shape = ids.shape;
        shape.extend_from_slice(&val.shape()[1..]);
        *val.shape_mut() = shape;
        self.push(val);
        Ok(())
    }
    pub(crate) fn try_recv(&mut self, id: Value) -> UiuaResult {
        let id = id.as_nat(self, "Thread id must be a natural number")?;
        let value = match self.channel(id)?.recv.try_recv() {
            Ok(value) => value,
            Err(TryRecvError::Empty) => return Err(self.error("No value available")),
            Err(_) => return Err(self.error("Thread channel closed")),
        };
        self.push(value);
        Ok(())
    }
    fn channel(&self, id: usize) -> UiuaResult<&Channel> {
        Ok(if id == 0 {
            self.thread
                .parent
                .as_ref()
                .ok_or_else(|| self.error("Thread has no parent"))?
        } else {
            &self
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

impl<F, T> StackArg for F
where
    F: FnOnce() -> T,
    T: StackArg,
{
    fn arg_name(self) -> String {
        self().arg_name()
    }
}
