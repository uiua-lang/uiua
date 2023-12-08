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
    algorithm, array::Array, boxed::Boxed, check::SigCheckError, constants, example_ua,
    function::*, lex::Span, parse::parse, value::Value, Assembly, Complex, Diagnostic,
    DiagnosticKind, Ident, NativeSys, Primitive, Sp, SysBackend, SysOp, TraceFrame, UiuaError,
    UiuaResult,
};

/// The Uiua interpreter
#[derive(Clone)]
pub struct Uiua {
    pub(crate) rt: Runtime,
    pub(crate) ct: CompileTime,
    pub(crate) asm: Assembly,
    /// The paths of files currently being imported (used to detect import cycles)
    current_imports: Arc<Mutex<Vec<PathBuf>>>,
    /// The bindings of imported files
    imports: Arc<Mutex<HashMap<PathBuf, HashMap<Ident, usize>>>>,
    /// Accumulated diagnostics
    pub(crate) diagnostics: BTreeSet<Diagnostic>,
    /// Print diagnostics as they are encountered
    pub(crate) print_diagnostics: bool,
}

/// Compile-time only data
#[derive(Clone)]
pub(crate) struct CompileTime {
    /// Functions which are under construction
    pub(crate) new_functions: Vec<EcoVec<Instr>>,
    pub(crate) next_global: usize,
    /// The current scope
    pub(crate) scope: CtScope,
    /// Ancestor scopes of the current one
    pub(crate) higher_scopes: Vec<CtScope>,
    /// Determines which How test scopes are run
    pub(crate) mode: RunMode,
}

#[derive(Clone)]
pub(crate) struct CtScope {
    /// Map local names to global indices
    pub names: HashMap<Ident, usize>,
    /// Whether to allow experimental features
    pub experimental: bool,
    /// The stack height between top-level statements
    pub stack_height: Result<usize, Sp<SigCheckError>>,
}

impl Default for CtScope {
    fn default() -> Self {
        Self {
            names: HashMap::new(),
            experimental: false,
            stack_height: Ok(0),
        }
    }
}

/// Runtime-only data
#[derive(Clone)]
pub(crate) struct Runtime {
    /// The thread's stack
    pub(crate) stack: Vec<Value>,
    /// The thread's function stack
    pub(crate) function_stack: Vec<Function>,
    last_slice_run: usize,
    /// The thread's temp stack for inlining
    temp_stacks: [Vec<Value>; TempStack::CARDINALITY],
    /// The thread's temp stack for functions
    temp_function_stack: Vec<Function>,
    /// The stack height at the start of each array currently being built
    array_stack: Vec<usize>,
    /// The call stack
    call_stack: Vec<StackFrame>,
    /// The recur stack
    this_stack: Vec<usize>,
    /// The shape fix stack
    fill_stack: Vec<Fill>,
    /// Whether to unpack boxed values
    pub unpack_boxes: bool,
    /// A limit on the execution duration in milliseconds
    execution_limit: Option<f64>,
    /// The time at which execution started
    execution_start: f64,
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
    Func(Function),
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

impl Runtime {
    fn with_native_sys() -> Self {
        Runtime {
            last_slice_run: 0,
            stack: Vec::new(),
            function_stack: Vec::new(),
            temp_stacks: [Vec::new(), Vec::new()],
            temp_function_stack: Vec::new(),
            array_stack: Vec::new(),
            call_stack: vec![StackFrame {
                slice: FuncSlice::default(),
                id: FunctionId::Main,
                sig: Signature::new(0, 0),
                call_span: 0,
                pc: 0,
                spans: Vec::new(),
            }],
            this_stack: Vec::new(),
            fill_stack: Vec::new(),
            unpack_boxes: false,
            backend: Arc::new(NativeSys),
            time_instrs: false,
            last_time: 0.0,
            cli_arguments: Vec::new(),
            cli_file_path: PathBuf::new(),
            execution_limit: None,
            execution_start: 0.0,
            thread: ThisThread::default(),
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::with_native_sys()
    }
}

/// A handle to a compiled chunk of Uiua bytecode
#[must_use = "Chunks must be run for their code to be executed"]
pub struct Chunk<'a> {
    env: &'a mut Uiua,
    start: usize,
    len: usize,
}

impl<'a> Chunk<'a> {
    /// Run the chunk
    pub fn run(self) -> UiuaResult {
        let slices = take(&mut self.env.asm.top_slices);
        let mut res = Ok(());
        for &slice in &slices[self.start..][..self.len] {
            res = self.env.call_slice(slice);
            if res.is_err() {
                self.env.rt = Runtime {
                    backend: self.env.rt.backend.clone(),
                    execution_limit: self.env.rt.execution_limit,
                    time_instrs: self.env.rt.time_instrs,
                    ..Runtime::default()
                };
                break;
            }
        }
        self.env.asm.top_slices = slices;
        res
    }
}

impl Uiua {
    /// Create a new Uiua runtime with the standard IO backend
    pub fn with_native_sys() -> Self {
        let mut scope = CtScope::default();
        let mut asm = Assembly::default();
        for def in constants() {
            scope.names.insert(def.name.into(), asm.globals.len());
            asm.globals.push(Global::Val(def.value.clone()));
        }
        let next_global = asm.globals.len();
        Uiua {
            asm,
            current_imports: Arc::new(Mutex::new(Vec::new())),
            imports: Arc::new(Mutex::new(HashMap::new())),
            diagnostics: BTreeSet::new(),
            print_diagnostics: false,
            ct: CompileTime {
                scope,
                higher_scopes: Vec::new(),
                next_global,
                new_functions: Vec::new(),
                mode: RunMode::Normal,
            },
            rt: Runtime::default(),
        }
    }
    /// Create a new Uiua runtime with a custom IO backend
    pub fn with_backend(backend: impl SysBackend) -> Self {
        Uiua {
            rt: Runtime {
                backend: Arc::new(backend),
                ..Runtime::default()
            },
            ..Default::default()
        }
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
        self.rt.time_instrs = time_instrs;
        self
    }
    /// Limit the execution duration
    pub fn with_execution_limit(mut self, limit: Duration) -> Self {
        self.rt.execution_limit = Some(limit.as_millis() as f64);
        self
    }
    /// Set the [`RunMode`]
    ///
    /// Default is [`RunMode::Normal`]
    pub fn with_mode(mut self, mode: RunMode) -> Self {
        self.ct.mode = mode;
        self
    }
    /// Get the [`RunMode`]
    pub fn mode(&self) -> RunMode {
        self.ct.mode
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
    /// Load a Uiua file from a path
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult<Chunk> {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e.into()))?;
        self.load_impl(&input, Some(path))
    }
    /// Load a Uiua file from a string
    pub fn load_str<'a>(&'a mut self, input: &str) -> UiuaResult<Chunk<'a>> {
        self.load_impl(input, None)
    }
    /// Load a Uiua file from a string with a path for error reporting
    pub fn load_str_path<'a, P: AsRef<Path>>(
        &'a mut self,
        input: &str,
        path: P,
    ) -> UiuaResult<Chunk<'a>> {
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
        self.ct.higher_scopes.push(take(&mut self.ct.scope));
        let start_height = self.rt.stack.len();
        let res = f(self);
        let scope = replace(&mut self.ct.scope, self.ct.higher_scopes.pop().unwrap());
        res?;
        let mut names = HashMap::new();
        for (name, idx) in scope.names {
            if idx >= constants().len() {
                names.insert(name, idx);
            }
        }
        self.rt.stack.truncate(start_height);
        Ok(names)
    }
    fn load_impl<'a>(&'a mut self, input: &str, path: Option<&Path>) -> UiuaResult<Chunk<'a>> {
        self.rt.execution_start = instant::now();
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
        res?;
        let start = self.rt.last_slice_run;
        let len = self.asm.top_slices.len() - start;
        Ok(Chunk {
            env: self,
            start,
            len,
        })
    }
    fn trace_error(&self, mut error: UiuaError, frame: StackFrame) -> UiuaError {
        let mut frames = Vec::new();
        for (span, prim) in &frame.spans {
            if let Some(prim) = prim {
                frames.push(TraceFrame {
                    id: FunctionId::Primitive(*prim),
                    span: self.asm.spans[*span].clone(),
                });
            }
        }
        frames.push(TraceFrame {
            id: frame.id.clone(),
            span: self.asm.spans[frame.call_span].clone(),
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
            let import =
                self.in_scope(|env| env.load_str_path(input, path).and_then(Chunk::run))?;
            self.imports.lock().insert(path.into(), import);
        }
        let imports_gaurd = self.imports.lock();
        let imports = &imports_gaurd[path];
        let idx = imports.get(item).ok_or_else(|| {
            self.error(format!("Item `{}` not found in {}", item, path.display()))
        })?;
        let global = self.asm.globals[*idx].clone();
        drop(imports_gaurd);
        match global {
            Global::Val(val) => self.push(val),
            Global::Func(f) => self.rt.function_stack.push(f),
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
    pub(crate) fn load_import_input(&self, path: &Path) -> UiuaResult<Arc<str>> {
        if let Some(input) = self.asm.import_inputs.get(path) {
            return Ok(input.clone());
        }
        String::from_utf8(
            self.rt
                .backend
                .file_read_all(path)
                .or_else(|e| {
                    if path.ends_with(Path::new("example.ua")) {
                        Ok(example_ua(|ex| ex.as_bytes().to_vec()))
                    } else {
                        Err(e)
                    }
                })
                .map_err(|e| self.error(e))?,
        )
        .map_err(|e| self.error(format!("Failed to read file: {e}")))
        .map(Into::into)
    }
    fn exec(&mut self, frame: StackFrame) -> UiuaResult {
        self.rt.call_stack.push(frame);
        let mut formatted_instr = String::new();
        loop {
            let frame = self.rt.call_stack.last().unwrap();
            let Some(instr) =
                self.asm.instrs[frame.slice.address..][..frame.slice.len].get(frame.pc)
            else {
                self.rt.call_stack.pop().unwrap();
                break;
            };
            // Uncomment to debug
            // for val in &self.rt.stack {
            //     print!("{:?} ", val);
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
            // println!("  {:?}", instr);

            if self.rt.time_instrs {
                formatted_instr = format!("{instr:?}");
                self.rt.last_time = instant::now();
            }
            let res = match instr {
                Instr::Comment(_) => Ok(()),
                &Instr::Prim(prim, span) => {
                    self.with_prim_span(span, Some(prim), |env| prim.run(env))
                }
                &Instr::ImplPrim(prim, span) => self.with_span(span, |env| prim.run(env)),
                Instr::Push(val) => {
                    self.rt.stack.push(Value::clone(val));
                    Ok(())
                }
                &Instr::CallGlobal { index, call } => (|| {
                    let global = self.asm.globals[index].clone();
                    match global {
                        Global::Val(val) => self.rt.stack.push(val),
                        Global::Func(f) if call => self.call(f)?,
                        Global::Func(f) => self.rt.function_stack.push(f),
                    }
                    Ok(())
                })(),
                &Instr::BindGlobal {
                    ref name,
                    span,
                    index,
                } => {
                    let name = name.clone();
                    (|| {
                        if let Some(f) = self.rt.function_stack.pop() {
                            // Binding is an imported function
                            self.compile_bind_function(&name, index, f, span)?;
                        } else if let Some(value) = self.rt.stack.pop() {
                            // Binding is a constant
                            self.bind_value(&name, index, value, span)?;
                        } else {
                            // Binding is an empty function
                            let id = match self.get_span(span) {
                                Span::Code(span) => FunctionId::Anonymous(span),
                                Span::Builtin => FunctionId::Unnamed,
                            };
                            let func = self.add_function(id, Signature::new(0, 0), Vec::new());
                            self.compile_bind_function(&name, index, func, span)?;
                        }
                        Ok(())
                    })()
                }
                Instr::BeginArray => {
                    self.rt.array_stack.push(self.rt.stack.len());
                    Ok(())
                }
                &Instr::EndArray { span, boxed } => self.with_span(span, |env| {
                    let start = env.rt.array_stack.pop().unwrap();
                    let values = env.rt.stack.drain(start..).rev();
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
                    self.rt.function_stack.push(f.clone());
                    Ok(())
                }
                &Instr::Switch { count, sig, span } => {
                    self.with_span(span, |env| algorithm::switch(count, sig, env))
                }
                &Instr::PushTempFunctions(n) => (|| {
                    for _ in 0..n {
                        let f = self.pop_function()?;
                        self.rt.temp_function_stack.push(f);
                    }
                    Ok(())
                })(),
                &Instr::PopTempFunctions(n) => {
                    self.rt
                        .temp_function_stack
                        .truncate(self.rt.temp_function_stack.len() - n);
                    Ok(())
                }
                &Instr::GetTempFunction { offset, sig, span } => self.with_span(span, |env| {
                    let f = env
                        .rt
                        .temp_function_stack
                        .get(env.rt.temp_function_stack.len() - 1 - offset)
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
                    env.rt.function_stack.push(f.clone());
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
                    if env.rt.stack.len() < count {
                        return Err(env.error(format!(
                            "Stack was empty evaluating argument {}",
                            count - env.rt.stack.len()
                        )));
                    }
                    env.touch_array_stack(count);
                    Ok(())
                }),
                &Instr::PushTemp { stack, count, span } => self.with_span(span, |env| {
                    for i in 0..count {
                        let value = env.pop(i + 1)?;
                        env.rt.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                &Instr::PopTemp { stack, count, span } => self.with_span(span, |env| {
                    for _ in 0..count {
                        let value = env.rt.temp_stacks[stack as usize]
                            .pop()
                            .ok_or_else(|| env.error("Stack was empty when getting saved value"))?;
                        env.push(value);
                    }

                    Ok(())
                }),
                &Instr::CopyToTemp { stack, count, span } => self.with_span(span, |env| {
                    if env.rt.stack.len() < count {
                        return Err(env.error(format!(
                            "Stack was empty evaluating argument {}",
                            count - env.rt.stack.len()
                        )));
                    }
                    for i in 0..count {
                        let value = env.rt.stack[env.rt.stack.len() - i - 1].clone();
                        env.rt.temp_stacks[stack as usize].push(value);
                    }
                    Ok(())
                }),
                &Instr::CopyFromTemp {
                    stack,
                    offset,
                    count,
                    span,
                } => self.with_span(span, |env| {
                    if env.rt.temp_stacks[stack as usize].len() < offset + count {
                        return Err(env.error("Stack was empty when copying saved value"));
                    }
                    let start = env.rt.temp_stacks[stack as usize].len() - offset;
                    for i in 0..count {
                        let value = env.rt.temp_stacks[stack as usize][start - i - 1].clone();
                        env.push(value);
                    }
                    Ok(())
                }),
                &Instr::DropTemp { stack, count, span } => self.with_span(span, |env| {
                    let stack = &mut env.rt.temp_stacks[stack as usize];
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
            if self.rt.time_instrs {
                let end_time = instant::now();
                let padding = self.rt.call_stack.len().saturating_sub(1) * 2;
                println!(
                    "  ⏲{:padding$}{:.2}ms - {}",
                    "",
                    end_time - self.rt.last_time,
                    formatted_instr
                );
                self.rt.last_time = instant::now();
            }
            if let Err(err) = res {
                // Trace errors
                let frame = self.rt.call_stack.pop().unwrap();
                return Err(self.trace_error(err, frame));
            } else {
                // Go to next instruction
                self.rt.call_stack.last_mut().unwrap().pc += 1;
                if let Some(limit) = self.rt.execution_limit {
                    if instant::now() - self.rt.execution_start > limit {
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
        let start_height = self.rt.stack.len();
        let sig = frame.sig;
        let slice = frame.slice;
        self.exec(frame)?;
        let height_diff = self.rt.stack.len() as isize - start_height as isize;
        let sig_diff = sig.outputs as isize - sig.args as isize;
        if height_diff != sig_diff
            && !self
                .instrs(slice)
                .iter()
                .any(|instr| matches!(instr, Instr::Prim(Primitive::Sys(SysOp::Import), _)))
        {
            return Err(self.asm.spans[call_span]
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
    pub fn add_span(&mut self, span: impl Into<Span>) -> usize {
        let idx = self.asm.spans.len();
        self.asm.spans.push(span.into());
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
        let res = match self.rt.stack.pop() {
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
    /// Simulates popping a value and imediately pushing it back
    pub(crate) fn touch_array_stack(&mut self, n: usize) {
        for bottom in &mut self.rt.array_stack {
            *bottom = (*bottom).min(self.rt.stack.len().saturating_sub(n));
        }
    }
    /// Push a value onto the stack
    pub fn push(&mut self, val: impl Into<Value>) {
        self.rt.stack.push(val.into());
    }
    /// Push a function onto the function stack
    pub fn push_func(&mut self, f: Function) {
        self.rt.function_stack.push(f);
    }
    /// Create a function
    pub fn create_function(
        &mut self,
        signature: impl Into<Signature>,
        f: impl Fn(&mut Uiua) -> UiuaResult + Send + Sync + 'static,
    ) -> Function {
        let signature = signature.into();
        self.add_function(
            FunctionId::Unnamed,
            signature,
            vec![Instr::Dynamic(DynamicFunction {
                id: SmallRng::seed_from_u64(instant::now().to_bits()).gen(),
                f: Arc::new(f),
                signature,
            })],
        )
    }
    /// Get a slice of instructions
    pub fn instrs(&self, slice: FuncSlice) -> &[Instr] {
        &self.asm.instrs[slice.address..][..slice.len]
    }
    /// Bind a function in the current scope
    ///
    /// # Errors
    /// Returns an error in the binding name is not valid
    pub fn bind_function(&mut self, name: impl Into<Arc<str>>, function: Function) -> UiuaResult {
        let index = self.ct.next_global;
        let name = name.into();
        self.compile_bind_function(&name, index, function, 0)?;
        self.ct.next_global += 1;
        self.ct.scope.names.insert(name, index);
        Ok(())
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
        for stack in &mut self.rt.temp_stacks {
            stack.clear();
        }
        self.rt.temp_function_stack.clear();
        self.rt.function_stack.clear();
        take(&mut self.rt.stack)
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
    /// Get the values for all bindings in the current scope
    pub fn all_values_in_scope(&self) -> HashMap<Ident, Value> {
        let mut bindings = HashMap::new();
        for (name, idx) in &self.ct.scope.names {
            if !constants().iter().any(|c| c.name == name.as_ref()) {
                if let Global::Val(val) = &self.asm.globals[*idx] {
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
        self.rt.stack.iter().rev().take(n).rev().cloned().collect()
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
        self.rt.stack.len()
    }
    pub(crate) fn truncate_stack(&mut self, size: usize) {
        self.rt.stack.truncate(size);
    }
    pub(crate) fn num_fill(&self) -> Result<f64, &'static str> {
        match self.rt.fill_stack.last() {
            Some(Fill::Num(n)) => Ok(*n),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn byte_fill(&self) -> Result<u8, &'static str> {
        match self.rt.fill_stack.last() {
            Some(Fill::Num(n)) if (n.fract() == 0.0 && (0.0..=255.0).contains(n)) => Ok(*n as u8),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn char_fill(&self) -> Result<char, &'static str> {
        match self.rt.fill_stack.last() {
            Some(Fill::Char(c)) => Ok(*c),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn box_fill(&self) -> Result<Boxed, &'static str> {
        match self.rt.fill_stack.last().cloned() {
            Some(Fill::Num(n)) => Ok(Value::from(n).into()),
            Some(Fill::Char(c)) => Ok(Value::from(c).into()),
            Some(Fill::Complex(c)) => Ok(Value::from(c).into()),
            Some(Fill::Box(b)) => Ok(b),
            _ => Err(self.fill_error()),
        }
    }
    pub(crate) fn complex_fill(&self) -> Result<Complex, &'static str> {
        match self.rt.fill_stack.last() {
            Some(Fill::Num(n)) => Ok(Complex::new(*n, 0.0)),
            Some(Fill::Complex(c)) => Ok(*c),
            _ => Err(self.fill_error()),
        }
    }
    fn fill_error(&self) -> &'static str {
        match self.rt.fill_stack.last() {
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
            self.rt.fill_stack.push(Fill::None)
        } else {
            if !fill.shape().is_empty() {
                return Err(self.error(format!(
                    "Fill values must be scalar or an empty list, but its shape is {}",
                    fill.format_shape()
                )));
            }
            self.rt.fill_stack.push(match fill {
                Value::Num(n) => Fill::Num(n.data.into_iter().next().unwrap()),
                #[cfg(feature = "bytes")]
                Value::Byte(b) => Fill::Num(b.data.into_iter().next().unwrap() as f64),
                Value::Char(c) => Fill::Char(c.data.into_iter().next().unwrap()),
                Value::Box(b) => Fill::Box(b.data.into_iter().next().unwrap()),
                Value::Complex(c) => Fill::Complex(c.data.into_iter().next().unwrap()),
            });
        }
        let res = in_ctx(self);
        self.rt.fill_stack.pop();
        res
    }
    pub(crate) fn with_pack(&mut self, in_ctx: impl FnOnce(&mut Self) -> UiuaResult) -> UiuaResult {
        let upper = replace(&mut self.rt.unpack_boxes, true);
        let res = in_ctx(self);
        self.rt.unpack_boxes = upper;
        res
    }
    pub(crate) fn unpack_boxes(&self) -> bool {
        self.rt.unpack_boxes
    }
    pub(crate) fn call_frames(&self) -> impl DoubleEndedIterator<Item = &StackFrame> {
        self.rt.call_stack.iter()
    }
    pub(crate) fn call_with_this(&mut self, f: Function) -> UiuaResult {
        let call_height = self.rt.call_stack.len();
        let with_height = self.rt.this_stack.len();
        self.rt.this_stack.push(self.rt.call_stack.len());
        let res = self.call(f);
        self.rt.call_stack.truncate(call_height);
        self.rt.this_stack.truncate(with_height);
        res
    }
    pub(crate) fn recur(&mut self) -> UiuaResult {
        let Some(i) = self.rt.this_stack.last().copied() else {
            return Err(self.error("No recursion context set"));
        };
        let mut frame = self.rt.call_stack[i].clone();
        frame.pc = 0;
        self.call_frame(frame)
    }
    /// Spawn a thread
    pub(crate) fn spawn(
        &mut self,
        capture_count: usize,
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
            current_imports: self.current_imports.clone(),
            imports: self.imports.clone(),
            diagnostics: BTreeSet::new(),
            print_diagnostics: self.print_diagnostics,
            ct: CompileTime {
                new_functions: Vec::new(),
                next_global: self.ct.next_global,
                scope: self.ct.scope.clone(),
                higher_scopes: Vec::new(),
                mode: self.ct.mode,
            },
            rt: Runtime {
                last_slice_run: 0,
                stack: self
                    .rt
                    .stack
                    .drain(self.rt.stack.len() - capture_count..)
                    .collect(),
                function_stack: Vec::new(),
                temp_stacks: [Vec::new(), Vec::new()],
                temp_function_stack: Vec::new(),
                array_stack: Vec::new(),
                fill_stack: Vec::new(),
                this_stack: self.rt.this_stack.clone(),
                call_stack: Vec::new(),
                unpack_boxes: self.rt.unpack_boxes,
                time_instrs: self.rt.time_instrs,
                last_time: self.rt.last_time,
                cli_arguments: self.rt.cli_arguments.clone(),
                cli_file_path: self.rt.cli_file_path.clone(),
                backend: self.rt.backend.clone(),
                execution_limit: self.rt.execution_limit,
                execution_start: self.rt.execution_start,
                thread,
            },
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

        let id = self.rt.thread.next_child_id;
        self.rt.thread.next_child_id += 1;
        self.rt.thread.children.insert(
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
                self.rt
                    .thread
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
            self.rt.stack.extend(thread_stack);
        } else {
            let mut rows = Vec::new();
            for handle in ids.data {
                #[cfg(not(target_arch = "wasm32"))]
                let thread_stack = Arc::into_inner(
                    self.rt
                        .thread
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

impl<F, T> StackArg for F
where
    F: FnOnce() -> T,
    T: StackArg,
{
    fn arg_name(self) -> String {
        self().arg_name()
    }
}
