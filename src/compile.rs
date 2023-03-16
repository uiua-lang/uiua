use std::{collections::HashMap, fmt, fs, mem::take, path::Path};

use enum_iterator::all;

use crate::{
    ast::*,
    function::{Function, FunctionId},
    lex::{Sp, Span},
    ops::{constants, Op1, Op2, Primitive},
    parse::{parse, ParseError},
    value::Value,
    vm::{dprintln, Instr, Vm},
    Ident, RuntimeError, UiuaError, UiuaResult,
};

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) start: usize,
    pub(crate) constants: Vec<Value>,
    pub(crate) function_ids: HashMap<Function, FunctionId>,
    pub(crate) spans: Vec<Span>,
}

impl Assembly {
    #[track_caller]
    pub fn function_id(&self, function: Function) -> &FunctionId {
        if let Some(id) = self.function_ids.get(&function) {
            id
        } else {
            panic!("function was compiled in a different assembly")
        }
    }
    pub fn find_function(&self, id: impl Into<FunctionId>) -> Option<Function> {
        let id = id.into();
        for (function, fid) in &self.function_ids {
            if fid == &id {
                return Some(*function);
            }
        }
        None
    }
    pub fn run(&self) -> UiuaResult<Option<Value>> {
        let mut vm = Vm::default();
        let res = self.run_with_vm(&mut vm)?;
        dprintln!("stack:");
        for val in &vm.stack {
            dprintln!("  {val:?}");
        }
        if let Some(val) = &res {
            dprintln!("  {val:?}");
        }
        Ok(res)
    }
    fn run_with_vm(&self, vm: &mut Vm) -> UiuaResult<Option<Value>> {
        vm.run_assembly(self)?;
        Ok(vm.stack.pop())
    }
    fn add_function_instrs(&mut self, mut instrs: Vec<Instr>) {
        self.instrs.append(&mut instrs);
        self.start = self.instrs.len();
    }
    fn add_non_function_instrs(&mut self, mut instrs: Vec<Instr>) {
        self.instrs.append(&mut instrs);
    }
    fn truncate(&mut self, len: usize) {
        self.instrs.truncate(len);
        self.start = len;
    }
    pub(crate) fn error(&self, span: usize, msg: impl Into<String>) -> RuntimeError {
        self.spans[span].error(msg.into())
    }
}

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(Ident),
    ConstEval(UiuaError),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "{e}"),
            CompileError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CompileError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CompileError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
            CompileError::ConstEval(e) => write!(f, "{e}"),
        }
    }
}

impl From<UiuaError> for CompileError {
    fn from(e: UiuaError) -> Self {
        CompileError::ConstEval(e)
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub struct Compiler {
    /// Instructions for stuff in the global scope
    global_instrs: Vec<Instr>,
    /// Instructions for functions that are currently being compiled
    in_progress_functions: Vec<InProgressFunction>,
    /// Stack of scopes
    scopes: Vec<Scope>,
    /// The relative height of the runtime stack
    height: usize,
    /// Errors that don't stop compilation
    errors: Vec<Sp<CompileError>>,
    /// The partially compiled assembly
    assembly: Assembly,
    /// Vm for constant evaluation
    vm: Vm,
}

struct InProgressFunction {
    instrs: Vec<Instr>,
    captures: Vec<Ident>,
    height: usize,
}

struct Scope {
    /// How many functions deep the scope is. 0 is the global scope.
    function_depth: usize,
    /// Values and functions that are in scope
    bindings: HashMap<Ident, Binding>,
    /// Map of function ids to the actual function values
    functions: HashMap<FunctionId, Function>,
}

#[derive(Debug, Clone)]
enum Binding {
    /// A function that is available but not on the stack.
    Function(FunctionId),
    /// A local variable is referenced by its height in the stack.
    Local(usize),
    /// A constant is referenced by its index in the constant array.
    Constant(usize),
    /// A dud binding used when a binding lookup fails
    Error,
}

impl Scope {
    fn new(function_depth: usize) -> Self {
        Self {
            function_depth,
            bindings: HashMap::default(),
            functions: HashMap::default(),
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        let mut assembly = Assembly {
            start: 0,
            instrs: Vec::new(),
            constants: Vec::new(),
            function_ids: HashMap::default(),
            spans: vec![Span::Builtin],
        };
        let mut scope = Scope::new(0);
        // Initialize builtins
        // Constants
        for (name, value) in constants() {
            let index = assembly.constants.len();
            assembly.constants.push(value);
            scope.bindings.insert(name.into(), Binding::Constant(index));
        }
        // Operations
        let mut init = |name: String, prim: Primitive| -> Function {
            let function = Function::Primitive(prim);
            // Scope
            scope
                .bindings
                .insert(name.into(), Binding::Function(prim.into()));
            scope.functions.insert(prim.into(), function);
            // Function info
            assembly.function_ids.insert(function, prim.into());
            function
        };
        // 1-parameter builtins
        for op1 in all::<Op1>() {
            init(op1.to_string(), op1.into());
        }
        // 2-parameter builtins
        for op2 in all::<Op2>() {
            init(op2.to_string(), op2.into());
        }
        // Higher-order builtins
        for op in all::<Primitive>() {
            init(op.to_string(), op);
        }

        assembly.start = assembly.instrs.len();

        Self {
            global_instrs: vec![Instr::Comment("BEGIN".into())],
            in_progress_functions: Vec::new(),
            scopes: vec![scope],
            height: 0,
            errors: Vec::new(),
            assembly,
            vm: Vm::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub id: FunctionId,
    pub params: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> UiuaResult {
        let path = path.as_ref();
        let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.into(), e))?;
        self.load(&input, path)?;
        Ok(())
    }
    pub fn load<P: AsRef<Path>>(
        &mut self,
        input: &str,
        path: P,
    ) -> Result<(), Vec<Sp<CompileError>>> {
        let (items, errors) = parse(input, path.as_ref());
        let mut errors: Vec<Sp<CompileError>> = errors
            .into_iter()
            .map(|e| e.map(CompileError::Parse))
            .collect();
        let function_start = self.assembly.instrs.len();
        let global_start = self.global_instrs.len();
        for item in items {
            if let Err(e) = self.item(item) {
                errors.push(e);
            }
        }
        errors.append(&mut self.errors);
        if errors.is_empty() {
            Ok(())
        } else {
            self.assembly.truncate(function_start);
            self.global_instrs.truncate(global_start);
            Err(errors)
        }
    }
    pub fn finish(mut self) -> Assembly {
        self.assembly.add_non_function_instrs(self.global_instrs);
        for (i, instr) in self.assembly.instrs.iter().enumerate() {
            dprintln!("{i:>3} {instr}");
        }
        self.assembly
    }
    fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }
    fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn push_scope(&mut self) -> usize {
        self.scopes
            .push(Scope::new(self.in_progress_functions.len()));
        self.height
    }
    fn pop_scope(&mut self, height: usize) {
        self.scopes.pop().unwrap();
        self.height = height;
    }
    /// Bind the value on the top of the stack to the given identifier
    fn bind_local(&mut self, ident: Ident) {
        let height = self.height - 1;
        self.scope_mut()
            .bindings
            .insert(ident, Binding::Local(height));
    }
    fn instrs_mut(&mut self) -> &mut Vec<Instr> {
        self.in_progress_functions
            .last_mut()
            .map(|f| &mut f.instrs)
            .unwrap_or(&mut self.global_instrs)
    }
    fn push_instr(&mut self, instr: Instr) {
        match &instr {
            Instr::CopyRel(_) => self.height += 1,
            Instr::CopyAbs(_) => self.height += 1,
            Instr::Push(_) => self.height += 1,
            Instr::Call(_) => self.height -= 1,
            Instr::Constant(_) => self.height += 1,
            Instr::Array(len) | Instr::List(len) if *len == 0 => self.height += 1,
            Instr::Array(len) | Instr::List(len) => self.height -= len - 1,
            _ => {}
        }
        self.instrs_mut().push(instr);
    }
    fn push_call_span(&mut self, span: Span) -> usize {
        let spot = self.assembly.spans.len();
        self.assembly.spans.push(span);
        spot
    }
    fn item(&mut self, item: Item) -> CompileResult {
        match item {
            Item::Words(words) => self.words(words),
            Item::Let(binding) => self.r#let(binding),
            Item::Const(r#const) => self.r#const(r#const),
        }
    }
    fn r#let(&mut self, binding: Let) -> CompileResult {
        self.words(binding.words)?;
        self.bind_local(binding.name.value);
        Ok(())
    }
    fn r#const(&mut self, r#const: Const) -> CompileResult {
        let index = self.assembly.constants.len();
        // Set a restore point
        let height = self.height;
        let instr_len = self.assembly.instrs.len();
        let global_instrs = self.global_instrs.clone();
        // Compile the words
        let words_span = r#const.words.last().unwrap().span.clone();
        self.words(r#const.words)?;
        self.assembly
            .add_non_function_instrs(take(&mut self.global_instrs));
        // Evaluate the words
        let value = self
            .assembly
            .run_with_vm(&mut self.vm)
            .map_err(|e| words_span.sp(e.into()))?
            .unwrap_or(Value::default());
        self.assembly.constants.push(value);
        // Bind the constant
        self.scope_mut()
            .bindings
            .insert(r#const.name.value, Binding::Constant(index));
        // Restore
        self.assembly.truncate(instr_len);
        self.height = height;
        self.global_instrs = global_instrs;
        Ok(())
    }
    fn words(&mut self, words: Vec<Sp<Word>>) -> CompileResult {
        for word in words.into_iter().rev() {
            self.word(word, true)?;
        }
        Ok(())
    }
    fn word(&mut self, word: Sp<Word>, call: bool) -> CompileResult {
        match word.value {
            Word::Real(s) => {
                let f: f64 = match s.parse() {
                    Ok(f) => f,
                    Err(_) => {
                        self.errors.push(word.span.sp(CompileError::InvalidReal(s)));
                        0.0
                    }
                };
                self.push_instr(Instr::Push(f.into()));
            }
            Word::Char(c) => self.push_instr(Instr::Push(c.into())),
            Word::String(s) => self.push_instr(Instr::Push(s.into())),
            Word::Ident(ident) => self.ident(ident, word.span, call)?,
            Word::Array(items) => self.list(Instr::Array, items)?,
            Word::Strand(items) => self.list(Instr::List, items)?,
            Word::Func(func) => self.func(func)?,
            Word::Primitive(prim) => self.primitive(prim, word.span, call),
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: Span, call: bool) -> CompileResult {
        let bind = self.scopes.iter().rev().find_map(|scope| {
            scope
                .bindings
                .get(&ident)
                .map(|binding| (binding, scope.function_depth))
        });
        let (binding, binding_depth) = match bind {
            Some(bind) => bind,
            None => {
                self.errors
                    .push(span.clone().sp(CompileError::UnknownBinding(ident.clone())));
                (&Binding::Error, self.scope().function_depth)
            }
        };
        let mut binding = binding.clone();
        if binding_depth > 0 && binding_depth != self.scope().function_depth {
            let ipf = self.in_progress_functions.last_mut().unwrap();
            let pos = ipf.captures.iter().position(|id| id == &ident);
            let pos = pos.unwrap_or_else(|| {
                ipf.captures.push(ident.clone());
                ipf.captures.len() - 1
            });
            binding = Binding::Local(ipf.height - pos - 1);
        }
        match binding {
            Binding::Local(index) => {
                if binding_depth == 0 {
                    self.push_instr(Instr::CopyAbs(index));
                } else {
                    let curr = self.height;
                    let diff = curr - index;
                    self.push_instr(Instr::CopyRel(diff));
                }
            }
            Binding::Function(id) => self.function_binding(id),
            Binding::Constant(index) => self.push_instr(Instr::Constant(index)),
            Binding::Error => self.push_instr(Instr::Push(Value::default())),
        }
        if call {
            let span = self.push_call_span(span);
            self.push_instr(Instr::Call(span));
        }
        Ok(())
    }
    fn function_binding(&mut self, id: FunctionId) {
        if let Some(function) = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.functions.get(&id))
            .cloned()
        {
            self.push_instr(Instr::Push(function.into()));
        } else {
            todo!("is this even reachable?")
        }
    }
    fn list(&mut self, make: fn(usize) -> Instr, items: Vec<Sp<Word>>) -> CompileResult {
        let len = items.len();
        for item in items {
            self.word(item, false)?;
        }
        self.push_instr(make(len));
        Ok(())
    }
    fn func(&mut self, func: Func) -> CompileResult {
        // Initialize the function's instruction list
        self.in_progress_functions.push(InProgressFunction {
            instrs: Vec::new(),
            captures: Vec::new(),
            height: self.height,
        });
        // Push the function's scope
        let height = self.push_scope();
        // Push the function's name as a comment
        let name = match &func.id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
            FunctionId::FormatString(span) => format!("format string at {span}"),
            FunctionId::Primitive(_) => unreachable!("Primitive functions should not be compiled"),
        };
        self.push_instr(Instr::Comment(name.clone()));
        // Compile the function's body
        self.words(func.body)?;
        self.push_instr(Instr::Comment(format!("end of {name}")));
        self.push_instr(Instr::Return);
        // Pop the function's scope
        self.pop_scope(height);
        // Determine the function's index
        let function = Function::Code(self.assembly.instrs.len() as u32);
        // Add the function's instructions to the global function list
        let ipf = self.in_progress_functions.pop().unwrap();
        self.assembly.add_function_instrs(ipf.instrs);
        self.scope_mut().functions.insert(func.id.clone(), function);
        // Add to the function id map
        self.assembly.function_ids.insert(function, func.id);
        // Push the function
        self.push_instr(Instr::Push(function.into()));
        Ok(())
    }
    fn primitive(&mut self, prim: Primitive, span: Span, call: bool) {
        self.push_instr(Instr::Push(Function::Primitive(prim).into()));
        if call {
            let span = self.push_call_span(span);
            self.push_instr(Instr::Call(span));
        }
    }
}
