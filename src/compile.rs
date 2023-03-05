use std::{collections::HashMap, fmt, fs, mem::take, path::Path};

use enum_iterator::all;
use nohash_hasher::BuildNoHashHasher;

use crate::{
    ast::*,
    builtin::{constants, Algorithm, Op1, Op2},
    lex::{Sp, Span},
    parse::{parse, ParseError},
    value::{Function, Value},
    vm::{dprintln, Instr, Vm},
    Ident, UiuaError, UiuaResult,
};

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) start: usize,
    pub(crate) constants: Vec<Value>,
    pub(crate) function_info: HashMap<Function, FunctionInfo, BuildNoHashHasher<Function>>,
    pub(crate) spans: Vec<Span>,
    pub(crate) cached_functions: CachedFunctions,
}

pub(crate) struct CachedFunctions {
    pub get: Function,
}

impl Assembly {
    #[track_caller]
    pub fn function_info(&self, function: Function) -> &FunctionInfo {
        if let Some(info) = self.function_info.get(&function) {
            info
        } else {
            panic!("function was compiled in a different assembly")
        }
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
        vm.run_assembly(self)
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
    in_progress_functions: Vec<Vec<Instr>>,
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

struct Scope {
    /// How many functions deep the scope is. 0 is the global scope.
    function_depth: usize,
    /// Values and functions that are in scope
    bindings: HashMap<Ident, Binding>,
    /// Map of function ids to the actual function values
    functions: HashMap<FunctionId, Function>,
}

#[derive(Debug)]
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
        let mut function_instrs = Vec::new();
        let mut scope = Scope::new(0);
        let mut function_info = HashMap::default();
        let mut consts = Vec::new();
        let mut cached_functions = CachedFunctions { get: Function(0) };
        // Initialize builtins
        // Constants
        for (name, value) in constants() {
            let index = consts.len();
            consts.push(value);
            scope
                .bindings
                .insert(ascend::static_str(name).into(), Binding::Constant(index));
        }
        // Operations
        let mut init =
            |name: &str, id: FunctionId, params: usize, mut instrs: Vec<Instr>| -> Function {
                let function = Function(function_instrs.len());
                // Instructions
                function_instrs.append(&mut instrs);
                function_instrs.push(Instr::Return);
                // Scope
                scope.bindings.insert(
                    ascend::static_str(name).into(),
                    Binding::Function(id.clone()),
                );
                scope.functions.insert(id.clone(), function);
                // Function info
                function_info.insert(function, FunctionInfo { id, params });
                function
            };
        // 1-parameter builtins
        for op1 in all::<Op1>() {
            init(
                &op1.to_string(),
                FunctionId::Op1(op1),
                1,
                vec![Instr::Op1(op1)],
            );
        }
        // 2-parameter builtins
        for op2 in all::<Op2>() {
            let function = init(
                &op2.to_string(),
                FunctionId::Op2(op2),
                2,
                vec![Instr::Op2(op2)],
            );
            if let Op2::Get = op2 {
                cached_functions.get = function;
            }
        }
        // Algorithms
        for algo in all::<Algorithm>() {
            init(
                &algo.to_string(),
                FunctionId::Algorithm(algo),
                algo.params(),
                algo.instrs(),
            );
        }

        // The default function is the identity function
        assert_eq!(function_instrs[0], Instr::Op1(Op1::Id));

        let assembly = Assembly {
            start: function_instrs.len(),
            instrs: function_instrs,
            constants: consts,
            function_info,
            spans: vec![Span::Builtin],
            cached_functions,
        };
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
    pub fn load(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<CompileError>>> {
        let (items, errors) = parse(input, path);
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
    fn instrs(&self) -> &[Instr] {
        self.in_progress_functions
            .last()
            .unwrap_or(&self.global_instrs)
    }
    fn instrs_mut(&mut self) -> &mut Vec<Instr> {
        self.in_progress_functions
            .last_mut()
            .unwrap_or(&mut self.global_instrs)
    }
    fn push_instr(&mut self, instr: Instr) {
        match &instr {
            Instr::CopyRel(_) => self.height += 1,
            Instr::CopyAbs(_) => self.height += 1,
            Instr::Push(_) => self.height += 1,
            Instr::PushUnresolvedFunction(_) => self.height += 1,
            Instr::BinOp(..) => self.height -= 1,
            Instr::Call(_) => self.height -= 1,
            Instr::Constant(_) => self.height += 1,
            Instr::List(len) => self.height -= len - 1,
            Instr::Array(len) => self.height -= len - 1,
            _ => {}
        }
        self.instrs_mut().push(instr);
    }
    fn push_spot(&mut self) -> usize {
        let spot = self.instrs().len();
        self.push_instr(Instr::Dud);
        spot
    }
    fn push_call_span(&mut self, span: Span) -> usize {
        let spot = self.assembly.spans.len();
        self.assembly.spans.push(span);
        spot
    }
    fn item(&mut self, item: Item) -> CompileResult {
        match item {
            Item::Expr(expr) => self.expr(resolve_placeholders(expr)),
            Item::Let(binding) => self.r#let(binding),
            Item::Const(r#const) => self.r#const(r#const),
            Item::FunctionDef(def) => self.function_def(def),
        }
    }
    fn function_def(&mut self, def: FunctionDef) -> CompileResult {
        self.scope_mut()
            .bindings
            .insert(def.name.value, Binding::Function(def.func.id.clone()));
        self.func(def.func, false)?;
        Ok(())
    }
    fn func(&mut self, func: Func, push: bool) -> CompileResult {
        self.func_outer(push, func.id.clone(), |this| {
            // Push and bind the function's parameters
            let params = func.params.len();
            for param in func.params.into_iter().rev() {
                this.height += 1;
                this.bind_local(param.value);
            }
            // Compile the function's body
            this.block(func.body)?;
            Ok(params)
        })
    }
    fn func_outer(
        &mut self,
        push: bool,
        id: FunctionId,
        f: impl FnOnce(&mut Self) -> CompileResult<usize>,
    ) -> CompileResult {
        // Initialize the function's instruction list
        self.in_progress_functions.push(Vec::new());
        // Push the function's scope
        let height = self.push_scope();
        // Push the function's name as a comment
        self.push_instr(Instr::Comment(match &id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
            FunctionId::FormatString(span) => format!("format string at {span}"),
            FunctionId::Op1(_) => unreachable!("Builtin1 functions should not be compiled"),
            FunctionId::Op2(_) => unreachable!("Builtin2 functions should not be compiled"),
            FunctionId::Algorithm(_) => unreachable!("Builtin algorithms should not be compiled"),
        }));
        let params = f(self)?;
        self.push_instr(Instr::Return);
        // Pop the function's scope
        self.pop_scope(height);
        //Determine the function's index
        let function = Function(self.assembly.instrs.len());
        // Resolve function references
        for instrs in &mut self.in_progress_functions {
            for instr in instrs {
                if matches!(instr, Instr::PushUnresolvedFunction(uid) if **uid == id) {
                    *instr = Instr::Push(function.into());
                }
            }
        }
        // Add the function's instructions to the global function list
        let instrs = self.in_progress_functions.pop().unwrap();
        self.assembly.add_function_instrs(instrs);
        self.scope_mut().functions.insert(id.clone(), function);
        // Add to the function info map
        self.assembly
            .function_info
            .insert(function, FunctionInfo { id, params });
        // Push the function if necessary
        if push {
            self.push_instr(Instr::Push(function.into()));
        }
        Ok(())
    }
    fn r#let(&mut self, binding: Let) -> CompileResult {
        // The expression is evaluated first because the pattern
        // will refer to the height of the stack after the expression
        self.expr(resolve_placeholders(binding.expr))?;
        self.pattern(binding.pattern)?;
        Ok(())
    }
    fn r#const(&mut self, r#const: Const) -> CompileResult {
        let index = self.assembly.constants.len();
        // Set a restore point
        let height = self.height;
        let instr_len = self.assembly.instrs.len();
        let global_instrs = self.global_instrs.clone();
        // Compile the expression
        let expr_span = r#const.expr.span.clone();
        self.expr(resolve_placeholders(r#const.expr))?;
        self.assembly
            .add_non_function_instrs(take(&mut self.global_instrs));
        // Evaluate the expression
        let value = self
            .assembly
            .run_with_vm(&mut self.vm)
            .map_err(|e| expr_span.sp(e.into()))?
            .unwrap_or(Value::Unit);
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
    fn pattern(&mut self, pattern: Sp<Pattern>) -> CompileResult {
        match pattern.value {
            Pattern::Ident(ident) => self.bind_local(ident),
            Pattern::List(patterns) => {
                let len = patterns.len();
                for pattern in patterns {
                    self.pattern(pattern)?;
                }
                self.push_instr(Instr::DestructureList(len, pattern.span.clone().into()));
            }
            Pattern::Discard => {}
        }
        Ok(())
    }
    fn expr(&mut self, expr: Sp<Expr>) -> CompileResult {
        match expr.value {
            Expr::Unit => self.push_instr(Instr::Push(Value::Unit)),
            Expr::Bool(b) => self.push_instr(Instr::Push(b.into())),
            Expr::Int(s) => {
                let i: i64 = match s.parse() {
                    Ok(i) => i,
                    Err(_) => {
                        self.errors
                            .push(expr.span.sp(CompileError::InvalidInteger(s)));
                        0
                    }
                };
                self.push_instr(Instr::Push(i.into()));
            }
            Expr::Real(s) => {
                let f: f64 = match s.parse() {
                    Ok(f) => f,
                    Err(_) => {
                        self.errors.push(expr.span.sp(CompileError::InvalidReal(s)));
                        0.0
                    }
                };
                self.push_instr(Instr::Push(f.into()));
            }
            Expr::Char(c) => self.push_instr(Instr::Push(c.into())),
            Expr::String(s) => self.push_instr(Instr::Push(s.into())),
            Expr::FormatString(parts) => self.format_string(parts, expr.span)?,
            Expr::Ident(ident) => self.ident(ident, expr.span)?,
            Expr::Placeholder => panic!("unresolved placeholder"),
            Expr::Bin(bin) => {
                self.expr(bin.left)?;
                self.expr(bin.right)?;
                let span = self.push_call_span(bin.op.span);
                self.push_instr(Instr::BinOp(bin.op.value, span));
            }
            Expr::Call(call) => self.call(*call)?,
            Expr::If(if_expr) => self.if_expr(*if_expr)?,
            Expr::Logic(log_expr) => self.logic_expr(*log_expr)?,
            Expr::Pipe(pipe_expr) => self.pipe_expr(*pipe_expr)?,
            Expr::List(items) => self.list(Instr::List, items)?,
            Expr::Array(items) => self.list(Instr::Array, items)?,
            Expr::Parened(inner) => self.expr(resolve_placeholders(*inner))?,
            Expr::Func(func) => self.func(*func, true)?,
        }
        Ok(())
    }
    fn ident(&mut self, ident: Ident, span: Span) -> CompileResult {
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
                    .push(span.clone().sp(CompileError::UnknownBinding(ident)));
                (&Binding::Error, self.scope().function_depth)
            }
        };
        if binding_depth > 0 && binding_depth != self.scope().function_depth {
            todo!("closure support ({})", span)
        }
        match binding {
            Binding::Local(index) => {
                if binding_depth == 0 {
                    self.push_instr(Instr::CopyAbs(*index));
                } else {
                    let curr = self.height;
                    let diff = curr - *index;
                    self.push_instr(Instr::CopyRel(diff));
                }
            }
            Binding::Function(id) => {
                if let Some(function) = self
                    .scopes
                    .iter()
                    .rev()
                    .find_map(|scope| scope.functions.get(id))
                    .cloned()
                {
                    self.push_instr(Instr::Push(function.into()));
                } else {
                    self.push_instr(Instr::PushUnresolvedFunction(id.clone().into()));
                }
            }
            Binding::Constant(index) => self.push_instr(Instr::Constant(*index)),
            Binding::Error => self.push_instr(Instr::Push(Value::Unit)),
        }
        Ok(())
    }
    fn list(&mut self, make: fn(usize) -> Instr, items: Vec<Sp<Expr>>) -> CompileResult {
        let len = items.len();
        for item in items {
            self.expr(item)?;
        }
        self.push_instr(make(len));
        Ok(())
    }
    fn call(&mut self, call: CallExpr) -> CompileResult {
        self.call_impl(call.func, call.arg)
    }
    fn call_impl(&mut self, func: Sp<Expr>, arg: Sp<Expr>) -> CompileResult {
        self.expr(arg)?;
        let span = self.push_call_span(func.span.clone());
        self.expr(func)?;
        self.push_instr(Instr::Call(span));
        Ok(())
    }
    fn block(&mut self, block: Block) -> CompileResult {
        let height = self.push_scope();
        for item in block.items {
            self.item(item)?;
        }
        self.expr(resolve_placeholders(block.expr))?;
        self.pop_scope(height);
        Ok(())
    }
    fn if_expr(&mut self, if_expr: IfExpr) -> CompileResult {
        self.expr(if_expr.cond)?;
        let jump_to_else_spot = self.push_spot();
        self.height -= 1;
        self.block(if_expr.if_true)?;
        let jump_to_end_spot = self.push_spot();
        self.instrs_mut()[jump_to_else_spot] = Instr::PopJumpIf(
            self.instrs().len() as isize - jump_to_else_spot as isize,
            false,
        );
        self.block(if_expr.if_false)?;
        self.instrs_mut()[jump_to_end_spot] =
            Instr::Jump(self.instrs().len() as isize - jump_to_end_spot as isize);
        Ok(())
    }
    fn logic_expr(&mut self, log_expr: LogicExpr) -> CompileResult {
        self.expr(log_expr.left)?;
        let jump_spot = self.push_spot();
        self.height -= 1;
        self.expr(log_expr.right)?;
        let jump_cond = match log_expr.op.value {
            LogicOp::And => false,
            LogicOp::Or => true,
        };
        self.instrs_mut()[jump_spot] =
            Instr::JumpIfElsePop(self.instrs().len() as isize - jump_spot as isize, jump_cond);
        Ok(())
    }
    fn pipe_expr(&mut self, pipe_expr: PipeExpr) -> CompileResult {
        match pipe_expr.op.value {
            PipeOp::Forward => self.call_impl(pipe_expr.right, pipe_expr.left),
            PipeOp::Backward => self.call_impl(pipe_expr.left, pipe_expr.right),
        }
    }
    fn format_string(&mut self, parts: Vec<String>, span: Span) -> CompileResult {
        if parts.len() <= 1 {
            self.push_instr(Instr::Push(
                parts.into_iter().next().unwrap_or_default().into(),
            ));
            return Ok(());
        }
        self.func_outer(true, FunctionId::FormatString(span), |this| {
            let params = parts.len() - 1;
            let mut parts = parts.into_iter();
            this.push_instr(Instr::Push(parts.next().unwrap().into()));
            for (i, part) in parts.enumerate() {
                this.push_instr(Instr::CopyRel(i + 2));
                this.push_instr(Instr::Op2(Op2::Concat));
                this.push_instr(Instr::Push(part.into()));
                this.push_instr(Instr::Op2(Op2::Concat));
            }
            Ok(params)
        })
    }
}

fn resolve_placeholders(mut expr: Sp<Expr>) -> Sp<Expr> {
    let mut params = Vec::new();
    resolve_placeholders_rec(&mut expr, &mut params);
    if params.is_empty() {
        return expr;
    }
    let span = expr.span.clone();
    span.clone().sp(Expr::Func(Box::new(Func {
        id: FunctionId::Anonymous(span),
        params,
        body: Block {
            items: Vec::new(),
            expr,
        },
    })))
}

fn resolve_placeholders_rec(expr: &mut Sp<Expr>, params: &mut Vec<Sp<Ident>>) {
    match &mut expr.value {
        Expr::Placeholder => {
            let ident = expr.span.clone().sp(Ident::Placeholder(params.len()));
            params.push(ident.clone());
            *expr = ident.map(Expr::Ident);
        }
        Expr::If(if_expr) => {
            resolve_placeholders_rec(&mut if_expr.cond, params);
            if if_expr.if_true.items.is_empty() && if_expr.if_false.items.is_empty() {
                resolve_placeholders_rec(&mut if_expr.if_true.expr, params);
                resolve_placeholders_rec(&mut if_expr.if_false.expr, params);
            }
        }
        Expr::Call(call_expr) => {
            resolve_placeholders_rec(&mut call_expr.func, params);
            resolve_placeholders_rec(&mut call_expr.arg, params);
        }
        Expr::Bin(bin_expr) => {
            resolve_placeholders_rec(&mut bin_expr.left, params);
            resolve_placeholders_rec(&mut bin_expr.right, params);
        }
        Expr::Logic(log_expr) => {
            resolve_placeholders_rec(&mut log_expr.left, params);
            resolve_placeholders_rec(&mut log_expr.right, params);
        }
        Expr::Pipe(pipe_expr) => {
            resolve_placeholders_rec(&mut pipe_expr.left, params);
            resolve_placeholders_rec(&mut pipe_expr.right, params);
        }
        Expr::List(items) => {
            for item in items {
                resolve_placeholders_rec(item, params);
            }
        }
        Expr::Array(items) => {
            for item in items {
                resolve_placeholders_rec(item, params);
            }
        }
        Expr::Parened(_) => {}
        Expr::Unit
        | Expr::Func(_)
        | Expr::Bool(_)
        | Expr::Int(_)
        | Expr::Real(_)
        | Expr::Char(_)
        | Expr::String(_)
        | Expr::FormatString(_)
        | Expr::Ident(_) => {}
    }
}
