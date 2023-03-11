use std::{collections::HashMap, fmt, fs, mem::take, path::Path};

use enum_iterator::all;

use crate::{
    ast::*,
    function::{Function, FunctionId, PrimitiveId},
    lex::{Sp, Span},
    ops::{constants, HigherOp, Op1, Op2},
    parse::{parse, ParseError},
    value::Value,
    vm::{dprintln, Instr, Vm},
    Ident, UiuaError, UiuaResult,
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
            scope
                .bindings
                .insert(ascend::static_str(name).into(), Binding::Constant(index));
        }
        // Operations
        let mut init = |name: String, id: PrimitiveId, params: u8, instr: Instr| -> Function {
            let function = Function {
                start: assembly.instrs.len() as u32,
                params,
                primitive: Some(id),
            };
            // Instructions
            assembly.instrs.push(instr);
            assembly.instrs.push(Instr::Return);
            // Scope
            scope.bindings.insert(
                ascend::static_str(&name).into(),
                Binding::Function(id.into()),
            );
            scope.functions.insert(id.into(), function);
            // Function info
            assembly.function_ids.insert(function, id.into());
            function
        };
        // 1-parameter builtins
        for op1 in all::<Op1>() {
            init(op1.to_string(), op1.into(), 1, Instr::Op1(op1));
        }
        // 2-parameter builtins
        for op2 in all::<Op2>() {
            init(op2.to_string(), op2.into(), 2, Instr::Op2(op2, 0));
        }
        // Higher-order builtins
        for hop in all::<HigherOp>() {
            init(
                hop.to_string(),
                hop.into(),
                hop.params(),
                Instr::HigherOp(hop),
            );
        }

        assembly.start = assembly.instrs.len();

        // The first function is the nil function
        assert_eq!(assembly.instrs[0], Instr::Op1(Op1::Nil));

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
            Instr::PushUnresolvedFunction(_) => self.height += 1,
            Instr::Op2(..) => self.height -= 1,
            Instr::Call(args, _) => self.height -= *args,
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
            Item::Expr(expr) => self.expr(preprocess_expr(expr)),
            Item::Let(binding) => self.r#let(binding),
            Item::Const(r#const) => self.r#const(r#const),
            Item::FunctionDef(def) => self.function_def(def),
        }
    }
    fn function_def(&mut self, def: FunctionDef) -> CompileResult {
        self.scope_mut()
            .bindings
            .insert(def.name.value, Binding::Function(def.func.id.clone()));
        self.func(def.func, def.name.span, false)?;
        Ok(())
    }
    fn func(&mut self, func: Func, span: Span, push: bool) -> CompileResult {
        self.func_outer(push, func.id.clone(), span, |this| {
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
        span: Span,
        params_and_body: impl FnOnce(&mut Self) -> CompileResult<usize>,
    ) -> CompileResult {
        // Initialize the function's instruction list
        self.in_progress_functions.push(InProgressFunction {
            instrs: Vec::new(),
            captures: Vec::new(),
            height: self.height,
        });
        // Push the function's scope
        let height = self.push_scope();
        // Push the function's name as a comment
        let name = match &id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
            FunctionId::FormatString(span) => format!("format string at {span}"),
            FunctionId::Primitive(_) => unreachable!("Primitive functions should not be compiled"),
        };
        self.push_instr(Instr::Comment(name.clone()));
        let params = params_and_body(self)?;
        self.push_instr(Instr::Comment(format!("end of {name}")));
        self.push_instr(Instr::Return);
        // Pop the function's scope
        self.pop_scope(height);
        // Rotate captures
        let ipf = self.in_progress_functions.last_mut().unwrap();
        if !ipf.captures.is_empty() {
            for _ in 0..ipf.captures.len() {
                ipf.instrs.insert(1, Instr::Rotate(ipf.captures.len() + 1));
            }
        }
        //Determine the function's index
        let params_count = params + ipf.captures.len();
        let function = Function {
            start: self.assembly.instrs.len() as u32,
            params: params_count as u8,
            primitive: None,
        };
        // Resolve function references
        for ipf in &mut self.in_progress_functions {
            for instr in &mut ipf.instrs {
                if matches!(instr, Instr::PushUnresolvedFunction(uid) if **uid == id) {
                    *instr = Instr::Push(function.into());
                }
            }
        }
        // Add the function's instructions to the global function list
        let ipf = self.in_progress_functions.pop().unwrap();
        self.assembly.add_function_instrs(ipf.instrs);
        self.scope_mut().functions.insert(id.clone(), function);
        // Add to the function id map
        self.assembly.function_ids.insert(function, id.clone());
        // Push the function if necessary
        if push || !ipf.captures.is_empty() {
            // Reevalutate captures so they are copied to the stack just before the function
            for &ident in ipf.captures.iter().rev() {
                self.ident(ident, span.clone())?;
            }
            // Push the function
            self.push_instr(Instr::Push(function.into()));

            if !ipf.captures.is_empty() {
                // Call the function to collect the captures
                let call_span = self.push_call_span(span);
                self.push_instr(Instr::Call(ipf.captures.len(), call_span));

                // Rebind to the partial that has the captures
                if let FunctionId::Named(name) = id {
                    self.bind_local(name);
                }
            }
        }
        Ok(())
    }
    fn r#let(&mut self, binding: Let) -> CompileResult {
        // The expression is evaluated first because the pattern
        // will refer to the height of the stack after the expression
        self.expr(preprocess_expr(binding.expr))?;
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
        self.expr(preprocess_expr(r#const.expr))?;
        self.assembly
            .add_non_function_instrs(take(&mut self.global_instrs));
        // Evaluate the expression
        let value = self
            .assembly
            .run_with_vm(&mut self.vm)
            .map_err(|e| expr_span.sp(e.into()))?
            .unwrap_or(Value::nil());
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
            Expr::Unit => self.push_instr(Instr::Push(Value::nil())),
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
                let left = bin.left;
                let right = bin.right;
                let span = bin.op.span;
                match bin_op_primitive(bin.op.value) {
                    PrimitiveId::Op1(_) => unreachable!("unary primitive id for binary operation"),
                    PrimitiveId::Op2(op2) => self.bin_expr(op2, left, right, span),
                    PrimitiveId::HigherOp(op) => self.higher_bin_expr(op, left, right, span),
                }?
            }
            Expr::Call(call) => self.call(*call)?,
            Expr::Pipe(pipe_expr) => self.pipe_expr(*pipe_expr)?,
            Expr::List(items) => self.list(Instr::List, items)?,
            Expr::Array(items) => self.list(Instr::Array, items)?,
            Expr::Func(func) => self.func(*func, expr.span, true)?,
            Expr::Parened(inner) => {
                // Inline binary operators surrounded by parentheses
                if let Expr::Bin(bin) = &inner.value {
                    if let (Expr::Placeholder, Expr::Placeholder) =
                        (&bin.left.value, &bin.right.value)
                    {
                        self.function_binding(bin_op_primitive(bin.op.value).into());
                        return Ok(());
                    }
                }
                self.expr(preprocess_expr(*inner))?
            }
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
                    .push(span.sp(CompileError::UnknownBinding(ident)));
                (&Binding::Error, self.scope().function_depth)
            }
        };
        let mut binding = binding.clone();
        if binding_depth > 0 && binding_depth != self.scope().function_depth {
            let ipf = self.in_progress_functions.last_mut().unwrap();
            let pos = ipf.captures.iter().position(|id| id == &ident);
            let pos = pos.unwrap_or_else(|| {
                ipf.captures.push(ident);
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
            Binding::Error => self.push_instr(Instr::Push(Value::nil())),
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
            self.push_instr(Instr::PushUnresolvedFunction(id.clone().into()));
        }
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
        self.push_instr(Instr::Call(1, span));
        Ok(())
    }
    fn block(&mut self, block: Block) -> CompileResult {
        let height = self.push_scope();
        for item in block.items {
            self.item(item)?;
        }
        self.expr(preprocess_expr(block.expr))?;
        self.pop_scope(height);
        Ok(())
    }
    fn bin_expr(&mut self, op2: Op2, left: Sp<Expr>, right: Sp<Expr>, span: Span) -> CompileResult {
        let span = self.push_call_span(span);
        self.bin_expr_impl([Instr::Op2(op2, span)], left, right)
    }
    fn higher_bin_expr(
        &mut self,
        hop: HigherOp,
        left: Sp<Expr>,
        right: Sp<Expr>,
        span: Span,
    ) -> CompileResult {
        let span = self.push_call_span(span);
        let func = self.assembly.find_function(hop).unwrap();
        self.bin_expr_impl(
            [Instr::Push(func.into()), Instr::Call(2, span)],
            left,
            right,
        )
    }
    fn bin_expr_impl(
        &mut self,
        instrs: impl IntoIterator<Item = Instr>,
        left: Sp<Expr>,
        right: Sp<Expr>,
    ) -> CompileResult {
        self.expr(left)?;
        self.expr(right)?;
        self.push_instr(Instr::Swap);
        for instr in instrs {
            self.push_instr(instr);
        }
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
        self.func_outer(true, FunctionId::FormatString(span.clone()), span, |this| {
            let params = parts.len() - 1;
            let mut parts = parts.into_iter().rev();
            this.push_instr(Instr::Push(parts.next().unwrap().into()));
            for (i, part) in parts.enumerate() {
                this.push_instr(Instr::CopyRel(i + 2));
                this.push_instr(Instr::Op2(Op2::Join, 0));
                this.push_instr(Instr::Push(part.into()));
                this.push_instr(Instr::Op2(Op2::Join, 0));
            }
            Ok(params)
        })
    }
}

fn preprocess_expr(mut expr: Sp<Expr>) -> Sp<Expr> {
    let mut params = Vec::new();
    preprocess_expr_rec(&mut expr, &mut params);
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

fn preprocess_expr_rec(expr: &mut Sp<Expr>, params: &mut Vec<Sp<Ident>>) {
    match &mut expr.value {
        Expr::Placeholder => {
            let ident = expr.span.clone().sp(Ident::Placeholder(params.len()));
            params.push(ident.clone());
            *expr = ident.map(Expr::Ident);
        }
        Expr::Call(call_expr) => {
            preprocess_expr_rec(&mut call_expr.func, params);
            preprocess_expr_rec(&mut call_expr.arg, params);
        }
        Expr::Bin(bin_expr) => {
            preprocess_expr_rec(&mut bin_expr.left, params);
            preprocess_expr_rec(&mut bin_expr.right, params);
        }
        Expr::Pipe(pipe_expr) => {
            preprocess_expr_rec(&mut pipe_expr.left, params);
            preprocess_expr_rec(&mut pipe_expr.right, params);
        }
        Expr::List(items) | Expr::Array(items) => {
            for item in items {
                preprocess_expr_rec(item, params);
            }
        }
        Expr::Parened(_) => {}
        Expr::Unit
        | Expr::Func(_)
        | Expr::Real(_)
        | Expr::Char(_)
        | Expr::String(_)
        | Expr::FormatString(_)
        | Expr::Ident(_) => {}
    }
}

fn bin_op_primitive(op: BinOp) -> PrimitiveId {
    match op {
        BinOp::Add => PrimitiveId::Op2(Op2::Add),
        BinOp::Sub => PrimitiveId::Op2(Op2::Sub),
        BinOp::Mul => PrimitiveId::Op2(Op2::Mul),
        BinOp::Div => PrimitiveId::Op2(Op2::Div),
        BinOp::Eq => PrimitiveId::Op2(Op2::Eq),
        BinOp::Ne => PrimitiveId::Op2(Op2::Ne),
        BinOp::Lt => PrimitiveId::Op2(Op2::Lt),
        BinOp::Le => PrimitiveId::Op2(Op2::Le),
        BinOp::Gt => PrimitiveId::Op2(Op2::Gt),
        BinOp::Ge => PrimitiveId::Op2(Op2::Ge),
        BinOp::Left => PrimitiveId::Op2(Op2::Left),
        BinOp::Right => PrimitiveId::Op2(Op2::Right),
        BinOp::Compose => PrimitiveId::HigherOp(HigherOp::Compose),
        BinOp::BlackBird => PrimitiveId::HigherOp(HigherOp::BlackBird),
        BinOp::LeftLeaf => PrimitiveId::HigherOp(HigherOp::LeftLeaf),
        BinOp::RightLeaf => PrimitiveId::HigherOp(HigherOp::RightLeaf),
        BinOp::LeftTree => PrimitiveId::HigherOp(HigherOp::LeftTree),
        BinOp::RightTree => PrimitiveId::HigherOp(HigherOp::RightTree),
        BinOp::Slf => PrimitiveId::HigherOp(HigherOp::Slf),
    }
}
