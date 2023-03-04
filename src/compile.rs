use std::{collections::HashMap, fmt, fs, path::Path};

use enum_iterator::all;
use nohash_hasher::BuildNoHashHasher;

use crate::{
    ast::*,
    builtin::BuiltinOp2,
    lex::{Sp, Span},
    parse::{parse, ParseError},
    value::{Function, List, Value},
    vm::{run_assembly, Instr},
    Ident, UiuaError, UiuaResult,
};

#[derive(Debug)]
pub enum CompileError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(Ident),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileError::Parse(e) => write!(f, "{e}"),
            CompileError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CompileError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CompileError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
        }
    }
}

pub type CompileResult<T = ()> = Result<T, Sp<CompileError>>;

pub struct Compiler {
    /// Instructions for fully compiled functions
    function_instrs: Vec<Instr>,
    /// Instructions for stuff in the global scope
    global_instrs: Vec<Instr>,
    /// Instructions for functions that are currently being compiled
    in_progress_functions: Vec<Vec<Instr>>,
    /// Information about functions. This is passed to the Assembly.
    function_info: HashMap<Function, FunctionInfo, BuildNoHashHasher<Function>>,
    /// The span of each function call. This is passed to the Assembly.
    call_spans: Vec<Span>,
    /// Stack of scopes
    scopes: Vec<Scope>,
    /// The relative height of the runtime stack
    height: usize,
    /// Errors that don't stop compilation
    errors: Vec<Sp<CompileError>>,
}

struct Scope {
    /// How many functions deep the scope is. 0 is the global scope.
    function_depth: usize,
    /// Values and functions that are in scope
    bindings: HashMap<Ident, Binding>,
    /// Map of function ids to the actual function values
    functions: HashMap<FunctionId, Function>,
}

enum Binding {
    /// A function that is available but not on the stack.
    Function(FunctionId),
    /// A local variable is referenced by its height in the stack.
    Local(usize),
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
        // Initialize builtins
        // 2-parameter builtins
        for op2 in all::<BuiltinOp2>() {
            let function = Function(function_instrs.len());
            // Instructions
            function_instrs.push(Instr::BuiltinOp2(op2, Span::builtin()));
            function_instrs.push(Instr::Return);
            // Scope
            scope.bindings.insert(
                ascend::static_str(&op2.to_string()).into(),
                Binding::Function(FunctionId::Builtin2(op2)),
            );
            scope.functions.insert(FunctionId::Builtin2(op2), function);
            // Function info
            function_info.insert(
                function,
                FunctionInfo {
                    id: FunctionId::Builtin2(op2),
                    params: 2,
                },
            );
        }
        Self {
            function_instrs,
            global_instrs: vec![Instr::Comment("BEGIN".into())],
            in_progress_functions: Vec::new(),
            function_info,
            call_spans: Vec::new(),
            scopes: vec![scope],
            height: 0,
            errors: Vec::new(),
        }
    }
}

pub struct Assembly {
    pub(crate) instrs: Vec<Instr>,
    pub(crate) start: usize,
    function_info: HashMap<Function, FunctionInfo, BuildNoHashHasher<Function>>,
    pub(crate) call_spans: Vec<Span>,
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
    pub fn run(&self) -> UiuaResult {
        run_assembly(self)
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
        let function_start = self.function_instrs.len();
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
            self.function_instrs.truncate(function_start);
            self.global_instrs.truncate(global_start);
            Err(errors)
        }
    }
    pub fn finish(mut self) -> Assembly {
        let start = self.function_instrs.len();
        self.function_instrs.append(&mut self.global_instrs);
        for (i, instr) in self.function_instrs.iter().enumerate() {
            println!("{i:>3} {instr}");
        }
        Assembly {
            instrs: self.function_instrs,
            start,
            function_info: self.function_info,
            call_spans: self.call_spans,
        }
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
            Instr::Push(_) => self.height += 1,
            Instr::PushUnresolvedFunction(_) => self.height += 1,
            Instr::BinOp(..) => self.height -= 1,
            Instr::Call { args, .. } => self.height -= *args,
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
        let spot = self.call_spans.len();
        self.call_spans.push(span);
        spot
    }
    fn item(&mut self, item: Item) -> CompileResult {
        match item {
            Item::Expr(expr) => self.expr(resolve_placeholders(expr)),
            Item::Let(binding) => self.r#let(binding),
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
        // Initialize the function's instruction list
        self.in_progress_functions.push(Vec::new());
        // Push the function's scope
        let height = self.push_scope();
        // Push the function's name as a comment
        self.push_instr(Instr::Comment(match &func.id {
            FunctionId::Named(name) => format!("fn {name}"),
            FunctionId::Anonymous(span) => format!("fn at {span}"),
            FunctionId::Builtin2(_) => unreachable!("Builtin2 functions should not be compiled"),
        }));
        // Push and bind the function's parameters
        let params = func.params.len();
        for param in func.params.into_iter().rev() {
            self.height += 1;
            self.bind_local(param.value);
        }
        // Compile the function's body
        self.block(func.body)?;
        self.push_instr(Instr::Return);
        // Pop the function's scope
        self.pop_scope(height);
        //Determine the function's index
        let function = Function(self.function_instrs.len());
        // Resolve function references
        for instrs in &mut self.in_progress_functions {
            for instr in instrs {
                if matches!(instr, Instr::PushUnresolvedFunction(id) if *id == func.id) {
                    *instr = Instr::Push(function.into());
                }
            }
        }
        // Add the function's instructions to the global function list
        let instrs = self.in_progress_functions.pop().unwrap();
        self.function_instrs.extend(instrs);
        self.scope_mut().functions.insert(func.id.clone(), function);
        // Add to the function info map
        self.function_info.insert(
            function,
            FunctionInfo {
                id: func.id,
                params,
            },
        );
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
    fn pattern(&mut self, pattern: Sp<Pattern>) -> CompileResult {
        match pattern.value {
            Pattern::Ident(ident) => self.bind_local(ident),
            Pattern::List(patterns) => {
                let len = patterns.len();
                for pattern in patterns {
                    self.pattern(pattern)?;
                }
                self.push_instr(Instr::DestructureList(len, pattern.span.clone()));
            }
            Pattern::Discard => {}
        }
        Ok(())
    }
    fn expr(&mut self, expr: Sp<Expr>) -> CompileResult {
        match expr.value {
            Expr::Unit => self.push_instr(Instr::Push(Value::unit())),
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
            Expr::Ident(ident) => self.ident(ident, expr.span)?,
            Expr::Placeholder => panic!("unresolved placeholder"),
            Expr::Bin(bin) => {
                self.expr(bin.left)?;
                self.expr(bin.right)?;
                self.push_instr(Instr::BinOp(bin.op.value, bin.op.span.clone()));
            }
            Expr::Call(call) => self.call(*call)?,
            Expr::If(if_expr) => self.if_expr(*if_expr)?,
            Expr::Logic(log_expr) => self.logic_expr(*log_expr)?,
            Expr::List(items) => self.list(items)?,
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
                    self.push_instr(Instr::PushUnresolvedFunction(id.clone()));
                }
            }
            Binding::Error => self.push_instr(Instr::Push(Value::unit())),
        }
        Ok(())
    }
    fn list(&mut self, items: Vec<Sp<Expr>>) -> CompileResult {
        self.push_instr(Instr::Push(List::default().into()));
        let height = self.height;
        let push = self.scopes[0].functions[&FunctionId::Builtin2(BuiltinOp2::Push)];
        for item in items {
            let span = self.push_call_span(item.span.clone());
            self.expr(item)?;
            self.push_instr(Instr::Push(push.into()));
            self.push_instr(Instr::Call { args: 2, span });
        }
        self.height = height;
        Ok(())
    }
    fn call(&mut self, call: CallExpr) -> CompileResult {
        let args = call.args.len();
        for arg in call.args.into_iter().rev() {
            self.expr(arg)?;
        }
        let span = self.push_call_span(call.func.span.clone());
        self.expr(call.func)?;
        self.push_instr(Instr::Call { args, span });
        Ok(())
    }
    fn block(&mut self, block: Block) -> CompileResult {
        let height = self.push_scope();
        for binding in block.bindings {
            self.r#let(binding)?;
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
            bindings: Vec::new(),
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
            if if_expr.if_true.bindings.is_empty() && if_expr.if_false.bindings.is_empty() {
                resolve_placeholders_rec(&mut if_expr.if_true.expr, params);
                resolve_placeholders_rec(&mut if_expr.if_false.expr, params);
            }
        }
        Expr::Call(call_expr) => {
            resolve_placeholders_rec(&mut call_expr.func, params);
            for arg in &mut call_expr.args {
                resolve_placeholders_rec(arg, params);
            }
        }
        Expr::Bin(bin_expr) => {
            resolve_placeholders_rec(&mut bin_expr.left, params);
            resolve_placeholders_rec(&mut bin_expr.right, params);
        }
        Expr::Logic(log_expr) => {
            resolve_placeholders_rec(&mut log_expr.left, params);
            resolve_placeholders_rec(&mut log_expr.right, params);
        }
        Expr::List(items) => {
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
        | Expr::Ident(_) => {}
    }
}
