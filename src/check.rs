use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
    path::Path,
};

use mlua::{Lua, Value};

use crate::{
    ast::{self, LogicalOp},
    builtin::{BuiltinFn, CmpOp},
    lex::{Sp, Span},
    parse::{parse, ParseError},
    transpile::Transpiler,
    types::{FunctionType, Type},
};

#[derive(Debug)]
pub enum CheckError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(String),
    UnknownType(String),
    TypeMismatch(Type, Type),
    CallNonFunction(Type),
    WrongNumberOfArgs(usize, usize),
    OverrideWrongParamCount(String, usize, usize),
}

impl fmt::Display for CheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CheckError::Parse(e) => write!(f, "{e}"),
            CheckError::InvalidInteger(s) => write!(f, "invalid integer: {s}"),
            CheckError::InvalidReal(s) => write!(f, "invalid real: {s}"),
            CheckError::UnknownBinding(s) => write!(f, "unknown binding: {s}"),
            CheckError::UnknownType(s) => write!(f, "unknown type: {s}"),
            CheckError::TypeMismatch(expected, actual) => {
                write!(f, "type mismatch: expected {expected}, got {actual}")
            }
            CheckError::CallNonFunction(ty) => write!(f, "cannot call non-function: {ty}"),
            CheckError::WrongNumberOfArgs(expected, actual) => {
                write!(f, "wrong number of arguments: expected {expected}, got {actual}")
            }
            CheckError::OverrideWrongParamCount(name, expected, actual) => write!(
                f,
                "override `{name}` has wrong number of parameters: expected {expected}, got {actual}"
            ),
        }
    }
}

pub type CheckResult<T> = Result<T, Sp<CheckError>>;

#[derive(Debug, Clone)]
pub enum Item {
    Expr(TypedExpr),
    Binding(Binding),
    FunctionDef(FunctionDef),
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: String,
    pub func: Function,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Param>,
    pub body: Block,
}

impl Function {
    pub fn ty(&self, name: Option<String>) -> FunctionType {
        FunctionType::new(
            name,
            self.params.iter().map(|p| p.ty.clone()),
            self.body.expr.ty.clone(),
        )
    }
    pub fn is_comptime(&self) -> bool {
        self.params.iter().any(|p| p.ty == Type::Type)
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub bindings: Vec<Binding>,
    pub expr: TypedExpr,
}

impl Block {
    fn replace_ident(&mut self, old: &str, new: &str) {
        for binding in &mut self.bindings {
            binding.expr.expr.replace_ident(old, new);
            if binding.pattern.contains_ident(old) {
                return;
            }
        }
    }
}

impl From<TypedExpr> for Block {
    fn from(expr: TypedExpr) -> Self {
        Block {
            bindings: Vec::new(),
            expr,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Param<T = Type> {
    pub name: String,
    pub ty: T,
}

impl<T> Param<T> {
    pub fn new(name: String, ty: T) -> Self {
        Param { name, ty }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pattern: Pattern,
    pub expr: TypedExpr,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
    Tuple(Vec<Pattern>),
}

impl Pattern {
    fn contains_ident(&self, ident: &str) -> bool {
        match self {
            Pattern::Ident(i) => i == ident,
            Pattern::Tuple(patterns) => patterns.iter().any(|p| p.contains_ident(ident)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Ident(String),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    Type(Type),
    If(Box<IfExpr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Call(Box<CallExpr>),
    Logic(Box<LogicalExpr>),
    Function(Box<Function>),
    BuiltinFn(BuiltinFn),
}

impl From<IfExpr> for Expr {
    fn from(if_expr: IfExpr) -> Self {
        Expr::If(Box::new(if_expr))
    }
}

impl From<CallExpr> for Expr {
    fn from(call_expr: CallExpr) -> Self {
        Expr::Call(Box::new(call_expr))
    }
}

impl From<LogicalExpr> for Expr {
    fn from(logical_expr: LogicalExpr) -> Self {
        Expr::Logic(Box::new(logical_expr))
    }
}

impl From<Function> for Expr {
    fn from(func: Function) -> Self {
        Expr::Function(Box::new(func))
    }
}

impl Expr {
    pub fn typed(self, ty: Type) -> TypedExpr {
        TypedExpr { expr: self, ty }
    }
    fn replace_ident(&mut self, old: &str, new: &str) {
        match self {
            Expr::Ident(ident) if ident == old => *ident = new.to_string(),
            Expr::If(if_expr) => {
                if_expr.cond.replace_ident(old, new);
                if_expr.if_true.replace_ident(old, new);
                if_expr.if_false.replace_ident(old, new);
            }
            Expr::Tuple(exprs) => {
                for expr in exprs {
                    expr.replace_ident(old, new);
                }
            }
            Expr::List(exprs) => {
                for expr in exprs {
                    expr.replace_ident(old, new);
                }
            }
            Expr::Call(call_expr) => {
                call_expr.func.replace_ident(old, new);
                for arg in &mut call_expr.args {
                    arg.replace_ident(old, new);
                }
            }
            Expr::Function(func) => func.body.replace_ident(old, new),
            _ => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Expr,
    pub if_true: Block,
    pub if_false: Block,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Expr,
    pub op: LogicalOp,
    pub right: Expr,
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub expr: Expr,
    pub ty: Type,
}

impl TypedExpr {
    pub fn map(self, f: impl FnOnce(Expr) -> Expr) -> Self {
        TypedExpr {
            expr: f(self.expr),
            ty: self.ty,
        }
    }
}

pub struct Checker {
    pub(crate) transpiler: Transpiler,
    scopes: Vec<Scope>,
    pub(crate) functions: HashMap<String, Function>,
    monomorphized_functions: HashMap<(String, Vec<Type>), Function>,
    lua_functions: HashMap<Vec<u8>, Function>,
    pub(crate) types: HashMap<String, Type>,
    errors: Vec<Sp<CheckError>>,
    unknown_types: HashSet<String>,
    lua: Lua,
}

#[derive(Default)]
pub(crate) struct Scope {
    pub bindings: HashMap<String, Type>,
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            lua: Lua::new(),
            transpiler: Transpiler::default(),
            scopes: vec![Scope::default()],
            functions: HashMap::new(),
            monomorphized_functions: HashMap::new(),
            lua_functions: HashMap::new(),
            types: HashMap::new(),
            errors: Vec::new(),
            unknown_types: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum FindPriority {
    Binding,
    Function,
}

impl Checker {
    pub fn exec_lua(&mut self, lua_code: &str) {
        println!("{lua_code}");
        self.lua
            .load(lua_code)
            .exec()
            .unwrap_or_else(|e| panic!("{e}"))
    }
    pub fn load(&mut self, input: &str, path: &Path) -> Result<(), Vec<Sp<CheckError>>> {
        let (ast_items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CheckError>> =
            (errors.into_iter().map(|e| e.map(CheckError::Parse))).collect();
        for item in &ast_items {
            println!("{item:#?}");
        }
        for item in ast_items {
            match self.item(item) {
                Ok(item) => {
                    self.transpiler.item(item);
                    let lua_code = self.transpiler.take();
                    println!("{lua_code}");
                    if let Err(e) = self.lua.load(&lua_code).exec() {
                        eprintln!("{e}");
                    }
                }
                Err(e) => errors.push(e),
            }
            errors.append(&mut self.errors);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
    fn find_binding(&self, name: &str, priority: FindPriority) -> Option<Type> {
        let binding = || {
            self.scopes
                .iter()
                .rev()
                .find_map(|scope| scope.bindings.get(name))
                .cloned()
        };
        let function = || {
            self.functions
                .get(name)
                .map(|f| Type::Function(f.ty(Some(name.into())).into()))
        };
        match priority {
            FindPriority::Binding => binding().or_else(function),
            FindPriority::Function => function().or_else(binding),
        }
    }
    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    pub(crate) fn add_function(
        &mut self,
        name: &str,
        mut func: Function,
        span: Span,
    ) -> CheckResult<()> {
        let func = if let Some(inner) = self.functions.remove(name) {
            if inner.params.len() != func.params.len() {
                return Err(span.sp(CheckError::OverrideWrongParamCount(
                    name.into(),
                    inner.params.len(),
                    func.params.len(),
                )));
            }
            let type_params: Vec<Param> = inner
                .params
                .into_iter()
                .map(|param| Param {
                    name: param.name,
                    ty: Type::Type,
                })
                .collect();
            for (type_param, func_param) in type_params.iter().zip(&func.params) {
                func.body.replace_ident(&func_param.name, &type_param.name);
            }
            let mut cond = Expr::Bool(true);
            for param in func.params {
                let check_type_expr = CallExpr {
                    func: Expr::BuiltinFn(BuiltinFn::Cmp(CmpOp::Eq, Type::Type)),
                    args: vec![Expr::Ident(param.name), Expr::Ident(param.ty.to_string())],
                }
                .into();
                cond = LogicalExpr {
                    left: cond,
                    op: LogicalOp::And,
                    right: check_type_expr,
                }
                .into();
            }
            Function {
                params: type_params,
                body: Block {
                    bindings: Vec::new(),
                    expr: Expr::from(IfExpr {
                        cond,
                        if_true: func.body,
                        if_false: inner.body,
                    })
                    .typed(Type::Polymorphic),
                },
            }
        } else {
            func
        };
        self.functions.insert(name.into(), func);
        Ok(())
    }
    fn item(&mut self, item: ast::Item) -> CheckResult<Item> {
        Ok(match item {
            ast::Item::Expr(expr) => Item::Expr(self.expr(expr)?),
            ast::Item::Binding(binding) => Item::Binding(self.binding(binding)?),
            ast::Item::FunctionDef(def) => Item::FunctionDef(self.function_def(def)?),
        })
    }
    fn function_def(&mut self, def: ast::FunctionDef) -> CheckResult<FunctionDef> {
        self.scopes.push(Scope::default());
        let params: Vec<Param> = def
            .params
            .into_iter()
            .map(|p| self.param(p))
            .collect::<CheckResult<_>>()?;
        let mut ret_ty = if let Some(ret_ty) = def.ret_ty {
            self.ty(ret_ty)?
        } else {
            Type::Unit
        };
        let expr_span = def.body.expr.span.clone();
        let mut body = self.block(def.body)?;
        if !body.expr.ty.matches(&mut ret_ty) {
            return Err(expr_span.sp(CheckError::TypeMismatch(ret_ty, body.expr.ty)));
        }
        let func = Function { params, body };
        self.add_function(&def.name.value, func.clone(), def.name.span.clone())?;
        Ok(FunctionDef {
            name: def.name.value,
            func,
        })
    }
    fn block(&mut self, body: ast::Block) -> CheckResult<Block> {
        self.scopes.push(Scope::default());
        let bindings = body
            .bindings
            .into_iter()
            .map(|b| self.binding(b))
            .collect::<CheckResult<_>>()?;
        let expr = self.expr(body.expr)?;
        self.scopes.pop().unwrap();
        Ok(Block { bindings, expr })
    }
    fn binding(&mut self, binding: ast::Binding) -> CheckResult<Binding> {
        let expr_span = binding.expr.span.clone();
        let mut expr = self.expr(binding.expr)?;
        let mut ty = if let Some(ty) = binding.ty {
            self.ty(ty)?
        } else {
            Type::Unknown
        };
        if !expr.ty.matches(&mut ty) {
            return Err(expr_span.sp(CheckError::TypeMismatch(ty, expr.ty)));
        }
        let pattern = self.pattern(binding.pattern, expr.ty.clone())?;
        Ok(Binding { pattern, expr })
    }
    fn param(&mut self, param: ast::Param) -> CheckResult<Param> {
        let ty = self.ty(param.ty)?;
        self.scope_mut()
            .bindings
            .insert(param.name.value.clone(), ty.clone());
        Ok(Param {
            name: param.name.value,
            ty,
        })
    }
    fn pattern(&mut self, pattern: Sp<ast::Pattern>, expr_ty: Type) -> CheckResult<Pattern> {
        Ok(match pattern.value {
            ast::Pattern::Ident(name) => {
                self.scope_mut().bindings.insert(name.clone(), expr_ty);
                Pattern::Ident(name)
            }
            ast::Pattern::Tuple(patterns) => {
                Pattern::Tuple(if let Type::Tuple(types) = expr_ty.clone() {
                    if types.len() != patterns.len() {
                        return Err(pattern.span.sp(CheckError::TypeMismatch(
                            Type::Tuple(vec![Type::Unknown; patterns.len()]),
                            expr_ty,
                        )));
                    }
                    patterns
                        .into_iter()
                        .zip(types)
                        .map(|(p, t)| self.pattern(p, t))
                        .collect::<CheckResult<_>>()?
                } else {
                    return Err(pattern.span.sp(CheckError::TypeMismatch(
                        Type::Tuple(vec![Type::Unknown; patterns.len()]),
                        expr_ty,
                    )));
                })
            }
        })
    }
    fn expr(&mut self, expr: Sp<ast::Expr>) -> CheckResult<TypedExpr> {
        self.priority_expr(expr, FindPriority::Binding)
    }
    fn priority_expr(
        &mut self,
        expr: Sp<ast::Expr>,
        priority: FindPriority,
    ) -> CheckResult<TypedExpr> {
        Ok(match expr.value {
            ast::Expr::Unit => Expr::Unit.typed(Type::Unit),
            ast::Expr::If(if_expr) => self.if_expr(*if_expr)?,
            ast::Expr::Call(call) => self.call_expr(*call)?,
            ast::Expr::Logic(log_expr) => self.logic_expr(*log_expr)?,
            ast::Expr::Struct(_) => todo!(),
            ast::Expr::Enum(_) => todo!(),
            ast::Expr::Bool(b) => Expr::Bool(b).typed(Type::Bool),
            ast::Expr::Integer(i) => Expr::Int(
                i.parse()
                    .map_err(|_| expr.span.sp(CheckError::InvalidInteger(i)))?,
            )
            .typed(Type::UnknownInt),
            ast::Expr::Real(r) => Expr::Real(
                r.parse()
                    .map_err(|_| expr.span.sp(CheckError::InvalidReal(r)))?,
            )
            .typed(Type::Real),
            ast::Expr::Ident(name) => {
                if let Some(ty) = self.find_binding(&name, priority) {
                    Expr::Ident(name).typed(ty)
                } else if let Some(ty) = self.types.get(&name) {
                    Expr::Type(ty.clone()).typed(Type::Type)
                } else {
                    return Err(expr.span.sp(CheckError::UnknownBinding(name)));
                }
            }
            ast::Expr::Tuple(items) => {
                let items: Vec<TypedExpr> = items
                    .into_iter()
                    .map(|item| self.expr(item))
                    .collect::<CheckResult<_>>()?;
                let mut exprs = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    exprs.push(item.expr);
                    types.push(item.ty);
                }
                Expr::Tuple(exprs).typed(Type::Tuple(types))
            }
            ast::Expr::List(items) => {
                let mut ty: Option<Type> = None;
                let mut exprs = Vec::new();
                for item in items {
                    let mut item = self.expr(item)?;
                    let ty = ty.get_or_insert(item.ty.clone());
                    if !ty.matches(&mut item.ty) {
                        return Err(expr.span.sp(CheckError::TypeMismatch(ty.clone(), item.ty)));
                    }
                    exprs.push(item.expr);
                }
                Expr::List(exprs).typed(Type::List(Box::new(ty.unwrap_or(Type::Unknown))))
            }
            ast::Expr::Parened(inner) => self.expr(expr.span.sp(*inner))?,
        })
    }
    fn ty(&mut self, ty: Sp<ast::Type>) -> CheckResult<Type> {
        Ok(match ty.value {
            ast::Type::Unit => Type::Unit,
            ast::Type::Unknown => Type::Unknown,
            ast::Type::Ident(name) => {
                if let Some(ty) = self.types.get(&name) {
                    ty.clone()
                } else {
                    if !self.unknown_types.contains(&name) {
                        self.errors
                            .push(ty.span.sp(CheckError::UnknownType(name.clone())));
                        self.unknown_types.insert(name);
                    }
                    Type::Unknown
                }
            }
            ast::Type::List(item) => Type::List(Box::new(self.ty(ty.span.sp(*item))?)),
            ast::Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|item| self.ty(item))
                    .collect::<CheckResult<_>>()?,
            ),
            ast::Type::Function(func_ty) => Type::Function(Box::new(FunctionType::new(
                None,
                func_ty
                    .params
                    .into_iter()
                    .map(|param| self.ty(param))
                    .collect::<CheckResult<Vec<_>>>()?,
                func_ty
                    .ret
                    .map(|ty| self.ty(ty))
                    .transpose()?
                    .unwrap_or(Type::Unit),
            ))),
            ast::Type::Parened(inner) => self.ty(ty.span.sp(*inner))?,
        })
    }
    fn if_expr(&mut self, if_expr: ast::IfExpr) -> CheckResult<TypedExpr> {
        let cond_span = if_expr.cond.span.clone();
        let mut cond = self.expr(if_expr.cond)?;
        if !cond.ty.matches(&mut Type::Bool) {
            return Err(cond_span.sp(CheckError::TypeMismatch(Type::Bool, cond.ty)));
        }
        let mut if_true = self.block(if_expr.if_true)?;
        let if_false_span = if_expr.if_false.expr.span.clone();
        let mut if_false = self.block(if_expr.if_false)?;
        if !if_true.expr.ty.matches(&mut if_false.expr.ty) {
            return Err(
                if_false_span.sp(CheckError::TypeMismatch(if_true.expr.ty, if_false.expr.ty))
            );
        }
        let ty = if_true.expr.ty.clone();
        Ok(Expr::from(IfExpr {
            cond: cond.expr,
            if_true,
            if_false,
        })
        .typed(ty))
    }
    fn call_expr(&mut self, call: ast::CallExpr) -> CheckResult<TypedExpr> {
        let func_span = call.func.span.clone();
        let expr = match call.func.value {
            ast::CallKind::Normal(func) => {
                self.priority_expr(call.func.span.sp(func), FindPriority::Function)?
            }
            ast::CallKind::Binary(op) => {
                let ast_expr = call.func.span.sp(ast::Expr::Ident(op.name().into()));
                self.expr(ast_expr)?
            }
        };
        let func = expr.expr;
        let func_type = expr.ty;
        // Ensure it is a function that is being called
        let Type::Function(mut func_type) = func_type else {
            return Err(func_span.sp(CheckError::CallNonFunction(func_type)));
        };
        let mut args = Vec::new();
        let mut arg_types = Vec::new();
        for ast_arg in call.args.iter().cloned() {
            let arg = self.expr(ast_arg)?;
            args.push(arg.expr);
            arg_types.push(arg.ty);
        }
        let checking_type_equality = matches!(func, Expr::BuiltinFn(BuiltinFn::Cmp(CmpOp::Eq, _)))
            && matches!(arg_types.as_slice(), [Type::Type, Type::Type]);
        if func_type.is_comptime() && !checking_type_equality {
            let name = func_type
                .name
                .clone()
                .unwrap_or_else(|| panic!("Comptime function has no name"));
            self.monomorphized_functions
                .entry((name.clone(), arg_types.clone()))
                .or_insert_with(|| {
                    let func: mlua::Function =
                        self.lua.globals().get(name.as_str()).unwrap_or_else(|e| {
                            panic!("unabled to find `{name}` function in lua: {e}")
                        });
                    let lua_ret: mlua::Function = func
                        .call(
                            arg_types
                                .iter()
                                .map(|ty| ty.to_string())
                                .collect::<Vec<_>>(),
                        )
                        .unwrap();
                    dbg!(lua_ret);
                    todo!()
                });
        }
        // Check arguments
        let mut args = Vec::new();
        let mut arg_types = Vec::new();
        let call_arg_count = call.args.len();
        for (param_ty, ast_arg) in func_type.params.iter_mut().zip(call.args) {
            let arg_span = ast_arg.span.clone();
            let mut arg = self.expr(ast_arg)?;
            if !param_ty.matches(&mut arg.ty) {
                return Err(arg_span.sp(CheckError::TypeMismatch(param_ty.clone(), arg.ty)));
            }
            args.push(arg.expr);
            arg_types.push(arg.ty);
        }
        // Figure out the return type
        let ty = match func_type.params.len().cmp(&call_arg_count) {
            // Too many arguments
            Ordering::Less => {
                return Err(func_span.sp(CheckError::WrongNumberOfArgs(
                    func_type.params.len(),
                    call_arg_count,
                )));
            }
            // All arguments are provided
            Ordering::Equal => func_type.ret.clone(),
            // Partial application
            Ordering::Greater => todo!("partial application"),
        };
        Ok(Expr::from(CallExpr { func, args }).typed(ty))
    }
    fn logic_expr(&mut self, log_expr: ast::LogicalExpr) -> CheckResult<TypedExpr> {
        let left_span = log_expr.left.span.clone();
        let right_span = log_expr.right.span.clone();
        let mut left = self.expr(log_expr.left)?;
        let mut right = self.expr(log_expr.right)?;
        if !left.ty.matches(&mut Type::Bool) {
            return Err(left_span.sp(CheckError::TypeMismatch(Type::Bool, left.ty)));
        }
        if !right.ty.matches(&mut Type::Bool) {
            return Err(right_span.sp(CheckError::TypeMismatch(Type::Bool, right.ty)));
        }
        Ok(Expr::from(LogicalExpr {
            op: log_expr.op.value,
            left: left.expr,
            right: right.expr,
        })
        .typed(Type::Bool))
    }
}
