use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt,
    path::Path,
};

use crate::{
    ast::{self, BinOp, UnOp},
    lex::Sp,
    parse::{parse, ParseError},
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
    TooManyArguments(usize, usize),
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
            CheckError::TooManyArguments(expected, actual) => {
                write!(f, "too many arguments: expected {expected}, got {actual}")
            }
        }
    }
}

pub type CheckResult<T> = Result<T, Sp<CheckError>>;

pub enum Item {
    Expr(TypedExpr),
    Binding(Binding),
    FunctionDef(FunctionDef),
}

pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub bindings: Vec<Binding>,
    pub expr: TypedExpr,
}

pub struct Param {
    pub name: String,
    pub ty: Type,
}

pub struct Binding {
    pub pattern: Pattern,
    pub expr: TypedExpr,
}

pub enum Pattern {
    Ident(String),
    Tuple(Vec<Pattern>),
}

pub enum Expr {
    Unit,
    Ident(String),
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    If(Box<IfExpr>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    Call(Box<CallExpr>),
}

impl Expr {
    fn typed(self, ty: Type) -> TypedExpr {
        TypedExpr { expr: self, ty }
    }
}

pub struct IfExpr {
    pub cond: Expr,
    pub if_true: Expr,
    pub if_false: Expr,
}

pub struct CallExpr {
    pub func: CallKind,
    pub args: Vec<Expr>,
}

pub enum CallKind {
    Normal(Expr),
    Binary(BinOp),
    Unary(UnOp),
}

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
    scopes: Vec<Scope>,
    pub(crate) types: HashMap<String, Type>,
    errors: Vec<Sp<CheckError>>,
    unknown_types: HashSet<String>,
}

#[derive(Default)]
pub(crate) struct Scope {
    pub bindings: HashMap<String, Type>,
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            scopes: vec![Scope::default()],
            types: HashMap::new(),
            errors: Vec::new(),
            unknown_types: HashSet::new(),
        }
    }
}

impl Checker {
    pub fn load(&mut self, input: &str, path: &Path) -> (Vec<Item>, Vec<Sp<CheckError>>) {
        let (ast_items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CheckError>> =
            (errors.into_iter().map(|e| e.map(CheckError::Parse))).collect();
        let mut items = Vec::new();
        for item in ast_items {
            match self.item(item) {
                Ok(item) => items.push(item),
                Err(e) => errors.push(e),
            }
            errors.append(&mut self.errors);
        }
        (items, errors)
    }
    fn find_type(&self, name: &str) -> Option<Type> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.bindings.get(name))
            .cloned()
    }
    pub(crate) fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
    fn item(&mut self, item: ast::Item) -> CheckResult<Item> {
        Ok(match item {
            ast::Item::Expr(expr) => Item::Expr(self.expr(expr)?),
            ast::Item::Binding(binding) => Item::Binding(self.binding(binding)?),
            ast::Item::FunctionDef(def) => {
                self.scopes.push(Scope::default());
                let params = def
                    .params
                    .into_iter()
                    .map(|p| self.param(p))
                    .collect::<CheckResult<_>>()?;
                let bindings = def
                    .bindings
                    .into_iter()
                    .map(|b| self.binding(b))
                    .collect::<CheckResult<_>>()?;
                let expr_span = def.expr.span.clone();
                let mut expr = self.expr(def.expr)?;
                let mut ret_ty = if let Some(ret_ty) = def.ret_ty {
                    self.ty(ret_ty)?
                } else {
                    Type::Unit
                };
                if !expr.ty.matches(&mut ret_ty) {
                    return Err(expr_span.sp(CheckError::TypeMismatch(ret_ty, expr.ty)));
                }
                self.scopes.pop().unwrap();
                Item::FunctionDef(FunctionDef {
                    name: def.name.value,
                    params,
                    bindings,
                    expr,
                })
            }
        })
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
        Ok(match expr.value {
            ast::Expr::Unit => Expr::Unit.typed(Type::Unit),
            ast::Expr::If(if_expr) => self.if_expr(*if_expr)?,
            ast::Expr::Call(call) => self.call_expr(*call)?,
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
                if let Some(ty) = self.find_type(&name) {
                    Expr::Ident(name).typed(ty)
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
            ast::Type::Function(func_ty) => Type::Function(Box::new(FunctionType {
                params: func_ty
                    .params
                    .into_iter()
                    .map(|param| self.ty(param))
                    .collect::<CheckResult<_>>()?,
                ret: func_ty
                    .ret
                    .map(|ty| self.ty(ty))
                    .transpose()?
                    .unwrap_or(Type::Unit),
            })),
            ast::Type::Parened(inner) => self.ty(ty.span.sp(*inner))?,
        })
    }
    fn if_expr(&mut self, if_expr: ast::IfExpr) -> CheckResult<TypedExpr> {
        let cond_span = if_expr.cond.span.clone();
        let mut cond = self.expr(if_expr.cond)?;
        if !cond.ty.matches(&mut Type::Bool) {
            return Err(cond_span.sp(CheckError::TypeMismatch(Type::Bool, cond.ty)));
        }
        let mut if_true = self.expr(if_expr.if_true)?;
        let if_false_span = if_expr.if_false.span.clone();
        let mut if_false = self.expr(if_expr.if_false)?;
        if !if_true.ty.matches(&mut if_false.ty) {
            return Err(if_false_span.sp(CheckError::TypeMismatch(if_true.ty, if_false.ty)));
        }
        Ok(Expr::If(Box::new(IfExpr {
            cond: cond.expr,
            if_true: if_true.expr,
            if_false: if_false.expr,
        }))
        .typed(if_true.ty))
    }
    fn call_expr(&mut self, call: ast::CallExpr) -> CheckResult<TypedExpr> {
        let func_span = call.func.span.clone();
        let (call_kind, func_type) = match call.func.value {
            ast::CallKind::Normal(func) => {
                let expr = self.expr(call.func.span.sp(func))?;
                (CallKind::Normal(expr.expr), expr.ty)
            }
            ast::CallKind::Unary(_) => todo!(),
            ast::CallKind::Binary(_) => todo!(),
        };
        // Ensure it is a function that is being called
        let Type::Function(mut func_type) = func_type else {
            return Err(func_span.sp(CheckError::CallNonFunction(func_type)));
        };
        // Check arguments
        let mut args = Vec::new();
        let mut arg_types = Vec::new();
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
        let ty = match func_type.params.len().cmp(&args.len()) {
            // Too many arguments
            Ordering::Less => {
                return Err(func_span.sp(CheckError::TooManyArguments(
                    func_type.params.len(),
                    args.len(),
                )));
            }
            // All arguments are provided
            Ordering::Equal => func_type.ret.clone(),
            // Partial application
            Ordering::Greater => Type::Function(Box::new(FunctionType {
                params: func_type.params[args.len()..].to_vec(),
                ret: func_type.ret.clone(),
            })),
        };
        Ok(Expr::Call(Box::new(CallExpr {
            func: call_kind,
            args,
        }))
        .typed(ty))
    }
}
