use std::{collections::HashMap, fmt, path::Path};

use crate::{
    ast,
    lex::Sp,
    parse::{parse, ParseError},
    types::Type,
};

#[derive(Debug)]
pub enum CheckError {
    Parse(ParseError),
    InvalidInteger(String),
    InvalidReal(String),
    UnknownBinding(String),
    UnknownType(String),
    TypeMismatch(Type, Type),
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
    Ident(String),
    Nat(u64),
    Int(i64),
    Real(f64),
    If(Box<IfExpr>),
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

pub struct TypedExpr {
    pub expr: Expr,
    pub ty: Type,
}

pub struct Checker {
    scopes: Vec<Scope>,
    types: HashMap<String, Type>,
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, Type>,
}

impl Default for Checker {
    fn default() -> Self {
        Checker {
            scopes: vec![Scope::default()],
            types: HashMap::new(),
        }
    }
}

impl Checker {
    pub fn load(&mut self, input: &str, path: &Path) -> (Vec<Item>, Vec<Sp<CheckError>>) {
        let (ast_items, errors) = parse(input, path);
        let mut errors: Vec<Sp<CheckError>> = errors
            .into_iter()
            .map(|e| e.map(CheckError::Parse))
            .collect();
        let mut items = Vec::new();
        for item in ast_items {
            match self.item(item) {
                Ok(item) => items.push(item),
                Err(e) => errors.push(e),
            }
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
    fn scope_mut(&mut self) -> &mut Scope {
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
                let expr = self.expr(def.expr)?;
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
        let expr = self.expr(binding.expr)?;
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
                            Type::Tuple(vec![Type::Unkown; patterns.len()]),
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
                        Type::Tuple(vec![Type::Unkown; patterns.len()]),
                        expr_ty,
                    )));
                })
            }
        })
    }
    fn expr(&mut self, expr: Sp<ast::Expr>) -> CheckResult<TypedExpr> {
        Ok(match expr.value {
            ast::Expr::Bin(_) => todo!(),
            ast::Expr::Un(_) => todo!(),
            ast::Expr::If(if_expr) => self.if_expr(*if_expr)?,
            ast::Expr::Call(_) => todo!(),
            ast::Expr::Struct(_) => todo!(),
            ast::Expr::Enum(_) => todo!(),
            ast::Expr::Bool(_) => todo!(),
            ast::Expr::Integer(i) => Expr::Int(
                i.parse()
                    .map_err(|_| expr.span.sp(CheckError::InvalidInteger(i)))?,
            )
            .typed(Type::Int),
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
            ast::Expr::Tuple(_) => todo!(),
            ast::Expr::List(_) => todo!(),
            ast::Expr::Parened(_) => todo!(),
        })
    }
    fn ty(&mut self, ty: Sp<ast::Type>) -> CheckResult<Type> {
        Ok(match ty.value {
            ast::Type::Ident(name) => self
                .types
                .get(&name)
                .cloned()
                .ok_or_else(|| ty.span.sp(CheckError::UnknownType(name)))?,
            ast::Type::List(item) => Type::List(Box::new(self.ty(ty.span.sp(*item))?)),
            ast::Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|item| self.ty(item))
                    .collect::<CheckResult<_>>()?,
            ),
        })
    }
    fn if_expr(&mut self, if_expr: ast::IfExpr) -> CheckResult<TypedExpr> {
        let cond_span = if_expr.cond.span.clone();
        let cond = self.expr(if_expr.cond)?;
        if cond.ty != Type::Bool {
            return Err(cond_span.sp(CheckError::TypeMismatch(Type::Bool, cond.ty)));
        }
        let if_true = self.expr(if_expr.if_true)?;
        let if_false_span = if_expr.if_false.span.clone();
        let if_false = self.expr(if_expr.if_false)?;
        if if_true.ty != if_false.ty {
            return Err(if_false_span.sp(CheckError::TypeMismatch(if_true.ty, if_false.ty)));
        }
        Ok(Expr::If(Box::new(IfExpr {
            cond: cond.expr,
            if_true: if_true.expr,
            if_false: if_false.expr,
        }))
        .typed(if_true.ty))
    }
}
