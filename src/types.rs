use std::fmt;

use crate::{
    ast::Expr,
    lex::Sp,
    transpile::{TranspileError, TranspileResult, Transpiler},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Int,
    Real,
    Function(Box<FunctionType>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Unkown,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Nat => write!(f, "nat"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function(func) => write!(f, "{func}"),
            Type::List(ty) => write!(f, "[{}]", ty),
            Type::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Type::Unkown => write!(f, "unknown"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: Type,
}

impl FunctionType {
    pub fn new(params: impl IntoIterator<Item = Type>, ret: Type) -> Self {
        Self {
            params: params.into_iter().collect(),
            ret,
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ")")?;
        if self.ret != Type::Unit {
            write!(f, " -> {}", self.ret)
        } else {
            Ok(())
        }
    }
}

impl Transpiler {
    pub fn expr_type(&self, expr: &Sp<Expr>) -> TranspileResult<Type> {
        Ok(match &expr.value {
            Expr::Bin(_) => todo!(),
            Expr::Un(_) => todo!(),
            Expr::If(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::Struct(_) => todo!(),
            Expr::Enum(_) => todo!(),
            Expr::Bool(_) => Type::Bool,
            Expr::Integer(_) => Type::Int,
            Expr::Real(_) => Type::Real,
            Expr::Ident(name) => self.find_binding(name).ok_or_else(|| {
                expr.span
                    .clone()
                    .sp(TranspileError::UnknownBinding(name.clone()))
            })?,
            Expr::Tuple(items) => Type::Tuple(
                items
                    .iter()
                    .map(|item| self.expr_type(item))
                    .collect::<TranspileResult<_>>()?,
            ),
            Expr::List(_) => todo!(),
            Expr::Parened(inner) => self.expr_type(&expr.span.clone().sp((**inner).clone()))?,
        })
    }
}
