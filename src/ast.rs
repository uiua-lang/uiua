use std::{fmt, sync::Arc};

use enum_iterator::Sequence;

use crate::{
    builtin::{Algorithm, Op1, Op2},
    lex::{Sp, Span},
    Ident,
};

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(FunctionDef),
    Expr(Sp<Expr>),
    Let(Let),
    Const(Const),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::FunctionDef(func) => func
                .name
                .span
                .clone()
                .merge(func.func.body.expr.span.clone()),
            Item::Expr(expr) => expr.span.clone(),
            Item::Let(r#let) => r#let.pattern.span.clone().merge(r#let.expr.span.clone()),
            Item::Const(r#const) => r#const.name.span.clone().merge(r#const.expr.span.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub pattern: Sp<Pattern>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub name: Sp<Ident>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub doc: Option<Sp<String>>,
    pub name: Sp<Ident>,
    pub func: Func,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub id: FunctionId,
    pub params: Vec<Sp<Ident>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(Span),
    FormatString(Span),
    Op1(Op1),
    Op2(Op2),
    Algorithm(Algorithm),
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "`{name}`"),
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::FormatString(span) => write!(f, "format string from {span}"),
            FunctionId::Op1(op) => write!(f, "`{op}`"),
            FunctionId::Op2(op) => write!(f, "`{op}`"),
            FunctionId::Algorithm(alg) => write!(f, "`{alg}`"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub items: Vec<Item>,
    pub expr: Sp<Expr>,
}

impl From<Sp<Expr>> for Block {
    fn from(expr: Sp<Expr>) -> Self {
        Self {
            items: Vec::new(),
            expr,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Expr {
    #[default]
    Unit,
    If(Box<IfExpr>),
    Call(Box<CallExpr>),
    Bin(Box<BinExpr>),
    Logic(Box<LogicExpr>),
    Pipe(Box<PipeExpr>),
    Bool(bool),
    Int(String),
    Real(String),
    Char(char),
    String(Arc<String>),
    FormatString(Vec<String>),
    Ident(Ident),
    Placeholder,
    List(Vec<Sp<Expr>>),
    Array(Vec<Sp<Expr>>),
    Parened(Box<Sp<Expr>>),
    Func(Box<Func>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Ident),
    List(Vec<Sp<Pattern>>),
    Discard,
}

#[derive(Debug, Clone)]
pub struct LogicExpr {
    pub left: Sp<Expr>,
    pub op: Sp<LogicOp>,
    pub right: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct PipeExpr {
    pub left: Sp<Expr>,
    pub op: Sp<PipeOp>,
    pub right: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Sp<Expr>,
    pub if_true: Block,
    pub if_false: Block,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub func: Sp<Expr>,
    pub args: Vec<Sp<Expr>>,
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub left: Sp<Expr>,
    pub op: Sp<BinOp>,
    pub right: Sp<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PipeOp {
    Forward,
    Backward,
}
