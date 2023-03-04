use enum_iterator::Sequence;

use crate::{
    lex::{Sp, Span},
    Ident,
};

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(FunctionDef),
    Expr(Sp<Expr>),
    Let(Let),
}

#[derive(Debug, Clone)]
pub struct Let {
    pub pattern: Sp<Pattern>,
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
}

#[derive(Debug, Clone)]
pub struct Block {
    pub bindings: Vec<Let>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    If(Box<IfExpr>),
    Call(Box<CallExpr>),
    Bin(Box<BinExpr>),
    Logic(Box<LogicExpr>),
    Bool(bool),
    Int(String),
    Real(String),
    Ident(Ident),
    Placeholder,
    List(Vec<Sp<Expr>>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOp {
    And,
    Or,
}
