use enum_iterator::Sequence;

use crate::lex::{Ident, Sp};

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(FunctionDef),
    Expr(Sp<Expr>),
    Binding(Binding),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub pattern: Sp<Pattern>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub doc: Option<Sp<String>>,
    pub name: Sp<Ident>,
    pub params: Vec<Sp<Ident>>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub bindings: Vec<Binding>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    If(Box<IfExpr>),
    Call(Box<CallExpr>),
    Bin(Box<BinExpr>),
    Logic(Box<LogicalExpr>),
    Bool(bool),
    Integer(String),
    Real(String),
    Ident(Ident),
    List(Vec<Sp<Expr>>),
    Parened(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(Ident),
    Tuple(Vec<Sp<Pattern>>),
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Sp<Expr>,
    pub op: Sp<LogicalOp>,
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
    RangeEx,
}

impl BinOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinOp::Add => "add",
            BinOp::Sub => "sub",
            BinOp::Mul => "mul",
            BinOp::Div => "div",
            BinOp::Eq => "eq",
            BinOp::Ne => "ne",
            BinOp::Lt => "lt",
            BinOp::Gt => "gt",
            BinOp::Le => "le",
            BinOp::Ge => "ge",
            BinOp::RangeEx => "range_ex",
        }
    }
    pub fn lua_op(&self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Eq => "==",
            BinOp::Ne => "~=",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::RangeEx => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOp {
    And,
    Or,
}
