use std::fmt;

use crate::lex::Sp;

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(FunctionDef),
    Expr(Sp<Expr>, bool),
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
    pub name: Sp<String>,
    pub params: Vec<Param>,
    pub ret_ty: Option<Sp<Type>>,
    pub body: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Sp<String>,
    pub ty: Sp<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Ident(String),
    Array(Box<Type>),
    Tuple(Vec<Sp<Type>>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Struct(Struct),
    Enum(Enum),
    Ident(String),
    Tuple(Vec<Sp<Expr>>),
    Array(Vec<Sp<Expr>>),
    Integer(String),
    Real(String),
    Bool(bool),
    Bin(Box<BinExpr>),
    Un(Box<UnExpr>),
    If(Box<IfExpr>),
}

#[derive(Clone)]
pub struct BinExpr {
    pub lhs: Sp<Expr>,
    pub rhs: Vec<(Sp<BinOp>, Sp<Expr>)>,
}

impl fmt::Debug for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut tuple = f.debug_tuple("BinExpr");
        tuple.field(&self.lhs);
        for (op, rhs) in &self.rhs {
            tuple.field(&op);
            tuple.field(&rhs);
        }
        tuple.finish()
    }
}

#[derive(Debug, Clone)]
pub struct UnExpr {
    pub op: Sp<UnOp>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
    Tuple(Vec<Sp<Pattern>>),
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub cond: Sp<Expr>,
    pub if_true: Sp<Expr>,
    pub if_false: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Option<Sp<String>>,
    pub fields: Vec<Param>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: Option<Sp<String>>,
    pub variants: Vec<Sp<Variant>>,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub name: Sp<String>,
    pub fields: Option<Vec<Sp<Type>>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    And,
    Or,
    RangeEx,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}
