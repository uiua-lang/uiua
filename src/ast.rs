use crate::lex::Sp;

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
    pub name: Sp<String>,
    pub params: Vec<Param>,
    pub body: Block,
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
pub struct Block {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Tuple(Vec<Sp<Expr>>),
    Array(Vec<Sp<Expr>>),
    Integer(String),
    Real(String),
    Bool(bool),
    Bin(Box<BinExpr>),
    Un(Box<UnExpr>),
    Assign(Box<Assignment>),
    Block(Block),
    Call(Box<Call>),
    Access(Box<Access>),
}

#[derive(Debug, Clone)]
pub struct BinExpr {
    pub lhs: Sp<Expr>,
    pub op: Sp<BinOp>,
    pub rhs: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnExpr {
    pub op: Sp<UnOp>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub lvalue: Sp<Expr>,
    pub op: Option<Sp<BinOp>>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub expr: Sp<Expr>,
    pub args: Vec<Sp<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Access {
    pub expr: Sp<Expr>,
    pub accessor: Sp<Accessor>,
}

#[derive(Debug, Clone)]
pub enum LValue {
    Pattern(Pattern),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Ident(String),
    Tuple(Vec<Sp<Pattern>>),
}

#[derive(Debug, Clone)]
pub enum Accessor {
    Field(String),
    Index(Expr),
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}
