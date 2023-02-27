use crate::lex::Sp;

#[derive(Debug, Clone)]
pub enum Item {
    FunctionDef(FunctionDef),
    Expr(Sp<Expr>),
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
    pub exprs: Vec<Sp<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Tuple(Vec<Sp<Expr>>),
    Integer(String),
    Real(String),
    Bin(Box<BinExpr>),
    Un(Box<UnExpr>),
    Assign(Box<Assignment>),
    Block(Block),
    Call(Box<Call>),
    Access(Sp<Box<Expr>>, Sp<Box<Accessor>>),
}

impl Expr {
    pub fn as_pattern(&self) -> Option<Pattern> {
        match self {
            Expr::Ident(name) => Some(Pattern::Ident(name.clone())),
            Expr::Tuple(exprs) => {
                let mut patterns = Vec::new();
                for expr in exprs {
                    patterns.push(expr.span.clone().sp(expr.value.as_pattern()?));
                }
                Some(Pattern::Tuple(patterns))
            }
            _ => None,
        }
    }
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
    pub lvalue: Sp<LValue>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub expr: Sp<Expr>,
    pub args: Vec<Sp<Expr>>,
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
