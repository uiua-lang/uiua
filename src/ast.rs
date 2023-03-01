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
    pub ty: Option<Sp<Type>>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub doc: Option<Sp<String>>,
    pub name: Sp<String>,
    pub params: Vec<Param>,
    pub ret_ty: Option<Sp<Type>>,
    pub bindings: Vec<Binding>,
    pub expr: Sp<Expr>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Sp<String>,
    pub ty: Sp<Type>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Ident(String),
    List(Box<Type>),
    Tuple(Vec<Sp<Type>>),
    Function(Box<FunctionType>),
    Parened(Box<Type>),
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub params: Vec<Sp<Type>>,
    pub ret: Option<Sp<Type>>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    If(Box<IfExpr>),
    Call(Box<CallExpr>),
    Struct(Struct),
    Enum(Enum),
    Bool(bool),
    Integer(String),
    Real(String),
    Ident(String),
    Tuple(Vec<Sp<Expr>>),
    List(Vec<Sp<Expr>>),
    Parened(Box<Expr>),
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
pub struct CallExpr {
    pub func: Sp<CallKind>,
    pub args: Vec<Sp<Expr>>,
}

#[derive(Debug, Clone)]
pub enum CallKind {
    Normal(Expr),
    Unary(UnOp),
    Binary(BinOp),
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
