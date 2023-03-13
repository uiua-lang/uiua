use std::fmt;

use enum_iterator::Sequence;

use crate::{
    function::FunctionId,
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
    pub name: Sp<Ident>,
    pub func: Func,
}

#[derive(Clone)]
pub struct Func {
    pub id: FunctionId,
    pub params: Vec<Sp<Ident>>,
    pub body: Block,
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("func")
            .field("id", &self.id)
            .field("params", &self.params)
            .field("body", &self.body)
            .finish()
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

#[derive(Clone, Default)]
pub enum Expr {
    #[default]
    Unit,
    Call(Box<CallExpr>),
    Bin(Box<BinExpr>),
    Real(String),
    Char(char),
    String(String),
    FormatString(Vec<String>),
    Ident(Ident),
    Placeholder,
    List(Vec<Sp<Expr>>),
    Array(Vec<Sp<Expr>>),
    Strand(Vec<Sp<Expr>>),
    Parened(Box<Sp<Expr>>),
    Func(Box<Func>),
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unit => write!(f, "unit"),
            Expr::Call(call) => call.fmt(f),
            Expr::Bin(bin) => bin.fmt(f),
            Expr::Real(real) => write!(f, "{real:?}"),
            Expr::Char(char) => write!(f, "{char:?}"),
            Expr::String(string) => write!(f, "{string:?}"),
            Expr::FormatString(parts) => write!(f, "${parts:?}"),
            Expr::Ident(ident) => write!(f, "ident({ident})"),
            Expr::Placeholder => write!(f, "_"),
            Expr::List(list) => write!(f, "list({list:?})"),
            Expr::Array(array) => write!(f, "array({array:?})"),
            Expr::Strand(items) => write!(f, "strand({items:?})"),
            Expr::Parened(expr) => {
                write!(f, "(")?;
                expr.value.fmt(f)?;
                write!(f, ")")
            }
            Expr::Func(func) => func.fmt(f),
        }
    }
}

#[derive(Clone)]
pub enum Pattern {
    Ident(Ident),
    List(Vec<Sp<Pattern>>),
    Discard,
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Ident(ident) => write!(f, "ident({:?})", ident),
            Pattern::List(list) => write!(f, "list({:?})", list),
            Pattern::Discard => write!(f, "discard"),
        }
    }
}

#[derive(Clone)]
pub struct CallExpr {
    pub func: Sp<Expr>,
    pub arg: Sp<Expr>,
}

impl fmt::Debug for CallExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("call")
            .field(&self.func.value)
            .field(&self.arg.value)
            .finish()
    }
}

#[derive(Clone)]
pub struct BinExpr {
    pub left: Sp<Expr>,
    pub op: Sp<BinOp>,
    pub right: Sp<Expr>,
}

impl fmt::Debug for BinExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("bin")
            .field(&self.op.value)
            .field(&self.left.value)
            .field(&self.right.value)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BinOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Compose,
    BlackBird,
    Left,
    Right,
    LeftLeaf,
    LeftTree,
    RightLeaf,
    RightTree,
    Slf,
    Flip,
    Pipe,
    BackPipe,
}
