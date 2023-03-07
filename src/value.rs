use std::{fmt, sync::Arc};

use crate::{array::Array, list::List};

#[derive(Clone, Default)]
pub enum Value {
    #[default]
    Unit,
    Bool(bool),
    Byte(u8),
    Int(i64),
    Real(f64),
    Char(char),
    Function(Function),
    Partial(Box<Partial>),
    String(Arc<String>),
    List(Box<List>),
    Array(Box<Array>),
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::Unit) };
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{:?}", s),
            Self::List(l) => write!(f, "{:?}", l),
            Self::Array(a) => write!(f, "{:?}", a),
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Char(c) => write!(f, "{}", c),
            Self::Function(func) => write!(f, "function({})", func.0),
            Self::Partial(p) => write!(f, "partial({} {})", p.function.0, p.args.len()),
            Self::String(s) => write!(f, "{}", s),
            Self::List(l) => write!(f, "{}", l),
            Self::Array(a) => write!(f, "{}", a),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Unit,
    Bool,
    Byte,
    Int,
    Real,
    Char,
    Function,
    String,
    List,
    Array,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Byte => write!(f, "byte"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Char => write!(f, "char"),
            Type::Function => write!(f, "function"),
            Type::String => write!(f, "string"),
            Type::List => write!(f, "list"),
            Type::Array => write!(f, "array"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Function(pub(crate) usize);

impl nohash_hasher::IsEnabled for Function {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Arc<[Value]>,
}

impl Value {
    pub const fn ty(&self) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::Bool(_) => Type::Bool,
            Self::Byte(_) => Type::Byte,
            Self::Int(_) => Type::Int,
            Self::Real(_) => Type::Real,
            Self::Char(_) => Type::Char,
            Self::Function(_) => Type::Function,
            Self::Partial(_) => Type::Function,
            Self::String(_) => Type::String,
            Self::List(_) => Type::List,
            Self::Array(_) => Type::Array,
        }
    }
    pub const fn is_truthy(&self) -> bool {
        !matches!(self, Self::Unit | Self::Bool(false))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Same types
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Byte(a), Value::Byte(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Partial(a), Value::Partial(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            // Different types
            (Value::Byte(a), Value::Int(b)) => *a as i64 == *b,
            (Value::Int(a), Value::Byte(b)) => *a == *b as i64,
            (Value::Byte(a), Value::Real(b)) => *a as f64 == *b,
            (Value::Real(a), Value::Byte(b)) => *a == *b as f64,
            (Value::Int(a), Value::Real(b)) => *a as f64 == *b,
            (Value::Real(a), Value::Int(b)) => *a == *b as f64,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<u8> for Value {
    fn from(b: u8) -> Self {
        Value::Byte(b)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<f64> for Value {
    fn from(r: f64) -> Self {
        Value::Real(r)
    }
}

impl From<char> for Value {
    fn from(c: char) -> Self {
        Value::Char(c)
    }
}

impl From<Function> for Value {
    fn from(f: Function) -> Self {
        Value::Function(f)
    }
}

impl From<Partial> for Value {
    fn from(p: Partial) -> Self {
        Value::Partial(p.into())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s.into())
    }
}

impl From<List> for Value {
    fn from(l: List) -> Self {
        Value::List(l.into())
    }
}

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        Value::Array(a.into())
    }
}
