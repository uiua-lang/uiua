use std::{fmt, sync::Arc};

use crate::array::Array;

#[derive(Clone, Default)]
pub enum Value {
    #[default]
    Unit,
    Int(i64),
    Real(f64),
    Char(char),
    Function(Function),
    Partial(Box<Partial>),
    String(Arc<String>),
    Array(Array),
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::Unit) };
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{:?}", s),
            Self::Array(a) => write!(f, "{:?}", a),
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Char(c) => write!(f, "{}", c),
            Self::Function(func) => write!(f, "function({} {})", func.start, func.params),
            Self::Partial(p) => write!(
                f,
                "function({} {}/{})",
                p.function.start,
                p.function.params,
                p.args.len()
            ),
            Self::String(s) => write!(f, "{}", s),
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
pub struct Function {
    pub(crate) start: u32,
    pub(crate) params: u8,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            start: 0,
            params: 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Arc<[Value]>,
}

impl Value {
    pub const fn ty(&self) -> Type {
        match self {
            Self::Unit => Type::Unit,
            Self::Int(_) => Type::Int,
            Self::Real(_) => Type::Real,
            Self::Char(_) => Type::Char,
            Self::Function(_) => Type::Function,
            Self::Partial(_) => Type::Function,
            Self::String(_) => Type::String,
            Self::Array(_) => Type::Array,
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Unit | Self::Int(0) => false,
            Self::Real(f) if *f == 0.0 => true,
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Same types
            (Value::Unit, Value::Unit) => true,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Partial(a), Value::Partial(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Array(a), Value::Array(b)) => a == b,
            // Different types
            (Value::Int(a), Value::Real(b)) => *a as f64 == *b,
            (Value::Real(a), Value::Int(b)) => *a == *b as f64,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Int(b as i64)
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

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        Value::Array(a)
    }
}
