use std::{fmt, sync::Arc};

use crate::array::Array;

#[derive(Clone)]
pub enum Value {
    Int(i64),
    Real(f64),
    Char(char),
    Function(Function),
    Partial(Box<Partial>),
    Array(Array),
}

fn _keep_value_small(_: std::convert::Infallible) {
    let _: [u8; 16] = unsafe { std::mem::transmute(Value::nil()) };
}

impl Default for Value {
    fn default() -> Self {
        Self::nil()
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Array(a) => {
                if let Some(s) = a.as_string() {
                    write!(f, "{s:?}")
                } else {
                    write!(f, "{a:?}")
                }
            }
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Char(c) => write!(f, "{}", c),
            Self::Function(func) if func.is_nil() => write!(f, "nil"),
            Self::Function(func) => write!(f, "function({} {})", func.start, func.params),
            Self::Partial(p) => write!(
                f,
                "function({} {}/{})",
                p.function.start,
                p.function.params,
                p.args.len()
            ),
            Self::Array(a) => {
                if let Some(s) = a.as_string() {
                    write!(f, "{s}")
                } else {
                    write!(f, "{a}")
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Num,
    Char,
    Function,
    Array,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Num => write!(f, "number"),
            Type::Char => write!(f, "character"),
            Type::Function => write!(f, "function"),
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
        Self::nil()
    }
}

impl Function {
    #[inline]
    pub const fn nil() -> Self {
        Self {
            start: 0,
            params: 1,
        }
    }
    #[inline]
    pub const fn is_nil(&self) -> bool {
        self.start == 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Arc<[Value]>,
}

impl Value {
    pub const fn nil() -> Self {
        Value::Function(Function::nil())
    }
    pub const fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Num,
            Self::Real(_) => Type::Num,
            Self::Char(_) => Type::Char,
            Self::Function(_) => Type::Function,
            Self::Partial(_) => Type::Function,
            Self::Array(_) => Type::Array,
        }
    }
    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Int(0) => false,
            Self::Real(f) => *f != 0.0,
            Self::Function(func) => !func.is_nil(),
            _ => true,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Same types
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::Partial(a), Value::Partial(b)) => a == b,
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
        Value::Array(Array::from_iter(s.chars().map(Value::from)))
    }
}

impl From<Array> for Value {
    fn from(a: Array) -> Self {
        Value::Array(a)
    }
}
