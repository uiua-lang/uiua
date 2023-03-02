use std::{fmt, ops::*, slice, sync::Arc};

use crate::{check::Function, interpret::RuntimeResult, lex::Span};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Unit,
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    Function(Function),
    List(List),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Unit,
    Bool,
    Nat,
    Int,
    Real,
    Function,
    List,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Nat => write!(f, "nat"),
            Type::Int => write!(f, "int"),
            Type::Real => write!(f, "real"),
            Type::Function => write!(f, "function"),
            Type::List => write!(f, "list"),
        }
    }
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Unit => Type::Unit,
            Value::Bool(_) => Type::Bool,
            Value::Nat(_) => Type::Nat,
            Value::Int(_) => Type::Int,
            Value::Real(_) => Type::Real,
            Value::Function(_) => Type::Function,
            Value::List(_) => Type::List,
        }
    }
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Unit | Value::Bool(false))
    }
}

macro_rules! value_bin_op {
    ($method:ident, $verb:literal) => {
        impl Value {
            pub fn $method(self, other: Self, span: &Span) -> RuntimeResult<Self> {
                match (self, other) {
                    (Value::Nat(a), Value::Nat(b)) => Ok(Value::Nat(a.$method(b))),
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.$method(b))),
                    (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a.$method(b))),
                    (a, b) => Err(span
                        .clone()
                        .sp(format!("cannot {} {} and {}", $verb, a.ty(), b.ty()))
                        .into()),
                }
            }
        }
    };
}

value_bin_op!(add, "add");
value_bin_op!(sub, "subtract");
value_bin_op!(mul, "multiply");
value_bin_op!(div, "divide");

#[derive(Debug, Clone, PartialEq, PartialOrd, Default)]
pub struct List(Arc<Vec<Value>>);

impl List {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn push(mut self, value: Value) -> Self {
        Arc::make_mut(&mut self.0).push(value);
        self
    }
    pub fn iter(&self) -> slice::Iter<Value> {
        self.0.iter()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<I: IntoIterator<Item = Value>>(iter: I) -> Self {
        Self(iter.into_iter().collect::<Vec<_>>().into())
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = Box<dyn Iterator<Item = Value>>;
    fn into_iter(self) -> Self::IntoIter {
        match Arc::try_unwrap(self.0) {
            Ok(vec) => Box::new(vec.into_iter()),
            Err(arc) => Box::new(Vec::clone(&*arc).into_iter()),
        }
    }
}
