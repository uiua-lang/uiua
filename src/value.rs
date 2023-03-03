use std::{cmp::Ordering, fmt, ops::*, slice, sync::Arc};

use crate::{ast::BinOp, check::Function, lex::Span, UiuaResult};

#[derive(Debug, Clone)]
pub enum Value<F = Function> {
    Unit,
    Bool(bool),
    Nat(u64),
    Int(i64),
    Real(f64),
    Function(F),
    List(List<F>),
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

impl<F> Value<F> {
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

impl<F: PartialEq + PartialOrd> Value<F> {
    pub fn bin_op(&mut self, other: Self, op: BinOp, span: &Span) -> UiuaResult {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        match op {
            BinOp::Add => self.add_assign(other, span)?,
            BinOp::Sub => self.sub_assign(other, span)?,
            BinOp::Mul => self.mul_assign(other, span)?,
            BinOp::Div => self.div_assign(other, span)?,
            BinOp::Eq => *self = Value::Bool(*self == other),
            BinOp::Ne => *self = Value::Bool(*self != other),
            BinOp::Lt => *self = Value::Bool(*self < other),
            BinOp::Gt => *self = Value::Bool(*self > other),
            BinOp::Le => *self = Value::Bool(*self <= other),
            BinOp::Ge => *self = Value::Bool(*self >= other),
            BinOp::RangeEx => todo!(),
        }
        Ok(())
    }
}

macro_rules! value_bin_op {
    ($method:ident, $verb:literal) => {
        impl<F> Value<F> {
            pub fn $method(&mut self, other: Self, span: &Span) -> UiuaResult {
                #[cfg(feature = "profile")]
                puffin::profile_function!();
                match (self, other) {
                    (Value::Nat(a), Value::Nat(b)) => a.$method(b),
                    (Value::Int(a), Value::Int(b)) => a.$method(b),
                    (Value::Real(a), Value::Real(b)) => a.$method(b),
                    (a, b) => {
                        return Err(span
                            .clone()
                            .sp(format!("cannot {} {} and {}", $verb, a.ty(), b.ty()))
                            .into())
                    }
                }
                Ok(())
            }
        }
    };
}

value_bin_op!(add_assign, "add");
value_bin_op!(sub_assign, "subtract");
value_bin_op!(mul_assign, "multiply");
value_bin_op!(div_assign, "divide");

impl<F: PartialEq> PartialEq for Value<F> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nat(a), Value::Nat(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Real(a), Value::Real(b)) => a.is_nan() && b.is_nan() || a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            _ => false,
        }
    }
}

impl<F: Eq> Eq for Value<F> {}

impl<F: PartialOrd> PartialOrd for Value<F> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(match (self, other) {
            (Value::Unit, Value::Unit) => Ordering::Equal,
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Nat(a), Value::Nat(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Real(a), Value::Real(b)) => match (a.is_nan(), b.is_nan()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Greater,
                (false, true) => Ordering::Less,
                (false, false) => a.partial_cmp(b).unwrap(),
            },
            (Value::Function(a), Value::Function(b)) => a.partial_cmp(b).unwrap(),
            (Value::List(a), Value::List(b)) => a.partial_cmp(b).unwrap(),
            (a, b) => a.ty().cmp(&b.ty()),
        })
    }
}

impl<F: Ord> Ord for Value<F> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct List<F>(Arc<Vec<Value<F>>>);

impl<F: Clone> List<F> {
    pub fn new() -> Self {
        Self(Arc::new(Vec::new()))
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn push(mut self, value: Value<F>) -> Self {
        Arc::make_mut(&mut self.0).push(value);
        self
    }
    pub fn iter(&self) -> slice::Iter<Value<F>> {
        self.0.iter()
    }
}

impl<F> FromIterator<Value<F>> for List<F> {
    fn from_iter<I: IntoIterator<Item = Value<F>>>(iter: I) -> Self {
        Self(iter.into_iter().collect::<Vec<_>>().into())
    }
}

impl<F: Clone + 'static> IntoIterator for List<F> {
    type Item = Value<F>;
    type IntoIter = Box<dyn DoubleEndedIterator<Item = Value<F>>>;
    fn into_iter(self) -> Self::IntoIter {
        match Arc::try_unwrap(self.0) {
            Ok(vec) => Box::new(vec.into_iter()),
            Err(arc) => Box::new(Vec::clone(&*arc).into_iter()),
        }
    }
}
