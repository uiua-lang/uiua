use std::{cmp::Ordering, fmt, mem::swap, ops::*, sync::Arc};

use crate::{array::Array, ast::BinOp, builtin::Env, list::List, RuntimeResult};

#[derive(Clone)]
pub enum Value {
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

impl Default for Value {
    #[inline(always)]
    fn default() -> Self {
        Self::Unit
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "unit"),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Byte(b) => write!(f, "{}", b),
            Self::Int(i) => write!(f, "{}", i),
            Self::Real(r) => write!(f, "{}", r),
            Self::Char(c) => write!(f, "{:?}", c),
            Self::Function(func) => write!(f, "function({})", func.0),
            Self::Partial(p) => write!(f, "partial({})", p.args.len()),
            Self::String(s) => write!(f, "{:?}", s),
            Self::List(l) => write!(f, "{:?}", l),
            Self::Array(a) => write!(f, "{:?}", a),
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
            Self::Partial(p) => write!(f, "partial({})", p.args.len()),
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
    pub(crate) args: Vec<Value>,
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

impl Value {
    pub(crate) fn bin_op(&mut self, other: &mut Self, op: BinOp, env: Env) -> RuntimeResult {
        #[cfg(feature = "profile")]
        puffin::profile_function!();
        match op {
            BinOp::Add => self.add_assign(other, env)?,
            BinOp::Sub => self.sub_assign(other, env)?,
            BinOp::Mul => self.mul_assign(other, env)?,
            BinOp::Div => self.div_assign(other, env)?,
            BinOp::Eq => self.compare(other, is_eq, env)?,
            BinOp::Ne => self.compare(other, is_ne, env)?,
            BinOp::Lt => self.compare(other, is_lt, env)?,
            BinOp::Le => self.compare(other, is_le, env)?,
            BinOp::Gt => self.compare(other, is_gt, env)?,
            BinOp::Ge => self.compare(other, is_ge, env)?,
        }
        Ok(())
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

fn real_ordering(a: f64, b: f64) -> Ordering {
    match (a.is_nan(), b.is_nan()) {
        (true, true) => Ordering::Equal,
        (false, true) => Ordering::Less,
        (true, false) => Ordering::Greater,
        (false, false) => a.partial_cmp(&b).unwrap(),
    }
}

fn is_eq(ordering: Ordering) -> bool {
    ordering == Ordering::Equal
}
fn is_ne(ordering: Ordering) -> bool {
    ordering != Ordering::Equal
}
fn is_lt(ordering: Ordering) -> bool {
    ordering == Ordering::Less
}
fn is_le(ordering: Ordering) -> bool {
    ordering != Ordering::Greater
}
fn is_gt(ordering: Ordering) -> bool {
    ordering == Ordering::Greater
}
fn is_ge(ordering: Ordering) -> bool {
    ordering != Ordering::Less
}

impl Value {
    #[inline(always)]
    fn compare(
        &mut self,
        mut other: &mut Self,
        ord: fn(Ordering) -> bool,
        env: Env,
    ) -> RuntimeResult {
        let mut this = self;
        match (&mut this, &mut other) {
            (Value::Unit, Value::Unit) => *this = ord(Ordering::Equal).into(),
            (Value::Bool(a), Value::Bool(b)) => *this = ord((*a).cmp(b)).into(),
            (Value::Byte(a), Value::Byte(b)) => *this = ord(a.cmp(&b)).into(),
            (Value::Int(a), Value::Int(b)) => *this = ord(a.cmp(&b)).into(),
            (Value::Real(a), Value::Real(b)) => *this = ord(real_ordering(*a, *b)).into(),
            (Value::Byte(a), Value::Int(b)) => *this = ord((*a as i64).cmp(b)).into(),
            (Value::Int(a), Value::Byte(b)) => *this = ord((*a).cmp(&(*b as i64))).into(),
            (Value::Byte(a), Value::Real(b)) => *this = ord(real_ordering(*a as f64, *b)).into(),
            (Value::Real(a), Value::Byte(b)) => *this = ord(real_ordering(*a, *b as f64)).into(),
            (Value::Real(a), Value::Int(b)) => *this = ord(real_ordering(*a, *b as f64)).into(),
            (Value::Int(a), Value::Real(b)) => *this = ord(real_ordering(*a as f64, *b)).into(),
            (Value::Char(a), Value::Char(b)) => *this = ord(a.cmp(&b)).into(),
            (Value::Function(a), Value::Function(b)) => *this = ord((*a).cmp(b)).into(),
            (Value::Partial(a), Value::Partial(b)) => {
                if ord(a.function.cmp(&b.function)) {
                    *this = true.into();
                } else {
                    for (a, b) in a.args.iter_mut().zip(&mut b.args) {
                        a.compare(b, ord, env)?;
                        if let Value::Bool(true) = a {
                            *this = true.into();
                            return Ok(());
                        }
                    }
                    *this = false.into();
                }
            }
            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() {
                    return Err(env.error(format!(
                        "Cannot compare arrays of different lengths: {} and {}",
                        a.len(),
                        b.len()
                    )));
                }
                for (a, b) in a.iter_mut().zip(b.iter_mut()) {
                    a.compare(b, ord, env)?;
                }
            }
            (Value::Array(a), b) => {
                for a in a.iter_mut() {
                    a.compare(b, ord, env)?;
                }
            }
            (a, Value::Array(b)) => {
                for b in b.iter_mut() {
                    let mut a_clone = (*a).clone();
                    a_clone.compare(b, ord, env)?;
                    *b = a_clone;
                }
                swap(this, other);
            }
            (a, b) => *this = ord(a.ty().cmp(&b.ty())).into(),
        }
        Ok(())
    }
}

macro_rules! value_math_op {
    ($trait:ident, $method:ident, $verb:literal) => {
        impl Value {
            #[inline(always)]
            fn $method(&mut self, mut other: &mut Self, env: Env) -> RuntimeResult {
                let mut this = self;
                match (&mut this, &mut other) {
                    (Value::Unit, Value::Unit) => {}
                    (Value::Byte(a), Value::Byte(b)) => $trait::$method(a, *b),
                    (Value::Int(a), Value::Int(b)) => $trait::$method(a, *b),
                    (Value::Real(a), Value::Real(b)) => $trait::$method(a, *b),
                    (Value::Real(a), Value::Int(b)) => $trait::$method(a, *b as f64),
                    (Value::Int(a), Value::Real(b)) => $trait::$method(a, *b as i64),
                    (Value::Array(a), Value::Array(b)) => {
                        if a.len() != b.len() {
                            return Err(env.error(format!(
                                "Cannot {} arrays of different lengths: {} and {}",
                                $verb,
                                a.len(),
                                b.len()
                            )));
                        }
                        for (a, b) in a.iter_mut().zip(b.iter_mut()) {
                            a.$method(b, env)?;
                        }
                    }
                    (Value::Array(a), b) => {
                        for a in a.iter_mut() {
                            a.$method(b, env)?;
                        }
                    }
                    (a, Value::Array(b)) => {
                        for b in b.iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$method(b, env)?;
                            *b = a_clone;
                        }
                        swap(this, other);
                    }
                    (a, b) => {
                        return Err(env.error(format!(
                            "Cannot {} {} and {}",
                            $verb,
                            a.ty(),
                            b.ty()
                        )))
                    }
                }
                Ok(())
            }
        }
    };
}

value_math_op!(AddAssign, add_assign, "add");
value_math_op!(SubAssign, sub_assign, "subtract");
value_math_op!(MulAssign, mul_assign, "multiply");
value_math_op!(DivAssign, div_assign, "divide");

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
