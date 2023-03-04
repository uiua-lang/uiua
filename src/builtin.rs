use std::{fmt, ptr};

use enum_iterator::Sequence;

use crate::{lex::Span, value::*, RuntimeResult};

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp1 {
    Not,
    Neg,
    Abs,
    Sqrt,
    Len,
}

impl fmt::Display for BuiltinOp1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp1::Not => write!(f, "not"),
            BuiltinOp1::Neg => write!(f, "neg"),
            BuiltinOp1::Abs => write!(f, "abs"),
            BuiltinOp1::Sqrt => write!(f, "sqrt"),
            BuiltinOp1::Len => write!(f, "len"),
        }
    }
}

type BuiltinOp1Fn = fn(*mut Value, &Span) -> RuntimeResult;
macro_rules! op1_table {
    ($(($op:ident, $name:ident, $f:expr)),* $(,)*) => {
        $(static $name: [BuiltinOp1Fn; Type::ARITY] = unsafe { type_line!($f) };)*

        impl Value {
            pub fn op1(&mut self, op: BuiltinOp1, span: &Span) -> RuntimeResult {
                match op {
                    $(BuiltinOp1::$op => $name[self.ty as usize](self, span),)*
                }
            }
        }
    };
}

op1_table!(
    (Not, NOT_TABLE, not_fn),
    (Neg, NEG_TABLE, neg_fn),
    (Abs, ABS_TABLE, abs_fn),
    (Sqrt, SQRT_TABLE, sqrt_fn),
    (Len, LEN_TABLE, len_fn),
);
const unsafe fn not_fn(_: Type) -> BuiltinOp1Fn {
    |a, _| {
        *a = (!(*a).is_truthy()).into();
        Ok(())
    }
}
const unsafe fn neg_fn(a: Type) -> BuiltinOp1Fn {
    match a {
        Type::Int => |a, _| {
            *a = (-(*a).data.int).into();
            Ok(())
        },
        Type::Real => |a, _| {
            *a = (-(*a).data.real).into();
            Ok(())
        },
        _ => |a, span| Err(span.error(format!("Cannot negate {}", (*a).ty))),
    }
}
const unsafe fn abs_fn(a: Type) -> BuiltinOp1Fn {
    match a {
        Type::Int => |a, _| {
            *a = (*a).data.int.abs().into();
            Ok(())
        },
        Type::Real => |a, _| {
            *a = (*a).data.real.abs().into();
            Ok(())
        },
        _ => |a, span| Err(span.error(format!("Cannot get absolute value of {}", (*a).ty))),
    }
}
const unsafe fn sqrt_fn(a: Type) -> BuiltinOp1Fn {
    match a {
        Type::Real => |a, _| {
            *a = (*a).data.real.sqrt().into();
            Ok(())
        },
        _ => |a, span| Err(span.error(format!("Cannot get square root of {}", (*a).ty))),
    }
}
const unsafe fn len_fn(a: Type) -> BuiltinOp1Fn {
    match a {
        Type::List => |a, _| {
            let len = (*a).list_mut().0.len();
            *a = (len as i64).into();
            Ok(())
        },
        _ => |a, span| Err(span.error(format!("Cannot get length of {}", (*a).ty))),
    }
}

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp2 {
    Pow,
    Push,
}

impl fmt::Display for BuiltinOp2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp2::Pow => write!(f, "pow"),
            BuiltinOp2::Push => write!(f, "push"),
        }
    }
}

type BuiltinOp2Fn = fn(*mut Value, Value, &Span) -> RuntimeResult;
macro_rules! op2_table {
    ($(($op:ident, $name:ident, $f:expr)),* $(,)*) => {
        $(static $name: [[BuiltinOp2Fn; Type::ARITY]; Type::ARITY] = unsafe { type_square!($f) };)*

        impl Value {
            pub fn op2(&mut self, other: Value, op: BuiltinOp2, span: &Span) -> RuntimeResult {
                match op {
                    $(BuiltinOp2::$op => $name[self.ty as usize][other.ty as usize](self, other, span),)*
                }
            }
        }
    };
}

op2_table!((Pow, POW_TABLE, pow_fn), (Push, PUSH_TABLE, push_fn),);
const unsafe fn pow_fn(a: Type, b: Type) -> BuiltinOp2Fn {
    match (a, b) {
        (Type::Int, Type::Int) => |a, b, _| {
            *a = (*a).data.int.pow(b.data.int as u32).into();
            Ok(())
        },
        (Type::Real, Type::Int) => |a, b, _| {
            *a = (*a).data.real.powi(b.data.int as i32).into();
            Ok(())
        },
        (Type::Real, Type::Real) => |a, b, _| {
            *a = (*a).data.real.powf(b.data.real).into();
            Ok(())
        },
        _ => |a, b, span| Err(span.error(format!("Cannot raise {} to {} power", (*a).ty, b.ty))),
    }
}
const unsafe fn push_fn(a: Type, b: Type) -> BuiltinOp2Fn {
    match (a, b) {
        (_, Type::List) => |a, mut b, _| {
            ptr::swap(a, &mut b);
            (*a).list_mut().0.push_back(b);
            Ok(())
        },
        _ => |_, b, span| Err(span.error(format!("Cannot push onto {}", b.ty))),
    }
}
