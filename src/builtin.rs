use std::{fmt, ptr};

use enum_iterator::Sequence;

use crate::{lex::Span, value::*, RuntimeResult};

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp1 {
    Int,
    Real,
    Not,
    Neg,
    Abs,
    Sqrt,
    Sin,
    Cos,
    Floor,
    Ceil,
    Round,
    Len,
}

impl fmt::Display for BuiltinOp1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp1::Int => write!(f, "int"),
            BuiltinOp1::Real => write!(f, "real"),
            BuiltinOp1::Not => write!(f, "not"),
            BuiltinOp1::Neg => write!(f, "neg"),
            BuiltinOp1::Abs => write!(f, "abs"),
            BuiltinOp1::Sqrt => write!(f, "sqrt"),
            BuiltinOp1::Sin => write!(f, "sin"),
            BuiltinOp1::Cos => write!(f, "cos"),
            BuiltinOp1::Floor => write!(f, "floor"),
            BuiltinOp1::Ceil => write!(f, "ceil"),
            BuiltinOp1::Round => write!(f, "round"),
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
macro_rules! op1_fn {
    ($name:ident, $message:literal, $(($a_ty:pat, |$a:ident| $f:expr)),* $(,)?) => {
        #[allow(unused_mut, clippy::no_effect)]
        const unsafe fn $name(a: Type) -> BuiltinOp1Fn {
            match a {
                $($a_ty => |$a, _| { $f; Ok(())},)*
                _ => |a, span| Err(span.error(format!($message, (*a).ty))),
            }
        }
    };
}

op1_table!(
    (Int, INT_TABLE, int_fn),
    (Real, REAL_TABLE, real_fn),
    (Not, NOT_TABLE, not_fn),
    (Neg, NEG_TABLE, neg_fn),
    (Abs, ABS_TABLE, abs_fn),
    (Sqrt, SQRT_TABLE, sqrt_fn),
    (Sin, SIN_TABLE, sin_fn),
    (Cos, COS_TABLE, cos_fn),
    (Floor, FLOOR_TABLE, floor_fn),
    (Ceil, CEIL_TABLE, ceil_fn),
    (Round, ROUND_TABLE, round_fn),
    (Len, LEN_TABLE, len_fn),
);
const unsafe fn not_fn(_: Type) -> BuiltinOp1Fn {
    |a, _| {
        *a = (!(*a).is_truthy()).into();
        Ok(())
    }
}
op1_fn!(
    int_fn,
    "Cannot convert {} to int",
    (Type::Int, |_a| ()),
    (Type::Real, |a| *a = ((*a).data.real as i64).into()),
);
op1_fn!(
    real_fn,
    "Cannot convert {} to real",
    (Type::Int, |a| *a = ((*a).data.int as f64).into()),
    (Type::Real, |_a| ()),
);
op1_fn!(
    neg_fn,
    "Cannot negate {}",
    (Type::Int, |a| *a = (-(*a).data.int).into()),
    (Type::Real, |a| *a = (-(*a).data.real).into())
);
op1_fn!(
    abs_fn,
    "Cannot get absolute value of {}",
    (Type::Int, |a| *a = (*a).data.int.abs().into()),
    (Type::Real, |a| *a = (*a).data.real.abs().into())
);
op1_fn!(
    sqrt_fn,
    "Cannot get square root of {}",
    (Type::Real, |a| *a = (*a).data.real.sqrt().into())
);
op1_fn!(
    sin_fn,
    "Cannot get sine of {}",
    (Type::Real, |a| *a = (*a).data.real.sin().into())
);
op1_fn!(
    cos_fn,
    "Cannot get cosine of {}",
    (Type::Real, |a| *a = (*a).data.real.cos().into())
);
op1_fn!(
    floor_fn,
    "Cannot get floor of {}",
    (Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.floor().into())
);
op1_fn!(
    ceil_fn,
    "Cannot get ceiling of {}",
    (Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.ceil().into())
);
op1_fn!(
    round_fn,
    "Cannot round {}",
    (Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.round().into())
);
op1_fn!(
    len_fn,
    "Cannot get length of {}",
    (Type::List, |a| *a = ((*a).list_mut().0.len() as i64).into()),
    (Type::Array, |a| *a = ((*a).array_mut().len() as i64).into()),
);

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp2 {
    Pow,
    Atan2,
    Push,
}

impl fmt::Display for BuiltinOp2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp2::Pow => write!(f, "pow"),
            BuiltinOp2::Atan2 => write!(f, "atan2"),
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
macro_rules! op2_fn {
    ($name:ident, $message:literal, $(($a_ty:pat, $b_ty:pat, |$a:ident, $b:ident| $f:expr)),* $(,)?) => {
        #[allow(unused_mut)]
        const unsafe fn $name(a: Type, b: Type) -> BuiltinOp2Fn {
            match (a, b) {
                $(($a_ty, $b_ty) => |$a, mut $b, _| { $f; Ok(())},)*
                _ => |a, b, span| Err(span.error(format!($message, (*a).ty, b.ty))),
            }
        }
    };
}

op2_table!(
    (Pow, POW_TABLE, pow_fn),
    (Atan2, ATAN2_TABLE, atan2_fn),
    (Push, PUSH_TABLE, push_fn),
);
op2_fn!(
    pow_fn,
    "Cannot raise {} to {} power",
    (Type::Int, Type::Int, |a, b| *a =
        (*a).data.int.pow(b.data.int as u32).into()),
    (Type::Real, Type::Int, |a, b| *a =
        (*a).data.real.powi(b.data.int as i32).into()),
    (Type::Real, Type::Real, |a, b| *a =
        (*a).data.real.powf(b.data.real).into()),
);
op2_fn!(
    atan2_fn,
    "Cannot get arctangent of {}/{}",
    (Type::Real, Type::Real, |a, b| *a =
        (*a).data.real.atan2(b.data.real).into()),
);
op2_fn!(
    push_fn,
    "Cannot push {} onto {}",
    (_, Type::List, |a, b| {
        ptr::swap(a, &mut b);
        (*a).list_mut().0.push_back(b);
    }),
    (_, Type::Array, |a, b| {
        ptr::swap(a, &mut b);
        (*a).array_mut().push(b);
    }),
);
