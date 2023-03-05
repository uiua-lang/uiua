use std::{f64::consts::*, fmt, ptr};

use enum_iterator::Sequence;

use crate::{array::Array, lex::Span, list::List, value::*, vm::Instr, RuntimeResult};

pub(crate) fn constants() -> Vec<(&'static str, Value)> {
    vec![
        ("PI", PI.into()),
        ("TAU", TAU.into()),
        ("E", E.into()),
        ("INFINITY", f64::INFINITY.into()),
        ("NEG_INFINITY", f64::NEG_INFINITY.into()),
        ("NAN", f64::NAN.into()),
        ("MAX_INT", i64::MAX.into()),
        ("MIN_INT", i64::MIN.into()),
        ("MAX_REAL", f64::MAX.into()),
        ("MIN_REAL", f64::MIN.into()),
        ("EPSILON", f64::EPSILON.into()),
    ]
}

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op1 {
    Id,
    Default,
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

impl fmt::Display for Op1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op1::Id => write!(f, "id"),
            Op1::Default => write!(f, "default"),
            Op1::Int => write!(f, "int"),
            Op1::Real => write!(f, "real"),
            Op1::Not => write!(f, "not"),
            Op1::Neg => write!(f, "neg"),
            Op1::Abs => write!(f, "abs"),
            Op1::Sqrt => write!(f, "sqrt"),
            Op1::Sin => write!(f, "sin"),
            Op1::Cos => write!(f, "cos"),
            Op1::Floor => write!(f, "floor"),
            Op1::Ceil => write!(f, "ceil"),
            Op1::Round => write!(f, "round"),
            Op1::Len => write!(f, "len"),
        }
    }
}

type ValueFn1 = fn(*mut Value, &Span) -> RuntimeResult;
macro_rules! op1_table {
    ($(($op:ident, $name:ident, $f:ident)),* $(,)*) => {
        $(static $name: [ValueFn1; Type::ARITY] = unsafe { type_line!($f) };)*

        impl Value {
            pub fn op1(&mut self, op: Op1, span: &Span) -> RuntimeResult {
                match op {
                    $(Op1::$op => self.$f(span),)*
                }
            }
            $(
                fn $f(&mut self, span: &Span) -> RuntimeResult {
                    $name[self.ty as usize](self, span)
                }
            )*
        }
    };
}
macro_rules! op1_fn {
    ($name:ident, $message:literal, $(($a_ty:pat, |$a:ident| $f:expr)),* $(,)?) => {
        #[allow(unused_mut, clippy::no_effect, unreachable_patterns)]
        const unsafe fn $name(a: Type) -> ValueFn1 {
            match a {
                $($a_ty => |$a, _| { $f; Ok(())},)*
                Type::Array => |a, span| {
                    for a in (*a).array_mut().iter_mut() {
                        a.$name(span)?;
                    }
                    Ok(())
                },
                _ => |a, span| Err(span.error(format!($message, (*a).ty))),
            }
        }
    };
}
pub(crate) use op1_fn;

op1_table!(
    (Id, ID_TABLE, id_fn),
    (Default, DEFAULT_TABLE, default_fn),
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
const unsafe fn not_fn(_: Type) -> ValueFn1 {
    |a, _| {
        *a = (!(*a).is_truthy()).into();
        Ok(())
    }
}
op1_fn!(id_fn, "Cannot convert {} to itself", (_, |_a| ()));
op1_fn!(
    default_fn,
    "Cannot convert {} to default",
    (Type::Unit, |_a| ()),
    (Type::Bool, |a| *a = false.into()),
    (Type::Int, |a| *a = 0.into()),
    (Type::Real, |a| *a = 0.0.into()),
    (Type::Function, |a| *a = Function(0).into()),
    (Type::Partial, |a| *a = Function(0).into()),
    (Type::List, |a| *a = List::new().into()),
    (Type::Array, |a| *a = Array::new().into()),
);
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
    (Type::List, |a| *a = ((*a).list_mut().len() as i64).into()),
    (Type::Array, |a| *a = ((*a).array_mut().len() as i64).into()),
);

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op2 {
    Mod,
    Pow,
    Atan2,
    Get,
    Push,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op2::Mod => write!(f, "mod"),
            Op2::Pow => write!(f, "pow"),
            Op2::Atan2 => write!(f, "atan2"),
            Op2::Get => write!(f, "get"),
            Op2::Push => write!(f, "push"),
        }
    }
}

pub(crate) type ValueFn2 = fn(*mut Value, Value, &Span) -> RuntimeResult;
macro_rules! op2_table {
    ($(($op:ident, $name:ident, $f:ident)),* $(,)*) => {
        $(static $name: [[ValueFn2; Type::ARITY]; Type::ARITY] = type_square!($f);)*

        impl Value {
            pub fn op2(&mut self, other: Value, op: Op2, span: &Span) -> RuntimeResult {
                match op {
                    $(Op2::$op => self.$f(other, span),)*
                }
            }
            $(
                fn $f(&mut self, other: Value, span: &Span) -> RuntimeResult {
                    $name[self.ty as usize][other.ty as usize](self, other, span)
                }
            )*
        }
    };
}
macro_rules! op2_fn {
    ($name:ident, $message:literal, $(($a_ty:pat, $b_ty:pat, |$a:ident, $b:ident| $f:expr)),* $(,)?) => {
        #[allow(unused_mut, unreachable_patterns)]
        const fn $name(a: Type, b: Type) -> $crate::builtin::ValueFn2 {
            unsafe {
                match (a, b) {
                    $(($a_ty, $b_ty) => |$a, mut $b, _| { $f; Ok(())},)*
                    (Type::Array, Type::Array) => |a, b, span| {
                        if (*a).array_mut().len() != b.data.array.len() {
                            return Err(span.error(format!(
                                concat!($message, " because they have different lengths: {} and {}"),
                                Type::Array,
                                Type::Array,
                                (*a).array_mut().len(),
                                b.data.array.len()
                            )));
                        }
                        for (a, b) in (*a)
                            .array_mut()
                            .iter_mut()
                            .zip(b.data.array.iter().cloned())
                        {
                            a.$name(b, span)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, span| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$name(b.clone(), span)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, mut b, span| {
                        for b in b.array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$name(b.clone(), span)?;
                            *b = a_clone;
                        }
                        *a = b;
                        Ok(())
                    },
                    _ => |a, b, span| Err(span.error(format!($message, (*a).ty, b.ty))),
                }
            }
        }
    };
}
pub(crate) use op2_fn;

op2_table!(
    (Mod, MOD_TABLE, mod_fn),
    (Pow, POW_TABLE, pow_fn),
    (Atan2, ATAN2_TABLE, atan2_fn),
    (Get, GET_TABLE, get_fn),
    (Push, PUSH_TABLE, push_fn),
);
op2_fn!(
    mod_fn,
    "Cannot get remainder of {} % {}",
    (Type::Int, Type::Int, |a, b| {
        let x = b.data.int;
        let m = (*a).data.int;
        *a = ((x % m + m) % m).into();
    }),
    (Type::Real, Type::Int, |a, b| {
        let x = b.data.int as f64;
        let m = (*a).data.real;
        *a = (((x % m + m) % m) as i64).into()
    }),
    (Type::Int, Type::Real, |a, b| {
        let x = b.data.real;
        let m = (*a).data.int as f64;
        *a = (((x % m + m) % m) as i64).into()
    }),
    (Type::Real, Type::Real, |a, b| {
        let x = b.data.real;
        let m = (*a).data.real;
        *a = (((x % m + m) % m) as i64).into()
    })
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
    get_fn,
    "Cannot get index {} from {}",
    (Type::Int, Type::List, |a, b| *a = b
        .list_mut()
        .get((*a).data.int as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (Type::Int, Type::Array, |a, b| *a = b
        .array_mut()
        .get((*a).data.int as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (Type::Array, _, |a, b| {
        for index in (*a).array_mut().iter_mut() {
            index.get_fn(b.clone(), &Span::Builtin)?;
        }
    }),
);
op2_fn!(
    push_fn,
    "Cannot push {} onto {}",
    (_, Type::List, |a, b| {
        ptr::swap(a, &mut b);
        (*a).list_mut().push(b);
    }),
    (_, Type::Array, |a, b| {
        ptr::swap(a, &mut b);
        (*a).array_mut().push(b);
    }),
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Algorithm {
    Each,
}

impl fmt::Display for Algorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Algorithm::Each => write!(f, "each"),
        }
    }
}

impl Algorithm {
    pub(crate) fn params(&self) -> usize {
        match self {
            Algorithm::Each => 2,
        }
    }
    pub(crate) fn instrs(&self) -> Vec<Instr> {
        #[allow(unused_imports)]
        use {
            crate::ast::BinOp::*,
            crate::builtin::{Op1, Op2},
            Instr::*,
        };
        const SPAN: Span = Span::Builtin;
        match self {
            Algorithm::Each => vec![
                Comment("each".into()),
                // [1, 2, 3], f = (_ * 2)
                CopyRel(2),        // [1, 2, 3], f, [1, 2, 3]
                Op1(Op1::Default), // [1, 2, 3], f, []
                CopyRel(3),        // [1, 2, 3], f, [], [1, 2, 3]
                Op1(Op1::Len),     // [1, 2, 3], f, [], 3
                Push(0.into()),    // [1, 2, 3], f, [], 3, 0
                // Loop start
                CopyRel(2),                // [1, 2, 3], f, [], 3, 0, 3
                CopyRel(2),                // [1, 2, 3], f, [], 3, 0, 3, 0
                BinOp(Eq, SPAN.into()),    // [1, 2, 3], f, [], 3, 0, false
                PopJumpIf(13, true),       // [1, 2, 3], f, [], 3, 0
                CopyRel(5),                // [1, 2, 3], f, [], 3, 0, [1, 2, 3]
                CopyRel(2),                // [1, 2, 3], f, [], 3, 0, [1, 2, 3], 0
                Op2(Op2::Get),             // [1, 2, 3], f, [], 3, 0, 1
                CopyRel(5),                // [1, 2, 3], f, [], 3, 0, 1, f
                Call { args: 1, span: 0 }, // [1, 2, 3], f, [], 3, 0, 2
                Move(4),                   // [1, 2, 3], f, 3, 0, 2, [],
                Move(2),                   // [1, 2, 3], f, 3, 0, [], 2
                Op2(Op2::Push),            // [1, 2, 3], f, 3, 0, [2]
                Rotate(3),                 // [1, 2, 3], f, [2], 3, 0
                Push(1.into()),            // [1, 2, 3], f, [2], 3, 0, 1
                BinOp(Add, SPAN.into()),   // [1, 2, 3], f, [2], 3, 1
                Jump(-15),
                // Loop end
                Pop(2), // [1, 2, 3], f, [2, 4, 6]
            ],
        }
    }
}
