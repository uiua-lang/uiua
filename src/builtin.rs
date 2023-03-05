use std::{f64::consts::*, fmt, mem::take, ptr, sync::Arc};

use enum_iterator::Sequence;

use crate::{
    array::Array, compile::Assembly, list::List, value::*, vm::Instr, RuntimeError, RuntimeResult,
};

#[derive(Clone, Copy)]
pub(crate) struct Env<'a> {
    pub span: usize,
    pub assembly: &'a Assembly,
}

impl<'a> Env<'a> {
    pub fn error(&self, message: impl Into<String>) -> RuntimeError {
        self.assembly.spans[self.span].error(message)
    }
}

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
    Byte,
    Int,
    Real,
    String,
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
    Print,
    Println,
}

impl fmt::Display for Op1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op1::Id => write!(f, "id"),
            Op1::Default => write!(f, "default"),
            Op1::Byte => write!(f, "byte"),
            Op1::Int => write!(f, "int"),
            Op1::Real => write!(f, "real"),
            Op1::String => write!(f, "string"),
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
            Op1::Print => write!(f, "print"),
            Op1::Println => write!(f, "println"),
        }
    }
}

type ValueFn1 = fn(*mut Value, Env) -> RuntimeResult;
macro_rules! op1_table {
    ($(($op:ident, $name:ident, $f:ident)),* $(,)*) => {
        #[allow(unused_unsafe)]
        $(static $name: [ValueFn1; Type::ARITY] = unsafe { type_line!($f) };)*

        impl Value {
            pub(crate) fn op1(&mut self, op: Op1, env: Env) -> RuntimeResult {
                match op {
                    $(Op1::$op => self.$f(env),)*
                }
            }
            $(
                fn $f(&mut self, env: Env) -> RuntimeResult {
                    $name[self.ty as usize](self, env)
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
                Type::Array => |a, env| {
                    for a in (*a).array_mut().iter_mut() {
                        a.$name(env)?;
                    }
                    Ok(())
                },
                _ => |a, env| Err(env.error(format!($message, (*a).ty))),
            }
        }
    };
}
pub(crate) use op1_fn;

op1_table!(
    (Id, ID_TABLE, id_fn),
    (Default, DEFAULT_TABLE, default_fn),
    (Byte, BYTE_TABLE, byte_fn),
    (Int, INT_TABLE, int_fn),
    (Real, REAL_TABLE, real_fn),
    (String, STRING_TABLE, string_fn),
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
    (Print, PRINT_TABLE, print_fn),
    (Println, PRINTLN_TABLE, println_fn),
);
const unsafe fn not_fn(_: Type) -> ValueFn1 {
    |a, _| {
        *a = (!(*a).is_truthy()).into();
        Ok(())
    }
}

const fn id_fn(_: Type) -> ValueFn1 {
    |_, _| Ok(())
}
op1_fn!(
    default_fn,
    "Cannot convert {} to default",
    (Type::Unit, |_a| ()),
    (Type::Bool, |a| *a = false.into()),
    (Type::Byte, |a| *a = 0u8.into()),
    (Type::Int, |a| *a = 0i64.into()),
    (Type::Real, |a| *a = 0.0.into()),
    (Type::Char, |a| *a = '\0'.into()),
    (Type::Function, |a| *a = Function(0).into()),
    (Type::Partial, |a| *a = Function(0).into()),
    (Type::String, |a| *a = Arc::new(String::new()).into()),
    (Type::List, |a| *a = List::new().into()),
    (Type::Array, |a| *a = Array::new().into()),
);
op1_fn!(
    byte_fn,
    "Cannot convert {} to byte",
    (Type::Bool, |a| *a = ((*a).data.bool as u8).into()),
    (Type::Byte, |_a| ()),
    (Type::Int, |a| *a = ((*a).data.int as u8).into()),
    (Type::Real, |a| *a = ((*a).data.real as u8).into()),
);
op1_fn!(
    int_fn,
    "Cannot convert {} to int",
    (Type::Bool, |a| *a = ((*a).data.bool as i64).into()),
    (Type::Byte, |a| *a = ((*a).data.byte as i64).into()),
    (Type::Int, |_a| ()),
    (Type::Real, |a| *a = ((*a).data.real as i64).into()),
);
op1_fn!(
    real_fn,
    "Cannot convert {} to real",
    (Type::Bool, |a| *a = ((*a).data.bool as u8 as f64).into()),
    (Type::Byte, |a| *a = ((*a).data.byte as f64).into()),
    (Type::Int, |a| *a = ((*a).data.int as f64).into()),
    (Type::Real, |_a| ()),
);
op1_fn!(
    string_fn,
    "Cannot format {}",
    (Type::Array, |a| *a = Arc::new(format!("{}", &*a)).into()),
    (_, |a| *a = Arc::new(format!("{}", &*a)).into())
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
    (Type::Byte | Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.floor().into())
);
op1_fn!(
    ceil_fn,
    "Cannot get ceiling of {}",
    (Type::Byte | Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.ceil().into())
);
op1_fn!(
    round_fn,
    "Cannot round {}",
    (Type::Byte | Type::Int, |_a| ()),
    (Type::Real, |a| *a = (*a).data.real.round().into())
);
op1_fn!(
    len_fn,
    "Cannot get length of {}",
    (Type::String, |a| *a =
        ((*a).data.string.len() as i64).into()),
    (Type::List, |a| *a = ((*a).data.list.len() as i64).into()),
    (Type::Array, |a| *a = ((*a).data.array.len() as i64).into()),
);
op1_fn!(
    print_fn,
    "Cannot print {}",
    (Type::Array, |a| print!("{}", &*a)),
    (_, |a| print!("{}", &*a))
);
op1_fn!(
    println_fn,
    "Cannot print {}",
    (Type::Array, |a| println!("{}", &*a)),
    (_, |a| println!("{}", &*a))
);

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op2 {
    Mod,
    Pow,
    Atan2,
    Get,
    Push,
    Concat,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op2::Mod => write!(f, "mod"),
            Op2::Pow => write!(f, "pow"),
            Op2::Atan2 => write!(f, "atan2"),
            Op2::Get => write!(f, "get"),
            Op2::Push => write!(f, "push"),
            Op2::Concat => write!(f, "concat"),
        }
    }
}

pub(crate) type ValueFn2 = fn(*mut Value, *mut Value, Env) -> RuntimeResult;
macro_rules! op2_table {
    ($(($op:ident, $name:ident, $f:ident)),* $(,)*) => {
        $(static $name: [[ValueFn2; Type::ARITY]; Type::ARITY] = type_square!($f);)*

        impl Value {
            pub(crate) fn op2(&mut self, other: &mut Value, op: Op2, env: Env) -> RuntimeResult {
                match op {
                    $(Op2::$op => self.$f(other, env),)*
                }
            }
            $(
                fn $f(&mut self, other: &mut Value, env: Env) -> RuntimeResult {
                    $name[self.ty as usize][other.ty as usize](self, other, env)
                }
            )*
        }
    };
}
macro_rules! op2_fn {
    ($name:ident, $message:literal,
        $(($a_ty:pat, $b_ty:pat, |$a:ident, $b:ident| $f:expr)),*
        $(,(!env, $ea_ty:pat, $eb_ty:pat, |$ea:ident, $eb:ident, $env:ident| $ef:expr))*
    $(,)?) => {
        #[allow(unused_mut, unreachable_patterns)]
        const fn $name(a: Type, b: Type) -> $crate::builtin::ValueFn2 {
            unsafe {
                match (a, b) {
                    $(($a_ty, $b_ty) => |$a, mut $b, _| { $f; Ok(())},)*
                    $(($ea_ty, $eb_ty) => |$ea, mut $eb, $env| { $ef; Ok(())},)*
                    (Type::Array, Type::Array) => |a, b, env| {
                        if (*a).array_mut().len() != (*b).data.array.len() {
                            return Err(env.error(format!(
                                concat!($message, " because they have different lengths: {} and {}"),
                                Type::Array,
                                Type::Array,
                                (*a).array_mut().len(),
                                (*b).data.array.len()
                            )));
                        }
                        for (a, b) in (*a)
                            .array_mut()
                            .iter_mut()
                            .zip((*b).array_mut().iter_mut())
                        {
                            a.$name(b, env)?;
                        }
                        Ok(())
                    },
                    (Type::Array, _) => |a, b, env| {
                        for a in (*a).array_mut().iter_mut() {
                            a.$name(&mut *b, env)?;
                        }
                        Ok(())
                    },
                    (_, Type::Array) => |a, b, env| {
                        for b in (*b).array_mut().iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$name(b, env)?;
                            *b = a_clone;
                        }
                        ptr::swap(a, b);
                        Ok(())
                    },
                    _ => |a, b, env| Err(env.error(format!($message, (*a).ty, (*b).ty))),
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
    (Concat, CONCAT_TABLE, concat_fn),
);
op2_fn!(
    mod_fn,
    "Cannot get remainder of {} % {}",
    (Type::Int, Type::Int, |a, b| {
        let x = (*b).data.int;
        let m = (*a).data.int;
        *a = ((x % m + m) % m).into();
    }),
    (Type::Real, Type::Int, |a, b| {
        let x = (*b).data.int as f64;
        let m = (*a).data.real;
        *a = (((x % m + m) % m) as i64).into()
    }),
    (Type::Int, Type::Real, |a, b| {
        let x = (*b).data.real;
        let m = (*a).data.int as f64;
        *a = (((x % m + m) % m) as i64).into()
    }),
    (Type::Real, Type::Real, |a, b| {
        let x = (*b).data.real;
        let m = (*a).data.real;
        *a = (((x % m + m) % m) as i64).into()
    })
);
op2_fn!(
    pow_fn,
    "Cannot raise {} to {} power",
    (Type::Int, Type::Int, |a, b| *a =
        (*a).data.int.pow((*b).data.int as u32).into()),
    (Type::Real, Type::Int, |a, b| *a =
        (*a).data.real.powi((*b).data.int as i32).into()),
    (Type::Real, Type::Real, |a, b| *a =
        (*a).data.real.powf((*b).data.real).into()),
);
op2_fn!(
    atan2_fn,
    "Cannot get arctangent of {}/{}",
    (Type::Real, Type::Real, |a, b| *a =
        (*a).data.real.atan2((*b).data.real).into()),
);
op2_fn!(
    get_fn,
    "Cannot get index {} from {}",
    (Type::Byte, Type::String, |a, b| *a = (*b)
        .data
        .string
        .chars()
        .nth((*a).data.byte as usize)
        .map(Into::into)
        .unwrap_or_else(Value::unit)),
    (Type::Byte, Type::List, |a, b| *a = (*b)
        .data
        .list
        .get((*a).data.byte as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (Type::Byte, Type::Array, |a, b| *a = (*b)
        .data
        .array
        .get((*a).data.byte as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (Type::Int, Type::String, |a, b| *a = (*b)
        .data
        .string
        .chars()
        .nth((*a).data.int as usize)
        .map(Into::into)
        .unwrap_or_else(Value::unit)),
    (Type::Int, Type::List, |a, b| *a = (*b)
        .data
        .list
        .get((*a).data.int as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (Type::Int, Type::Array, |a, b| *a = (*b)
        .data
        .array
        .get((*a).data.int as usize)
        .cloned()
        .unwrap_or_else(Value::unit)),
    (!env, Type::Array, _, |a, b, env| {
        for index in (*a).array_mut().iter_mut() {
            index.get_fn(&mut *b, env)?;
        }
    }),
);
op2_fn!(
    push_fn,
    "Cannot push {} onto {}",
    (Type::Char, Type::String, |a, b| {
        ptr::swap(a, b);
        (*a).string_mut().push((*b).data.char);
    }),
    (_, Type::List, |a, b| {
        ptr::swap(a, b);
        (*a).list_mut().push(take(&mut *b));
    }),
    (_, Type::Array, |a, b| {
        ptr::swap(a, b);
        (*a).array_mut().push(take(&mut *b));
    }),
);
op2_fn!(
    concat_fn,
    "Cannot concatenate {} and {}",
    (Type::String, Type::String, |a, b| {
        ptr::swap(a, b);
        (*a).string_mut().push_str(&(*b).data.string);
    }),
    (Type::List, Type::List, |a, b| {
        ptr::swap(a, b);
        (*a).list_mut().extend(take((*b).list_mut()).into_iter())
    }),
    (Type::Array, Type::Array, |a, b| {
        ptr::swap(a, b);
        (*a).array_mut().extend(take((*b).array_mut()).into_iter())
    }),
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Algorithm {
    Flip,
    Fold,
    Each,
}

impl fmt::Display for Algorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Algorithm::Flip => write!(f, "flip"),
            Algorithm::Fold => write!(f, "fold"),
            Algorithm::Each => write!(f, "each"),
        }
    }
}

impl Algorithm {
    pub(crate) fn params(&self) -> usize {
        match self {
            Algorithm::Flip => 3,
            Algorithm::Each => 2,
            Algorithm::Fold => 3,
        }
    }
    pub(crate) fn instrs(&self) -> Vec<Instr> {
        #[allow(unused_imports)]
        use {
            crate::ast::BinOp::*,
            crate::builtin::{Op1, Op2},
            Instr::*,
        };
        let mut instrs = match self {
            Algorithm::Flip => vec![
                // b, a, f
                Rotate(3), // f, b, a
                Rotate(2), // f, a, b
                Move(3),   // a, b, f
                Call(0),
                Call(0),
            ],
            Algorithm::Fold => vec![
                // [1, 2, 3], f = (_+_), 0
                CopyRel(3),        // [1, 2, 3], f, 0, [1, 2, 3]
                Op1(Op1::Len),     // [1, 2, 3], f, 0, 3
                Push(0i64.into()), // [1, 2, 3], f, 0, 3, 0
                // Loop start
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, 3
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, 3, 0
                BinOp(Eq, 0),        // [1, 2, 3], f, 0, 3, 0, false
                PopJumpIf(11, true), // [1, 2, 3], f, 0, 3, 0
                CopyRel(5),          // [1, 2, 3], f, 0, 3, 0, [1, 2, 3]
                CopyRel(2),          // [1, 2, 3], f, 0, 3, 0, [1, 2, 3], 0
                Op2(Op2::Get),       // [1, 2, 3], f, 0, 3, 0, 1
                Move(4),             // [1, 2, 3], f, 3, 0, 1, 0
                CopyRel(5),          // [1, 2, 3], f, 3, 0, 1, 0, f
                Call(0),             // partial
                Call(0),             // [1, 2, 3], f, 3, 0, 1
                Rotate(3),           // [1, 2, 3], f, 1, 3, 0
                Push(1i64.into()),   // [1, 2, 3], f, 1, 3, 0, 1
                BinOp(Add, 0),       // [1, 2, 3], f, 1, 3, 1
                Jump(-13),
                // Loop end
                Pop(2), // [1, 2, 3], f, 6
            ],
            Algorithm::Each => vec![
                // [1, 2, 3], f = (_ * 2)
                CopyRel(2),        // [1, 2, 3], f, [1, 2, 3]
                Op1(Op1::Default), // [1, 2, 3], f, []
                CopyRel(3),        // [1, 2, 3], f, [], [1, 2, 3]
                Op1(Op1::Len),     // [1, 2, 3], f, [], 3
                Push(0i64.into()), // [1, 2, 3], f, [], 3, 0
                // Loop start
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, 3
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, 3, 0
                BinOp(Eq, 0),        // [1, 2, 3], f, [], 3, 0, false
                PopJumpIf(13, true), // [1, 2, 3], f, [], 3, 0
                CopyRel(5),          // [1, 2, 3], f, [], 3, 0, [1, 2, 3]
                CopyRel(2),          // [1, 2, 3], f, [], 3, 0, [1, 2, 3], 0
                Op2(Op2::Get),       // [1, 2, 3], f, [], 3, 0, 1
                CopyRel(5),          // [1, 2, 3], f, [], 3, 0, 1, f
                Call(0),             // [1, 2, 3], f, [], 3, 0, 2
                Move(4),             // [1, 2, 3], f, 3, 0, 2, [],
                Move(2),             // [1, 2, 3], f, 3, 0, [], 2
                Op2(Op2::Push),      // [1, 2, 3], f, 3, 0, [2]
                Rotate(3),           // [1, 2, 3], f, [2], 3, 0
                Push(1i64.into()),   // [1, 2, 3], f, [2], 3, 0, 1
                BinOp(Add, 0),       // [1, 2, 3], f, [2], 3, 1
                Jump(-15),
                // Loop end
                Pop(2), // [1, 2, 3], f, [2, 4, 6]
            ],
        };
        instrs.insert(0, Comment(self.to_string()));
        instrs
    }
}
