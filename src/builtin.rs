use std::{
    f64::consts::*,
    fmt,
    mem::{swap, take},
    sync::Arc,
};

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

macro_rules! op1_table {
    ($(($op:ident, $f:ident)),* $(,)*) => {
        impl Value {
            pub(crate) fn op1(&mut self, op: Op1, env: Env) -> RuntimeResult {
                match op {
                    $(Op1::$op => self.$f(env),)*
                }
            }
        }
    };
}
macro_rules! op1_fn {
    ($name:ident, $message:literal, $this:ident, $(($a_val:pat, $f:expr)),* $(,)?) => {
        #[allow(unused_mut, clippy::no_effect, unreachable_patterns)]
        impl Value {
            pub(crate) fn $name(&mut self, env: Env) -> RuntimeResult {
                let mut $this = self;
                match &mut $this {
                    $($a_val => $f,)*
                    Value::Array(a) => {
                        for a in a.iter_mut() {
                            a.$name(env)?;
                        }
                    },
                    a => return Err(env.error(format!($message, a.ty()))),
                }
                Ok(())
            }
        }
    };
}
pub(crate) use op1_fn;

op1_table!(
    (Id, id),
    (Default, default),
    (Byte, byte),
    (Int, int),
    (Real, real),
    (String, string),
    (Not, not),
    (Neg, neg),
    (Abs, abs),
    (Sqrt, sqrt),
    (Sin, sin),
    (Cos, cos),
    (Floor, floor),
    (Ceil, ceil),
    (Round, round),
    (Len, len),
    (Print, print),
    (Println, println),
);
impl Value {
    pub(crate) fn id(&mut self, _env: Env) -> RuntimeResult {
        Ok(())
    }
    pub(crate) fn not(&mut self, _env: Env) -> RuntimeResult {
        *self = (!self.is_truthy()).into();
        Ok(())
    }
}
op1_fn!(
    default,
    "Cannot convert {} to default",
    this,
    (Value::Unit, ()),
    (Value::Bool(_), *this = false.into()),
    (Value::Byte(_), *this = 0u8.into()),
    (Value::Int(_), *this = 0i64.into()),
    (Value::Real(_), *this = 0.0.into()),
    (Value::Char(_), *this = '\0'.into()),
    (Value::Function(_), *this = Function(0).into()),
    (Value::Partial(_), *this = Function(0).into()),
    (Value::String(_), *this = String::new().into()),
    (Value::List(_), *this = List::new().into()),
    (Value::Array(_), *this = Array::new().into()),
);
op1_fn!(
    byte,
    "Cannot convert {} to byte",
    this,
    (Value::Bool(a), *this = (*a as u8).into()),
    (Value::Byte(_), ()),
    (Value::Int(a), *this = (*a as u8).into()),
    (Value::Real(a), *this = (*a as u8).into()),
);
op1_fn!(
    int,
    "Cannot convert {} to int",
    this,
    (Value::Bool(a), *this = (*a as i64).into()),
    (Value::Byte(a), *this = (*a as i64).into()),
    (Value::Int(_), ()),
    (Value::Real(a), *this = (*a as i64).into()),
);
op1_fn!(
    real,
    "Cannot convert {} to real",
    this,
    (Value::Bool(a), *this = (*a as u8 as f64).into()),
    (Value::Byte(a), *this = (*a as f64).into()),
    (Value::Int(a), *this = (*a as f64).into()),
    (Value::Real(_), ()),
);
op1_fn!(
    string,
    "Cannot format {}",
    this,
    (Value::Array(a), *this = format!("{a}").into()),
    (a, *this = format!("{a}").into())
);
op1_fn!(
    neg,
    "Cannot negate {}",
    this,
    (Value::Int(a), *this = (-*a).into()),
    (Value::Real(a), *this = (-*a).into())
);
op1_fn!(
    abs,
    "Cannot get absolute value of {}",
    this,
    (Value::Int(a), *this = a.abs().into()),
    (Value::Real(a), *this = a.abs().into())
);
op1_fn!(
    sqrt,
    "Cannot get square root of {}",
    this,
    (Value::Real(a), *this = a.sqrt().into())
);
op1_fn!(
    sin,
    "Cannot get sine of {}",
    this,
    (Value::Real(a), *this = a.sin().into())
);
op1_fn!(
    cos,
    "Cannot get cosine of {}",
    this,
    (Value::Real(a), *this = a.cos().into())
);
op1_fn!(
    floor,
    "Cannot get floor of {}",
    this,
    (Value::Byte(_) | Value::Int(_), ()),
    (Value::Real(a), *this = a.floor().into())
);
op1_fn!(
    ceil,
    "Cannot get ceiling of {}",
    this,
    (Value::Byte(_) | Value::Int(_), ()),
    (Value::Real(a), *this = a.ceil().into())
);
op1_fn!(
    round,
    "Cannot round {}",
    this,
    (Value::Byte(_) | Value::Int(_), ()),
    (Value::Real(a), *this = a.round().into())
);
op1_fn!(
    len,
    "Cannot get length of {}",
    this,
    (Value::String(a), *this = (a.len() as i64).into()),
    (Value::List(a), *this = (a.len() as i64).into()),
    (Value::Array(a), *this = (a.len() as i64).into()),
);
op1_fn!(
    print,
    "Cannot print {}",
    this,
    (Value::Array(a), print!("{}", a)),
    (a, print!("{a}"))
);
op1_fn!(
    println,
    "Cannot print {}",
    this,
    (Value::Array(a), println!("{}", a)),
    (a, println!("{a}"))
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

macro_rules! op2_table {
    ($(($op:ident, $f:ident)),* $(,)*) => {
        impl Value {
            pub(crate) fn op2(&mut self, other: &mut Value, op: Op2, env: Env) -> RuntimeResult {
                match op {
                    $(Op2::$op => self.$f(other, env),)*
                }
            }
        }
    };
}
macro_rules! op2_fn {
    ($name:ident, $message:literal, $this:ident, $other:ident,
        $(($a_ty:pat, $b_ty:pat, $f:expr)),*
        $(,(!$env:ident, $ea_ty:pat, $eb_ty:pat, $ef:expr))*
    $(,)?) => {
        impl Value {
            #[allow(unused_mut, unreachable_patterns)]
            pub(crate) fn $name(&mut self, other: &mut Self, env: Env) -> RuntimeResult {
                let mut $this = self;
                let mut $other = other;
                match (&mut $this, &mut $other) {
                    $(($a_ty, $b_ty) => $f,)*
                    $(($ea_ty, $eb_ty) => {
                        let $env = env;
                        $ef
                    },)*
                    (Value::Array(a), Value::Array(b)) => {
                        if a.len() != b.len() {
                            return Err(env.error(format!(
                                concat!($message, " because they have different lengths: {} and {}"),
                                Type::Array,
                                Type::Array,
                                a.len(),
                                b.len()
                            )));
                        }
                        for (a, b) in a
                            .iter_mut()
                            .zip(b.iter_mut())
                        {
                            a.$name(b, env)?;
                        }
                    },
                    (Value::Array(a), b) => {
                        for a in a.iter_mut() {
                            a.$name(&mut *b, env)?;
                        }
                    },
                    (a, Value::Array(b)) => {
                        for b in b.iter_mut() {
                            let mut a_clone = (*a).clone();
                            a_clone.$name(b, env)?;
                            *b = a_clone;
                        }
                        swap(*a, $other);
                    },
                    (a, b) => return Err(env.error(format!($message, a.ty(), b.ty()))),
                }
                Ok(())
            }
        }
    };
}
pub(crate) use op2_fn;

op2_table!(
    (Mod, mod_fn),
    (Pow, pow_fn),
    (Atan2, atan2_fn),
    (Get, get_fn),
    (Push, push_fn),
    (Concat, concat_fn),
);
op2_fn!(
    mod_fn,
    "Cannot get remainder of {} % {}",
    this,
    other,
    (Value::Int(a), Value::Int(b), {
        let x = *b;
        let m = *a;
        *this = ((x % m + m) % m).into();
    }),
    (Value::Real(a), Value::Int(b), {
        let x = *b as f64;
        let m = *a;
        *this = (((x % m + m) % m) as i64).into()
    }),
    (Value::Int(a), Value::Real(b), {
        let x = *b;
        let m = *a as f64;
        *this = (((x % m + m) % m) as i64).into()
    }),
    (Value::Real(a), Value::Real(b), {
        let x = *b;
        let m = *a;
        *this = (((x % m + m) % m) as i64).into()
    })
);
op2_fn!(
    pow_fn,
    "Cannot raise {} to {} power",
    this,
    other,
    (
        Value::Int(a),
        Value::Int(b),
        *this = a.pow(*b as u32).into()
    ),
    (
        Value::Real(a),
        Value::Int(b),
        *this = a.powi(*b as i32).into()
    ),
    (Value::Real(a), Value::Real(b), *this = a.powf(*b).into()),
);
op2_fn!(
    atan2_fn,
    "Cannot get arctangent of {}/{}",
    this,
    other,
    (Value::Real(a), Value::Real(b), *this = a.atan2(*b).into()),
);
op2_fn!(
    get_fn,
    "Cannot get index {} from {}",
    this,
    other,
    (
        Value::Byte(a),
        Value::String(b),
        *this = b
            .chars()
            .nth(*a as usize)
            .map(Into::into)
            .unwrap_or(Value::Unit)
    ),
    (
        Value::Byte(a),
        Value::List(b),
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    ),
    (
        Value::Byte(a),
        Value::Array(b),
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    ),
    (
        Value::Int(a),
        Value::String(b),
        *this = b
            .chars()
            .nth(*a as usize)
            .map(Into::into)
            .unwrap_or(Value::Unit)
    ),
    (
        Value::Int(a),
        Value::List(b),
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    ),
    (
        Value::Int(a),
        Value::Array(b),
        *this = b.get(*a as usize).cloned().unwrap_or(Value::Unit)
    ),
    (!env, Value::Array(a), b, {
        for index in a.iter_mut() {
            index.get_fn(b, env)?;
        }
    }),
);
op2_fn!(
    push_fn,
    "Cannot push {} onto {}",
    this,
    other,
    (Value::Char(a), Value::String(b), {
        Arc::make_mut(b).push(*a);
        swap(this, other);
    }),
    (a, Value::List(b), {
        b.push(take(a));
        swap(this, other);
    }),
    (a, Value::Array(b), {
        b.push(take(a));
        swap(this, other);
    }),
);
op2_fn!(
    concat_fn,
    "Cannot concatenate {} and {}",
    this,
    other,
    (Value::String(a), Value::String(b), {
        swap(a, b);
        Arc::make_mut(a).push_str(b);
    }),
    (Value::List(a), Value::List(b), {
        swap(a, b);
        a.extend(take(b).into_iter());
    }),
    (Value::Array(a), Value::Array(b), {
        swap(a, b);
        a.extend(take(b).into_iter())
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
                PopJumpIf(12, true), // [1, 2, 3], f, 0, 3, 0
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
                Jump(-14),
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
