use std::{
    f64::consts::*,
    fmt,
    io::{stdout, Write},
    mem::swap,
};

use enum_iterator::Sequence;

use crate::{
    array::Array,
    grid_fmt::GridFmt,
    value::*,
    vm::{CallEnv, Env},
    RuntimeResult,
};

pub(crate) fn constants() -> Vec<(&'static str, Value)> {
    vec![
        ("PI", PI.into()),
        ("TAU", TAU.into()),
        ("E", E.into()),
        ("INFINITY", f64::INFINITY.into()),
        ("NEG_INFINITY", f64::NEG_INFINITY.into()),
        ("NAN", f64::NAN.into()),
        ("MAX_REAL", f64::MAX.into()),
        ("MIN_REAL", f64::MIN.into()),
        ("EPSILON", f64::EPSILON.into()),
    ]
}

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op2 {
    Left,
    Right,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Min,
    Max,
    Atan2,
    Join,
    Reshape,
    Pick,
    Filter,
    Take,
    Drop,
    Rotate,
}

impl fmt::Display for Op2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op2::Left => write!(f, "left"),
            Op2::Right => write!(f, "right"),
            Op2::Eq => write!(f, "eq"),
            Op2::Ne => write!(f, "ne"),
            Op2::Lt => write!(f, "lt"),
            Op2::Le => write!(f, "le"),
            Op2::Gt => write!(f, "gt"),
            Op2::Ge => write!(f, "ge"),
            Op2::Add => write!(f, "add"),
            Op2::Sub => write!(f, "sub"),
            Op2::Mul => write!(f, "mul"),
            Op2::Div => write!(f, "div"),
            Op2::Mod => write!(f, "mod"),
            Op2::Pow => write!(f, "pow"),
            Op2::Min => write!(f, "min"),
            Op2::Max => write!(f, "max"),
            Op2::Atan2 => write!(f, "atan2"),
            Op2::Join => write!(f, "join"),
            Op2::Reshape => write!(f, "reshape"),
            Op2::Pick => write!(f, "pick"),
            Op2::Filter => write!(f, "filter"),
            Op2::Take => write!(f, "take"),
            Op2::Drop => write!(f, "drop"),
            Op2::Rotate => write!(f, "rotate"),
        }
    }
}

impl Value {
    pub(crate) fn op2(&mut self, other: &mut Value, op: Op2, env: &Env) -> RuntimeResult {
        match op {
            Op2::Left => {}
            Op2::Right => *self = other.clone(),
            Op2::Eq => *self = self.is_eq(other, env)?,
            Op2::Ne => *self = self.is_ne(other, env)?,
            Op2::Lt => *self = self.is_lt(other, env)?,
            Op2::Le => *self = self.is_le(other, env)?,
            Op2::Gt => *self = self.is_gt(other, env)?,
            Op2::Ge => *self = self.is_ge(other, env)?,
            Op2::Add => *self = self.add(other, env)?,
            Op2::Sub => *self = self.sub(other, env)?,
            Op2::Mul => *self = self.mul(other, env)?,
            Op2::Div => *self = self.div(other, env)?,
            Op2::Mod => *self = self.modulus(other, env)?,
            Op2::Pow => *self = self.pow(other, env)?,
            Op2::Min => *self = Self::min(self, other, env)?,
            Op2::Max => *self = Self::max(self, other, env)?,
            Op2::Atan2 => *self = self.atan2(other, env)?,
            Op2::Join => self.join(other.clone(), env)?,
            Op2::Reshape => self.reshape(other, env)?,
            Op2::Pick => self.pick(other, env)?,
            Op2::Filter => self.replicate(other, env)?,
            Op2::Take => self.take(other, env)?,
            Op2::Drop => self.drop(other, env)?,
            Op2::Rotate => self.rotate(other, env)?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Primitive {
    Not,
    Neg,
    Abs,
    Sqrt,
    Sin,
    Cos,
    Asin,
    Acos,
    Floor,
    Ceil,
    Round,
    Op2(Op2),
    Dup,
    Swap,
    Pop,
    ExclusiveFork,
    MonadicFork,
    DyadicFork,
    Fold,
    Reduce,
    Each,
    Cells,
    Table,
    Scan,
    Show,
    Print,
    Println,
    String,
    Len,
    Rank,
    Shape,
    First,
    Range,
    Reverse,
    Deshape,
    ScanLn,
    Args,
    Var,
}

fn _keep_primitive_id_small(_: std::convert::Infallible) {
    let _: u8 = unsafe { std::mem::transmute(Some(Primitive::Not)) };
}

impl From<Op2> for Primitive {
    fn from(op: Op2) -> Self {
        Self::Op2(op)
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Not => write!(f, "not"),
            Primitive::Neg => write!(f, "neg"),
            Primitive::Abs => write!(f, "abs"),
            Primitive::Sqrt => write!(f, "sqrt"),
            Primitive::Sin => write!(f, "sin"),
            Primitive::Cos => write!(f, "cos"),
            Primitive::Asin => write!(f, "asin"),
            Primitive::Acos => write!(f, "acos"),
            Primitive::Floor => write!(f, "floor"),
            Primitive::Ceil => write!(f, "ceil"),
            Primitive::Round => write!(f, "round"),
            Primitive::Op2(op) => write!(f, "{op}"),
            Primitive::Dup => write!(f, "dup"),
            Primitive::Swap => write!(f, "swap"),
            Primitive::Pop => write!(f, "pop"),
            Primitive::ExclusiveFork => write!(f, "exclusive_fork"),
            Primitive::MonadicFork => write!(f, "monadic_fork"),
            Primitive::DyadicFork => write!(f, "dyadic_fork"),
            Primitive::Fold => write!(f, "fold"),
            Primitive::Reduce => write!(f, "reduce"),
            Primitive::Each => write!(f, "each"),
            Primitive::Cells => write!(f, "cells"),
            Primitive::Table => write!(f, "table"),
            Primitive::Scan => write!(f, "scan"),
            Primitive::Show => write!(f, "show"),
            Primitive::Print => write!(f, "print"),
            Primitive::Println => write!(f, "println"),
            Primitive::String => write!(f, "string"),
            Primitive::Len => write!(f, "len"),
            Primitive::Rank => write!(f, "rank"),
            Primitive::Shape => write!(f, "shape"),
            Primitive::First => write!(f, "first"),
            Primitive::Range => write!(f, "range"),
            Primitive::Reverse => write!(f, "reverse"),
            Primitive::Deshape => write!(f, "deshape"),
            Primitive::ScanLn => write!(f, "scanln"),
            Primitive::Args => write!(f, "args"),
            Primitive::Var => write!(f, "var"),
        }
    }
}

impl Primitive {
    pub(crate) fn run(&self, env: &mut CallEnv) -> RuntimeResult {
        match self {
            Primitive::Not => env.monadic_env(Value::not)?,
            Primitive::Neg => env.monadic_env(Value::neg)?,
            Primitive::Abs => env.monadic_env(Value::abs)?,
            Primitive::Sqrt => env.monadic_env(Value::sqrt)?,
            Primitive::Sin => env.monadic_env(Value::sin)?,
            Primitive::Cos => env.monadic_env(Value::cos)?,
            Primitive::Asin => env.monadic_env(Value::asin)?,
            Primitive::Acos => env.monadic_env(Value::acos)?,
            Primitive::Floor => env.monadic_env(Value::floor)?,
            Primitive::Ceil => env.monadic_env(Value::ceil)?,
            Primitive::Round => env.monadic_env(Value::round)?,
            Primitive::Op2(op) => {
                let subenv = env.env();
                let mut a = env.pop()?;
                let b = env.top_mut()?;
                swap(&mut a, b);
                b.op2(&mut a, *op, &subenv)?;
            }
            Primitive::Dup => {
                let x = env.top_mut()?.clone();
                env.push(x);
            }
            Primitive::Swap => {
                let a = env.pop()?;
                let b = env.pop()?;
                env.push(a);
                env.push(b);
            }
            Primitive::Pop => {
                env.pop()?;
            }
            Primitive::ExclusiveFork => {
                let fs = env.pop()?;
                if !fs.is_array() {
                    env.push(fs);
                    return env.call();
                }
                let arr = fs.into_array();
                let values = env.pop_n(arr.len())?;
                for (f, v) in arr.into_values().into_iter().rev().zip(values.into_iter()) {
                    env.push(v);
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::MonadicFork => {
                let fs = env.pop()?;
                if !fs.is_array() {
                    env.push(fs);
                    return env.call();
                }
                let x = env.pop()?;
                for f in fs.into_array().into_values().into_iter().rev() {
                    env.push(x.clone());
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::DyadicFork => {
                let fs = env.pop()?;
                if !fs.is_array() {
                    env.push(fs);
                    return env.call();
                }
                let x = env.pop()?;
                let y = env.pop()?;
                for f in fs.into_array().into_values().into_iter().rev() {
                    env.push(y.clone());
                    env.push(x.clone());
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::Fold => {
                let f = env.pop()?;
                let mut acc = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(acc);
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                for cell in xs.into_array().into_values() {
                    env.push(acc);
                    env.push(cell);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop()?;
                }
                env.push(acc);
            }
            Primitive::Reduce => {
                let f = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(xs);
                    return Ok(());
                }
                let mut cells = xs.into_array().into_values().into_iter();
                let Some(mut acc) = cells.next() else {
                    return Err(env.error("Cannot reduce empty array"));
                };
                for cell in cells {
                    env.push(cell);
                    env.push(acc);
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop()?;
                }
                env.push(acc);
            }
            Primitive::Each => {
                let f = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let (shape, values) = xs.into_array().into_parts();
                let mut new_values = Vec::with_capacity(values.len());
                for val in values {
                    env.push(val);
                    env.push(f.clone());
                    env.call()?;
                    new_values.push(env.pop()?);
                }
                env.push(Array::from((shape, new_values)).normalized(0));
            }
            Primitive::Cells => {
                let f = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let array = xs.into_array();
                let mut cells = Vec::with_capacity(array.len());
                for cell in array.into_values() {
                    env.push(cell);
                    env.push(f.clone());
                    env.call()?;
                    cells.push(env.pop()?);
                }
                env.push(Array::from(cells).normalized(1));
            }
            Primitive::Table => {
                let f = env.pop()?;
                let xs = env.pop()?;
                let ys = env.pop()?;
                if !xs.is_array() && !ys.is_array() {
                    env.push(ys);
                    env.push(xs);
                    env.push(f);
                    return env.call();
                }
                let a = if xs.is_array() {
                    xs.into_array()
                } else {
                    Array::from(xs)
                };
                let b = if ys.is_array() {
                    ys.into_array()
                } else {
                    Array::from(ys)
                };
                let mut table = Vec::with_capacity(a.len());
                for a in a.into_values() {
                    let mut row = Vec::with_capacity(b.len());
                    for b in b.clone().into_values() {
                        env.push(b);
                        env.push(a.clone());
                        env.push(f.clone());
                        env.call()?;
                        row.push(env.pop()?);
                    }
                    table.push(Value::from(Array::from(row).normalized(1)));
                }
                env.push(Array::from(table).normalized(1));
            }
            Primitive::Scan => {
                let f = env.pop()?;
                let xs = env.pop()?;
                if !xs.is_array() {
                    env.push(xs);
                    return Ok(());
                }
                let arr = xs.into_array();
                let ty = arr.ty();
                let len = arr.len();
                let mut cells = arr.into_values().into_iter();
                let Some(mut acc) = cells.next() else {
                    env.push(Array::from(ty));
                    return Ok(())
                };
                let mut scanned = Vec::with_capacity(len);
                scanned.push(acc.clone());
                for cell in cells {
                    env.push(cell);
                    env.push(acc.clone());
                    env.push(f.clone());
                    env.call()?;
                    acc = env.pop()?;
                    scanned.push(acc.clone());
                }
                env.push(Array::from(scanned).normalized(1));
            }
            Primitive::Show => {
                println!("{}", env.pop()?.grid_string());
            }
            Primitive::Print => {
                print!("{}", env.pop()?);
                let _ = stdout().flush();
            }
            Primitive::Println => println!("{}", env.pop()?),
            Primitive::Len => env.monadic(|v| v.len() as f64)?,
            Primitive::Rank => env.monadic(|v| v.rank() as f64)?,
            Primitive::Shape => {
                env.monadic(|v| Array::from_iter(v.shape().into_iter().map(|i| i as f64)))?
            }
            Primitive::Range => env.monadic_mut_env(Value::range)?,
            Primitive::Reverse => env.monadic_mut(Value::reverse)?,
            Primitive::Deshape => env.monadic_mut(Value::deshape)?,
            Primitive::First => env.monadic_mut_env(Value::first)?,
            Primitive::String => env.monadic(|v| v.to_string())?,
            Primitive::ScanLn => {
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap_or_default();
                line.pop();
                line.pop();
                env.push(line);
            }
            Primitive::Args => env.push(Array::from_iter(
                std::env::args().map(Array::from).map(Value::from),
            )),
            Primitive::Var => {
                let name = env.top_mut()?;
                if !name.is_array() || !name.array().is_chars() {
                    return Err(env.error("Argument to var must be a string"));
                }
                let key: String = name.array().chars().iter().collect();
                *name = std::env::var(key).unwrap_or_default().into();
            }
        }
        Ok(())
    }
}
