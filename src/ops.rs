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

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Op1 {
    Id,
    String,
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

impl fmt::Display for Op1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op1::Id => write!(f, "id"),
            Op1::String => write!(f, "string"),
            Op1::Not => write!(f, "not"),
            Op1::Neg => write!(f, "neg"),
            Op1::Abs => write!(f, "abs"),
            Op1::Sqrt => write!(f, "sqrt"),
            Op1::Sin => write!(f, "sin"),
            Op1::Cos => write!(f, "cos"),
            Op1::Asin => write!(f, "asin"),
            Op1::Acos => write!(f, "acos"),
            Op1::Floor => write!(f, "floor"),
            Op1::Ceil => write!(f, "ceil"),
            Op1::Round => write!(f, "round"),
            Op1::Len => write!(f, "len"),
            Op1::Rank => write!(f, "rank"),
            Op1::Shape => write!(f, "shape"),
            Op1::First => write!(f, "first"),
            Op1::Range => write!(f, "range"),
            Op1::Reverse => write!(f, "reverse"),
            Op1::Deshape => write!(f, "deshape"),
            Op1::ScanLn => write!(f, "scanln"),
            Op1::Args => write!(f, "args"),
            Op1::Var => write!(f, "var"),
        }
    }
}

impl Value {
    pub(crate) fn op1(&mut self, op: Op1, env: &Env) -> RuntimeResult {
        match op {
            Op1::Id => {}
            Op1::Len => *self = (self.len() as f64).into(),
            Op1::Rank => *self = (self.rank() as f64).into(),
            Op1::Shape => {
                *self = Array::from_iter(self.shape().into_iter().map(|i| i as f64)).into()
            }
            Op1::Range => *self = self.range(env)?.into(),
            Op1::Reverse => self.reverse(),
            Op1::Not => *self = self.not(env)?,
            Op1::Neg => *self = self.neg(env)?,
            Op1::Abs => *self = self.abs(env)?,
            Op1::Sqrt => *self = self.sqrt(env)?,
            Op1::Sin => *self = self.sin(env)?,
            Op1::Cos => *self = self.cos(env)?,
            Op1::Asin => *self = self.asin(env)?,
            Op1::Acos => *self = self.acos(env)?,
            Op1::Floor => *self = self.floor(env)?,
            Op1::Ceil => *self = self.ceil(env)?,
            Op1::Round => *self = self.round(env)?,
            Op1::Deshape => self.deshape(),
            Op1::First => self.first(env)?,
            Op1::String => *self = self.to_string().into(),
            Op1::ScanLn => {
                let mut line = String::new();
                std::io::stdin().read_line(&mut line).unwrap_or_default();
                line.pop();
                line.pop();
                *self = line.into();
            }
            Op1::Args => {
                *self = Array::from_iter(std::env::args().map(Array::from).map(Value::from)).into()
            }
            Op1::Var => {
                if !self.is_array() || !self.array().is_chars() {
                    return Err(env.error("Argument to var must be a string"));
                }
                let key: String = self.array().chars().iter().collect();
                *self = std::env::var(key).unwrap_or_default().into();
            }
        }
        Ok(())
    }
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
    Op1(Op1),
    Op2(Op2),
    Dup,
    Swap,
    Pop,
    Fork,
    ForkArray1,
    ForkArray2,
    Fold,
    Reduce,
    Each,
    Cells,
    Table,
    Scan,
    Show,
    Print,
    Println,
}

fn _keep_primitive_id_small(_: std::convert::Infallible) {
    let _: [u8; 2] = unsafe { std::mem::transmute(Some(Primitive::Op1(Op1::Neg))) };
}

impl From<Op1> for Primitive {
    fn from(op: Op1) -> Self {
        Self::Op1(op)
    }
}

impl From<Op2> for Primitive {
    fn from(op: Op2) -> Self {
        Self::Op2(op)
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Op1(op) => write!(f, "{op}"),
            Primitive::Op2(op) => write!(f, "{op}"),
            Primitive::Dup => write!(f, "dup"),
            Primitive::Swap => write!(f, "swap"),
            Primitive::Pop => write!(f, "pop"),
            Primitive::ForkArray1 => write!(f, "fork_array1"),
            Primitive::ForkArray2 => write!(f, "fork_array2"),
            Primitive::Fork => write!(f, "fork"),
            Primitive::Fold => write!(f, "fold"),
            Primitive::Reduce => write!(f, "reduce"),
            Primitive::Each => write!(f, "each"),
            Primitive::Cells => write!(f, "cells"),
            Primitive::Table => write!(f, "table"),
            Primitive::Scan => write!(f, "scan"),
            Primitive::Show => write!(f, "show"),
            Primitive::Print => write!(f, "print"),
            Primitive::Println => write!(f, "println"),
        }
    }
}

impl Primitive {
    pub(crate) fn run(&self, env: &mut CallEnv) -> RuntimeResult {
        match self {
            Primitive::Op1(op) => {
                let subenv = env.env();
                env.top_mut()?.op1(*op, &subenv)?;
            }
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
            Primitive::Fork => {
                let f = env.pop()?;
                let g = env.pop()?;
                let x = env.pop()?;
                env.push(x.clone());
                env.push(g);
                env.call()?;
                env.push(x);
                env.push(f);
                env.call()?;
            }
            Primitive::ForkArray1 => {
                let fs = env.pop()?;
                let x = env.pop()?;
                if !fs.is_array() {
                    env.push(x);
                    env.push(fs);
                    return env.call();
                }
                for f in fs.into_array().into_values().into_iter().rev() {
                    env.push(x.clone());
                    env.push(f);
                    env.call()?;
                }
            }
            Primitive::ForkArray2 => {
                let fs = env.pop()?;
                let x = env.pop()?;
                let y = env.pop()?;
                if !fs.is_array() {
                    env.push(y);
                    env.push(x);
                    env.push(fs);
                    return env.call();
                }
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
                    env.push(acc);
                    env.push(cell);
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
        }
        Ok(())
    }
}
