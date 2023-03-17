use std::{
    f64::consts::*,
    fmt,
    io::{stdout, Write},
};

use enum_iterator::Sequence;

use crate::{array::Array, grid_fmt::GridFmt, lex::Simple, value::*, vm::CallEnv, RuntimeResult};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Primitive {
    // Pervasive monadic ops
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
    // Pervasive dyadic ops
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
    // Stack ops
    Dup,
    Flip,
    Pop,
    // Control flow ops
    ExclusiveFork,
    MonadicFork,
    DyadicFork,
    // Monadic array ops
    Len,
    Rank,
    Shape,
    First,
    Range,
    Reverse,
    Deshape,
    // Dyadic array ops
    Join,
    Reshape,
    Pick,
    Filter,
    Take,
    Drop,
    Rotate,
    // Higher order ops
    Fold,
    Reduce,
    Each,
    Cells,
    Table,
    Scan,
    // IO ops
    Show,
    Print,
    Println,
    String,
    ScanLn,
    Args,
    Var,
}

fn _keep_primitive_id_small(_: std::convert::Infallible) {
    let _: u8 = unsafe { std::mem::transmute(Some(Primitive::Not)) };
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.public_name() {
            Ok(name) => write!(f, "{name}"),
            Err(simple) => write!(f, "{simple}"),
        }
    }
}

impl Primitive {
    pub fn public_name(&self) -> Result<&'static str, Simple> {
        use Simple::*;
        match self {
            Primitive::Not => Ok("not"),
            Primitive::Neg => Ok("neg"),
            Primitive::Abs => Ok("abs"),
            Primitive::Sqrt => Ok("sqrt"),
            Primitive::Sin => Ok("sin"),
            Primitive::Cos => Ok("cos"),
            Primitive::Asin => Ok("asin"),
            Primitive::Acos => Ok("acos"),
            Primitive::Floor => Ok("floor"),
            Primitive::Ceil => Ok("ceil"),
            Primitive::Round => Ok("round"),
            Primitive::Eq => Err(Equal),
            Primitive::Ne => Err(BangEqual),
            Primitive::Lt => Err(Less),
            Primitive::Le => Err(LessEqual),
            Primitive::Gt => Err(Greater),
            Primitive::Ge => Err(GreaterEqual),
            Primitive::Add => Err(Plus),
            Primitive::Sub => Err(Minus),
            Primitive::Mul => Err(Star),
            Primitive::Div => Err(Percent),
            Primitive::Mod => Ok("mod"),
            Primitive::Pow => Ok("pow"),
            Primitive::Min => Ok("min"),
            Primitive::Max => Ok("max"),
            Primitive::Atan2 => Ok("atan2"),
            Primitive::Join => Ok("join"),
            Primitive::Reshape => Ok("reshape"),
            Primitive::Pick => Ok("pick"),
            Primitive::Filter => Ok("filter"),
            Primitive::Take => Ok("take"),
            Primitive::Drop => Ok("drop"),
            Primitive::Rotate => Ok("rotate"),
            Primitive::Dup => Err(Period),
            Primitive::Flip => Err(Tilde),
            Primitive::Pop => Err(SemiColon),
            Primitive::ExclusiveFork => Err(Bang),
            Primitive::MonadicFork => Err(Colon),
            Primitive::DyadicFork => Err(DoubleColon),
            Primitive::Fold => Ok("fold"),
            Primitive::Reduce => Err(Slash),
            Primitive::Each => Ok("each"),
            Primitive::Cells => Err(BackTick),
            Primitive::Table => Err(Caret),
            Primitive::Scan => Err(BackSlash),
            Primitive::Show => Ok("show"),
            Primitive::Print => Ok("print"),
            Primitive::Println => Ok("println"),
            Primitive::String => Ok("string"),
            Primitive::Len => Ok("len"),
            Primitive::Rank => Ok("rank"),
            Primitive::Shape => Ok("shape"),
            Primitive::First => Ok("first"),
            Primitive::Range => Ok("range"),
            Primitive::Reverse => Ok("reverse"),
            Primitive::Deshape => Ok("deshape"),
            Primitive::ScanLn => Ok("scanln"),
            Primitive::Args => Ok("args"),
            Primitive::Var => Ok("var"),
        }
    }
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
            Primitive::Eq => env.dyadic_env(Value::is_eq)?,
            Primitive::Ne => env.dyadic_env(Value::is_ne)?,
            Primitive::Lt => env.dyadic_env(Value::is_lt)?,
            Primitive::Le => env.dyadic_env(Value::is_le)?,
            Primitive::Gt => env.dyadic_env(Value::is_gt)?,
            Primitive::Ge => env.dyadic_env(Value::is_ge)?,
            Primitive::Add => env.dyadic_env(Value::add)?,
            Primitive::Sub => env.dyadic_env(Value::sub)?,
            Primitive::Mul => env.dyadic_env(Value::mul)?,
            Primitive::Div => env.dyadic_env(Value::div)?,
            Primitive::Mod => env.dyadic_env(Value::modulus)?,
            Primitive::Pow => env.dyadic_env(Value::pow)?,
            Primitive::Min => env.dyadic_env(Value::min)?,
            Primitive::Max => env.dyadic_env(Value::max)?,
            Primitive::Atan2 => env.dyadic_env(Value::atan2)?,
            Primitive::Join => env.dyadic_mut_env(Value::join)?,
            Primitive::Reshape => env.dyadic_mut_env(Value::reshape)?,
            Primitive::Pick => env.dyadic_mut_env(Value::pick)?,
            Primitive::Filter => env.dyadic_mut_env(Value::replicate)?,
            Primitive::Take => env.dyadic_mut_env(Value::take)?,
            Primitive::Drop => env.dyadic_mut_env(Value::drop)?,
            Primitive::Rotate => env.dyadic_mut_env(Value::rotate)?,
            Primitive::Dup => {
                let x = env.top_mut()?.clone();
                env.push(x);
            }
            Primitive::Flip => {
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
