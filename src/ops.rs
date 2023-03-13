use std::{f64::consts::*, fmt};

use enum_iterator::Sequence;

use crate::{
    array::Array,
    function::PrimitiveId,
    grid_fmt::GridFmt,
    value::*,
    vm::{Env, Vm},
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
    Show,
    Print,
    Println,
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
            Op1::Show => write!(f, "show"),
            Op1::Print => write!(f, "print"),
            Op1::Println => write!(f, "println"),
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
            Op1::Show => print!("{}", self.grid_string()),
            Op1::Print => print!("{self}"),
            Op1::Println => println!("{self}"),
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

impl Op1 {
    pub fn inverse(&self) -> Option<Op1> {
        Some(match self {
            Op1::Id | Op1::Not | Op1::Neg | Op1::Reverse => *self,
            Op1::Sin => Op1::Asin,
            Op1::Cos => Op1::Acos,
            Op1::Asin => Op1::Sin,
            Op1::Acos => Op1::Cos,
            _ => return None,
        })
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
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum HigherOp {
    Pipe,
    BackPipe,
    Compose,
    Slf,
    BlackBird,
    Flip,
    LeftLeaf,
    LeftTree,
    RightLeaf,
    RightTree,
    Fold,
    Reduce,
    Each,
    Cells,
    Table,
    Undo1,
}

impl fmt::Display for HigherOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HigherOp::Pipe => write!(f, "pipe"),
            HigherOp::BackPipe => write!(f, "backpipe"),
            HigherOp::Compose => write!(f, "compose"),
            HigherOp::BlackBird => write!(f, "blackbird"),
            HigherOp::Slf => write!(f, "self"),
            HigherOp::Flip => write!(f, "flip"),
            HigherOp::LeftLeaf => write!(f, "left_leaf"),
            HigherOp::LeftTree => write!(f, "left_tree"),
            HigherOp::RightLeaf => write!(f, "right_leaf"),
            HigherOp::RightTree => write!(f, "right_tree"),
            HigherOp::Fold => write!(f, "fold"),
            HigherOp::Reduce => write!(f, "reduce"),
            HigherOp::Each => write!(f, "each"),
            HigherOp::Cells => write!(f, "cells"),
            HigherOp::Table => write!(f, "table"),
            HigherOp::Undo1 => write!(f, "undo1"),
        }
    }
}

impl HigherOp {
    pub(crate) fn params(&self) -> u8 {
        match self {
            HigherOp::Pipe => 2,
            HigherOp::BackPipe => 2,
            HigherOp::Compose => 3,
            HigherOp::Slf => 2,
            HigherOp::Flip => 3,
            HigherOp::BlackBird => 4,
            HigherOp::LeftLeaf => 4,
            HigherOp::LeftTree => 5,
            HigherOp::RightLeaf => 4,
            HigherOp::RightTree => 5,
            HigherOp::Each => 2,
            HigherOp::Cells => 2,
            HigherOp::Fold => 3,
            HigherOp::Reduce => 2,
            HigherOp::Table => 3,
            HigherOp::Undo1 => 2,
        }
    }
    pub fn run(&self, vm: &mut Vm, env: &Env) -> RuntimeResult {
        match self {
            HigherOp::Pipe => {
                let x = vm.pop();
                let f = vm.pop();
                vm.push(x);
                vm.push(f);
                vm.call(1, env.assembly, 0)?;
            }
            HigherOp::BackPipe => {
                let f = vm.pop();
                let x = vm.pop();
                vm.push(x);
                vm.push(f);
                vm.call(1, env.assembly, 0)?;
            }
            HigherOp::Compose => {
                // x g f
                let f = vm.pop(); // x g
                vm.call(1, env.assembly, 0)?; // gx
                vm.push(f); // gx f
                vm.call(1, env.assembly, 0)?; // f(gx)
            }
            HigherOp::Slf => {
                let f = vm.pop();
                let x = vm.pop();
                vm.push(x.clone());
                vm.push(x);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::BlackBird => {
                // y x g f
                let f = vm.pop(); // y x g
                vm.call(2, env.assembly, 0)?; // gxy
                vm.push(f); // gxy f
                vm.call(1, env.assembly, 0)?; // f(gxy)
            }
            HigherOp::Flip => {
                let f = vm.pop();
                let a = vm.pop();
                let b = vm.pop();
                vm.push(a);
                vm.push(b);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::LeftLeaf => {
                /*
                      f
                     / \
                    g   b
                    |
                    a
                */
                let g = vm.pop();
                let f = vm.pop();
                let a = vm.pop();
                let b = vm.pop();
                if f.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its right argument to be binary, but it is unary"
                    )));
                }
                vm.push(a);
                vm.push(g);
                vm.call(1, env.assembly, 0)?;
                let ga = vm.pop();
                vm.push(b);
                vm.push(ga);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::RightLeaf => {
                /*
                      f
                     / \
                    a   g
                        |
                        b
                */
                let f = vm.pop();
                let g = vm.pop();
                let a = vm.pop();
                let b = vm.pop();
                if f.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its left argument to be binary, but it is unary",
                    )));
                }
                vm.push(b);
                vm.push(g);
                vm.call(1, env.assembly, 0)?;
                vm.push(a);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::LeftTree => {
                /*
                      f
                     / \
                    g   c
                   / \
                  a   b
                */
                let g = vm.pop();
                let f = vm.pop();
                let a = vm.pop();
                let b = vm.pop();
                let c = vm.pop();
                if f.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its right argument to be binary, but it is unary",
                    )));
                }
                if g.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its left argument to be binary, but it is unary",
                    )));
                }
                vm.push(b);
                vm.push(a);
                vm.push(g);
                vm.call(2, env.assembly, 0)?;
                let gab = vm.pop();
                vm.push(c);
                vm.push(gab);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::RightTree => {
                /*
                      f
                     / \
                    a   g
                       / \
                      b   c
                */
                let f = vm.pop();
                let g = vm.pop();
                let a = vm.pop();
                let b = vm.pop();
                let c = vm.pop();
                if f.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its left argument to be binary, but it is unary",
                    )));
                }
                if g.params() < 2 {
                    return Err(env.error(format!(
                        "{self} expects its right argument to be binary, but it is unary",
                    )));
                }
                vm.push(c);
                vm.push(b);
                vm.push(g);
                vm.call(2, env.assembly, 0)?;
                vm.push(a);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::Fold => {
                let f = vm.pop();
                let mut acc = vm.pop();
                let xs = vm.pop();
                if !xs.is_array() {
                    vm.push(acc);
                    vm.push(xs);
                    vm.push(f);
                    return vm.call(2, env.assembly, 0);
                }
                for cell in xs.into_array().into_values() {
                    vm.push(acc);
                    vm.push(cell);
                    vm.push(f.clone());
                    vm.call(2, env.assembly, 0)?;
                    acc = vm.pop();
                }
                vm.push(acc);
            }
            HigherOp::Reduce => {
                let f = vm.pop();
                let xs = vm.pop();
                if !xs.is_array() {
                    vm.push(xs);
                    return Ok(());
                }
                let mut cells = xs.into_array().into_values().into_iter();
                let Some(mut acc) = cells.next() else {
                    return Err(env.error("Cannot reduce empty array"));
                };
                for cell in cells {
                    vm.push(acc);
                    vm.push(cell);
                    vm.push(f.clone());
                    vm.call(2, env.assembly, 0)?;
                    acc = vm.pop();
                }
                vm.push(acc);
            }
            HigherOp::Each => {
                let f = vm.pop();
                let xs = vm.pop();
                if !xs.is_array() {
                    vm.push(xs);
                    vm.push(f);
                    return vm.call(1, env.assembly, 0);
                }
                let (shape, values) = xs.into_array().into_parts();
                let mut new_values = Vec::with_capacity(values.len());
                for val in values {
                    vm.push(val);
                    vm.push(f.clone());
                    vm.call(1, env.assembly, 0)?;
                    new_values.push(vm.pop());
                }
                vm.push(Array::from((shape, new_values)).normalized(0));
            }
            HigherOp::Cells => {
                let f = vm.pop();
                let xs = vm.pop();
                if !xs.is_array() {
                    vm.push(xs);
                    vm.push(f);
                    return vm.call(1, env.assembly, 0);
                }
                let array = xs.into_array();
                let mut cells = Vec::with_capacity(array.len());
                for cell in array.into_values() {
                    vm.push(cell);
                    vm.push(f.clone());
                    vm.call(1, env.assembly, 0)?;
                    cells.push(vm.pop());
                }
                vm.push(Array::from(cells).normalized(1));
            }
            HigherOp::Table => {
                let f = vm.pop();
                let xs = vm.pop();
                let ys = vm.pop();
                if !xs.is_array() && !ys.is_array() {
                    vm.push(ys);
                    vm.push(xs);
                    vm.push(f);
                    return vm.call(2, env.assembly, 0);
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
                        vm.push(b);
                        vm.push(a.clone());
                        vm.push(f.clone());
                        vm.call(2, env.assembly, 0)?;
                        row.push(vm.pop());
                    }
                    table.push(Value::from(Array::from(row).normalized(1)));
                }
                vm.push(Array::from(table).normalized(1));
            }
            HigherOp::Undo1 => {
                let f = vm.pop();
                let mut x = vm.pop();
                match f.raw_ty() {
                    RawType::Function => {
                        match f.function().primitive {
                            Some(PrimitiveId::Op1(op1)) => {
                                if let Some(inverse) = op1.inverse() {
                                    x.op1(inverse, env)?;
                                    vm.push(x);
                                    return Ok(());
                                }
                            }
                            _ => {}
                        }
                        return Err(env.error(format!("{} cannot be undone", f.function())));
                    }
                    _ => return Err(env.error("Only functions can be undone")),
                }
            }
        }
        Ok(())
    }
}
