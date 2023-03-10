use std::{f64::consts::*, fmt};

use enum_iterator::Sequence;

use crate::{
    array::Array,
    array_fmt::GridFmt,
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
    Nil,
    Id,
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
    Rank,
    Shape,
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
            Op1::Nil => write!(f, "nil"),
            Op1::Id => write!(f, "id"),
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
            Op1::Rank => write!(f, "rank"),
            Op1::Shape => write!(f, "shape"),
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
            Op1::Nil => *self = Value::nil(),
            Op1::Id => {}
            Op1::Show => println!("{}", self.grid_string()),
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
            Op1::Floor => *self = self.floor(env)?,
            Op1::Ceil => *self = self.ceil(env)?,
            Op1::Round => *self = self.round(env)?,
            Op1::Deshape => self.deshape(),
            op => todo!("{op}"),
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
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum HigherOp {
    Compose,
    Slf,
    BlackBird,
    Phoenix,
    Psi,
    Flip,
    While,
    LeftThen,
    RightThen,
    Fold,
    Each,
    Table,
}

impl fmt::Display for HigherOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HigherOp::Compose => write!(f, "compose"),
            HigherOp::BlackBird => write!(f, "blackbird"),
            HigherOp::Phoenix => write!(f, "phoenix"),
            HigherOp::Psi => write!(f, "psi"),
            HigherOp::Slf => write!(f, "self"),
            HigherOp::Flip => write!(f, "flip"),
            HigherOp::While => write!(f, "while"),
            HigherOp::LeftThen => write!(f, "left_then"),
            HigherOp::RightThen => write!(f, "right_then"),
            HigherOp::Fold => write!(f, "fold"),
            HigherOp::Each => write!(f, "each"),
            HigherOp::Table => write!(f, "table"),
        }
    }
}

impl HigherOp {
    pub(crate) fn params(&self) -> u16 {
        match self {
            HigherOp::Compose => 3,
            HigherOp::Slf => 2,
            HigherOp::BlackBird => 4,
            HigherOp::Phoenix => 4,
            HigherOp::Psi => 5,
            HigherOp::Flip => 3,
            HigherOp::While => 3,
            HigherOp::LeftThen => 3,
            HigherOp::RightThen => 3,
            HigherOp::Each => 2,
            HigherOp::Fold => 3,
            HigherOp::Table => 3,
        }
    }
    pub fn run(&self, vm: &mut Vm, env: &Env) -> RuntimeResult {
        match self {
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
                vm.call(1, env.assembly, 0)?; // gxy
                vm.push(f); // gxy f
                vm.call(2, env.assembly, 0)?; // f(gxy)
            }
            HigherOp::Phoenix => {
                let a = vm.pop();
                let b = vm.pop();
                let c = vm.pop();
                let x = vm.pop();
                vm.push(x.clone());
                vm.push(b);
                vm.call(1, env.assembly, 0)?;
                vm.push(x);
                vm.push(c);
                vm.call(1, env.assembly, 0)?;
                vm.push(a);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::Psi => {
                let a = vm.pop();
                let b = vm.pop();
                let c = vm.pop();
                let x = vm.pop();
                let y = vm.pop();
                vm.push(x);
                vm.push(b);
                vm.call(1, env.assembly, 0)?;
                vm.push(y);
                vm.push(c);
                vm.call(1, env.assembly, 0)?;
                vm.push(a);
                vm.call(2, env.assembly, 0)?;
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
            HigherOp::While => {
                let cond = vm.pop();
                let body = vm.pop();
                let mut init = vm.pop();
                loop {
                    vm.push(init.clone());
                    vm.push(cond.clone());
                    vm.call(1, env.assembly, 0)?;
                    if vm.pop().is_falsy() {
                        break;
                    }
                    vm.push(init.clone());
                    vm.push(body.clone());
                    vm.call(1, env.assembly, 0)?;
                    init = vm.pop();
                }
                vm.push(init);
            }
            HigherOp::LeftThen => {
                let g = vm.pop();
                let f = vm.pop();
                let x = vm.pop();
                vm.push(x.clone());
                vm.push(g);
                vm.call(1, env.assembly, 0)?;
                let gx = vm.pop();
                vm.push(x);
                vm.push(gx);
                vm.push(f);
                vm.call(2, env.assembly, 0)?;
            }
            HigherOp::RightThen => {
                let f = vm.pop();
                let g = vm.pop();
                let x = vm.pop();
                vm.push(x.clone());
                vm.push(g);
                vm.call(1, env.assembly, 0)?;
                let gx = vm.pop();
                vm.push(x);
                vm.push(gx);
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
                    vm.push(acc.clone());
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
        }
        Ok(())
    }
}
