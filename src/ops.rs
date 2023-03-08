use std::{f64::consts::*, fmt};

use enum_iterator::Sequence;

use crate::{
    array_fmt::GridFmt,
    compile::Assembly,
    value::*,
    vm::{Env, Instr},
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
    Range,
    Reverse,
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
            Op1::Range => write!(f, "range"),
            Op1::Reverse => write!(f, "reverse"),
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
            Op1::Range => *self = self.range(env)?.into(),
            Op1::Reverse => self.reverse(),
            Op1::Neg => *self = self.neg(env)?,
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
    Concat,
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
            Op2::Concat => write!(f, "concat"),
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
            Op2::Concat => todo!("concat"),
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum Algorithm {
    Compose,
    BlackBird,
    Flip,
    Fold,
    Each,
    Sum,
    Product,
    While,
    LeftThen,
    RightThen,
}

impl fmt::Display for Algorithm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Algorithm::Compose => write!(f, "compose"),
            Algorithm::BlackBird => write!(f, "blackbird"),
            Algorithm::Flip => write!(f, "flip"),
            Algorithm::Fold => write!(f, "fold"),
            Algorithm::Each => write!(f, "each"),
            Algorithm::Sum => write!(f, "sum"),
            Algorithm::Product => write!(f, "product"),
            Algorithm::While => write!(f, "while"),
            Algorithm::LeftThen => write!(f, "left_then"),
            Algorithm::RightThen => write!(f, "right_then"),
        }
    }
}

impl Algorithm {
    pub(crate) fn params(&self) -> u16 {
        match self {
            Algorithm::Compose => 3,
            Algorithm::BlackBird => 4,
            Algorithm::Flip => 3,
            Algorithm::Each => 2,
            Algorithm::Fold => 3,
            Algorithm::Sum => 1,
            Algorithm::Product => 1,
            Algorithm::While => 3,
            Algorithm::LeftThen => 3,
            Algorithm::RightThen => 3,
        }
    }
    pub(crate) fn instrs(&self, _assembly: &Assembly) -> Vec<Instr> {
        #[allow(unused_imports)]
        use {
            crate::ast::BinOp::*,
            crate::ops::{Op1, Op2},
            Instr::*,
        };
        let mut instrs = match self {
            Algorithm::Compose => vec![
                // x g f
                Rotate(3), // f x g
                Call(0),   // f gx
                Swap,      // gx f
                Call(0),   // f(gx)
            ],
            Algorithm::BlackBird => vec![
                // y x g f
                Rotate(4), // f y x g
                Call(0),   // f y gx
                Call(0),   // f gxy
                Swap,      // gxy f
                Call(0),   // f(gxy)
            ],
            Algorithm::Flip => vec![
                // b, a, f
                Rotate(3), // f, b, a
                Swap,      // f, a, b
                Move(3),   // a, b, f
                Call(0),
                Call(0),
            ],
            Algorithm::While => vec![
                // 0, next = (+1), cond = (<3)
                Move(3), // next, cond, 0
                // Loop start
                CopyRel(1),          // next, cond, 0, 0
                CopyRel(3),          // next, cond, 0, 0, cond
                Call(0),             // next, cond, 0, true
                PopJumpIf(4, false), // next, cond, 0
                CopyRel(3),          // next, cond, 0, next
                Call(0),             // next, cond, 1
                Jump(-6),
            ],
            Algorithm::LeftThen => vec![
                // x f g
                CopyRel(3), // x, f, g, x
                Swap,       // x, f, x, g
                Call(0),    // x, f, gx
                Swap,       // x, gx, f
                Call(0),    // x, f(gx)
                Call(0),    // f(gx)x
            ],
            Algorithm::RightThen => vec![
                // x g f
                CopyRel(3), // x, g, f, x
                Move(3),    // x, f, x, g
                Call(0),    // x, f, gx
                Rotate(3),  // gx, x, f
                Call(0),    // gx, fx
                Call(0),    // fx(gx)
            ],
            Algorithm::Each => vec![
                // [1, 2, 3], f = neg
                Swap,              // f, [1, 2, 3]
                Op1(Op1::Reverse), // f, [3, 2, 1]
                Array(0),          // f, [3, 2, 1], []
                // Loop start
                Swap,               // f, [], [3, 2, 1]
                CopyRel(1),         // f, [], [3, 2, 1], [3, 2, 1]
                Op1(Op1::Len),      // f, [], [3, 2, 1], 3
                Push(0.0.into()),   // f, [], [3, 2, 1], 3, 0
                Op2(Op2::Eq, 0),    // f, [], [3, 2, 1], false
                PopJumpIf(8, true), // f, [], [3, 2, 1]
                ArrayPop,           // f, [], [3, 2], 1
                CopyRel(4),         // f, [], [3, 2], 1, f
                Call(0),            // f, [], [3, 2], -1
                Move(3),            // f, [3, 2], -1, []
                Swap,               // f, [3, 2], [], -1
                ArrayPush,          // f, [3, 2], [-1]
                Jump(-12),
                // Loop end
                Swap,
            ],
            _ => vec![],
        };
        instrs.insert(0, Comment(self.to_string()));
        instrs
    }
}
