use std::{fmt, mem::transmute};

use nanbox::{NanBox, NanBoxable};

use crate::{
    lex::Span,
    ops::{HigherOp, Op1, Op2},
    Ident,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primitive {
    Op1(Op1),
    Op2(Op2),
    HigherOp(HigherOp),
}

fn _keep_primitive_id_small(_: std::convert::Infallible) {
    let _: [u8; 2] = unsafe { std::mem::transmute(Some(Primitive::Op1(Op1::Neg))) };
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Op1(op) => write!(f, "{op}"),
            Primitive::Op2(op) => write!(f, "{op}"),
            Primitive::HigherOp(op) => write!(f, "{op}"),
        }
    }
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

impl From<HigherOp> for Primitive {
    fn from(alg: HigherOp) -> Self {
        Self::HigherOp(alg)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(Span),
    FormatString(Span),
    Primitive(Primitive),
}

impl From<Ident> for FunctionId {
    fn from(name: Ident) -> Self {
        Self::Named(name)
    }
}

impl From<Op1> for FunctionId {
    fn from(op: Op1) -> Self {
        Self::Primitive(op.into())
    }
}

impl From<Op2> for FunctionId {
    fn from(op: Op2) -> Self {
        Self::Primitive(op.into())
    }
}

impl From<HigherOp> for FunctionId {
    fn from(hop: HigherOp) -> Self {
        Self::Primitive(hop.into())
    }
}

impl From<Primitive> for FunctionId {
    fn from(id: Primitive) -> Self {
        Self::Primitive(id)
    }
}

impl fmt::Display for FunctionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionId::Named(name) => write!(f, "`{name}`"),
            FunctionId::Anonymous(span) => write!(f, "fn from {span}"),
            FunctionId::FormatString(span) => write!(f, "format string from {span}"),
            FunctionId::Primitive(id) => write!(f, "{id}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Function {
    Code(u32),
    Primitive(Primitive),
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Code(start) => write!(f, "({start})"),
            Function::Primitive(prim) => write!(f, "{prim}"),
        }
    }
}

impl NanBoxable for Function {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        let [a, b, c, d, e]: [u8; 5] = NanBoxable::from_nan_box(n);
        let start = u32::from_le_bytes([b, c, d, e]);
        if a == 0 {
            Function::Code(start)
        } else {
            Function::Primitive(transmute(u16::from_le_bytes([b, c])))
        }
    }
    fn into_nan_box(self) -> NanBox {
        match self {
            Function::Code(start) => {
                let [b, c, d, e] = start.to_le_bytes();
                NanBoxable::into_nan_box([0, b, c, d, e])
            }
            Function::Primitive(prim) => {
                let [b, c]: [u8; 2] = unsafe { transmute(prim) };
                NanBoxable::into_nan_box([1, b, c, 0, 0])
            }
        }
    }
}
