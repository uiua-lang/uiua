use std::{fmt, mem::transmute};

use nanbox::{NanBox, NanBoxable};

use crate::{lex::Span, ops::Primitive, Ident};

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

impl From<Primitive> for FunctionId {
    fn from(op: Primitive) -> Self {
        Self::Primitive(op)
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
        match a {
            0 => Function::Code(start),
            1 => Function::Primitive(transmute([b, c])),
            _ => unreachable!(),
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
