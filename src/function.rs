use std::{fmt, mem::transmute, sync::Arc};

use nanbox::{NanBox, NanBoxable};

use crate::{
    lex::Span,
    ops::{HigherOp, Op1, Op2},
    value::Value,
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

impl Primitive {
    pub fn params(&self) -> u8 {
        match self {
            Primitive::Op1(_) => 1,
            Primitive::Op2(_) => 2,
            Primitive::HigherOp(op) => op.params(),
        }
    }
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
    Code { start: u32, params: u8 },
    Primitive(Primitive),
}

impl Function {
    pub fn params(&self) -> u8 {
        match self {
            Function::Code { params, .. } => *params,
            Function::Primitive(prim) => prim.params(),
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Code { start, params } => write!(f, "fn({} {})", start, params),
            Function::Primitive(prim) => write!(f, "{prim}"),
        }
    }
}

impl NanBoxable for Function {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        let [a, b, c, d, e]: [u8; 5] = NanBoxable::from_nan_box(n);
        if a == 0 {
            Function::Code {
                start: u32::from_le_bytes([b, c, d, 0]),
                params: e,
            }
        } else {
            Function::Primitive(transmute(u16::from_le_bytes([b, c])))
        }
    }
    fn into_nan_box(self) -> NanBox {
        match self {
            Function::Code { start, params } => {
                let [b, c, d, z] = start.to_le_bytes();
                debug_assert_eq!(z, 0);
                NanBoxable::into_nan_box([0, b, c, d, params])
            }
            Function::Primitive(prim) => {
                let [b, c]: [u8; 2] = unsafe { transmute(prim) };
                NanBoxable::into_nan_box([1, b, c, 0, 0])
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Arc<[Value]>,
    pub(crate) span: usize,
}

impl Partial {
    pub fn new(function: Function, args: impl IntoIterator<Item = Value>, span: usize) -> Self {
        Self {
            function,
            args: args.into_iter().collect(),
            span,
        }
    }
}

impl fmt::Debug for Partial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Partial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.function {
            Function::Code { start, .. } => write!(f, "fn({}", start)?,
            Function::Primitive(prim) => write!(f, "({prim}")?,
        }
        for arg in self.args.iter().rev() {
            if arg.is_array() {
                let arr = arg.array();
                match arr.rank() {
                    0 => write!(f, " {arr}")?,
                    1 => {
                        write!(f, " [")?;
                        for (i, value) in arr.clone().into_values().into_iter().take(3).enumerate()
                        {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{value}")?;
                        }
                        if arr.len() > 3 {
                            write!(f, ", ...")?;
                        }
                        write!(f, "]")?
                    }
                    _ => {
                        write!(f, " [shape")?;
                        for dim in arr.shape() {
                            write!(f, " {dim}")?;
                        }
                        write!(f, "]")?
                    }
                }
            } else {
                write!(f, " {}", arg)?
            }
        }
        write!(f, " /{})", self.function.params())
    }
}
