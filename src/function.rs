use std::{fmt, mem::transmute, sync::Arc};

use nanbox::{NanBox, NanBoxable};

use crate::{
    lex::Span,
    ops::{HigherOp, Op1, Op2},
    value::Value,
    Ident,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveId {
    Op1(Op1),
    Op2(Op2),
    HigherOp(HigherOp),
}

fn _keep_primitive_id_small(_: std::convert::Infallible) {
    let _: [u8; 2] = unsafe { std::mem::transmute(Some(PrimitiveId::Op1(Op1::Neg))) };
}

impl fmt::Display for PrimitiveId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimitiveId::Op1(op) => write!(f, "{op}"),
            PrimitiveId::Op2(op) => write!(f, "{op}"),
            PrimitiveId::HigherOp(op) => write!(f, "{op}"),
        }
    }
}

impl From<Op1> for PrimitiveId {
    fn from(op: Op1) -> Self {
        Self::Op1(op)
    }
}

impl From<Op2> for PrimitiveId {
    fn from(op: Op2) -> Self {
        Self::Op2(op)
    }
}

impl From<HigherOp> for PrimitiveId {
    fn from(alg: HigherOp) -> Self {
        Self::HigherOp(alg)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(Span),
    FormatString(Span),
    Primitive(PrimitiveId),
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

impl From<PrimitiveId> for FunctionId {
    fn from(id: PrimitiveId) -> Self {
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
pub struct Function {
    pub(crate) start: u32,
    pub(crate) params: u8,
    pub(crate) primitive: Option<PrimitiveId>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(primitive) = self.primitive {
            write!(f, "{primitive}")
        } else {
            write!(f, "fn({} {})", self.start, self.params)
        }
    }
}

impl NanBoxable for Function {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        let [a, b, c, d, e, f]: [u8; 6] = NanBoxable::from_nan_box(n);
        Self {
            start: u32::from_le_bytes([a, b, c, 0]),
            params: d,
            primitive: transmute(u16::from_le_bytes([e, f])),
        }
    }
    fn into_nan_box(self) -> NanBox {
        let [a, b, c, z] = self.start.to_le_bytes();
        debug_assert_eq!(z, 0);
        let d = self.params;
        let [e, f]: [u8; 2] = unsafe { transmute(self.primitive) };
        NanBoxable::into_nan_box([a, b, c, d, e, f])
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
        if let Some(primitive) = self.function.primitive {
            write!(f, "({primitive}")?
        } else {
            write!(f, "fn({}", self.function.start)?
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
        write!(f, " /{})", self.function.params)
    }
}
