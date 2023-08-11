use std::{cmp::Ordering, fmt, sync::Arc};

use crate::{
    check::instrs_stack_delta, lex::CodeSpan, primitive::Primitive, value::Value, Ident, Uiua,
    UiuaResult,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Box<Value>),
    BeginArray,
    EndArray(usize),
    Prim(Primitive, usize),
    Call(usize),
    DfnVal(usize),
}

impl Instr {
    pub fn push(val: impl Into<Value>) -> Self {
        Self::Push(Box::new(val.into()))
    }
    pub fn as_push(&self) -> Option<&Value> {
        match self {
            Instr::Push(val) => Some(val),
            _ => None,
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray(_) => write!(f, "["),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, ":"),
            Instr::DfnVal(n) => write!(f, "{}", (*n as u8 + b'a') as char),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
    pub kind: FunctionKind,
}

#[derive(Clone)]
pub enum FunctionKind {
    Normal,
    Dfn(u8),
    Dynamic {
        f: Arc<dyn Fn(&mut Uiua) -> UiuaResult + Send + Sync>,
        inputs: u8,
        delta: i8,
    },
}

impl From<Primitive> for Function {
    fn from(prim: Primitive) -> Self {
        Self {
            id: FunctionId::Primitive(prim),
            instrs: vec![Instr::Prim(prim, 0)],
            kind: FunctionKind::Normal,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.instrs == other.instrs
    }
}

impl Eq for Function {}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id
            .cmp(&other.id)
            .then_with(|| self.instrs.cmp(&other.instrs))
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let FunctionId::Named(name) = &self.id {
            return write!(f, "{name}");
        }
        if let Some(prim) = self.as_primitive() {
            return write!(f, "{prim}");
        }
        if let FunctionKind::Dynamic { .. } = self.kind {
            return write!(f, "<dynamic>");
        }
        if let FunctionKind::Dfn(_) = self.kind {
            write!(f, "{{")?;
        } else {
            write!(f, "(")?;
        }
        for instr in self.instrs.iter().rev() {
            instr.fmt(f)?;
        }
        if let FunctionKind::Dfn(_) = self.kind {
            write!(f, "}}")?;
        } else {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Function {
    /// Get how many arguments this function takes and by how much it changes the height of the stack.
    /// Returns `None` if either of these values are dynamic.
    pub fn args_delta(&self) -> Option<(usize, isize)> {
        if let FunctionKind::Dynamic { inputs, delta, .. } = &self.kind {
            Some((*inputs as usize, *delta as isize))
        } else {
            instrs_stack_delta(&self.instrs)
        }
    }
    pub fn constant(value: impl Into<Value>) -> Self {
        Function {
            id: FunctionId::Constant,
            instrs: vec![Instr::push(value.into())],
            kind: FunctionKind::Normal,
        }
    }
    pub fn as_primitive(&self) -> Option<Primitive> {
        match &self.id {
            FunctionId::Primitive(prim) => Some(*prim),
            _ => None,
        }
    }
    pub(crate) fn as_flipped_primitive(&self) -> Option<(Primitive, bool)> {
        match &self.id {
            FunctionId::Primitive(prim) => Some((*prim, false)),
            _ => match self.instrs.as_slice() {
                [Instr::Prim(Primitive::Flip, _), Instr::Prim(prim, _)] => Some((*prim, true)),
                _ => None,
            },
        }
    }
    pub fn compose(self, other: Self) -> Self {
        match (&self.kind, &other.kind) {
            (FunctionKind::Dynamic { .. }, _) | (_, FunctionKind::Dynamic { .. }) => Function {
                id: self.id.clone().compose(other.id.clone()),
                instrs: vec![
                    Instr::push(other),
                    Instr::Call(0),
                    Instr::push(self),
                    Instr::Call(0),
                ],
                kind: FunctionKind::Normal,
            },
            _ => Function {
                id: self.id.compose(other.id),
                instrs: {
                    let mut instrs = other.instrs;
                    instrs.extend(self.instrs);
                    instrs
                },
                kind: FunctionKind::Normal,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(CodeSpan),
    Primitive(Primitive),
    Constant,
    Main,
    Composed(Vec<Self>),
}

impl FunctionId {
    pub fn compose(self, other: Self) -> Self {
        match (self, other) {
            (FunctionId::Composed(mut a), FunctionId::Composed(b)) => {
                a.extend(b);
                FunctionId::Composed(a)
            }
            (FunctionId::Composed(mut a), b) => {
                a.push(b);
                FunctionId::Composed(a)
            }
            (a, FunctionId::Composed(mut b)) => {
                b.insert(0, a);
                FunctionId::Composed(b)
            }
            (a, b) => FunctionId::Composed(vec![a, b]),
        }
    }
}

impl PartialEq<&str> for FunctionId {
    fn eq(&self, other: &&str) -> bool {
        match self {
            FunctionId::Named(name) => name == other,
            _ => false,
        }
    }
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
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Constant => write!(f, "constant"),
            FunctionId::Main => write!(f, "main"),
            FunctionId::Composed(ids) => write!(f, "{ids:?}"),
        }
    }
}
