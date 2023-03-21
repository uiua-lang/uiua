use std::fmt;

use crate::{lex::Span, ops::Primitive, value::Value, Ident};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FunctionId {
    Named(Ident),
    Anonymous(Span),
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
            FunctionId::Primitive(id) => write!(f, "{id}"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for instr in &self.instrs {
            instr.fmt(f)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Value),
    BeginArray,
    EndArray(bool, usize),
    CopyGlobal(usize),
    Primitive(Primitive, usize),
    Call(usize),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray(..) => write!(f, "["),
            Instr::CopyGlobal(idx) => write!(f, "g{idx}"),
            Instr::Primitive(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "call"),
        }
    }
}
