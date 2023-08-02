use std::{fmt, rc::Rc};

use crate::{lex::CodeSpan, primitive::Primitive, value::Value, Ident};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Value),
    BeginArray,
    EndArray(usize),
    Prim(Primitive, usize),
    Call(usize),
    DfnVal(usize),
    If(Rc<Function>, Rc<Function>),
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
            Instr::If(if_true, if_false) => write!(f, "(if {if_true} {if_false})"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub id: FunctionId,
    pub instrs: Vec<Instr>,
    pub dfn_args: Option<u8>,
}

impl From<Primitive> for Function {
    fn from(prim: Primitive) -> Self {
        Self {
            id: FunctionId::Primitive(prim),
            instrs: vec![Instr::Prim(prim, 0)],
            dfn_args: None,
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
        if let FunctionId::Named(name) = &self.id {
            return write!(f, "{name}");
        }
        if let Some(prim) = self.as_primitive() {
            return write!(f, "{prim}");
        }
        if self.dfn_args.is_some() {
            write!(f, "{{")?;
        } else {
            write!(f, "(")?;
        }
        for instr in self.instrs.iter().rev() {
            instr.fmt(f)?;
        }
        if self.dfn_args.is_some() {
            write!(f, "}}")?;
        } else {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl Function {
    pub fn as_primitive(&self) -> Option<Primitive> {
        match &self.id {
            FunctionId::Primitive(prim) => Some(*prim),
            _ => None,
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
            FunctionId::Constant => write!(f, "constant"),
            FunctionId::Main => write!(f, "main"),
        }
    }
}

fn invert_primitive(prim: Primitive, span: usize) -> Option<Vec<Instr>> {
    Some(match prim {
        Primitive::Sqrt => vec![Instr::Push(2.0.into()), Instr::Prim(Primitive::Pow, span)],
        prim => vec![Instr::Prim(prim.inverse()?, span)],
    })
}

fn invert_instr_fragment(instrs: &[Instr]) -> Option<Vec<Instr>> {
    use Instr::*;
    use Primitive::*;
    Some(match instrs {
        [Prim(prim, span)] => invert_primitive(*prim, *span)?,
        [Push(val), Prim(Rotate, span)] => {
            vec![Push(val.clone()), Prim(Neg, *span), Prim(Rotate, *span)]
        }
        [Push(val), Prim(Neg, _), Prim(Rotate, span)] => {
            vec![Push(val.clone()), Prim(Rotate, *span)]
        }
        [Push(val), Prim(Add, span)] => {
            vec![Push(val.clone()), Prim(Sub, *span)]
        }
        [Push(val), Prim(Sub, span)] => {
            vec![Push(val.clone()), Prim(Add, *span)]
        }
        [Push(val), Prim(Mul, span)] => {
            vec![Push(val.clone()), Prim(Div, *span)]
        }
        [Push(val), Prim(Div, span)] => {
            vec![Push(val.clone()), Prim(Mul, *span)]
        }
        [Push(val), Prim(Pow, span)] => vec![
            Push(1.into()),
            Push(val.clone()),
            Prim(Div, *span),
            Prim(Pow, *span),
        ],
        _ => return None,
    })
}

fn invert_instrs(instrs: &[Instr]) -> Option<Vec<Instr>> {
    let mut inverted = Vec::new();
    let mut start = 0;
    let mut len = 1;
    while start + len <= instrs.len() {
        if let Some(mut inverted_fragment) = invert_instr_fragment(&instrs[start..start + len]) {
            inverted_fragment.append(&mut inverted);
            inverted = inverted_fragment;
            start += len;
            len = 1;
        } else if len >= 3 {
            return None;
        } else {
            len += 1;
        }
    }
    Some(inverted)
}

impl Function {
    pub fn inverse(&self) -> Option<Self> {
        Some(Function {
            id: self.id.clone(),
            instrs: invert_instrs(&self.instrs)?,
            dfn_args: self.dfn_args,
        })
    }
}
