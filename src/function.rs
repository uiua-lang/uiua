use std::{fmt, rc::Rc};

use crate::{lex::Span, primitive::Primitive, value::Value, Ident};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Rc<Value>),
    BeginArray,
    EndArray(usize),
    Primitive(Primitive, usize),
    Call(usize),
    DfnVal(usize),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray(_) => write!(f, "["),
            Instr::Primitive(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, ":"),
            Instr::DfnVal(n) => write!(f, "{}", (*n as u8 + b'a') as char),
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
            instrs: vec![Instr::Primitive(prim, 0)],
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
    Anonymous(Span),
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
        Primitive::Sqrt => vec![
            Instr::Push(Rc::new(0.5.into())),
            Instr::Primitive(Primitive::Pow, span),
        ],
        prim => vec![Instr::Primitive(prim.inverse()?, span)],
    })
}

fn invert_instr_fragment(instrs: &[Instr]) -> Option<Vec<Instr>> {
    Some(match instrs {
        [Instr::Primitive(prim, span)] => invert_primitive(*prim, *span)?,
        [Instr::Push(val), Instr::Primitive(Primitive::Rotate, span)] => vec![
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Neg, *span),
            Instr::Primitive(Primitive::Rotate, *span),
        ],
        [Instr::Push(val), Instr::Primitive(Primitive::Neg, _), Instr::Primitive(Primitive::Rotate, span)] =>
        {
            vec![
                Instr::Push(val.clone()),
                Instr::Primitive(Primitive::Rotate, *span),
            ]
        }
        [Instr::Push(val), Instr::Primitive(Primitive::Add, span)] => vec![
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Sub, *span),
        ],
        [Instr::Push(val), Instr::Primitive(Primitive::Sub, span)] => vec![
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Add, *span),
        ],
        [Instr::Push(val), Instr::Primitive(Primitive::Mul, span)] => vec![
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Div, *span),
        ],
        [Instr::Push(val), Instr::Primitive(Primitive::Div, span)] => vec![
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Mul, *span),
        ],
        [Instr::Push(val), Instr::Primitive(Primitive::Pow, span)] => vec![
            Instr::Push(Rc::new(1.into())),
            Instr::Push(val.clone()),
            Instr::Primitive(Primitive::Div, *span),
            Instr::Primitive(Primitive::Pow, *span),
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
