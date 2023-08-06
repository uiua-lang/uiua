use std::{cmp::Ordering, fmt, rc::Rc};

use crate::{lex::CodeSpan, primitive::Primitive, value::Value, Ident, Uiua, UiuaResult};

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
    Dynamic(Rc<dyn Fn(&mut Uiua) -> UiuaResult>),
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
        if let FunctionKind::Dynamic(_) = self.kind {
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
            FunctionId::Primitive(prim) => write!(f, "{prim}"),
            FunctionId::Constant => write!(f, "constant"),
            FunctionId::Main => write!(f, "main"),
        }
    }
}

impl Function {
    pub fn inverse(&self) -> Option<Self> {
        if !matches!(self.kind, FunctionKind::Normal) {
            return None;
        }
        Some(Function {
            id: self.id.clone(),
            instrs: invert_instrs(&self.instrs)?,
            kind: FunctionKind::Normal,
        })
    }
}

fn invert_primitive(prim: Primitive, span: usize) -> Option<Vec<Instr>> {
    Some(match prim {
        Primitive::Sqrt => vec![Instr::push(2.0), Instr::Prim(Primitive::Pow, span)],
        prim => vec![Instr::Prim(prim.inverse()?, span)],
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
        } else {
            len += 1;
        }
    }
    if len > 1 {
        return None;
    }
    Some(inverted)
}

fn invert_instr_fragment(instrs: &[Instr]) -> Option<Vec<Instr>> {
    use Instr::*;
    use Primitive::*;
    if let [Prim(prim, span)] = instrs {
        return invert_primitive(*prim, *span);
    }

    let patterns: &[&dyn InstrPattern] = &[
        &(Val, ([Rotate], [Neg, Rotate])),
        &(Val, ([Add], [Sub])),
        &(Val, ([Sub], [Add])),
        &(Val, ([Mul], [Div])),
        &(Val, ([Div], [Mul])),
        &invert_pow_pattern,
        &invert_scalar_pattern,
    ];

    for pattern in patterns {
        let mut input = instrs;
        if let Some(inverted) = pattern.extract(&mut input) {
            if input.is_empty() {
                return Some(inverted);
            }
        }
    }

    None
}

trait InstrPattern {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>>;
}

impl<A: InstrPattern, B: InstrPattern> InstrPattern for (A, B) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b) = self;
        let mut a = a.extract(input)?;
        let b = b.extract(input)?;
        a.extend(b);
        Some(a)
    }
}

impl InstrPattern for (&[Primitive], &[Primitive]) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b) = *self;
        if a.len() > input.len() {
            return None;
        }
        let mut spans = Vec::new();
        for (instr, prim) in input.iter().zip(a.iter()) {
            match instr {
                Instr::Prim(instr_prim, span) if instr_prim == prim => spans.push(*span),
                _ => return None,
            }
        }
        *input = &input[a.len()..];
        Some(
            b.iter()
                .zip(spans.iter().cycle())
                .map(|(p, s)| Instr::Prim(*p, *s))
                .collect(),
        )
    }
}

impl<const A: usize, const B: usize> InstrPattern for ([Primitive; A], [Primitive; B]) {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        let (a, b) = *self;
        (a.as_ref(), b.as_ref()).extract(input)
    }
}

impl<F> InstrPattern for F
where
    F: Fn(&mut &[Instr]) -> Option<Vec<Instr>>,
{
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        self(input)
    }
}

fn invert_pow_pattern(input: &mut &[Instr]) -> Option<Vec<Instr>> {
    let val = Val.extract(input)?;
    let next = input.get(0)?;
    if let Instr::Prim(Primitive::Pow, span) = next {
        *input = &input[1..];
        Some(vec![
            Instr::push(1),
            val[0].clone(),
            Instr::Prim(Primitive::Div, *span),
            Instr::Prim(Primitive::Pow, *span),
        ])
    } else {
        None
    }
}

fn invert_scalar_pattern(input: &mut &[Instr]) -> Option<Vec<Instr>> {
    Val.extract(input)?;
    Some(vec![Instr::Prim(Primitive::Pop, 0)])
}

pub struct Val;
impl InstrPattern for Val {
    fn extract(&self, input: &mut &[Instr]) -> Option<Vec<Instr>> {
        match input.get(0) {
            Some(instr @ (Instr::Push(_) | Instr::DfnVal(_))) => {
                *input = &input[1..];
                Some(vec![instr.clone()])
            }
            Some(instr @ Instr::Prim(prim, _))
                if prim.args() == Some(0) && prim.outputs() == Some(1) =>
            {
                *input = &input[1..];
                Some(vec![instr.clone()])
            }
            Some(Instr::BeginArray) => {
                let mut depth = 1;
                let mut i = 1;
                loop {
                    if let Instr::EndArray(_) = input.get(i)? {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    } else if let Instr::BeginArray = input.get(i)? {
                        depth += 1;
                    }
                    i += 1;
                }
                let array_construction = &input[..=i];
                *input = &input[i + 1..];
                Some(array_construction.to_vec())
            }
            _ => None,
        }
    }
}
