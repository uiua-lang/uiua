use std::fmt;

use crate::{lex::Span, primitive::Primitive, value::Value, Ident, Uiua, UiuaResult};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instr {
    Push(Value),
    BeginArray,
    EndArray(bool, usize),
    CopyGlobal(usize),
    Primitive(Primitive, usize),
    Call(usize),
    CallRef(usize, usize),
    CopyRef(usize),
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => write!(f, "{val}"),
            Instr::BeginArray => write!(f, "]"),
            Instr::EndArray(..) => write!(f, "["),
            Instr::CopyGlobal(idx) => write!(f, "g{idx}"),
            Instr::Primitive(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => Ok(()),
            Instr::CallRef(n, _) => write!(f, "ref{n}"),
            Instr::CopyRef(n) => write!(f, "{}", (*n as u8 + b'a') as char),
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
        if let FunctionId::Named(name) = &self.id {
            return write!(f, "{name}");
        }
        if self.instrs.len() != 1 {
            write!(f, "(")?;
        }
        for instr in self.instrs.iter().rev() {
            instr.fmt(f)?;
        }
        if self.instrs.len() != 1 {
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
    pub fn inverse(&self, env: &Uiua, require_unary: bool) -> UiuaResult<Self> {
        match self.instrs.as_slice() {
            [Instr::Primitive(prim, span)] => {
                return if let Some(inv) = prim.inverse() {
                    Ok(Function {
                        id: FunctionId::Primitive(inv),
                        instrs: vec![Instr::Primitive(inv, *span)],
                    })
                } else {
                    Err(env.error(format!("No inverse found for {prim}")))
                }
            }
            _ => {}
        }
        let mut args = 0;
        let mut groups: Vec<Vec<Instr>> = Vec::new();
        let no_inverse = || env.error("No inverse found");
        macro_rules! last_group {
            () => {
                groups.last_mut().ok_or_else(no_inverse)?
            };
        }
        for instr in self.instrs.iter().rev() {
            match instr {
                Instr::Push(val) => {
                    if args > 0 {
                        last_group!().push(Instr::Push(val.clone()));
                        args -= 1;
                    } else {
                        return Err(no_inverse());
                    }
                }
                Instr::Primitive(prim, span) => {
                    if let Some(inv) = prim.inverse() {
                        if let Some((a, o)) = prim.args().zip(prim.outputs()) {
                            args = a.saturating_sub(o);
                            groups.push(vec![Instr::Primitive(inv, *span)]);
                        } else {
                            return Err(no_inverse());
                        }
                    } else {
                        return Err(env.error(format!("No inverse found for {prim}")));
                    }
                }
                &Instr::Call(n) => groups.push(vec![Instr::Call(n)]),
                Instr::BeginArray => {
                    last_group!().push(Instr::EndArray(false, 0));
                }
                &Instr::EndArray(n, span) => last_group!().push(Instr::EndArray(n, span)),
                &Instr::CopyGlobal(n) => last_group!().push(Instr::CopyGlobal(n)),
                &Instr::CallRef(_, _) => return Err(no_inverse()),
                &Instr::CopyRef(_) => return Err(no_inverse()),
            }
        }
        if require_unary && args != 0 {
            return Err(env.error("Only unary functions can be inverted"));
        }
        let function = Function {
            id: FunctionId::Anonymous(env.span().clone()),
            instrs: groups
                .into_iter()
                .flat_map(|g| g.into_iter().rev())
                .collect(),
        };
        Ok(function)
    }
}

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
