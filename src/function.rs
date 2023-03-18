use std::{fmt, mem::transmute, str::FromStr};

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
    Selector(Selector),
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
            Function::Selector(sel) => write!(f, "{sel}"),
        }
    }
}

impl NanBoxable for Function {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        let [a, b, c, d, e, f]: [u8; 6] = NanBoxable::from_nan_box(n);
        let start = u32::from_le_bytes([b, c, d, e]);
        match a {
            0 => Function::Code(start),
            1 => Function::Primitive(transmute([b, c])),
            2 => Function::Selector(Selector([b, c, d, e, f])),
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
            Function::Selector(sel) => {
                let [b, c, d, e, f] = sel.0;
                NanBoxable::into_nan_box([2, b, c, d, e, f])
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Selector([u8; 5]);

impl Selector {
    pub fn inputs(&self) -> u8 {
        self.0[0]
    }
    pub fn outputs(&self) -> u8 {
        self.0[1..].iter().position(|&i| i == 0).unwrap_or(4) as u8
    }
    pub fn get(&self, index: u8) -> u8 {
        self.0[index as usize + 1]
    }
    pub fn output_indices(&self) -> impl Iterator<Item = u8> + '_ {
        self.0[1..]
            .iter()
            .copied()
            .take_while(|&i| i != 0)
            .map(|i| i - 1)
    }
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self.0 {
            if i == 0 {
                break;
            }
            write!(f, "{}", (b'a' + i - 1) as char)?;
        }
        Ok(())
    }
}

impl FromStr for Selector {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.len() < 2 {
            return Err(());
        }
        let mut chars = s.chars();
        let start = chars.next().and_then(selector_char).ok_or(())?;
        let mut inner = [start, 0, 0, 0, 0];
        for (i, c) in chars.enumerate() {
            if i >= 4 {
                return Err(());
            }
            let c = selector_char(c).ok_or(())?;
            if c > start {
                return Err(());
            }
            inner[i + 1] = c;
        }
        Ok(Self(inner))
    }
}

fn selector_char(c: char) -> Option<u8> {
    if ('a'..='d').contains(&c) {
        Some(c as u8 - b'a' + 1)
    } else {
        None
    }
}

#[test]
fn selector_correctness() {
    let ba = Selector::from_str("ba").unwrap();
    assert_eq!(ba.inputs(), 2);
    assert_eq!(ba.outputs(), 1);
    for i in ba.output_indices() {
        assert!(i < ba.inputs());
    }

    let bb = Selector::from_str("bb").unwrap();
    assert_eq!(bb.inputs(), 2);
    assert_eq!(bb.outputs(), 1);
    for i in bb.output_indices() {
        assert!(i < bb.inputs());
    }
}
