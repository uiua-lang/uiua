use std::fmt;

use crate::{
    function::{FunctionId, Signature},
    lex::{CodeSpan, Sp},
    parse::ident_modifier_args,
    Ident, Primitive,
};

#[derive(Debug, Clone)]
pub enum Item {
    TestScope(Vec<Item>),
    Words(Vec<Sp<Word>>),
    Binding(Binding),
    ExtraNewlines(CodeSpan),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: Sp<Ident>,
    pub arrow_span: CodeSpan,
    pub signature: Option<Sp<Signature>>,
    pub words: Vec<Sp<Word>>,
}

#[derive(Clone)]
pub enum Word {
    Number(String, f64),
    Char(String),
    String(String),
    FormatString(Vec<String>),
    MultilineString(Vec<Sp<Vec<String>>>),
    Ident(Ident),
    Strand(Vec<Sp<Word>>),
    Array(Arr),
    Func(Func),
    Switch(Switch),
    Ocean(Vec<Sp<Primitive>>),
    Primitive(Primitive),
    Modified(Box<Modified>),
    Placeholder(Signature),
    Comment(String),
    Spaces,
}

impl Word {
    pub fn is_code(&self) -> bool {
        !matches!(self, Word::Comment(_) | Word::Spaces)
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Number(s, ..) => write!(f, "{s:?}"),
            Word::Char(char) => write!(f, "{char:?}"),
            Word::String(string) => write!(f, "{string:?}"),
            Word::FormatString(parts) => {
                write!(f, "$\"")?;
                for part in parts {
                    let escaped = format!("{part:?}");
                    let part = &escaped[1..escaped.len() - 1];
                    write!(f, "{part}")?;
                }
                write!(f, "\"")
            }
            Word::MultilineString(lines) => {
                for line in lines {
                    write!(f, "$ ")?;
                    for part in &line.value {
                        let escaped = format!("{part:?}");
                        let part = &escaped[1..escaped.len() - 1];
                        write!(f, "{part}")?;
                    }
                }
                Ok(())
            }
            Word::Ident(ident) => write!(f, "ident({ident})"),
            Word::Array(arr) => arr.fmt(f),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::Func(func) => func.fmt(f),
            Word::Switch(sw) => sw.fmt(f),
            Word::Ocean(prims) => write!(f, "ocean({prims:?})"),
            Word::Primitive(prim) => prim.fmt(f),
            Word::Modified(modified) => modified.fmt(f),
            Word::Spaces => write!(f, "' '"),
            Word::Comment(comment) => write!(f, "# {comment}"),
            Word::Placeholder(sig) => write!(f, "^{}.{}", sig.args, sig.outputs),
        }
    }
}

#[derive(Clone)]
pub struct Arr {
    pub lines: Vec<Vec<Sp<Word>>>,
    pub constant: bool,
}

impl fmt::Debug for Arr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("arr");
        for line in &self.lines {
            for word in line {
                d.field(&word.value);
            }
        }
        d.finish()
    }
}

#[derive(Clone)]
pub struct Func {
    pub id: FunctionId,
    pub signature: Option<Sp<Signature>>,
    pub lines: Vec<Vec<Sp<Word>>>,
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("func");
        d.field(&self.id);
        for line in &self.lines {
            for word in line {
                d.field(&word.value);
            }
        }
        d.finish()
    }
}

#[derive(Debug, Clone)]
pub struct Switch {
    pub branches: Vec<Sp<Func>>,
}

#[derive(Clone)]
pub struct Modified {
    pub modifier: Sp<Modifier>,
    pub operands: Vec<Sp<Word>>,
}

impl fmt::Debug for Modified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.modifier.value)?;
        for word in &self.operands {
            write!(f, "({:?})", word.value)?;
        }
        Ok(())
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Modifier {
    Primitive(Primitive),
    Ident(Ident),
}

impl fmt::Debug for Modifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modifier::Primitive(prim) => prim.fmt(f),
            Modifier::Ident(ident) => write!(f, "binding({ident})"),
        }
    }
}

impl Modifier {
    pub fn args(&self) -> u8 {
        match self {
            Modifier::Primitive(prim) => prim.modifier_args().unwrap_or(0),
            Modifier::Ident(ident) => ident_modifier_args(ident),
        }
    }
}
