use std::fmt;

use crate::{function::FunctionId, lex::Sp, primitive::Primitive, Ident};

#[derive(Debug, Clone)]
pub enum Item {
    Scoped { items: Vec<Item>, test: bool },
    Words(Vec<Sp<Word>>),
    Binding(Binding),
    Newlines,
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: Sp<Ident>,
    pub words: Vec<Sp<Word>>,
}

#[derive(Clone)]
pub enum Word {
    Number(String, f64),
    Char(char),
    String(String),
    FormatString(Vec<String>),
    MultilineString(Vec<Sp<Vec<String>>>),
    Ident(Ident),
    Strand(Vec<Sp<Word>>),
    Array(Vec<Vec<Sp<Word>>>),
    Func(Func, bool),
    Primitive(Primitive),
    Modified(Box<Modified>),
    Comment(String),
    Spaces,
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Number(s, _) => write!(f, "{s:?}"),
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
            Word::Array(array) => write!(f, "array({array:?})"),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::Func(func, _) => func.fmt(f),
            Word::Primitive(prim) => prim.fmt(f),
            Word::Modified(modified) => modified.fmt(f),
            Word::Spaces => write!(f, "' '"),
            Word::Comment(comment) => write!(f, "# {comment}"),
        }
    }
}

#[derive(Clone)]
pub struct Func {
    pub id: FunctionId,
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

#[derive(Clone)]
pub struct Modified {
    pub modifier: Sp<Primitive>,
    pub words: Vec<Sp<Word>>,
}

impl fmt::Debug for Modified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.modifier.value)?;
        for word in &self.words {
            write!(f, "({:?})", word.value)?;
        }
        Ok(())
    }
}
