use std::fmt;

use crate::{function::FunctionId, lex::Sp, ops::Primitive, Ident};

#[derive(Debug, Clone)]
pub enum Item {
    Words(Vec<Sp<Word>>),
    Binding(Binding),
    Newlines,
    Comment(String),
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: Sp<Ident>,
    pub words: Vec<Sp<Word>>,
}

#[derive(Clone)]
pub enum Word {
    Number(String),
    Char(char),
    String(String),
    Ident(Ident),
    Strand(Vec<Sp<Word>>),
    Array(Vec<Sp<Word>>),
    Func(Func),
    RefFunc(Func),
    Primitive(Primitive),
    Modified(Box<Modified>),
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Number(real) => write!(f, "{real:?}"),
            Word::Char(char) => write!(f, "{char:?}"),
            Word::String(string) => write!(f, "{{{string}}}"),
            Word::Ident(ident) => write!(f, "ident({ident})"),
            Word::Array(array) => write!(f, "array({array:?})"),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::Func(func) => func.fmt(f),
            Word::RefFunc(func) => {
                write!(f, "{{")?;
                func.fmt(f)?;
                write!(f, "}}")
            }
            Word::Primitive(prim) => prim.fmt(f),
            Word::Modified(modified) => modified.fmt(f),
        }
    }
}

#[derive(Clone)]
pub struct Func {
    pub id: FunctionId,
    pub body: Vec<Sp<Word>>,
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("func");
        d.field(&self.id);
        for word in &self.body {
            d.field(&word.value);
        }
        d.finish()
    }
}

#[derive(Clone)]
pub struct Modified {
    pub modifier: Sp<Primitive>,
    pub word: Sp<Word>,
}

impl fmt::Debug for Modified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({:?})", self.modifier.value, self.word.value)
    }
}
