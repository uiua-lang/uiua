use std::fmt::{self, Write};

use crate::{
    function::{FunctionId, Selector},
    lex::Sp,
    ops::Primitive,
    Ident,
};

#[derive(Debug, Clone)]
pub enum Item {
    Words(Vec<Sp<Word>>),
    Let(Let),
    Const(Const),
    Newlines,
    Comment(String),
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Sp<Ident>,
    pub words: Vec<Sp<Word>>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub name: Sp<Ident>,
    pub words: Vec<Sp<Word>>,
}

#[derive(Clone)]
pub enum Word {
    Real(String),
    Char(char),
    String(String),
    Ident(Ident),
    Strand(Vec<Sp<Word>>),
    Array(Vec<Sp<Word>>),
    Func(Func),
    Selector(Selector),
    FuncArray(Vec<Func>),
    Primitive(Primitive),
    Modified(Box<Modified>),
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Real(real) => write!(f, "{real:?}"),
            Word::Char(char) => write!(f, "{char:?}"),
            Word::String(string) => write!(f, "{{{string}}}"),
            Word::Ident(ident) => write!(f, "ident({ident})"),
            Word::Array(array) => write!(f, "array({array:?})"),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::FuncArray(funcs) => funcs.fmt(f),
            Word::Selector(selector) => selector.fmt(f),
            Word::Func(func) => func.fmt(f),
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
        let mut d = f.debug_tuple(&self.id.to_string());
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

#[derive(Default)]
pub(crate) struct FormatState {
    pub string: String,
}

impl FormatState {
    fn push<T: fmt::Display>(&mut self, t: T) {
        write!(&mut self.string, "{t}").unwrap();
    }
    fn space_if_alphabetic(&mut self) {
        if self.string.ends_with(char::is_alphanumeric) {
            self.push(' ');
        }
    }
    fn space_if_numeric(&mut self) {
        if self.string.ends_with(char::is_numeric) {
            self.push(' ');
        }
    }
}

pub(crate) trait Format {
    fn format(&self, state: &mut FormatState);
}

impl Format for Item {
    fn format(&self, state: &mut FormatState) {
        match self {
            Item::Words(words) => {
                state.push("do");
                for word in words {
                    word.value.format(state);
                }
            }
            Item::Let(l) => l.format(state),
            Item::Const(c) => c.format(state),
            Item::Comment(comment) => {
                state.push("# ");
                state.push(comment);
            }
            Item::Newlines => {}
        }
        state.push('\n');
    }
}

impl Format for Let {
    fn format(&self, state: &mut FormatState) {
        state.push("let ");
        state.push(&self.name.value);
        state.push(" = ");
        for word in &self.words {
            word.value.format(state);
        }
    }
}

impl Format for Const {
    fn format(&self, state: &mut FormatState) {
        state.push("const ");
        state.push(&self.name.value);
        state.push(" = ");
        for word in &self.words {
            word.value.format(state);
        }
    }
}

impl Format for Word {
    fn format(&self, state: &mut FormatState) {
        state.space_if_numeric();
        match self {
            Word::Real(f) => {
                state.space_if_alphabetic();
                state.push(f);
            }
            Word::Char(c) => state.push(&format!("{c:?}")),
            Word::String(s) => state.push(&format!("{s:?}")),
            Word::Ident(ident) => {
                state.space_if_alphabetic();
                state.push(ident);
            }
            Word::Strand(items) => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        state.push('_');
                    }
                    item.value.format(state);
                }
            }
            Word::Array(items) => {
                state.push('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        state.push(' ');
                    }
                    item.value.format(state);
                }
                state.push(']');
            }
            Word::Func(f) => {
                state.push('(');
                for word in &f.body {
                    word.value.format(state);
                }
                state.push(')');
            }
            Word::Selector(s) => {
                state.space_if_alphabetic();
                state.push(&s.to_string());
            }
            Word::FuncArray(fs) => {
                state.push('(');
                for (i, f) in fs.iter().enumerate() {
                    if i > 0 {
                        state.push('|');
                    }
                    for word in &f.body {
                        word.value.format(state);
                    }
                }
                state.push(')');
            }
            Word::Primitive(prim) => prim.format(state),
            Word::Modified(m) => {
                m.modifier.value.format(state);
                m.word.value.format(state);
            }
        }
    }
}

impl Format for Primitive {
    fn format(&self, state: &mut FormatState) {
        let s = self.to_string();
        if s.starts_with(char::is_alphabetic) {
            state.space_if_alphabetic();
        }
        state.push(s);
    }
}
