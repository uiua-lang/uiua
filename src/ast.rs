//! Uiua's abstract syntax tree

use std::fmt;

use crate::{
    function::{FunctionId, Signature},
    lex::{CodeSpan, Sp},
    parse::ident_modifier_args,
    Ident, Primitive,
};

/// A top-level item
#[derive(Debug, Clone)]
pub enum Item {
    /// Just some code
    Words(Vec<Sp<Word>>),
    /// A binding
    Binding(Binding),
    /// A test scope
    TestScope(Sp<Vec<Item>>),
    /// Extra newlines between items
    ExtraNewlines(CodeSpan),
}

impl Item {
    /// Get the span of this item
    pub fn span(&self) -> CodeSpan {
        match self {
            Item::TestScope(items) => items.span.clone(),
            Item::Words(words) => {
                let first = words.first().expect("empty words").span.clone();
                let last = words.last().expect("empty words").span.clone();
                first.merge(last)
            }
            Item::Binding(binding) => binding.span(),
            Item::ExtraNewlines(span) => span.clone(),
        }
    }
}

/// A binding
#[derive(Debug, Clone)]
pub struct Binding {
    /// The name of the binding
    pub name: Sp<Ident>,
    /// The span of the arrow
    pub arrow_span: CodeSpan,
    /// The signature
    pub signature: Option<Sp<Signature>>,
    /// The code
    pub words: Vec<Sp<Word>>,
}

impl Binding {
    /// Get the span of this binding
    pub fn span(&self) -> CodeSpan {
        (self.name.span.clone()).merge(if let Some(last_word) = self.words.last() {
            last_word.span.clone()
        } else {
            self.arrow_span.clone()
        })
    }
}

/// A word
#[derive(Clone)]
#[allow(missing_docs)]
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
    /// Whether this word is code
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

/// A stack array notation term
#[derive(Clone)]
pub struct Arr {
    /// The words in the array
    pub lines: Vec<Vec<Sp<Word>>>,
    /// Whether this is a constant-item function
    pub constant: bool,
    /// Whether a closing bracket was found
    pub closed: bool,
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

/// An inline function
#[derive(Clone)]
pub struct Func {
    /// The function's id
    pub id: FunctionId,
    /// The function's signature
    pub signature: Option<Sp<Signature>>,
    /// The function's code
    pub lines: Vec<Vec<Sp<Word>>>,
    /// Whether a closing parenthesis was found
    pub closed: bool,
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

/// A switch function
#[derive(Debug, Clone)]
pub struct Switch {
    /// The branches of the switch
    pub branches: Vec<Sp<Func>>,
    /// Whether a closing parenthesis was found
    pub closed: bool,
}

/// A modifier with operands
#[derive(Clone)]
pub struct Modified {
    /// The modifier itself
    pub modifier: Sp<Modifier>,
    /// The operands
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

/// A modifier
#[derive(Clone, PartialEq, Eq)]
pub enum Modifier {
    /// A primitive modifier
    Primitive(Primitive),
    /// A user-defined modifier
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
    /// Get the number of arguments this modifier takes
    pub fn args(&self) -> u8 {
        match self {
            Modifier::Primitive(prim) => prim.modifier_args().unwrap_or(0),
            Modifier::Ident(ident) => ident_modifier_args(ident),
        }
    }
}
