//! Uiua's abstract syntax tree

use core::mem::discriminant;
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
    Words(Vec<Vec<Sp<Word>>>),
    /// A binding
    Binding(Binding),
    /// An import
    Import(Import),
    /// A test scope
    TestScope(Sp<Vec<Item>>),
}

/// A binding
#[derive(Debug, Clone)]
pub struct Binding {
    /// The name of the binding
    pub name: Sp<Ident>,
    /// The span of the arrow
    pub arrow_span: CodeSpan,
    /// Whether the binding is public
    pub public: bool,
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

/// An import
#[derive(Debug, Clone)]
pub struct Import {
    /// The name given to the imported module
    pub name: Option<Sp<Ident>>,
    /// The span of the ~
    pub tilde_span: CodeSpan,
    /// The import path
    pub path: Sp<String>,
    /// The import lines
    pub lines: Vec<Option<ImportLine>>,
}

#[derive(Debug, Clone)]
/// A line of imported items
pub struct ImportLine {
    /// The span of the ~
    pub tilde_span: CodeSpan,
    /// The imported items
    pub items: Vec<Sp<Ident>>,
}

impl Import {
    /// The full span of the import
    pub fn span(&self) -> CodeSpan {
        let first = (self.name.as_ref())
            .map(|n| n.span.clone())
            .unwrap_or_else(|| self.path.span.clone());
        let last = (self.items().last())
            .map(|i| i.span.clone())
            .unwrap_or_else(|| self.path.span.clone());
        first.merge(last)
    }
    /// The imported items
    pub fn items(&self) -> impl Iterator<Item = &Sp<Ident>> {
        self.lines.iter().flatten().flat_map(|line| &line.items)
    }
}

/// A word
#[derive(Clone)]
#[allow(missing_docs)]
pub enum Word {
    Number(String, f64),
    Char(String),
    String(String),
    Label(String),
    FormatString(Vec<String>),
    MultilineString(Vec<Sp<Vec<String>>>),
    Ref(Ref),
    Strand(Vec<Sp<Word>>),
    Array(Arr),
    Func(Func),
    Switch(Switch),
    Primitive(Primitive),
    Modified(Box<Modified>),
    Placeholder(PlaceholderOp),
    Comment(String),
    Spaces,
    BreakLine,
    UnbreakLine,
    OutputComment { i: usize, n: usize },
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(_, a), Self::Number(_, b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Label(a), Self::Label(b)) => a == b,
            (Self::FormatString(a), Self::FormatString(b)) => a == b,
            (Self::MultilineString(a), Self::MultilineString(b)) => a == b,
            (Self::Ref(a), Self::Ref(b)) => a == b,
            (Self::Strand(a), Self::Strand(b)) => {
                a.iter().map(|w| &w.value).eq(b.iter().map(|w| &w.value))
            }
            (Self::Array(a), Self::Array(b)) => a.lines.iter().flatten().map(|w| &w.value).eq(b
                .lines
                .iter()
                .flatten()
                .map(|w| &w.value)),
            (Self::Func(a), Self::Func(b)) => a.lines.iter().flatten().map(|w| &w.value).eq(b
                .lines
                .iter()
                .flatten()
                .map(|w| &w.value)),
            (Self::Switch(a), Self::Switch(b)) => (a.branches.iter())
                .flat_map(|br| &br.value.lines)
                .flatten()
                .map(|w| &w.value)
                .eq((b.branches.iter())
                    .flat_map(|br| &br.value.lines)
                    .flatten()
                    .map(|w| &w.value)),
            (Self::Primitive(a), Self::Primitive(b)) => a == b,
            (Self::Modified(a), Self::Modified(b)) => {
                a.modifier == b.modifier
                    && a.code_operands()
                        .map(|w| &w.value)
                        .eq(b.code_operands().map(|w| &w.value))
            }
            (Self::Placeholder(_), Self::Placeholder(_)) => false,
            (Self::Comment(a), Self::Comment(b)) => a == b,
            _ => discriminant(self) == discriminant(other),
        }
    }
}

impl Word {
    /// Whether this word is code
    pub fn is_code(&self) -> bool {
        !matches!(
            self,
            Word::Comment(_) | Word::Spaces | Word::BreakLine | Word::UnbreakLine
        )
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Number(s, ..) => write!(f, "{s:?}"),
            Word::Char(char) => write!(f, "{char:?}"),
            Word::String(string) => write!(f, "{string:?}"),
            Word::Label(label) => write!(f, "${label}"),
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
            Word::Ref(ident) => write!(f, "ref({ident})"),
            Word::Array(arr) => arr.fmt(f),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::Func(func) => func.fmt(f),
            Word::Switch(sw) => sw.fmt(f),
            Word::Primitive(prim) => prim.fmt(f),
            Word::Modified(modified) => modified.fmt(f),
            Word::Spaces => write!(f, "' '"),
            Word::Comment(comment) => write!(f, "# {comment}"),
            Word::Placeholder(op) => write!(f, "{op}"),
            Word::BreakLine => write!(f, "'"),
            Word::UnbreakLine => write!(f, "''"),
            Word::OutputComment { i, n, .. } => write!(f, "output_comment({i}/{n})"),
        }
    }
}

/// A placeholder operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlaceholderOp {
    /// Pop and inline the top expression
    Call,
    /// Duplicate the top expression
    Dup,
    /// Swap the top two expressions
    Flip,
    /// Copy the 2nd-to-top expression to the top
    Over,
}

impl PlaceholderOp {
    /// Get the name of this placeholder operation
    pub fn name(&self) -> &'static str {
        match self {
            PlaceholderOp::Call => "call",
            PlaceholderOp::Dup => "dup",
            PlaceholderOp::Flip => "flip",
            PlaceholderOp::Over => "over",
        }
    }
}

impl fmt::Display for PlaceholderOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlaceholderOp::Call => write!(f, "^!"),
            PlaceholderOp::Dup => write!(f, "^."),
            PlaceholderOp::Flip => write!(f, "^:"),
            PlaceholderOp::Over => write!(f, "^,"),
        }
    }
}

/// A refered-to item
#[derive(Clone)]
pub struct Ref {
    /// The module path of the item
    pub path: Vec<RefComponent>,
    /// The name of the item
    pub name: Sp<Ident>,
}

/// A component of a reference
#[derive(Clone)]
pub struct RefComponent {
    /// The name of the module
    pub module: Sp<Ident>,
    /// The span of the ~
    pub tilde_span: CodeSpan,
}

impl Ref {
    /// Get the number of modifier arguments this reference's name implies
    pub fn modifier_args(&self) -> usize {
        ident_modifier_args(&self.name.value)
    }
    /// Get the root module of this reference
    pub fn root_module(&self) -> Option<&Ident> {
        self.path.first().map(|c| &c.module.value)
    }
}

impl PartialEq for Ref {
    fn eq(&self, other: &Self) -> bool {
        self.path
            .iter()
            .map(|c| &c.module.value)
            .eq(other.path.iter().map(|c| &c.module.value))
            && self.name.value == other.name.value
    }
}

impl Eq for Ref {}

impl fmt::Debug for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for comp in &self.path {
            write!(f, "{}~", comp.module.value)?;
        }
        write!(f, "{}", self.name.value)
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for comp in &self.path {
            write!(f, "{}~", comp.module.value)?;
        }
        write!(f, "{}", self.name.value)
    }
}

/// A stack array notation term
#[derive(Clone)]
pub struct Arr {
    /// The words in the array
    pub lines: Vec<Vec<Sp<Word>>>,
    /// Whether this is a box array
    pub boxes: bool,
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
        // d.field(&self.id);
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
    /// Whether the switch uses angle brackets
    pub angled: bool,
}

/// A modifier with operands
#[derive(Clone)]
pub struct Modified {
    /// The modifier itself
    pub modifier: Sp<Modifier>,
    /// The operands
    pub operands: Vec<Sp<Word>>,
}

impl Modified {
    /// Get an iterator over the functions that are actual code
    pub fn code_operands(&self) -> impl Iterator<Item = &Sp<Word>> {
        self.operands.iter().filter(|word| word.value.is_code())
    }
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
    Ref(Ref),
}

impl fmt::Debug for Modifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modifier::Primitive(prim) => prim.fmt(f),
            Modifier::Ref(refer) => write!(f, "ref({refer:?})"),
        }
    }
}

impl fmt::Display for Modifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modifier::Primitive(prim) => prim.format().fmt(f),
            Modifier::Ref(refer) => write!(f, "{refer}"),
        }
    }
}

impl Modifier {
    /// Get the number of arguments this modifier takes
    pub fn args(&self) -> usize {
        match self {
            Modifier::Primitive(prim) => prim.modifier_args().unwrap_or(0),
            Modifier::Ref(refer) => refer.modifier_args(),
        }
    }
}
