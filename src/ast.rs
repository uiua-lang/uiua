//! Uiua's abstract syntax tree

use core::mem::discriminant;
use std::{collections::HashMap, fmt};

use ecow::EcoString;

use crate::{
    function::Signature,
    lex::{CodeSpan, Sp},
    parse::ident_modifier_args,
    Ident, Primitive, SemanticComment, SUBSCRIPT_NUMS,
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
    /// A scope
    Module(Sp<ScopedModule>),
    /// A data definition
    Data(DataDef),
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
    /// Whether the binding is a code macro
    pub code_macro: bool,
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

/// A scoped module
#[derive(Debug, Clone)]
pub struct ScopedModule {
    /// The span of the opening delimiter
    pub open_span: CodeSpan,
    /// The module kind
    pub kind: ModuleKind,
    /// The items
    pub items: Vec<Item>,
    /// The imports
    pub imports: Option<ImportLine>,
    /// The span of the closing delimiter
    pub close_span: Option<CodeSpan>,
}

/// The kind of a module
#[derive(Debug, Clone)]
pub enum ModuleKind {
    /// A named module
    Named(Sp<Ident>),
    /// A test scope
    Test,
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

/// A line of imported items
#[derive(Debug, Clone)]
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

/// A data definition
#[derive(Debug, Clone)]
pub struct DataDef {
    /// The span of the ~ or |
    pub init_span: CodeSpan,
    /// Whether this is a variant
    pub variant: bool,
    /// The name of the module
    pub name: Option<Sp<Ident>>,
    /// The fields of the data definition
    pub fields: Option<DataFields>,
    /// The function
    pub func: Option<Vec<Sp<Word>>>,
}

/// The fields of a data definition
#[derive(Debug, Clone)]
pub struct DataFields {
    /// Whether the array is boxed
    pub boxed: bool,
    /// The open delimiter span
    pub open_span: CodeSpan,
    /// The data fields
    pub fields: Vec<DataField>,
    /// A trailing newline
    pub trailing_newline: bool,
    /// The close delimiter span
    pub close_span: Option<CodeSpan>,
}

/// A data field
#[derive(Debug, Clone)]
pub struct DataField {
    /// Leading comments
    pub comments: Option<Comments>,
    /// The name of the field
    pub name: Sp<Ident>,
    /// The validator of the field
    pub validator: Option<FieldValidator>,
    /// The default value of the field
    pub init: Option<FieldInit>,
    /// The span of a trailing bar
    pub bar_span: Option<CodeSpan>,
}

/// A data field validator
#[derive(Debug, Clone)]
pub struct FieldValidator {
    /// The span of the colon (may be an open paren)
    pub open_span: CodeSpan,
    /// The validator function
    pub words: Vec<Sp<Word>>,
    /// The closing paren span (if a paren was used to open)
    pub close_span: Option<CodeSpan>,
}

/// A data field initializer
#[derive(Debug, Clone)]
pub struct FieldInit {
    /// The span of the assignment arrow
    pub arrow_span: CodeSpan,
    /// The initializing words
    pub words: Vec<Sp<Word>>,
}

impl DataDef {
    /// Get the span of this data definition
    pub fn span(&self) -> CodeSpan {
        let end = self
            .fields
            .as_ref()
            .map(|fields| fields.span())
            .unwrap_or_else(|| {
                self.name
                    .as_ref()
                    .map(|name| name.span.clone())
                    .unwrap_or_else(|| self.init_span.clone())
            });
        let mut span = (self.init_span.clone()).merge(end);
        if let Some(words) = &self.func {
            if let Some(word) = words.last() {
                span = span.merge(word.span.clone());
            }
        }
        span
    }
}

impl DataFields {
    /// Get the span of these fields
    pub fn span(&self) -> CodeSpan {
        let end = self
            .close_span
            .clone()
            .or_else(|| {
                self.fields.last().map(|field| {
                    field
                        .bar_span
                        .clone()
                        .unwrap_or_else(|| field.name.span.clone())
                })
            })
            .unwrap_or_else(|| self.open_span.clone());
        self.open_span.clone().merge(end)
    }
}

impl DataField {
    /// Get the span of the field
    pub fn span(&self) -> CodeSpan {
        let Some(end) = self.bar_span.clone().or_else(|| {
            self.init.as_ref().map(|d| {
                d.words
                    .last()
                    .map(|w| w.span.clone())
                    .unwrap_or_else(|| d.arrow_span.clone())
            })
        }) else {
            return self.name.span.clone();
        };
        self.name.span.clone().merge(end)
    }
}

/// A cluster of comments
#[derive(Debug, Clone)]
pub struct Comments {
    /// The normal comment lines
    pub lines: Vec<Sp<EcoString>>,
    /// The semantic comments
    pub semantic: HashMap<SemanticComment, CodeSpan>,
}

/// A word
#[derive(Clone)]
#[allow(missing_docs)]
pub enum Word {
    Number(String, f64),
    Char(String),
    String(String),
    MultilineString(Vec<Sp<String>>),
    FormatString(Vec<String>),
    MultilineFormatString(Vec<Sp<Vec<String>>>),
    Label(String),
    Ref(Ref),
    IncompleteRef {
        path: Vec<RefComponent>,
        in_macro_arg: bool,
    },
    Strand(Vec<Sp<Word>>),
    Array(Arr),
    Func(Func),
    Pack(FunctionPack),
    Primitive(Primitive),
    Modified(Box<Modified>),
    Placeholder(usize),
    Comment(String),
    Spaces,
    BreakLine,
    FlipLine,
    SemanticComment(SemanticComment),
    OutputComment {
        i: usize,
        n: usize,
    },
    Subscript(Box<Subscript>),
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(_, a), Self::Number(_, b)) => a == b,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Label(a), Self::Label(b)) => a == b,
            (Self::FormatString(a), Self::FormatString(b)) => a == b,
            (Self::MultilineFormatString(a), Self::MultilineFormatString(b)) => a == b,
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
            (Self::Pack(a), Self::Pack(b)) => (a.branches.iter())
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
            Word::Comment(_) | Word::Spaces | Word::BreakLine | Word::FlipLine
        )
    }
    /// Whether this word is a literal
    pub fn is_literal(&self) -> bool {
        matches!(self, Word::Number(..) | Word::Char(_) | Word::String(_))
            || matches!(self, Word::Array(arr) if arr.lines.iter().flatten().filter(|w| w.value.is_code()).all(|w| w.value.is_literal()))
            || matches!(self, Word::Strand(items) if items.iter().all(|w| w.value.is_literal()))
    }
    /// Whether this word must come at the end of a line
    pub fn is_end_of_line(&self) -> bool {
        matches!(
            self,
            Word::Comment(_)
                | Word::SemanticComment(_)
                | Word::OutputComment { .. }
                | Word::MultilineString(_)
                | Word::MultilineFormatString(_)
        )
    }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Word::Number(s, ..) => write!(f, "{s:?}"),
            Word::Char(char) => write!(f, "{char:?}"),
            Word::String(string) => write!(f, "{string:?}"),
            Word::MultilineString(string) => write!(f, "{string:?}"),
            Word::FormatString(parts) => {
                write!(f, "$\"")?;
                for part in parts {
                    let escaped = format!("{part:?}");
                    let part = &escaped[1..escaped.len() - 1];
                    write!(f, "{part}")?;
                }
                write!(f, "\"")
            }
            Word::MultilineFormatString(lines) => {
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
            Word::Label(label) => write!(f, "${label}"),
            Word::Ref(ident) => write!(f, "ref({ident})"),
            Word::IncompleteRef { path, .. } => {
                write!(f, "incomplete_ref({}~...)", path[0].module.value)
            }
            Word::Array(arr) => arr.fmt(f),
            Word::Strand(items) => write!(f, "strand({items:?})"),
            Word::Func(func) => func.fmt(f),
            Word::Pack(pack) => pack.fmt(f),
            Word::Primitive(prim) => prim.fmt(f),
            Word::Modified(modified) => modified.fmt(f),
            Word::Spaces => write!(f, "' '"),
            Word::Comment(comment) => write!(f, "# {comment}"),
            Word::Placeholder(op) => write!(f, "{op}"),
            Word::BreakLine => write!(f, "break_line"),
            Word::FlipLine => write!(f, "unbreak_line"),
            Word::SemanticComment(comment) => write!(f, "{comment}"),
            Word::OutputComment { i, n, .. } => write!(f, "output_comment({i}/{n})"),
            Word::Subscript(sub) => sub.fmt(f),
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
    /// Whether this ref is in a macro argument
    ///
    /// This allows macros to be hygienic
    pub in_macro_arg: bool,
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
    /// The array's inner signature
    pub signature: Option<Sp<Signature>>,
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
            if line.is_empty() {
                d.field(&"newline");
            }
        }
        d.finish()
    }
}

/// A function pack
#[derive(Debug, Clone)]
pub struct FunctionPack {
    /// The branches of the pack
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

impl Modified {
    /// Get an iterator over the functions that are actual code
    pub fn code_operands(&self) -> impl DoubleEndedIterator<Item = &Sp<Word>> {
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
            Modifier::Ref(r) => r.modifier_args(),
        }
    }
}

/// A subscript
#[derive(Clone)]
pub struct Subscript {
    /// The subscript number
    pub n: Sp<Option<usize>>,
    /// The modified word
    pub word: Sp<Word>,
}

impl Subscript {
    /// Get the subscript number as a string
    pub fn n_string(&self) -> String {
        if let Some(n) = &self.n.value {
            (n.to_string().chars())
                .map(|c| SUBSCRIPT_NUMS[(c as u32 as u8 - b'0') as usize])
                .collect()
        } else {
            String::new()
        }
    }
}

impl fmt::Debug for Subscript {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.word.value.fmt(f)?;
        write!(f, "{}", self.n_string())
    }
}
