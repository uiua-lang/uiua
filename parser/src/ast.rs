//! Uiua's abstract syntax tree

use std::{collections::BTreeMap, fmt, mem::discriminant};

use ecow::EcoString;
use serde::*;

use crate::{
    parse::ident_modifier_args, BindingCounts, CodeSpan, Complex, Ident, Primitive,
    SemanticComment, Signature, Sp, Subscript,
};

/// A top-level item
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Item {
    /// Just some code
    Words(Vec<Sp<Word>>),
    /// A binding
    Binding(Binding),
    /// An import
    Import(Import),
    /// A scope
    Module(Sp<ScopedModule>),
    /// A line of data definitions
    Data(Vec<DataDef>),
}

impl PartialEq for Item {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Item::Words(a), Item::Words(b)) => words_eq(a, b),
            (Item::Binding(a), Item::Binding(b)) => a == b,
            (Item::Import(a), Item::Import(b)) => a == b,
            (Item::Module(a), Item::Module(b)) => a == b,
            (Item::Data(a), Item::Data(b)) => a == b,
            _ => false,
        }
    }
}

fn words_eq(a: &[Sp<Word>], b: &[Sp<Word>]) -> bool {
    a.iter().map(|w| &w.value).eq(b.iter().map(|w| &w.value))
}

impl Item {
    /// Get the span of this item
    pub fn span(&self) -> Option<CodeSpan> {
        match self {
            Item::Words(words) => (words.first().zip(words.last()))
                .map(|(first, last)| first.span.clone().merge(last.span.clone())),
            Item::Binding(binding) => Some(binding.span()),
            Item::Import(import) => Some(import.span()),
            Item::Module(module) => Some(module.span.clone()),
            Item::Data(data) => {
                (data.first().zip(data.last())).map(|(first, last)| first.span().merge(last.span()))
            }
        }
    }
    /// Get a string representation of the kind of this item
    pub fn kind_str(&self) -> &'static str {
        match self {
            Item::Words(_) => "words",
            Item::Binding(_) => "binding",
            Item::Import(_) => "import",
            Item::Module(_) => "module",
            Item::Data(_) => "data definition",
        }
    }
    /// Operate on words or provide a default
    pub fn words_or<'a, T>(&'a self, default: T, on_words: impl FnOnce(&'a [Sp<Word>]) -> T) -> T {
        match self {
            Item::Words(words) => on_words(words),
            _ => default,
        }
    }
    /// Whether this item is an empty line
    pub fn is_empty_line(&self) -> bool {
        self.words_or(false, |words| {
            words.iter().all(|w| matches!(w.value, Word::Spaces))
        })
    }
}

/// A binding
#[derive(Debug, Clone, PartialEq, Serialize)]
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
    /// Character counts for golfing
    pub counts: BindingCounts,
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ScopedModule {
    /// The span of the opening delimiter
    pub open_span: CodeSpan,
    /// Whether the module is public
    pub public: bool,
    /// The module kind
    pub kind: ModuleKind,
    /// The items
    pub items: Vec<Item>,
    /// The local imports exported from the module
    pub imports: Option<ImportLine>,
    /// The span of the closing delimiter
    pub close_span: Option<CodeSpan>,
}

/// The kind of a module
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ModuleKind {
    /// A named module
    Named(Sp<Ident>),
    /// A test scope
    Test,
}

/// An import
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Import {
    /// The name given to the imported module
    pub name: Option<Sp<Ident>>,
    /// The span of the ~
    pub tilde_span: CodeSpan,
    /// Whether the import is public
    pub public: bool,
    /// The import path
    pub path: Sp<String>,
    /// The import lines
    pub lines: Vec<Option<ImportLine>>,
}

/// A line of imported items
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct DataDef {
    /// The span of the ~ or |
    pub init_span: CodeSpan,
    /// Whether the def is public
    pub public: bool,
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
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct FieldValidator {
    /// The span of the colon (may be an open paren)
    pub open_span: CodeSpan,
    /// The validator function
    pub words: Vec<Sp<Word>>,
    /// The closing paren span (if a paren was used to open)
    pub close_span: Option<CodeSpan>,
}

/// A data field initializer
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Comments {
    /// The normal comment lines
    pub lines: Vec<Sp<EcoString>>,
    /// The semantic comments
    pub semantic: BTreeMap<SemanticComment, CodeSpan>,
}

/// An inline macro
#[derive(Clone, PartialEq, Serialize)]
pub struct InlineMacro {
    /// The function
    pub func: Sp<Func>,
    /// The span of a `^` that makes this an array macro
    pub caret_span: Option<CodeSpan>,
    /// The identifier, which consists of only exclamation marks
    pub ident: Sp<Ident>,
}

/// A word
#[derive(Clone, Serialize)]
#[allow(missing_docs)]
#[serde(tag = "type", content = "value")]
pub enum Word {
    Number(NumWord, String),
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
    Subscripted(Box<Subscripted>),
    InlineMacro(InlineMacro),
    ArgSetter(ArgSetter),
}

impl PartialEq for Word {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(a_n, a_s), Self::Number(b_n, b_s)) => a_n == b_n && a_s == b_s,
            (Self::Char(a), Self::Char(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Label(a), Self::Label(b)) => a == b,
            (Self::FormatString(a), Self::FormatString(b)) => a == b,
            (Self::MultilineFormatString(a), Self::MultilineFormatString(b)) => a == b,
            (Self::Ref(a), Self::Ref(b)) => a == b,
            (Self::Strand(a), Self::Strand(b)) => words_eq(a, b),
            (Self::Array(a), Self::Array(b)) => a.lines == b.lines,
            (Self::Func(a), Self::Func(b)) => a.lines == b.lines,
            (Self::Pack(a), Self::Pack(b)) => (a.branches.iter().flat_map(|br| &br.value.lines))
                .eq(b.branches.iter().flat_map(|br| &br.value.lines)),
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
        match self {
            Word::Number(..) | Word::Char(_) | Word::String(_) => true,
            Word::Array(arr) => arr.lines.iter().all(|item| {
                item.words_or(false, |words| {
                    (words.iter())
                        .filter(|w| w.value.is_code())
                        .all(|w| w.value.is_literal())
                })
            }),
            Word::Strand(items) => items.iter().all(|w| w.value.is_literal()),
            _ => false,
        }
    }
    /// Whether this word must come at the end of a line
    pub fn is_end_of_line(&self) -> bool {
        match self {
            Word::Comment(_)
            | Word::SemanticComment(_)
            | Word::OutputComment { .. }
            | Word::MultilineString(_)
            | Word::MultilineFormatString(_) => true,
            Word::Modified(m) => m.operands.last().is_some_and(|w| w.value.is_end_of_line()),
            _ => false,
        }
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
            Word::Placeholder(op) => write!(f, "^{op}"),
            Word::BreakLine => write!(f, "break_line"),
            Word::FlipLine => write!(f, "unbreak_line"),
            Word::SemanticComment(comment) => write!(f, "{comment}"),
            Word::OutputComment { i, n, .. } => write!(f, "output_comment({i}/{n})"),
            Word::Subscripted(sub) => sub.fmt(f),
            Word::InlineMacro(InlineMacro { ident, func, .. }) => {
                write!(f, "inline_macro({:?}{}))", func.value, ident.value)
            }
            Word::ArgSetter(setter) => setter.fmt(f),
        }
    }
}

/// A refered-to item
#[derive(Clone, Serialize)]
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
#[derive(Debug, Clone, Serialize)]
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
#[derive(Clone, Serialize)]
pub struct Arr {
    /// The span of preceding `↓`
    pub down_span: Option<CodeSpan>,
    /// The words in the array
    pub lines: Vec<Item>,
    /// Whether this is a box array
    pub boxes: bool,
    /// Whether a closing bracket was found
    pub closed: bool,
}

impl Arr {
    /// Get the lines that contain words
    pub fn word_lines(&self) -> impl DoubleEndedIterator<Item = &[Sp<Word>]> {
        self.lines
            .iter()
            .filter_map(|line| match line {
                Item::Words(words) => Some(words),
                _ => None,
            })
            .map(|v| v.as_slice())
    }
    /// Get the mutable lines that contain words
    pub fn word_lines_mut(&mut self) -> impl Iterator<Item = &mut Vec<Sp<Word>>> {
        self.lines.iter_mut().filter_map(|line| match line {
            Item::Words(words) => Some(words),
            _ => None,
        })
    }
}

impl fmt::Debug for Arr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("arr");
        for line in &self.lines {
            match line {
                Item::Words(line) => {
                    for word in line {
                        d.field(&word.value);
                    }
                    if line.is_empty() {
                        d.field(&"newline");
                    }
                }
                item => {
                    d.field(item);
                }
            }
        }
        d.finish()
    }
}

/// An inline function
#[derive(Clone, PartialEq, Serialize)]
pub struct Func {
    /// The function's signature
    pub signature: Option<Sp<Signature>>,
    /// The function's code
    pub lines: Vec<Item>,
    /// Whether a closing parenthesis was found
    pub closed: bool,
}

impl Func {
    /// Get the lines of the function without leading or trailing empty lines
    pub fn trimmed_lines(&self) -> &[Item] {
        let mut lines = self.lines.as_slice();
        while lines.first().is_some_and(Item::is_empty_line) {
            lines = &lines[1..];
        }
        while lines.last().is_some_and(Item::is_empty_line) {
            lines = &lines[..lines.len() - 1];
        }
        lines
    }
    /// Whether the function visibly has multiple lines
    pub fn is_multiline(&self) -> bool {
        self.trimmed_lines().len() > 1
    }
    /// Get the lines that contain words
    pub fn word_lines(&self) -> impl Iterator<Item = &[Sp<Word>]> {
        self.lines
            .iter()
            .filter_map(|line| match line {
                Item::Words(words) => Some(words),
                _ => None,
            })
            .map(|v| v.as_slice())
    }
    /// Get the mutable lines that contain words
    pub fn word_lines_mut(&mut self) -> impl Iterator<Item = &mut Vec<Sp<Word>>> {
        self.lines.iter_mut().filter_map(|line| match line {
            Item::Words(words) => Some(words),
            _ => None,
        })
    }
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut d = f.debug_tuple("func");
        // d.field(&self.id);
        for line in &self.lines {
            match line {
                Item::Words(line) => {
                    for word in line {
                        d.field(&word.value);
                    }
                    if line.is_empty() {
                        d.field(&"newline");
                    }
                }
                item => {
                    d.field(item);
                }
            }
        }
        d.finish()
    }
}

/// A function pack
#[derive(Debug, Clone, Serialize)]
pub struct FunctionPack {
    /// The span of preceding `↓`
    pub down_span: Option<CodeSpan>,
    /// Whether this is an array pack (and whether it boxes)
    pub is_array: Option<bool>,
    /// The branches of the pack
    pub branches: Vec<Sp<Func>>,
    /// Whether a closing parenthesis was found
    pub closed: bool,
}

impl FunctionPack {
    /// Iterate over the branches in lexical order
    pub fn lexical_order(&self) -> impl DoubleEndedIterator<Item = &Sp<Func>> {
        let mut branches: Vec<_> = self.branches.iter().collect();
        branches.sort_by_key(|func| func.span.start.col);
        if self.down_span.is_some() {
            branches.sort_by_key(|func| -(func.span.start.line as i32));
        } else {
            branches.sort_by_key(|func| func.span.start.line);
        }
        branches.into_iter()
    }
    /// Iterate over the branches in lexical order
    pub fn into_lexical_order(self) -> impl DoubleEndedIterator<Item = Sp<Func>> {
        let mut branches = self.branches;
        branches.sort_by_key(|func| func.span.start.col);
        if self.down_span.is_some() {
            branches.sort_by_key(|func| -(func.span.start.line as i32));
        } else {
            branches.sort_by_key(|func| func.span.start.line);
        }
        branches.into_iter()
    }
}

/// A modifier with operands
#[derive(Clone, Serialize)]
pub struct Modified {
    /// The modifier itself
    pub modifier: Sp<Modifier>,
    /// The operands
    pub operands: Vec<Sp<Word>>,
    /// Whether this was generated with a function pack
    pub pack_expansion: bool,
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
#[derive(Clone, PartialEq, Serialize)]
#[serde(tag = "type", content = "value")]
pub enum Modifier {
    /// A primitive modifier
    Primitive(Primitive),
    /// A user-defined modifier
    Ref(Ref),
    /// An inline macro
    Macro(InlineMacro),
}

impl fmt::Debug for Modifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modifier::Primitive(prim) => prim.fmt(f),
            Modifier::Ref(refer) => write!(f, "ref({refer:?})"),
            Modifier::Macro(mac) => write!(f, "macro({:?}{})", mac.func, mac.ident),
        }
    }
}

impl fmt::Display for Modifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Modifier::Primitive(prim) => prim.format().fmt(f),
            Modifier::Ref(refer) => write!(f, "{refer}"),
            Modifier::Macro(mac) => match ident_modifier_args(&mac.ident.value) {
                0 | 1 => write!(f, "monadic inline macro"),
                2 => write!(f, "dyadic inline macro"),
                3 => write!(f, "triadic inline macro"),
                4 => write!(f, "tetradic inline macro"),
                _ => write!(f, "inline macro"),
            },
        }
    }
}

impl Modifier {
    /// Get the number of arguments this modifier takes
    pub fn args(&self) -> usize {
        match self {
            Modifier::Primitive(prim) => prim.modifier_args().unwrap_or(0),
            Modifier::Ref(r) => r.modifier_args(),
            Modifier::Macro(mac) => ident_modifier_args(&mac.ident.value),
        }
    }
}

/// An argument setter
#[derive(Clone, Serialize)]
pub struct ArgSetter {
    /// The name of the field
    pub ident: Sp<Ident>,
    /// The span of the colon
    pub colon_span: CodeSpan,
}

impl fmt::Debug for ArgSetter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:", self.ident.value)
    }
}

impl ArgSetter {
    /// Get the full span
    pub fn span(&self) -> CodeSpan {
        self.ident.span.clone().merge(self.colon_span.clone())
    }
}

/// A subscripted word
#[derive(Clone, Serialize)]
pub struct Subscripted {
    /// The subscript
    pub script: Sp<Subscript>,
    /// The modified word
    pub word: Sp<Word>,
}

impl fmt::Debug for Subscripted {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.word.value.fmt(f)?;
        write!(f, "{}", self.script.value)
    }
}

/// A number word
#[derive(Clone, PartialEq, Serialize)]
#[allow(missing_docs)]
pub enum NumWord {
    Real(f64),
    Complex(Complex),
    Err(String),
}

impl From<f64> for NumWord {
    fn from(value: f64) -> Self {
        Self::Real(value)
    }
}

impl From<Complex> for NumWord {
    fn from(value: Complex) -> Self {
        Self::Complex(value)
    }
}

impl From<Result<f64, String>> for NumWord {
    fn from(value: Result<f64, String>) -> Self {
        match value {
            Ok(v) => Self::Real(v),
            Err(e) => Self::Err(e),
        }
    }
}

impl From<Result<Complex, String>> for NumWord {
    fn from(value: Result<Complex, String>) -> Self {
        match value {
            Ok(v) => Self::Complex(v),
            Err(e) => Self::Err(e),
        }
    }
}

impl NumWord {
    /// Map the number
    pub fn map<R, C>(self, real: impl FnOnce(f64) -> R, complex: impl FnOnce(Complex) -> C) -> Self
    where
        C: Into<Self>,
        R: Into<Self>,
    {
        match self {
            Self::Real(r) => real(r).into(),
            Self::Complex(c) => complex(c).into(),
            Self::Err(e) => Self::Err(e),
        }
    }
    /// Map the number with another
    pub fn map_with<R, C>(
        self,
        other: Self,
        real: impl FnOnce(f64, f64) -> R,
        complex: impl FnOnce(Complex, Complex) -> C,
    ) -> Self
    where
        C: Into<Self>,
        R: Into<Self>,
    {
        match (self, other) {
            (Self::Real(a), Self::Real(b)) => real(a, b).into(),
            (Self::Complex(a), Self::Complex(b)) => complex(a, b).into(),
            (Self::Real(a), Self::Complex(b)) => complex(a.into(), b).into(),
            (Self::Complex(a), Self::Real(b)) => complex(a, b.into()).into(),
            (Self::Err(e), _) | (_, Self::Err(e)) => Self::Err(e),
        }
    }
}

impl fmt::Debug for NumWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for NumWord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumWord::Real(r) => write!(f, "{r}"),
            NumWord::Complex(c) => write!(f, "{c}"),
            NumWord::Err(e) => write!(f, "error({e})"),
        }
    }
}
