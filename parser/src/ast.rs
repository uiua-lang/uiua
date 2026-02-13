//! Uiua's abstract syntax tree

use std::{collections::BTreeMap, fmt, iter::once, mem::discriminant};

use ecow::EcoString;
use serde::*;

use crate::{
    BindingCounts, CodeSpan, Complex, Ident, Primitive, SemanticComment, Signature, Sp, Subscript,
    parse::ident_modifier_args,
};

/// A top-level item
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ModuleKind {
    /// A named module
    Named(Sp<Ident>),
    /// A test scope
    Test,
}

/// An import
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ImportLine {
    /// The span of the ~
    pub tilde_span: CodeSpan,
    /// Whether the imports are public
    pub public: bool,
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
            .map(|(i, _)| i.span.clone())
            .unwrap_or_else(|| self.path.span.clone());
        first.merge(last)
    }
    /// The imported items
    pub fn items(&self) -> impl Iterator<Item = (&Sp<Ident>, bool)> {
        (self.lines.iter().flatten())
            .flat_map(|line| line.items.iter().map(|item| (item, line.public)))
    }
}

/// A data definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DataFields {
    /// Whether the array is boxed
    pub boxed: bool,
    /// The open delimiter span
    pub open_span: CodeSpan,
    /// The data fields
    pub fields: Vec<DataField>,
    /// Comments after the fields
    pub post_comments: Option<Comments>,
    /// A trailing newline
    pub trailing_newline: bool,
    /// The close delimiter span
    pub close_span: Option<CodeSpan>,
}

/// A data field
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DataField {
    /// Leading comments
    pub comments: Option<Comments>,
    /// The name of the field
    pub name: Sp<Ident>,
    /// The validator of the field
    pub validator: Option<FieldValidator>,
    /// An inline comment when there is no init
    pub eol_comment: Option<Sp<EcoString>>,
    /// The default value of the field
    pub init: Option<FieldInit>,
    /// The span of a trailing bar
    pub bar_span: Option<CodeSpan>,
}

/// A data field validator
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FieldValidator {
    /// The span of the colon (may be an open paren)
    pub open_span: CodeSpan,
    /// The validator function
    pub words: Vec<Sp<Word>>,
    /// The closing paren span (if a paren was used to open)
    pub close_span: Option<CodeSpan>,
}

/// A data field initializer
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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
        if let Some(words) = &self.func
            && let Some(word) = words.last()
        {
            span = span.merge(word.span.clone());
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
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Comments {
    /// The normal comment lines
    pub lines: Vec<Sp<EcoString>>,
    /// The semantic comments
    pub semantic: BTreeMap<SemanticComment, CodeSpan>,
}

/// An inline macro
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct InlineMacro {
    /// The function
    pub func: Sp<Func>,
    /// The span of a `^` that makes this an array macro
    pub caret_span: Option<CodeSpan>,
    /// The identifier, which consists of only exclamation marks
    pub ident: Sp<Ident>,
}

/// A word
#[derive(Clone, Serialize, Deserialize)]
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
    Ref(Ref, Vec<ChainComponent>),
    IncompleteRef(Vec<RefComponent>),
    Strand(Vec<Sp<Word>>),
    Array(Arr),
    Func(Func),
    Pack(FunctionPack),
    Primitive(Primitive),
    Modified(Box<Modified>),
    Placeholder(Option<usize>),
    Comment(EcoString),
    Spaces,
    BreakLine,
    FlipLine,
    SemanticComment(SemanticComment),
    OutputComment { i: usize, n: usize },
    Subscripted(Box<Subscripted>),
    InlineMacro(InlineMacro),
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
            (Self::Ref(a, ach), Self::Ref(b, bch)) => a == b && ach == bch,
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
            Word::MultilineString(string) => write!(f, "$ {string:?}"),
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
            Word::Ref(r, chained) => {
                write!(f, "ref({r}")?;
                for comp in chained {
                    write!(f, "≈{}", comp.item)?;
                }
                write!(f, ")")
            }
            Word::IncompleteRef(path) => {
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
            Word::Placeholder(Some(i)) => write!(f, "^{i}"),
            Word::Placeholder(None) => write!(f, "^"),
            Word::BreakLine => write!(f, "break_line"),
            Word::FlipLine => write!(f, "unbreak_line"),
            Word::SemanticComment(comment) => write!(f, "{comment}"),
            Word::OutputComment { i, n, .. } => write!(f, "output_comment({i}/{n})"),
            Word::Subscripted(sub) => sub.fmt(f),
            Word::InlineMacro(InlineMacro { ident, func, .. }) => {
                write!(f, "inline_macro({:?}{}))", func.value, ident.value)
            }
        }
    }
}

/// A refered-to item
#[derive(Clone, Serialize, Deserialize)]
pub struct Ref {
    /// The module path of the item
    pub path: Vec<RefComponent>,
    /// The name of the item
    pub name: Sp<Ident>,
}

/// A component of a reference
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefComponent {
    /// The name of the module
    pub module: Sp<Ident>,
    /// The span of the .
    pub dot_span: CodeSpan,
}

/// A component of a reference
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ChainComponent {
    /// The span of the ‥
    pub dot_span: CodeSpan,
    /// The ref of the item
    pub item: Ref,
}

impl Ref {
    /// Get the span of this reference
    pub fn span(&self) -> CodeSpan {
        if let Some(comp) = self.path.first() {
            comp.module.span.clone().merge(self.name.span.clone())
        } else {
            self.name.span.clone()
        }
    }
    /// Get the number of modifier arguments this reference's name implies
    pub fn modifier_args(&self) -> usize {
        ident_modifier_args(&self.name.value)
    }
    /// Get the root module of this reference
    pub fn root_module(&self) -> Option<&Ident> {
        self.path.first().map(|c| &c.module.value)
    }
    /// Get all `Ref`s that desugar from chaining this ref with some names
    pub fn chain_refs(
        self,
        chained: impl IntoIterator<Item = ChainComponent>,
    ) -> impl Iterator<Item = Self> {
        let mut prev = self.name.clone();
        once(self).chain(chained.into_iter().map(move |comp| {
            let mut path = vec![RefComponent {
                module: prev.clone(),
                dot_span: comp.dot_span,
            }];
            path.extend(comp.item.path);
            let r = Ref {
                path,
                name: comp.item.name,
            };
            prev = r.name.clone();
            r
        }))
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
#[derive(Clone, Serialize, Deserialize)]
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
#[derive(Clone, PartialEq, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Clone, Serialize, Deserialize)]
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
#[derive(Clone, PartialEq, Serialize, Deserialize)]
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
    /// Get the number of arguments this modifier takes given a subscript
    pub fn subscript_margs(&self, sub: Option<&Subscript>) -> usize {
        match self {
            Modifier::Primitive(prim) => prim.subscript_margs(sub).unwrap_or_else(|| self.args()),
            m => m.args(),
        }
    }
}

/// An argument setter
#[derive(Clone, Serialize, Deserialize)]
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
#[derive(Clone, Serialize, Deserialize)]
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
#[derive(Clone, PartialEq, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum NumWord {
    Real(f64),
    Infinity(bool),
    Complex(Complex),
    Err(String),
}

impl From<f64> for NumWord {
    fn from(value: f64) -> Self {
        Self::Real(value).normalize()
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
            Ok(v) => v.into(),
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
    fn normalize(self) -> Self {
        match self {
            Self::Real(f64::INFINITY) => Self::Infinity(false),
            Self::Real(f64::NEG_INFINITY) => Self::Infinity(true),
            _ => self,
        }
    }
    /// Map the number
    pub fn map<R, C>(self, real: impl FnOnce(f64) -> R, complex: impl FnOnce(Complex) -> C) -> Self
    where
        C: Into<Self>,
        R: Into<Self>,
    {
        match self {
            Self::Real(r) => real(r).into(),
            Self::Infinity(false) => real(f64::INFINITY).into(),
            Self::Infinity(true) => real(f64::NEG_INFINITY).into(),
            Self::Complex(c) => complex(c).into(),
            Self::Err(e) => Self::Err(e),
        }
        .normalize()
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
            (Self::Infinity(false), b) => Self::Real(f64::INFINITY).map_with(b, real, complex),
            (Self::Infinity(true), b) => Self::Real(f64::NEG_INFINITY).map_with(b, real, complex),
            (a, Self::Infinity(false)) => a.map_with(Self::Real(f64::INFINITY), real, complex),
            (a, Self::Infinity(true)) => a.map_with(Self::Real(f64::NEG_INFINITY), real, complex),
            (Self::Complex(a), Self::Complex(b)) => complex(a, b).into(),
            (Self::Real(a), Self::Complex(b)) => complex(a.into(), b).into(),
            (Self::Complex(a), Self::Real(b)) => complex(a, b.into()).into(),
            (Self::Err(e), _) | (_, Self::Err(e)) => Self::Err(e),
        }
        .normalize()
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
            NumWord::Infinity(false) => write!(f, "∞"),
            NumWord::Infinity(true) => write!(f, "-∞"),
            NumWord::Complex(c) => write!(f, "{c}"),
            NumWord::Err(e) => write!(f, "error({e})"),
        }
    }
}
