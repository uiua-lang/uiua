//! The Uiua formatter

use std::{
    any::Any,
    collections::BTreeMap,
    env,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

use paste::paste;

use crate::{
    ast::*,
    function::Signature,
    grid_fmt::GridFmt,
    lex::{is_ident_char, CodeSpan, Loc, Sp},
    parse::parse,
    value::Value,
    Ident, Primitive, SysBackend, SysOp, Uiua, UiuaError, UiuaResult,
};

// For now disallow any syscalls in the format config file.
struct FormatConfigBackend;

impl SysBackend for FormatConfigBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

trait ConfigValue: Sized {
    fn from_value(value: &Value, env: &Uiua, requirement: &'static str) -> UiuaResult<Self>;
}

impl ConfigValue for bool {
    fn from_value(value: &Value, env: &Uiua, requirement: &'static str) -> UiuaResult<bool> {
        value.as_bool(env, requirement)
    }
}

impl ConfigValue for usize {
    fn from_value(value: &Value, env: &Uiua, requirement: &'static str) -> UiuaResult<usize> {
        value.as_nat(env, requirement)
    }
}

/// Ways of choosing whether multiline arrays and functions are formatted compactly or not
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum CompactMultilineMode {
    /// Multiline formatting will always be compact.
    Always,
    /// Multiline formatting will never be compact.
    Never,
    /// Multiline arrays and functions that start on or before `multiline_compact_threshold` will be compact, and those that start after will not be.
    #[default]
    Auto,
}

impl ConfigValue for CompactMultilineMode {
    fn from_value(value: &Value, env: &Uiua, requirement: &'static str) -> UiuaResult<Self> {
        let string = value.as_string(env, requirement)?;
        match string.to_lowercase().as_str() {
            "always" => Ok(Self::Always),
            "never" => Ok(Self::Never),
            "auto" => Ok(Self::Auto),
            _ => Err(env.error(format!("{requirement}, but it is \"{string}\""))),
        }
    }
}

macro_rules! requirement {
    ($name:ident, bool) => {
        concat!(
            "Format config option '",
            stringify!($name),
            "' expects a boolean"
        )
    };
    ($name:ident, usize) => {
        concat!(
            "Format config option '",
            stringify!($name),
            "' expects a natural number"
        )
    };
    ($name:ident, CompactMultilineMode) => {
        concat!(
            "Format config option '",
            stringify!($name),
            r#"' expects one of "always", "never", or "auto""#
        )
    };
}

macro_rules! create_config {
    ($(
        $(#[doc = $doc:literal])*
        (
            $name:ident,
            $ty:ident, // this should ideally be ty, not ident, but that doesn't work with the requirement macro
            $default:expr
        )
    ),* $(,)?) => {
        #[derive(Debug, Clone)]
        struct PartialFormatConfig {
            $(
                $name: Option<$ty>,
            )*
        }

        impl PartialFormatConfig {
            paste! {
                fn from_file(file_path: PathBuf) -> UiuaResult<Self> {
                    let mut env = Uiua::with_backend(FormatConfigBackend)
                        .print_diagnostics(true);
                    env.load_file(file_path)?;
                    let mut bindings = env.all_values_is_scope();
                    $(
                        let $name = {
                            let requirement = requirement!([<$name:camel>], $ty);
                            let function_name = stringify!([<$name:camel>]);
                            if let Some(binding) = bindings.remove(function_name) {
                                Some($ty::from_value(&binding, &env, requirement)?)
                            } else {
                                None
                            }
                        };
                    )*

                    return Ok(Self {
                        $(
                            $name,
                        )*
                    });
                }
            }
        }

        /// Configuration for the Uiua formatter.
        #[derive(Debug, Clone)]
        pub struct FormatConfig {
            $(
                $(#[doc = $doc])*
                #[doc = concat!("Default: `", stringify!($default), "`")]
                pub $name: $ty,
            )*
        }

        paste! {
            impl FormatConfig {
                $(
                    #[allow(missing_docs)]
                    pub fn [<with_ $name>](self, $name: $ty) -> Self {
                        Self {
                            $name,
                            ..self
                        }
                    }
                )*
            }
        }

        impl Default for FormatConfig {
            fn default() -> Self {
                Self {
                    $(
                        $name: $default,
                    )*
                }
            }
        }

        impl From<PartialFormatConfig> for FormatConfig {
            fn from(config: PartialFormatConfig) -> Self {
                Self {
                    $(
                        $name: config.$name.unwrap_or($default),
                    )*
                }
            }
        }
    }
}

create_config!(
    /// Whether to add a trailing newline to the output.
    (trailing_newline, bool, true),
    /// Whether to add a space after the `#` in comments.
    (comment_space_after_hash, bool, true),
    /// The number of spaces to indent multiline arrays and functions
    (multiline_indent, usize, 2),
    /// The mode for formatting multiline arrays and functions.
    (
        compact_multiline_mode,
        CompactMultilineMode,
        CompactMultilineMode::Auto
    ),
    /// The number of characters on line preceding a multiline array or function, at or before which the multiline will be compact.
    (multiline_compact_threshold, usize, 10),
    /// Whether to align consecutive end-of-line comments
    (align_comments, bool, true),
    /// Whether to indent item imports
    (indent_item_imports, bool, true),
);

/// The source from which to populate the formatter configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatConfigSource {
    /// Recursively search for a .fmt.ua file and use it as the formatter configuration,
    /// if none is found, use the default formatter configuration
    SearchFile,
    /// Use the default formatter configuration
    Default,
    /// Use the formatter configuration in the specified file
    Path(PathBuf),
}

impl From<&str> for FormatConfigSource {
    fn from(s: &str) -> Self {
        match s {
            "search-file" => Self::SearchFile,
            "default" => Self::Default,
            path => Self::Path(path.into()),
        }
    }
}

impl Display for FormatConfigSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormatConfigSource::SearchFile => write!(f, "search-file"),
            FormatConfigSource::Default => write!(f, "default"),
            FormatConfigSource::Path(path) => write!(f, "{}", path.display()),
        }
    }
}

impl FormatConfig {
    /// Load the formatter configuration from the specified file
    pub fn from_file(path: PathBuf) -> UiuaResult<Self> {
        println!("Loading format config from {}", path.display());
        let partial = PartialFormatConfig::from_file(path);
        partial.map(Into::into)
    }
    /// Find the formatter configuration relative to the current directory
    pub fn find() -> UiuaResult<Self> {
        Self::from_source(FormatConfigSource::SearchFile, None)
    }
    /// Find the formatter configuration with the specified source
    pub fn from_source(source: FormatConfigSource, target_path: Option<&Path>) -> UiuaResult<Self> {
        match source {
            FormatConfigSource::SearchFile => {
                if let Some(file_path) = Self::search_config_file(target_path) {
                    Self::from_file(file_path)
                } else {
                    Ok(Self::default())
                }
            }
            FormatConfigSource::Default => Ok(Self::default()),
            FormatConfigSource::Path(file_path) => Self::from_file(file_path),
        }
    }

    fn search_config_file(path: Option<&Path>) -> Option<PathBuf> {
        let mut path = path
            .and_then(|p| std::fs::canonicalize(p).ok())
            .unwrap_or(env::current_dir().ok()?);
        loop {
            let file_path = path.join(".fmt.ua");
            if file_path.exists() {
                return Some(file_path);
            }
            if !path.pop() {
                return None;
            }
        }
    }
}

/// Formatter output
pub struct FormatOutput {
    /// The formatted code
    pub output: String,
    /// A map from the original code spans to the formatted code spans
    pub glyph_map: BTreeMap<CodeSpan, (Loc, Loc)>,
}

impl FormatOutput {
    /// Map a cursor position in unfomatted code to glyph start/end positions in formatted code
    pub fn map_char_pos(&self, pos: usize) -> (usize, usize) {
        let mut pairs = self.glyph_map.iter();
        let Some((mut a_span, (mut a_start, mut a_end))) = pairs.next() else {
            return (pos, pos);
        };
        if pos <= a_span.start.char_pos {
            return (pos, pos);
        }
        if (a_span.start.char_pos + 1..=a_span.end.char_pos).contains(&pos) {
            return (a_start.char_pos, a_end.char_pos);
        }
        for (b_span, (b_start, b_end)) in pairs {
            if (a_span.end.char_pos + 1..=b_span.start.char_pos).contains(&pos) {
                return (
                    a_start.char_pos + (pos - a_span.end.char_pos),
                    a_end.char_pos + (pos - a_span.end.char_pos),
                );
            }
            if (b_span.start.char_pos + 1..=b_span.end.char_pos).contains(&pos) {
                return (b_start.char_pos, b_end.char_pos);
            }
            a_span = b_span;
            a_start = *b_start;
            a_end = *b_end;
        }
        (
            a_start.char_pos + (pos - a_span.end.char_pos),
            a_end.char_pos + (pos - a_span.end.char_pos),
        )
    }
}

/// Format Uiua code
///
/// The path is used for error reporting
pub fn format<P: AsRef<Path>>(
    input: &str,
    path: P,
    config: &FormatConfig,
) -> UiuaResult<FormatOutput> {
    format_impl(input, Some(path.as_ref()), config)
}

/// Format Uiua code without a path
pub fn format_str(input: &str, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    format_impl(input, None, config)
}

pub(crate) fn format_items(items: &[Item], config: &FormatConfig) -> FormatOutput {
    let mut formatter = Formatter {
        config,
        output: String::new(),
        glyph_map: BTreeMap::new(),
        end_of_line_comments: Vec::new(),
        prev_import_function: None,
    };
    formatter.format_items(items);
    let mut output = formatter.output;
    while output.ends_with('\n') {
        output.pop();
    }
    if config.trailing_newline && !output.trim().is_empty() {
        output.push('\n');
    }
    FormatOutput {
        output,
        glyph_map: formatter.glyph_map,
    }
}

fn format_impl(
    input: &str,
    path: Option<&Path>,
    config: &FormatConfig,
) -> UiuaResult<FormatOutput> {
    let (items, errors, _) = parse(input, path);
    if errors.is_empty() {
        Ok(format_items(&items, config))
    } else {
        Err(errors.into())
    }
}

/// Format Uiua code in a file at the given path
///
/// This modifies the file
pub fn format_file<P: AsRef<Path>>(path: P, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    let path = path.as_ref();
    let input =
        fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e.into()))?;
    let formatted = format(&input, path, config)?;
    if formatted.output == input {
        return Ok(formatted);
    }
    let dont_write = env::var("UIUA_NO_FORMAT").is_ok_and(|val| val == "1");
    if !dont_write {
        fs::write(path, &formatted.output)
            .map_err(|e| UiuaError::Format(path.to_path_buf(), e.into()))?;
    }
    Ok(formatted)
}

struct Formatter<'a> {
    config: &'a FormatConfig,
    output: String,
    glyph_map: BTreeMap<CodeSpan, (Loc, Loc)>,
    end_of_line_comments: Vec<(usize, String)>,
    prev_import_function: Option<Ident>,
}

impl<'a> Formatter<'a> {
    fn format_items(&mut self, items: &[Item]) {
        for item in items {
            self.format_item(item);
            self.output.push('\n');
        }
        // Align end-of-line comments
        if self.config.align_comments && !self.end_of_line_comments.is_empty() {
            // Group comments by consecutive lines
            let mut groups: Vec<(usize, Vec<(usize, String)>)> = Vec::new();
            let mut lines: Vec<String> = self
                .output
                .split('\n')
                .map(|s| s.trim_end().into())
                .collect();
            for (line_number, comment) in self.end_of_line_comments.drain(..) {
                let line = &lines[line_number - 1];
                let line_len = line.chars().count();
                if let Some((max, group)) = groups.last_mut() {
                    if line_number - group.last().unwrap().0 == 1 {
                        *max = (*max).max(line_len);
                        group.push((line_number, comment));
                    } else {
                        groups.push((line_len, vec![(line_number, comment)]));
                    }
                } else {
                    groups.push((line_len, vec![(line_number, comment)]));
                }
            }
            // Append comments to lines
            for (max, group) in groups {
                for (line_number, comment) in group {
                    let line = &mut lines[line_number - 1];
                    let spaces = max + 1 - line.chars().count();
                    line.push_str(&" ".repeat(spaces));
                    line.push('#');
                    if !comment.starts_with(' ')
                        && self.config.comment_space_after_hash
                        && !comment.starts_with('!')
                    {
                        line.push(' ');
                    }
                    line.push_str(&comment);
                }
            }
            self.output = lines.join("\n");
        }
    }
    fn format_item(&mut self, item: &Item) {
        match item {
            Item::TestScope(items) => {
                self.prev_import_function = None;
                self.output.push_str("---");
                self.output.push('\n');
                self.format_items(&items.value);
                self.output.push_str("---");
            }
            Item::Words(w) => {
                self.prev_import_function = None;
                self.format_words(w, true, 0);
            }
            Item::Binding(binding) => {
                match binding.words.first().map(|w| &w.value) {
                    Some(Word::Primitive(Primitive::Sys(SysOp::Import))) => {
                        self.prev_import_function = Some(binding.name.value.clone());
                    }
                    Some(Word::Ident(ident)) => {
                        if (self.prev_import_function.as_ref()).is_some_and(|prev| prev == ident) {
                            for _ in 0..self.config.multiline_indent {
                                self.output.push(' ');
                            }
                        } else {
                            self.prev_import_function = None;
                        }
                    }
                    _ => self.prev_import_function = None,
                }

                self.output.push_str(&binding.name.value);
                self.output.push_str(" ←");
                if !binding.words.is_empty() || binding.signature.is_some() {
                    self.output.push(' ');
                }
                if let Some(sig) = &binding.signature {
                    self.format_signature('|', sig.value, true);
                }
                self.format_words(&binding.words, true, 0);
            }
            Item::ExtraNewlines(_) => {
                self.prev_import_function = None;
            }
        }
    }
    fn format_signature(&mut self, init_char: char, sig: Signature, trailing_space: bool) {
        self.output.push(init_char);
        self.output.push_str(&sig.args.to_string());
        if sig.outputs != 1 {
            self.output.push('.');
            self.output.push_str(&sig.outputs.to_string());
        }
        if trailing_space {
            self.output.push(' ');
        }
    }
    fn format_words(&mut self, words: &[Sp<Word>], trim_end: bool, depth: usize) {
        for word in trim_spaces(words, trim_end) {
            self.format_word(word, depth);
        }
    }
    fn format_word(&mut self, word: &Sp<Word>, depth: usize) {
        match &word.value {
            Word::Number(s, n) => {
                let grid_str = n.grid_string();
                let formatted = if grid_str.len() < s.trim_end_matches('i').len() {
                    grid_str
                } else {
                    s.replace('`', "¯")
                };
                if formatted.starts_with(|c: char| c.is_ascii_digit())
                    && self.output.ends_with(|c: char| c.is_ascii_digit())
                {
                    self.output.push(' ');
                }
                self.push(&word.span, &formatted);
            }
            Word::Char(_) => self.output.push_str(word.span.as_str()),
            Word::String(_) => self.output.push_str(word.span.as_str()),
            Word::FormatString(_) => self.output.push_str(word.span.as_str()),
            Word::MultilineString(lines) => {
                if lines.len() == 1 {
                    self.output.push_str(lines[0].span.as_str());
                    return;
                }
                let curr_line_pos = if self.output.ends_with('\n') {
                    0
                } else {
                    self.output
                        .split('\n')
                        .last()
                        .unwrap_or_default()
                        .chars()
                        .count()
                };
                for (i, line) in lines.iter().enumerate() {
                    if i > 0 {
                        self.output.push('\n');
                        for _ in 0..curr_line_pos {
                            self.output.push(' ');
                        }
                    }
                    self.output.push_str(line.span.as_str());
                }
            }
            Word::Ident(ident) => {
                if self.output.chars().next_back().is_some_and(is_ident_char) {
                    self.output.push(' ');
                }
                self.output.push_str(ident)
            }
            Word::Strand(items) => {
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        self.output.push('_');
                    }
                    self.format_word(item, depth);
                }
                if items.len() == 1 {
                    self.output.push('_');
                }
            }
            Word::Array(arr) => {
                if arr.constant {
                    self.output.push('{');
                } else {
                    self.output.push('[');
                }
                self.format_multiline_words(&arr.lines, true, depth + 1);
                if arr.constant {
                    self.output.push('}');
                } else {
                    self.output.push(']');
                }
            }
            Word::Func(func) => {
                self.output.push('(');
                if let Some(sig) = &func.signature {
                    let trailing_space = func.lines.len() <= 1
                        && !(func.lines.iter().flatten())
                            .any(|word| word_is_multiline(&word.value));
                    self.format_signature('|', sig.value, trailing_space);
                    if func.lines.is_empty() {
                        self.output.pop();
                    }
                }
                self.format_multiline_words(&func.lines, false, depth + 1);
                self.output.push(')');
            }
            Word::Switch(sw) => {
                self.output.push('(');
                for (i, branch) in sw.branches.iter().enumerate() {
                    if i > 0 {
                        self.output.push('|');
                    }
                    if let Some(sig) = &branch.value.signature {
                        self.format_signature('|', sig.value, branch.value.lines.len() <= 1);
                        if branch.value.lines.is_empty() {
                            self.output.pop();
                        }
                    }
                    self.format_multiline_words(&branch.value.lines, false, depth + 1);
                }
                self.output.push(')');
            }
            Word::Ocean(prims) => {
                for prim in prims {
                    self.push(&prim.span, &prim.value.to_string());
                }
            }
            Word::Primitive(prim) => self.push(&word.span, &prim.to_string()),
            Word::Modified(m) => {
                self.push(
                    &m.modifier.span,
                    &match &m.modifier.value {
                        Modifier::Primitive(prim) => prim.to_string(),
                        Modifier::Ident(ident) => ident.to_string(),
                    },
                );
                self.format_words(&m.operands, true, depth);
            }
            Word::Placeholder(sig) => {
                self.format_signature('^', *sig, word.span.as_str().ends_with(' '))
            }
            Word::Spaces => self.push(&word.span, " "),
            Word::Comment(comment) => {
                let beginning_of_line = self
                    .output
                    .split('\n')
                    .last()
                    .unwrap_or_default()
                    .trim()
                    .is_empty();
                if beginning_of_line || !self.config.align_comments {
                    self.output.push('#');
                    if !comment.starts_with(' ')
                        && self.config.comment_space_after_hash
                        && !comment.starts_with('!')
                    {
                        self.output.push(' ');
                    }
                    self.output.push_str(comment);
                } else {
                    let line_number = self.output.split('\n').count();
                    self.end_of_line_comments
                        .push((line_number, comment.to_string()));
                }
            }
        }
    }
    fn format_multiline_words(
        &mut self,
        lines: &[Vec<Sp<Word>>],
        allow_compact: bool,
        depth: usize,
    ) {
        if lines.is_empty() {
            return;
        }
        let last_word_comment = lines
            .last()
            .unwrap()
            .last()
            .is_some_and(|word| matches!(word.value, Word::Comment(_)));
        if lines.len() == 1
            && !last_word_comment
            && !lines[0].iter().any(|word| word_is_multiline(&word.value))
        {
            self.format_words(&lines[0], true, depth);
            return;
        }
        let curr_line = self.output.split('\n').last().unwrap_or_default();
        let start_line_pos = if self.output.ends_with('\n') {
            0
        } else {
            curr_line.chars().count()
        };
        let compact = allow_compact
            && !last_word_comment
            && match self.config.compact_multiline_mode {
                CompactMultilineMode::Always => true,
                CompactMultilineMode::Never => false,
                CompactMultilineMode::Auto => {
                    start_line_pos <= self.config.multiline_compact_threshold
                        || curr_line.starts_with(' ')
                }
            }
            && (lines.iter().flatten()).all(|word| !word_is_multiline(&word.value));
        let indent = if compact {
            start_line_pos
        } else {
            self.config.multiline_indent * depth
        };
        for (i, line) in lines.iter().enumerate() {
            if i > 0 || !compact {
                self.output.push('\n');
                if !line.is_empty() {
                    for _ in 0..indent {
                        self.output.push(' ');
                    }
                }
            }
            self.format_words(line, true, depth);
        }
        if !compact {
            self.output.push('\n');
            for _ in 0..self.config.multiline_indent * depth.saturating_sub(1) {
                self.output.push(' ');
            }
        }
    }
    fn push(&mut self, span: &CodeSpan, formatted: &str) {
        let start = end_loc(&self.output);
        self.output.push_str(formatted);
        if span.as_str() != formatted {
            let end = end_loc(&self.output);
            self.glyph_map.insert(span.clone(), (start, end));
        }
    }
}

fn trim_spaces(words: &[Sp<Word>], trim_end: bool) -> &[Sp<Word>] {
    let mut start = 0;
    for word in words {
        if let Word::Spaces = word.value {
            start += 1;
        } else {
            break;
        }
    }
    let mut end = words.len();
    if trim_end {
        for word in words.iter().rev() {
            if let Word::Spaces = word.value {
                end -= 1;
            } else {
                break;
            }
        }
    }
    if start >= end {
        return &[];
    }
    &words[start..end]
}

fn word_is_multiline(word: &Word) -> bool {
    match word {
        Word::Number(..) => false,
        Word::Char(_) => false,
        Word::String(_) => false,
        Word::FormatString(_) => false,
        Word::MultilineString(_) => true,
        Word::Ident(_) => false,
        Word::Strand(_) => false,
        Word::Array(arr) => {
            arr.lines.len() > 1
                || (arr.lines.iter()).any(|words| {
                    words.len() > 1 && words.iter().any(|word| word_is_multiline(&word.value))
                })
        }
        Word::Func(func) => {
            func.lines.len() > 1
                || (func.lines.iter())
                    .any(|words| words.iter().any(|word| word_is_multiline(&word.value)))
        }
        Word::Switch(sw) => sw.branches.iter().any(|br| {
            br.value.lines.len() > 1
                || (br.value.lines.iter())
                    .any(|words| words.iter().any(|word| word_is_multiline(&word.value)))
        }),
        Word::Ocean(_) => false,
        Word::Primitive(_) => false,
        Word::Modified(m) => m.operands.iter().any(|word| word_is_multiline(&word.value)),
        Word::Placeholder(_) => false,
        Word::Comment(_) => false,
        Word::Spaces => false,
    }
}

fn end_loc(s: &str) -> Loc {
    let mut line = 0;
    let mut col = 0;
    let mut char_pos = 0;
    let mut byte_pos = 0;
    for c in s.chars() {
        if c == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
        char_pos += 1;
        byte_pos += c.len_utf8();
    }
    Loc {
        line,
        col,
        char_pos,
        byte_pos,
    }
}
