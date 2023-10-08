//! Functions for formatting Uiua code.

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
    lex::{CodeSpan, Loc, Sp},
    parse::parse,
    value::Value,
    SysBackend, Uiua, UiuaError, UiuaResult,
};

// For now disallow any syscalls in the format config file.
struct FormatConfigBackend;

impl SysBackend for FormatConfigBackend {
    fn any(&self) -> &dyn Any {
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
                    let mut bindings = env.all_bindings_in_scope();

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
    pub fn from_file(path: PathBuf) -> UiuaResult<Self> {
        println!("Loading format config from {}", path.display());
        let partial = PartialFormatConfig::from_file(path);
        partial.map(Into::into)
    }

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

pub struct FormatOutput {
    pub output: String,
    pub glyph_map: BTreeMap<CodeSpan, Loc>,
}

impl FormatOutput {
    pub fn map_char_pos(&self, pos: usize) -> usize {
        let mut pairs = self.glyph_map.iter();
        let Some((mut a_span, mut a_loc)) = pairs.next() else {
            return pos;
        };
        if pos <= a_span.start.char_pos {
            return pos;
        }
        if (a_span.start.char_pos + 1..=a_span.end.char_pos).contains(&pos) {
            return a_loc.char_pos;
        }
        for (b_span, b_loc) in pairs {
            if (a_span.end.char_pos + 1..=b_span.start.char_pos).contains(&pos) {
                return a_loc.char_pos + (pos - a_span.end.char_pos);
            }
            if (b_span.start.char_pos + 1..=b_span.end.char_pos).contains(&pos) {
                return b_loc.char_pos;
            }
            a_span = b_span;
            a_loc = b_loc;
        }
        a_loc.char_pos + (pos - a_span.end.char_pos)
    }
}

pub fn format<P: AsRef<Path>>(
    input: &str,
    path: P,
    config: &FormatConfig,
) -> UiuaResult<FormatOutput> {
    format_impl(input, Some(path.as_ref()), config)
}
pub fn format_str(input: &str, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    format_impl(input, None, config)
}

pub fn format_items(items: &[Item], config: &FormatConfig) -> FormatOutput {
    let mut formatter = Formatter {
        config,
        output: String::new(),
        glyph_map: BTreeMap::new(),
        end_of_line_comments: Vec::new(),
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
    glyph_map: BTreeMap<CodeSpan, Loc>,
    end_of_line_comments: Vec<(usize, String)>,
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
            let mut lines: Vec<String> = self.output.lines().map(Into::into).collect();
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
            println!("{:#?}", groups);
            // Append comments to lines
            for (max, group) in groups {
                for (line_number, comment) in group {
                    let line = &mut lines[line_number - 1];
                    let spaces = max - line.chars().count();
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
            Item::Scoped { items, test } => {
                let delim = if *test { "~~~" } else { "---" };
                self.output.push_str(delim);
                self.output.push('\n');
                self.format_items(items);
                self.output.push_str(delim);
            }
            Item::Words(w) => {
                self.format_words(w, true, 0);
            }
            Item::Binding(binding) => {
                self.output.push_str(&binding.name.value);
                self.output.push_str(" ←");
                if !binding.words.is_empty() || binding.signature.is_some() {
                    self.output.push(' ');
                }
                if let Some(sig) = &binding.signature {
                    self.format_signature(sig.value, true);
                }
                self.format_words(&binding.words, true, 0);
            }
            Item::ExtraNewlines(_) => {}
        }
    }
    fn format_signature(&mut self, sig: Signature, trailing_space: bool) {
        self.output.push('|');
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
                if grid_str.len() < s.len() {
                    self.output.push_str(&grid_str);
                } else {
                    self.output.push_str(&s.replace('`', "¯"));
                }
            }
            Word::Char(c) => {
                let formatted = format!("{c:?}");
                let formatted = &formatted[1..formatted.len() - 1];
                self.output.push('@');
                self.output.push_str(formatted);
            }
            Word::String(s) => self.output.push_str(&format!("{:?}", s)),
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
                        .lines()
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
            Word::Ident(ident) => self.output.push_str(ident),
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
                    self.format_signature(sig.value, func.lines.len() <= 1);
                }
                self.format_multiline_words(&func.lines, false, depth + 1);
                self.output.push(')');
            }
            Word::Primitive(prim) => {
                self.push(&word.span, &prim.to_string());
                if prim.is_modifier() {
                    self.output.push('|');
                }
            }
            Word::Modified(m) => {
                self.push(&m.modifier.span, &m.modifier.value.to_string());
                self.format_words(&m.operands, true, depth);
                if m.terminated {
                    self.output.push('|');
                }
            }
            Word::Spaces => self.push(&word.span, " "),
            Word::Comment(comment) => {
                let beginning_of_line = self
                    .output
                    .lines()
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
                    let line_number = self.output.lines().count();
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
            && (lines[0].len() == 1 || !lines[0].iter().any(|word| word_is_multiline(&word.value)))
        {
            self.format_words(&lines[0], true, depth);
            return;
        }
        let curr_line = self.output.lines().last().unwrap_or_default();
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
        self.output.push_str(formatted);
        if span.as_str() != formatted {
            let loc = end_loc(&self.output);
            self.glyph_map.insert(span.clone(), loc);
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
        Word::Number(_, _) => false,
        Word::Char(_) => false,
        Word::String(_) => false,
        Word::FormatString(_) => false,
        Word::MultilineString(lines) => lines.len() > 1,
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
        Word::Primitive(_) => false,
        Word::Modified(m) => m.operands.iter().any(|word| word_is_multiline(&word.value)),
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
