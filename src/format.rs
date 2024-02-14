//! The Uiua formatter

use std::{
    borrow::Cow,
    collections::HashMap,
    env,
    fmt::Display,
    fs,
    iter::repeat,
    path::{Path, PathBuf},
    sync::Arc,
};

use instant::Duration;
use paste::paste;

use crate::{
    ast::*,
    function::Signature,
    grid_fmt::GridFmt,
    lex::{is_ident_char, CodeSpan, Loc, Sp},
    parse::{parse, split_words, trim_spaces, unsplit_words},
    value::Value,
    Compiler, FunctionId, Ident, InputSrc, Inputs, Primitive, RunMode, SafeSys, SysBackend, SysOp,
    Uiua, UiuaError, UiuaResult,
};

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

#[cfg(test)]
macro_rules! param_type {
    (bool) => {
        "boolean"
    };
    (usize) => {
        "natural number"
    };
    (CompactMultilineMode) => {
        r#"`"always"`, `"never"`, or `"auto"`"#
    };
}

#[cfg(test)]
macro_rules! default_to_uiua {
    ($default:expr) => {{
        let default = format!("{:?}", $default);
        match default.as_str() {
            "true" => "1".into(),
            "false" => "0".into(),
            s if s.chars().all(|c| c.is_ascii_digit()) => s.into(),
            s => format!("{:?}", s.to_lowercase()),
        }
    }};
}

macro_rules! create_config {
    ($(
        $(#[doc = $doc:literal])+
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

        #[test]
        fn generate_format_cfg_docs() {
            paste! {
                let mut s: String = r#"
# Uiua Formatter Configuration

You can configure Uiua's formatter by creating a file called `.fmt.ua` in the directory from which you run the interpreter. This configuration file is also a Uiua program.

Configuration options are specified by binding values to specific names.

Example with default values:
```uiua
"#.into();
                $(
                    s.push_str(&format!("{} ← {}\n", stringify!([<$name:camel>]), default_to_uiua!($default)));
                )*
                s.push_str(r#"```
The following configuration options are available:

"#);

                $(
                    s.push_str(&format!("### {}\n", stringify!([<$name:camel>])));
                    s.push_str(&format!("Type: {}\n\n", param_type!($ty)));
                    s.push_str(&format!("Default: `{}`\n\n", default_to_uiua!($default)));
                    $(s.push_str(&format!("{}\n", $doc.trim()));)*
                    s.push_str("\n---\n\n");
                )*

                fs::write("site/format_config.md", s).unwrap();
            }
        }

        impl PartialFormatConfig {
            paste! {
                fn from_file(file_path: PathBuf) -> UiuaResult<Self> {
                    let asm = Compiler::new().print_diagnostics(true).load_file(file_path)?.finish();
                    let mut env = Uiua::with_backend(SafeSys::default());
                    env.run_asm(&asm)?;
                    let mut bindings = env.all_values_in_scope();
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
            /// The source inputs for the formatter
            pub inputs: Inputs,
            /// The system backend used for output comments
            pub backend: Arc<dyn SysBackend>,
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
                    inputs: Inputs::default(),
                    backend: Arc::new(SafeSys::default()),
                }
            }
        }

        impl From<PartialFormatConfig> for FormatConfig {
            fn from(config: PartialFormatConfig) -> Self {
                Self {
                    $(
                        $name: config.$name.unwrap_or($default),
                    )*
                    inputs: Inputs::default(),
                    backend: Arc::new(SafeSys::default()),
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
    ///
    /// - `"always"`: Always format multiline expressions in compact mode.
    /// - `"never"`: Never format multiline expressions in compact mode.
    /// - `"auto"`: Format multiline expressions in compact mode if they exceed `MultilineCompactThreshold`.
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
    pub glyph_map: Vec<(CodeSpan, (Loc, Loc))>,
}

impl FormatOutput {
    /// Map a cursor position in unfomatted code to glyph start/end positions in formatted code
    pub fn map_char_pos(&self, pos: u32) -> (u32, u32) {
        let mut pairs = self.glyph_map.iter().cloned();
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
            a_start = b_start;
            a_end = b_end;
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
    format_impl(input, path.as_ref().into(), config)
}

/// Format Uiua code without a path
pub fn format_str(input: &str, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    format_impl(input, InputSrc::Str(0), config)
}

fn format_impl(input: &str, src: InputSrc, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    let mut inputs = Inputs::default();
    let (items, errors, _) = parse(input, src.clone(), &mut inputs);
    if errors.is_empty() {
        Ok(Formatter {
            src,
            config,
            inputs: &inputs,
            output: String::new(),
            glyph_map: Vec::new(),
            end_of_line_comments: Vec::new(),
            prev_import_function: None,
            output_comments: None,
        }
        .format_top_items(&items))
    } else {
        Err(UiuaError::Parse(errors, inputs.into()))
    }
}

/// Format Uiua code in a file at the given path
///
/// This modifies the file
pub fn format_file<P: AsRef<Path>>(
    path: P,
    config: &FormatConfig,
    dont_write: bool,
) -> UiuaResult<FormatOutput> {
    let path = path.as_ref();
    let input =
        fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e.into()))?;
    let formatted = format(&input, path, config)?;
    if formatted.output == input {
        return Ok(formatted);
    }
    let is_no_format_set = env::var("UIUA_NO_FORMAT").is_ok_and(|val| val == "1");
    let should_write = !dont_write && !is_no_format_set;
    if should_write {
        fs::write(path, &formatted.output)
            .map_err(|e| UiuaError::Format(path.to_path_buf(), e.into()))?;
    }
    Ok(formatted)
}

struct Formatter<'a> {
    src: InputSrc,
    config: &'a FormatConfig,
    inputs: &'a Inputs,
    output: String,
    glyph_map: Vec<(CodeSpan, (Loc, Loc))>,
    end_of_line_comments: Vec<(usize, String)>,
    prev_import_function: Option<Ident>,
    output_comments: Option<HashMap<usize, Vec<Vec<Value>>>>,
}

impl<'a> Formatter<'a> {
    fn format_top_items(mut self, items: &[Item]) -> FormatOutput {
        self.format_items(items);
        let mut output = self.output;
        while output.ends_with('\n') {
            output.pop();
        }
        if self.config.trailing_newline && !output.trim().is_empty() {
            output.push('\n');
        }
        FormatOutput {
            output,
            glyph_map: self.glyph_map,
        }
    }
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
                .map(|s| s.trim_end_matches(' ').into())
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
                    // Add comment back to line
                    let line = &mut lines[line_number - 1];
                    let start_byte_len = line.len();
                    let start_char_len = line.chars().count();
                    let spaces = (max + 1).saturating_sub(line.chars().count());
                    line.push_str(&" ".repeat(spaces));
                    line.push('#');
                    if !comment.starts_with(' ')
                        && self.config.comment_space_after_hash
                        && !comment.starts_with('!')
                    {
                        line.push(' ');
                    }
                    line.push_str(&comment);
                    // Update subsequent mappings
                    let byte_len_diff = line.len() - start_byte_len;
                    let char_len_diff = line.chars().count() - start_char_len;
                    for (before, after) in self.glyph_map.iter_mut() {
                        if before.start.line as usize > line_number {
                            after.0.byte_pos += byte_len_diff as u32;
                            after.0.char_pos += char_len_diff as u32;
                            after.1.byte_pos += byte_len_diff as u32;
                            after.1.char_pos += char_len_diff as u32;
                        }
                    }
                }
            }
            let mut new_output = String::new();
            for (i, line) in lines.into_iter().enumerate() {
                if i > 0 {
                    new_output.push('\n');
                }
                new_output.push_str(&line);
            }
            self.output = new_output;
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
            Item::Words(lines) => {
                self.prev_import_function = None;
                let lines = unsplit_words(lines.iter().cloned().flat_map(split_words));
                self.format_multiline_words(&lines, false, false, 0);
            }
            Item::Binding(binding) => {
                match binding.words.first().map(|w| &w.value) {
                    Some(Word::Primitive(Primitive::Sys(SysOp::Import)))
                        if (binding.words.iter())
                            .filter(|word| word.value.is_code())
                            .count()
                            == 2 =>
                    {
                        self.prev_import_function = Some(binding.name.value.clone());
                    }
                    Some(Word::Ref(r)) => {
                        if r.root_module() == self.prev_import_function.as_ref() {
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
                let span = binding
                    .words
                    .first()
                    .zip(binding.words.last())
                    .map(|(first, last)| first.span.clone().merge(last.span.clone()))
                    .or_else(|| binding.signature.as_ref().map(|sig| sig.span.clone()))
                    .unwrap_or_else(|| binding.arrow_span.clone());
                let mut lines = unsplit_words(split_words(binding.words.clone()));
                if lines.len() == 1 {
                    self.format_words(&lines[0], true, 0);
                } else {
                    lines.push(Vec::new());
                    self.format_words(
                        &[span.clone().sp(Word::Func(Func {
                            id: FunctionId::Anonymous(span),
                            signature: None,
                            lines,
                            closed: true,
                        }))],
                        true,
                        0,
                    );
                }
            }
            Item::Import(import) => {
                self.prev_import_function = None;
                if let Some(name) = &import.name {
                    self.push(&name.span, &name.value);
                    self.output.push(' ');
                    self.prev_import_function = Some(name.value.clone());
                }
                self.output.push_str("~ ");
                self.push(&import.path.span, &format!("{:?}", import.path.value));

                let mut import = import.clone();
                let lines = &mut import.lines;
                // Sort each line
                for line in lines.iter_mut().flatten() {
                    line.items.sort_by_key(|item| item.value.clone());
                }
                // Sort contiguous slices of non-empty lines
                let mut i = 0;
                while i < lines.len() {
                    while i < lines.len() && lines[i].is_none() {
                        i += 1;
                    }
                    let start = i;
                    while i < lines.len() && lines[i].is_some() {
                        i += 1;
                    }
                    lines[start..i]
                        .sort_by_key(|line| line.as_ref().unwrap().items[0].value.clone());
                }
                if lines.iter().flatten().count() == 1 {
                    let line = lines.iter().flatten().next().unwrap();
                    self.output.push(' ');
                    self.push(&line.tilde_span, "~");
                    for item in &line.items {
                        self.output.push(' ');
                        self.push(&item.span, &item.value);
                    }
                    if lines.last().unwrap().is_none() {
                        self.output.push('\n');
                    }
                } else {
                    for line in lines {
                        self.output.push('\n');
                        if let Some(line) = line {
                            if self.config.indent_item_imports {
                                for _ in 0..self.config.multiline_indent {
                                    self.output.push(' ');
                                }
                            }
                            self.push(&line.tilde_span, "~");
                            for item in &line.items {
                                self.output.push(' ');
                                self.push(&item.span, &item.value);
                            }
                        }
                    }
                }
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
    fn format_ref(&mut self, r: &Ref) {
        for comp in &r.path {
            self.push(&comp.module.span, &comp.module.value);
            self.push(&comp.tilde_span, "~");
        }
        self.push(&r.name.span, &r.name.value);
    }
    fn format_words(&mut self, words: &[Sp<Word>], trim_end: bool, depth: usize) {
        for word in trim_spaces(words, trim_end) {
            self.format_word(word, depth);
        }
    }
    fn format_word(&mut self, word: &Sp<Word>, depth: usize) {
        match &word.value {
            Word::Number(s, n) => {
                let grid_str = n.grid_string(false);
                let formatted = if !grid_str.contains('ε')
                    && grid_str.chars().count() < s.trim_end_matches('i').chars().count()
                {
                    grid_str
                } else {
                    fn format_frag(s: &str) -> Cow<str> {
                        let mut s = Cow::Borrowed(s);
                        if s.contains('`') {
                            s = Cow::Owned(s.replace('`', "¯"));
                        }
                        for (name, glyph) in [("eta", "η"), ("pi", "π"), ("tau", "τ")] {
                            if s.contains(name) {
                                s = Cow::Owned(s.replace(name, glyph));
                            }
                        }
                        for i in (3..="infinity".len()).rev() {
                            if s.contains(&"infinity"[..i]) {
                                s = Cow::Owned(s.replace(&"infinity"[..i], "∞"));
                            }
                        }
                        s
                    }
                    if let Some((num, denom)) = s.split_once('/') {
                        let num = format_frag(num);
                        let denom = format_frag(denom);
                        format!("{num}/{denom}")
                    } else {
                        format_frag(s).into_owned()
                    }
                };
                if formatted.starts_with(|c: char| c.is_ascii_digit())
                    && self.output.ends_with(|c: char| c.is_ascii_digit())
                {
                    self.output.push(' ');
                }
                self.push(&word.span, &formatted);
            }
            Word::Char(_) | Word::String(_) | Word::Label(_) | Word::FormatString(_) => self
                .output
                .push_str(&self.inputs.get(&word.span.src)[word.span.byte_range()]),
            Word::MultilineString(lines) => {
                if lines.len() == 1 {
                    let span = &lines[0].span;
                    self.output
                        .push_str(&self.inputs.get(&span.src)[span.byte_range()]);
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
                    self.output
                        .push_str(&self.inputs.get(&line.span.src)[line.span.byte_range()]);
                }
            }
            Word::Ref(r) => {
                if self.output.chars().next_back().is_some_and(is_ident_char) {
                    self.output.push(' ');
                }
                self.format_ref(r);
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
                if arr.boxes {
                    self.output.push('{');
                } else {
                    self.output.push('[');
                }
                self.format_multiline_words(&arr.lines, true, true, depth + 1);
                if arr.boxes {
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
                self.format_multiline_words(&func.lines, false, true, depth + 1);
                self.output.push(')');
            }
            Word::Switch(sw) => {
                self.output.push('(');
                let any_multiline = sw.branches.iter().any(|br| {
                    br.value.lines.len() > 1
                        || br
                            .value
                            .lines
                            .iter()
                            .flatten()
                            .any(|word| word_is_multiline(&word.value))
                });
                for (i, br) in sw.branches.iter().enumerate() {
                    let add_leading_newline = i == 0
                        && any_multiline
                        && !(br.value.lines.first()).is_some_and(|line| line.is_empty());
                    if add_leading_newline {
                        self.output.push('\n');
                        for _ in 0..self.config.multiline_indent * (depth + 1) {
                            self.output.push(' ');
                        }
                    }
                    if i > 0 {
                        if any_multiline {
                            for _ in 0..(self.config.multiline_indent * depth.saturating_sub(1))
                                .saturating_sub(2)
                            {
                                self.output.push(' ');
                            }
                        }
                        self.output.push('|');
                        if any_multiline {
                            self.output.push(' ');
                        }
                    }
                    if let Some(sig) = &br.value.signature {
                        self.format_signature(
                            '|',
                            sig.value,
                            any_multiline || br.value.lines.len() <= 1,
                        );
                        if br.value.lines.is_empty() {
                            self.output.pop();
                        }
                    }
                    self.format_multiline_words(&br.value.lines, false, false, depth + 1);
                    if any_multiline
                        && br.value.lines.last().is_some_and(|line| !line.is_empty())
                        && !self.output.ends_with('\n')
                    {
                        self.output.push('\n');
                        for _ in 0..self.config.multiline_indent * depth {
                            self.output.push(' ');
                        }
                    }
                }
                self.output.push(')');
            }
            Word::Primitive(prim) => self.push(&word.span, &prim.to_string()),
            Word::Modified(m) => {
                match &m.modifier.value {
                    Modifier::Primitive(prim) => self.push(&m.modifier.span, &prim.to_string()),
                    Modifier::Ref(r) => self.format_ref(r),
                }
                self.format_words(&m.operands, true, depth);
            }
            Word::Placeholder(sig) => self.format_signature(
                '^',
                *sig,
                self.inputs.get(&word.span.src)[word.span.byte_range()].ends_with(' '),
            ),
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
            Word::BreakLine => self.output.push('\''),
            Word::UnbreakLine => self.output.push_str("''"),
            Word::OutputComment { i, n } => {
                let stacks = self.output_comment(*i);
                let mut s = String::new();
                if stacks.is_empty() {
                    for _ in 0..=*n {
                        s.push('#');
                    }
                }
                let start_line_pos = if self.output.ends_with('\n') {
                    0
                } else {
                    self.output
                        .split('\n')
                        .last()
                        .unwrap_or_default()
                        .chars()
                        .count()
                };
                // Build grid
                const MAX_HEIGHT: usize = 100;
                let mut grid: Vec<Vec<Vec<String>>> = vec![Vec::new(); stacks.len()];
                for (i, stack) in stacks.iter().enumerate() {
                    for value in stack.iter().take(MAX_HEIGHT) {
                        let shown = value.show();
                        let mut lines = shown.lines();
                        grid[i].push(lines.by_ref().take(15).map(Into::into).collect());
                        if lines.next().is_some() {
                            let mut line = String::new();
                            for c in grid[i].last().unwrap().last().unwrap().chars() {
                                line.push(if c.is_alphanumeric() { '⋮' } else { ' ' });
                            }
                            grid[i].last_mut().unwrap().push(line);
                        }
                    }
                }
                // Pad grid cells
                let stack_height = stacks
                    .first()
                    .map_or(0, |stack| stack.len().min(MAX_HEIGHT));
                let max_widths: Vec<usize> = (0..stack_height)
                    .map(|i| {
                        grid.iter()
                            .map(|row| {
                                row[i]
                                    .iter()
                                    .map(|line| line.chars().count())
                                    .max()
                                    .unwrap_or_default()
                            })
                            .max()
                            .unwrap_or_default()
                    })
                    .collect();
                for row in &mut grid {
                    for (i, cell) in row.iter_mut().enumerate() {
                        let width = max_widths[i];
                        for line in cell {
                            for _ in line.chars().count()..=width {
                                line.push(' ');
                            }
                        }
                    }
                }
                // Collapse grid
                let mut lines: Vec<String> = Vec::new();
                for row in grid {
                    let top_row = lines.len();
                    for (i, cell) in row.into_iter().enumerate() {
                        for (j, line) in cell.iter().enumerate() {
                            let j = top_row + j;
                            let prepad = i + max_widths.iter().take(i).sum::<usize>();
                            for line in &mut lines {
                                for _ in line.chars().count()..prepad {
                                    line.push(' ');
                                }
                            }
                            while j >= lines.len() {
                                lines.push(str::repeat(" ", prepad));
                            }
                            lines[j].push_str(line);
                            if lines[j].chars().count() > 200 {
                                lines[j] = lines[j].chars().take(199).collect();
                                lines[j].push('…');
                            }
                        }
                    }
                }
                for (i, line) in lines.into_iter().enumerate() {
                    if i > 0 {
                        s.push('\n');
                        for _ in 0..start_line_pos {
                            s.push(' ');
                        }
                    }
                    s.extend(repeat('#').take(*n + 1));
                    s.push(' ');
                    s.push_str(&line);
                }
                self.push(&word.span, &s);
            }
        }
    }
    fn format_multiline_words(
        &mut self,
        lines: &[Vec<Sp<Word>>],
        allow_compact: bool,
        allow_leading_space: bool,
        depth: usize,
    ) {
        if lines.is_empty() {
            return;
        }
        let prevent_compact = lines
            .iter()
            .flatten()
            .filter(|word| !matches!(word.value, Word::Spaces))
            .last()
            .is_some_and(|word| {
                matches!(
                    word.value,
                    Word::Comment(_) | Word::OutputComment { .. } | Word::MultilineString(_)
                )
            });
        if lines.len() == 1
            && !prevent_compact
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
            && !prevent_compact
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
            if i > 0 || (!compact && allow_leading_space) {
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
            for _ in 0..self.config.multiline_indent * depth.saturating_sub(1) {
                self.output.push(' ');
            }
        }
    }
    fn push(&mut self, span: &CodeSpan, formatted: &str) {
        let start = end_loc(&self.output);
        self.output.push_str(formatted);
        if &self.inputs.get(&span.src)[span.byte_range()] != formatted {
            let end = end_loc(&self.output);
            self.glyph_map.push((span.clone(), (start, end)));
        }
    }
    fn output_comment(&mut self, index: usize) -> Vec<Vec<Value>> {
        let values = self.output_comments.get_or_insert_with(|| {
            let mut env = Uiua::with_backend(self.config.backend.clone())
                .with_execution_limit(Duration::from_secs(2));
            let res = env.compile_run(|comp| {
                comp.print_diagnostics(true)
                    .mode(RunMode::All)
                    .load_str_src(&self.inputs.get(&self.src), self.src.clone())
            });
            let mut values = env.rt.output_comments;
            if let Err(e) = res {
                let next = (0..).take_while(|i| values.contains_key(i)).count();
                values.insert(next, vec![vec![e.to_string().into()]]);
            }
            values
        });
        values.remove(&index).unwrap_or_default()
    }
}

fn word_is_multiline(word: &Word) -> bool {
    match word {
        Word::Number(..) => false,
        Word::Char(_) => false,
        Word::Label(_) => false,
        Word::String(_) => false,
        Word::FormatString(_) => false,
        Word::MultilineString(_) => true,
        Word::Ref(_) => false,
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
        Word::Primitive(_) => false,
        Word::Modified(m) => m.operands.iter().any(|word| word_is_multiline(&word.value)),
        Word::Placeholder(_) => false,
        Word::Comment(_) => true,
        Word::Spaces => false,
        Word::BreakLine | Word::UnbreakLine => false,
        Word::OutputComment { .. } => true,
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
        byte_pos += c.len_utf8() as u32;
    }
    Loc {
        line,
        col,
        char_pos,
        byte_pos,
    }
}
