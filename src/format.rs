//! The Uiua formatter

use std::{
    any::Any,
    collections::HashMap,
    env,
    fmt::Display,
    fs,
    iter::repeat_n,
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use paste::paste;
use InlineMacro;

use crate::{
    ast::*,
    is_ident_char,
    lex::{CodeSpan, Loc, Sp},
    parse::{flip_unsplit_items, flip_unsplit_lines, parse, split_words, trim_spaces},
    Compiler, Handle, Ident, InputSrc, Inputs, PreEvalMode, Primitive, RunMode, SafeSys, Signature,
    SysBackend, Uiua, UiuaErrorKind, UiuaResult, Value, SUBSCRIPT_DIGITS,
};

trait ConfigValue: Sized {
    fn from_value(
        value: &Value,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<Self>;
}

impl ConfigValue for bool {
    fn from_value(
        value: &Value,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<bool> {
        value.as_bool(env, requirement)
    }
}

impl ConfigValue for usize {
    fn from_value(
        value: &Value,
        env: &Uiua,
        requirement: impl Into<Option<&'static str>>,
    ) -> UiuaResult<usize> {
        value.as_nat(env, requirement)
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

                fs::write("site/text/format_config.md", s).unwrap();
            }
        }

        impl PartialFormatConfig {
            paste! {
                fn from_file(file_path: PathBuf) -> UiuaResult<Self> {
                    let asm = Compiler::new().print_diagnostics(false).load_file(file_path)?.finish();
                    let mut env = Uiua::with_backend(SafeSys::default());
                    env.run_asm(asm)?;
                    let mut bindings = env.bound_values();
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
            /// An optional backend for the formatter
            pub backend: Option<Arc<dyn SysBackend>>,
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
                    backend: None,
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
                    backend: None,
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
    /// The inputs that were formatted
    pub inputs: Inputs,
}

impl FormatOutput {
    /// Map a cursor position in unfomatted code to glyph start/end positions in formatted code
    pub fn map_char_pos(&self, pos: u32) -> (u32, u32) {
        for (span, (start, end)) in &self.glyph_map {
            if span.start.char_pos == pos {
                return (start.char_pos, start.char_pos);
            }
            let same_len = span.char_count() == end.char_pos - start.char_pos;
            if same_len && span.end.char_pos == pos {
                return (end.char_pos, end.char_pos);
            }
            if (span.start.char_pos..=span.end.char_pos).contains(&pos) {
                return if same_len {
                    let front = start.char_pos + pos - span.start.char_pos;
                    let back = end.char_pos + pos - span.end.char_pos;
                    (front, back)
                } else {
                    (start.char_pos, end.char_pos)
                };
            }
        }
        for win in self.glyph_map.windows(2) {
            let (a_span, (_, a_end)) = &win[0];
            let (b_span, (b_start, _)) = &win[1];
            if (a_span.end.char_pos..=b_span.start.char_pos).contains(&pos) {
                let front = a_end.char_pos + pos - a_span.end.char_pos;
                let back = b_start.char_pos + pos - b_span.start.char_pos;
                return (front, back);
            }
        }
        (pos, pos)
    }
}

#[test]
#[cfg(test)]
fn map_char_pos() {
    let input = "\
+1drop1⇡40 # Range 2 to 40
⊸(♭⊞×.)    # List of products
▽¬⊸(mem:)  # Keep not in list
"
    .replace('\r', "");
    let output = format_str(&input, &FormatConfig::default()).unwrap();
    assert_eq!(output.map_char_pos(0), (0, 0));
    assert_eq!(output.map_char_pos(1), (1, 1));
    assert_eq!(output.map_char_pos(2), (2, 2));
    assert_eq!(output.map_char_pos(3), (2, 3));
    assert_eq!(output.map_char_pos(4), (2, 3));
    assert_eq!(output.map_char_pos(5), (2, 3));
    assert_eq!(output.map_char_pos(6), (2, 3));
    assert_eq!(output.map_char_pos(9), (6, 6)); // Inside `40`
    assert_eq!(output.map_char_pos(10), (7, 7));
    assert_eq!(output.map_char_pos(26), (23, 23)); // End of line 1
    assert_eq!(output.map_char_pos(27), (24, 24)); // Beginning of line 2
    assert_eq!(output.map_char_pos(28), (25, 25));
    assert_eq!(output.map_char_pos(29), (26, 26));
    assert_eq!(output.map_char_pos(30), (27, 27));
    assert_eq!(output.map_char_pos(31), (28, 28));
    assert_eq!(output.map_char_pos(32), (29, 29));
    assert_eq!(output.map_char_pos(33), (30, 30));
    assert_eq!(output.map_char_pos(34), (31, 31));
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
        let (output, glyph_map) = Formatter {
            src,
            config,
            inputs: &inputs,
            output: String::new(),
            glyph_map: Vec::new(),
            end_of_line_comments: Vec::new(),
            prev_import_function: None,
            output_comments: None,
            eval_output_comments: true,
        }
        .format_top_items(&items);
        let formatted = FormatOutput {
            output,
            glyph_map,
            inputs,
        };
        // for (span, (start, end)) in &formatted.glyph_map {
        //     span.as_str(&formatted.inputs, |s| {
        //         println!(
        //             "{s:?}: {}-{} -> {}-{}",
        //             span.start.char_pos, span.end.char_pos, start.char_pos, end.char_pos
        //         )
        //     });
        // }
        // println!("{:?}", formatted.map_char_pos(24));
        Ok(formatted)
    } else {
        Err(UiuaErrorKind::Parse(errors, inputs.into()).into())
    }
}

/// Format Uiua code in a file at the given path
///
/// This modifies the file
pub fn format_file<P: AsRef<Path>>(path: P, config: &FormatConfig) -> UiuaResult<FormatOutput> {
    let path = path.as_ref();
    let input =
        fs::read_to_string(path).map_err(|e| UiuaErrorKind::Load(path.to_path_buf(), e.into()))?;
    let formatted = format(&input, path, config)?;
    if formatted.output == input {
        return Ok(formatted);
    }
    let is_no_format_set = env::var("UIUA_NO_FORMAT").is_ok_and(|val| val == "1");
    let should_write = !is_no_format_set;
    if should_write {
        fs::write(path, &formatted.output)
            .map_err(|e| UiuaErrorKind::Format(path.to_path_buf(), e.into()))?;
    }
    Ok(formatted)
}

pub(crate) fn format_words(words: &[Sp<Word>], inputs: &Inputs) -> String {
    let src = if let Some(word) = words.first() {
        word.span.src.clone()
    } else {
        InputSrc::Str(0)
    };
    let mut formatter = Formatter {
        src,
        config: &FormatConfig::default(),
        inputs,
        output: String::new(),
        glyph_map: Vec::new(),
        end_of_line_comments: Vec::new(),
        prev_import_function: None,
        output_comments: None,
        eval_output_comments: false,
    };
    formatter.format_words(words, true, 0);
    formatter.output
}

pub(crate) fn format_word(word: &Sp<Word>, inputs: &Inputs) -> String {
    let mut formatter = Formatter {
        src: word.span.src.clone(),
        config: &FormatConfig::default(),
        inputs,
        output: String::new(),
        glyph_map: Vec::new(),
        end_of_line_comments: Vec::new(),
        prev_import_function: None,
        output_comments: None,
        eval_output_comments: false,
    };
    formatter.format_word(word, 0);
    formatter.output
}

struct Formatter<'a> {
    src: InputSrc,
    config: &'a FormatConfig,
    inputs: &'a Inputs,
    output: String,
    glyph_map: GlyphMap,
    end_of_line_comments: Vec<EoLComment>,
    prev_import_function: Option<Ident>,
    output_comments: Option<HashMap<usize, Vec<Vec<Value>>>>,
    eval_output_comments: bool,
}

type EoLComment = (usize, usize, String);
type GlyphMap = Vec<(CodeSpan, (Loc, Loc))>;

impl Formatter<'_> {
    fn format_top_items(mut self, items: &[Item]) -> (String, GlyphMap) {
        self.format_items(items, 0);
        let mut output = self.output;
        while output.ends_with('\n') {
            output.pop();
        }
        if self.config.trailing_newline && !output.trim().is_empty() {
            output.push('\n');
        }
        (output, self.glyph_map)
    }
    fn format_items(&mut self, items: &[Item], depth: usize) {
        let items = flip_unsplit_items(items.to_vec());
        let mut max_name_len = 0;
        for (i, item) in items.iter().enumerate() {
            if i > 0 || depth > 0 {
                if item.is_empty_line() {
                    self.output.push('\n');
                } else {
                    self.newline(depth);
                }
            }
            // Calculate max name length to align single-line bindings
            match item {
                Item::Binding(binding) if !words_are_multiline(&binding.words) => {
                    if max_name_len == 0 {
                        max_name_len = items[i..]
                            .iter()
                            .take_while(|item| match item {
                                Item::Binding(binding) => !words_are_multiline(&binding.words),
                                _ => false,
                            })
                            .map(|item| match item {
                                Item::Binding(binding) => binding.name.value.chars().count(),
                                _ => 0,
                            })
                            .max()
                            .unwrap();
                    }
                }
                _ => max_name_len = 0,
            }
            // Format item
            self.format_item(item, max_name_len, depth);
        }
        // Align end-of-line comments
        if self.config.align_comments && !self.end_of_line_comments.is_empty() {
            // Group comments by consecutive lines
            let mut groups: Vec<(usize, Vec<EoLComment>)> = Vec::new();
            let mut lines: Vec<String> = (self.output.split('\n'))
                .map(|s| {
                    if s.ends_with(' ') {
                        let mut trim_s = s.trim_end().to_string();
                        if trim_s.ends_with(['@', '$']) && trim_s != s {
                            trim_s.push(' ');
                        }
                        trim_s
                    } else {
                        s.to_string()
                    }
                })
                .collect();
            for (line_number, octos, comment) in self.end_of_line_comments.drain(..) {
                let line = &lines[line_number - 1];
                let line_len = line.chars().count();
                if let Some((max, group)) = groups.last_mut() {
                    if line_number - group.last().unwrap().0 == 1 {
                        *max = (*max).max(line_len);
                        group.push((line_number, octos, comment));
                    } else {
                        groups.push((line_len, vec![(line_number, octos, comment)]));
                    }
                } else {
                    groups.push((line_len, vec![(line_number, octos, comment)]));
                }
            }
            // Append comments to lines
            for (max, group) in groups {
                for (line_number, octos, comment) in group {
                    // Add comment back to line
                    let line = &mut lines[line_number - 1];
                    let start_byte_len = line.len();
                    let start_char_len = line.chars().count();
                    let spaces = (max + 1).saturating_sub(line.chars().count());
                    // line.push_str(&" ".repeat(spaces));
                    line.extend(repeat_n(' ', spaces));
                    line.extend(repeat_n('#', octos));
                    if (!comment.starts_with(' ') || octos > 1)
                        && self.config.comment_space_after_hash
                        // Shebang
                        && !comment.starts_with('!')
                    {
                        line.push(' ');
                    }
                    line.push_str(&comment);
                    // Update subsequent mappings
                    let byte_len_diff = line.len() - start_byte_len - 1;
                    let char_len_diff = line.chars().count() - start_char_len - 1;
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
    fn newline(&mut self, depth: usize) {
        self.output.push('\n');
        self.indent(depth);
    }
    fn indent(&mut self, depth: usize) {
        for _ in 0..self.config.multiline_indent * depth {
            self.output.push(' ');
        }
    }
    fn format_item(&mut self, item: &Item, max_name_len: usize, depth: usize) {
        match item {
            Item::Module(m) => {
                self.prev_import_function = None;
                self.output.push_str("┌─╴");
                match &m.value.kind {
                    ModuleKind::Named(name) => self.push(&name.span, &name.value),
                    ModuleKind::Test => self.output.push_str("test"),
                }
                if let Some(line) = &m.value.imports {
                    self.output.push(' ');
                    self.push(&line.tilde_span, "~");
                    let mut items = line.items.clone();
                    items.sort_by_key(|item| item.value.clone());
                    for item in items {
                        self.output.push(' ');
                        self.push(&item.span, &item.value);
                    }
                }
                self.format_items(&m.value.items, depth + 1);
                if self.output.ends_with('\n') {
                    self.output.pop();
                }
                self.newline(depth);
                self.output.push_str("└─╴");
            }
            Item::Words(words) => {
                self.prev_import_function = None;
                let lines = flip_unsplit_lines(split_words(words.to_vec()));
                let extra_newlines = lines.len() > 1 && self.output.ends_with('(') && depth > 0;
                for (i, line) in lines.into_iter().enumerate() {
                    let line = trim_spaces(&line, true);
                    if i > 0 || extra_newlines {
                        self.newline(depth);
                    }
                    for (j, word) in line.iter().enumerate() {
                        self.format_word(word, depth);
                        if word_is_multiline(&word.value) && j < words.len() - 1 {
                            for (end, empty) in [(')', "()"), (']', "[]"), ('}', "{}")] {
                                if self.output.ends_with(end) && !self.output.ends_with(empty) {
                                    self.output.pop();
                                    while self.output.ends_with(' ') {
                                        self.output.pop();
                                    }
                                    if !self.output.ends_with('\n') {
                                        self.output.push('\n');
                                    }
                                    for _ in 0..self.config.multiline_indent * depth {
                                        self.output.push(' ');
                                    }
                                    self.output.push(end);
                                    break;
                                }
                            }
                        }
                    }
                }
                if extra_newlines {
                    self.newline(depth.saturating_sub(1));
                }
            }
            Item::Binding(binding) => {
                if let Some(tilde_span) = &binding.tilde_span {
                    self.push(tilde_span, "~");
                }

                match binding.words.first().map(|w| &w.value) {
                    Some(Word::Ref(r))
                        if binding.words.len() == 1
                            && r.root_module()
                                .zip(self.prev_import_function.as_ref())
                                .is_some_and(|(a, b)| a == b) =>
                    {
                        for _ in 0..self.config.multiline_indent {
                            self.output.push(' ');
                        }
                    }
                    _ => self.prev_import_function = None,
                }

                self.output.push_str(&binding.name.value);
                let len = binding.name.value.chars().count();
                if len < max_name_len {
                    for _ in 0..max_name_len - len {
                        self.output.push(' ');
                    }
                }
                self.output
                    .push_str(if binding.public { " ←" } else { " ↚" });
                if binding.code_macro {
                    self.output.push('^');
                }
                if !binding.words.is_empty() || binding.signature.is_some() {
                    self.output.push(' ');
                }
                if let Some(sig) = &binding.signature {
                    self.format_signature(sig.value, true);
                }
                let span = binding
                    .words
                    .first()
                    .zip(binding.words.last())
                    .map(|(first, last)| first.span.clone().merge(last.span.clone()))
                    .or_else(|| binding.signature.as_ref().map(|sig| sig.span.clone()))
                    .unwrap_or_else(|| binding.arrow_span.clone());
                let mut lines = flip_unsplit_lines(split_words(binding.words.clone()));
                if lines.len() == 1 {
                    self.format_words(&lines[0], true, depth);
                } else {
                    lines.insert(0, Vec::new());
                    lines.push(Vec::new());
                    self.format_words(
                        &[span.sp(Word::Func(Func {
                            signature: None,
                            lines: lines.into_iter().map(Item::Words).collect(),
                            closed: true,
                        }))],
                        true,
                        depth,
                    );
                }
            }
            Item::Data(defs) => {
                for (i, data) in defs.iter().enumerate() {
                    if i > 0 {
                        self.output.push(if data.variant && defs[i - 1].variant {
                            ' '
                        } else {
                            '\n'
                        });
                    }
                    self.push(&data.init_span, if data.variant { "|" } else { "~" });
                    if let Some(name) = &data.name {
                        self.push(&name.span, &name.value);
                    }
                    if let Some(fields) = &data.fields {
                        self.output.push(' ');
                        self.push(&fields.open_span, if fields.boxed { "{" } else { "[" });
                        let multiline = fields.trailing_newline
                            || fields.fields.len() > 1
                                && fields.fields.iter().enumerate().any(|(i, f)| {
                                    f.init.is_some()
                                        && (f.bar_span.is_none() && i < fields.fields.len() - 1)
                                })
                            || (fields.fields.iter())
                                .filter_map(|f| f.init.as_ref())
                                .any(|def| words_are_multiline(&def.words))
                            || fields.fields.len() >= 5
                                && fields.fields.iter().any(|f| f.init.is_some());
                        if multiline {
                            self.newline(depth + 1);
                        }
                        for (i, field) in fields.fields.iter().enumerate() {
                            if let Some(comments) = &field.comments {
                                self.format_comments(comments, depth + 1);
                            }
                            self.push(&field.name.span, &field.name.value);
                            let mut parts = Vec::new();
                            if let Some(validator) = &field.validator {
                                parts.push((&validator.open_span, ": ", &validator.words));
                            }
                            if let Some(default) = &field.init {
                                parts.push((&default.arrow_span, " ← ", &default.words));
                            }
                            for (span, sep, words) in parts {
                                self.push(span, sep);
                                let mut lines = flip_unsplit_lines(split_words(words.clone()));
                                if lines.len() == 1 {
                                    self.format_words(&lines[0], true, depth + 1);
                                } else {
                                    let span = lines
                                        .iter()
                                        .find_map(|l| l.first())
                                        .zip(lines.iter().rev().find_map(|l| l.last()))
                                        .map(|(s, e)| s.span.clone().merge(e.span.clone()))
                                        .unwrap_or_else(|| span.clone());
                                    lines.push(Vec::new());
                                    self.format_words(
                                        &[span.sp(Word::Func(Func {
                                            signature: None,
                                            lines: lines.into_iter().map(Item::Words).collect(),
                                            closed: true,
                                        }))],
                                        true,
                                        depth + 1,
                                    );
                                }
                            }
                            if i < fields.fields.len() - 1 {
                                if multiline {
                                    self.newline(depth + 1);
                                } else if field.validator.is_some() || field.init.is_some() {
                                    if let Some(span) = &field.bar_span {
                                        self.push(span, "|");
                                    } else {
                                        self.output.push('|');
                                    }
                                } else {
                                    self.output.push(' ');
                                }
                            }
                        }
                        if multiline {
                            self.newline(depth);
                        }
                        if let Some(span) = &fields.close_span {
                            self.push(span, if fields.boxed { "}" } else { "]" });
                        }
                    }
                    if let Some(words) = &data.func {
                        self.output.push(' ');
                        self.format_words(words, true, depth + 1);
                    }
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
                while lines.len() >= 3 && lines.iter().rev().take(2).all(Option::is_none) {
                    lines.pop();
                }
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
                let one_line = lines.iter().filter(|line| line.is_some()).count() == 1;
                for (i, line) in lines.iter_mut().enumerate() {
                    if line.is_some() && (i > 0 || !one_line) || line.is_none() && i > 0 {
                        self.output.push('\n');
                    }
                    if let Some(line) = line {
                        if i == 0 && one_line {
                            self.output.push(' ');
                        } else if self.config.indent_item_imports {
                            for _ in 0..self.config.multiline_indent * (depth + 1) {
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
    fn format_signature(&mut self, sig: Signature, trailing_space: bool) {
        self.output.push('|');
        self.output.push_str(&sig.args().to_string());
        if sig.outputs() != 1 {
            self.output.push('.');
            self.output.push_str(&sig.outputs().to_string());
        }
        if trailing_space {
            self.output.push(' ');
        }
    }
    fn pre_space(&mut self, next: &str) {
        if next.starts_with(|c: char| c.is_lowercase())
            && (self.output.chars().last()).is_some_and(|c| c.is_lowercase() && is_ident_char(c))
        {
            self.output.push(' ');
        }
    }
    fn format_ref(&mut self, r: &Ref) {
        self.pre_space(
            (r.path.first())
                .map(|comp| comp.module.value.as_str())
                .unwrap_or(r.name.value.as_str()),
        );
        self.format_ref_path(&r.path, false);
        self.push(&r.name.span, &r.name.value);
    }
    fn format_ref_path(&mut self, comps: &[RefComponent], incomplete: bool) {
        if incomplete {
            self.pre_space(
                (comps.first())
                    .map(|comp| comp.module.value.as_str())
                    .unwrap_or(""),
            );
        }
        for comp in comps {
            self.push(&comp.module.span, &comp.module.value);
            self.push(&comp.tilde_span, "~");
        }
    }
    fn format_words(&mut self, words: &[Sp<Word>], trim_end: bool, depth: usize) {
        let words = trim_spaces(words, trim_end);
        for (i, word) in words.iter().enumerate() {
            self.format_word(word, depth);
            if word_is_multiline(&word.value) && i < words.len() - 1 {
                for (end, empty) in [(')', "()"), (']', "[]"), ('}', "{}")] {
                    if self.output.ends_with(end) && !self.output.ends_with(empty) {
                        self.output.pop();
                        while self.output.ends_with(' ') {
                            self.output.pop();
                        }
                        if !self.output.ends_with('\n') {
                            self.output.push('\n');
                        }
                        for _ in 0..self.config.multiline_indent * depth {
                            self.output.push(' ');
                        }
                        self.output.push(end);
                        break;
                    }
                }
            }
        }
    }
    fn format_word(&mut self, word: &Sp<Word>, depth: usize) {
        match &word.value {
            Word::Number(_, s) => self.push(&word.span, s),
            Word::Label(label) => self.push(&word.span, &format!("${label}")),
            Word::Char(_) | Word::String(_) | Word::FormatString(_) => self
                .output
                .push_str(&self.inputs.get(&word.span.src)[word.span.byte_range()]),
            Word::MultilineString(lines) => {
                let curr_line_pos = if self.output.ends_with('\n') {
                    0
                } else {
                    (self.output.split('\n').next_back())
                        .unwrap_or_default()
                        .chars()
                        .count()
                };
                for (i, line) in lines.iter().enumerate() {
                    let mut line = line.value.as_str();
                    if line.ends_with('\r') {
                        line = &line[..line.len() - 1];
                    }
                    if i > 0 {
                        self.output.push('\n');
                        for _ in 0..curr_line_pos {
                            self.output.push(' ');
                        }
                    }
                    self.output.push_str("$ ");
                    self.output.push_str(line);
                }
            }
            Word::MultilineFormatString(lines) => {
                if lines.len() == 1 {
                    let span = &lines[0].span;
                    self.output
                        .push_str(&self.inputs.get(&span.src)[span.byte_range()]);
                    return;
                }
                let curr_line_pos = if self.output.ends_with('\n') {
                    0
                } else {
                    (self.output.split('\n').next_back())
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
            Word::Ref(r) => self.format_ref(r),
            Word::IncompleteRef { path, .. } => self.format_ref_path(path, true),
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
                if let Some(down_span) = &arr.down_span {
                    self.push(down_span, "↓");
                }
                if arr.boxes {
                    self.output.push('{');
                } else {
                    self.output.push('[');
                }

                self.format_inner_items(&arr.lines, true, depth + 1);
                if arr.boxes {
                    self.output.push('}');
                } else {
                    self.output.push(']');
                }
            }
            Word::Func(func) => self.func(func, depth),
            Word::Pack(pack) => self.pack(pack, depth),
            Word::Primitive(prim) => self.format_primitive(*prim, &word.span),
            Word::Modified(m) => {
                self.format_modifier(&m.modifier, depth);
                self.format_words(&m.operands, true, depth);
            }
            Word::Placeholder(i) => self.push(&word.span, &format!("^{i}")),
            Word::Subscripted(sub) => match &sub.word.value {
                Word::Modified(m) => {
                    if sub.script.value.num.is_some()
                        && matches!(m.modifier.value, Modifier::Primitive(Primitive::Each))
                    {
                        let modifier =
                            (m.modifier.span.clone()).sp(Modifier::Primitive(Primitive::Rows));
                        self.format_modifier(&modifier, depth);
                    } else {
                        self.format_modifier(&m.modifier, depth);
                    }
                    self.subscript(&sub.script);
                    self.format_words(&m.operands, true, depth);
                }
                Word::Primitive(Primitive::Utf8) => {
                    self.push(&sub.word.span, "utf");
                    self.subscript(&sub.script);
                }
                _ => {
                    self.format_word(&sub.word, depth);
                    if self.output.ends_with(SUBSCRIPT_DIGITS) {
                        self.output.push(' ');
                    }
                    self.subscript(&sub.script);
                }
            },
            Word::Spaces => self.push(&word.span, " "),
            Word::Comment(comment) => {
                let beginning_of_line = self
                    .output
                    .split('\n')
                    .next_back()
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
                        .push((line_number, 1, comment.to_string()));
                }
            }
            Word::BreakLine => self.output.push_str(";;"),
            Word::FlipLine => self.output.push(';'),
            Word::SemanticComment(sc) => {
                if !self.output.is_empty() && !self.output.ends_with(['\n', ' ']) {
                    self.output.push(' ');
                }
                self.push(&word.span, &sc.to_string());
            }
            Word::OutputComment { i, n } => {
                let stacks = self.eval_output_comment(*i);
                let mut s = String::new();
                if stacks.is_empty() {
                    for _ in 0..=*n {
                        s.push('#');
                    }
                }
                let start_line_pos = if self.output.ends_with('\n') {
                    0
                } else {
                    (self.output.split('\n').next_back())
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
                let beginning_of_line = self
                    .output
                    .split('\n')
                    .next_back()
                    .unwrap_or_default()
                    .trim()
                    .is_empty();
                if beginning_of_line || !self.config.align_comments {
                    for (i, line) in lines.into_iter().enumerate() {
                        if i > 0 {
                            s.push('\n');
                            for _ in 0..start_line_pos {
                                s.push(' ');
                            }
                        }
                        s.extend(repeat_n('#', *n + 1));
                        s.push(' ');
                        s.push_str(&line);
                    }
                    self.push(&word.span, &s);
                } else {
                    let start_line = self.output.split('\n').count();
                    for (i, line) in lines.into_iter().enumerate() {
                        if i > 0 {
                            self.output.push('\n');
                            for _ in 0..start_line_pos {
                                self.output.push(' ');
                            }
                        }
                        self.end_of_line_comments
                            .push((start_line + i, *n + 1, line));
                    }
                }
            }
            Word::InlineMacro(InlineMacro {
                func,
                caret_span,
                ident,
            }) => {
                self.func(&func.value, depth);
                if let Some(span) = caret_span {
                    self.push(span, "^");
                }
                self.push(&ident.span, &ident.value);
            }
        }
    }
    fn format_primitive(&mut self, prim: Primitive, span: &CodeSpan) {
        let as_str = prim.to_string();
        if self.output.ends_with(' ')
            && span.end.char_pos - span.start.char_pos > 1
            && !(as_str.starts_with(is_ident_char) || as_str.starts_with('&'))
            && self.output[..self.output.len() - 1].ends_with(is_ident_char)
            && (self.glyph_map.last()).is_some_and(|(last_span, _)| {
                last_span.end.char_pos - last_span.start.char_pos == 1
            })
        {
            self.output.pop();
            self.glyph_map.pop();
        }
        match prim {
            Primitive::Utf8 => self.push(span, "utf₈"),
            Primitive::Eq => {
                if self.output.ends_with('!') {
                    self.output.push(' ');
                }
                self.push(span, &as_str)
            }
            _ => self.push(span, &as_str),
        }
    }
}

impl Formatter<'_> {
    fn format_inner_items(&mut self, mut items: &[Item], allow_compact: bool, depth: usize) {
        // println!("items:");
        // for item in items {
        //     println!("  {item:?}");
        // }
        if items.is_empty() {
            return;
        }
        let prevent_compact = (items.iter())
            .filter(|item| !item.is_empty_line())
            .next_back()
            .is_some_and(item_is_end_of_line);
        if items.len() == 1 && !prevent_compact && !item_is_multiline(&items[0]) {
            self.format_item(&items[0], 0, depth);
            return;
        }
        // Remove trailing empty lines
        let has_trailing_newline = items.last().is_some_and(Item::is_empty_line);
        if depth > 0 {
            while items.last().is_some_and(Item::is_empty_line)
                && (items.iter().nth_back(1)).is_some_and(Item::is_empty_line)
                && !(items.iter().nth_back(1)).is_some_and(item_is_end_of_line)
            {
                items = &items[..items.len() - 1];
            }
        }
        // Remove leading empty lines
        let mut has_leading_newline = false;
        while items.first().is_some_and(|line| line.is_empty_line()) {
            items = &items[1..];
            has_leading_newline = true;
        }
        let curr_line = self.output.split('\n').next_back().unwrap_or_default();
        let start_line_pos = if self.output.ends_with('\n') {
            0
        } else {
            curr_line.chars().count()
        };
        let depth_indent = self.config.multiline_indent * depth;
        let starts_indented = start_line_pos > depth_indent;
        let allow_leading_newline = starts_indented || has_trailing_newline && allow_compact;
        let indent = if allow_leading_newline && has_leading_newline {
            depth_indent
        } else {
            start_line_pos
        };
        let last_index = items.len() - 1;
        for (i, item) in items.iter().enumerate() {
            let is_empty_line = item.is_empty_line();
            if is_empty_line {
                if i == 0 && allow_leading_newline || i > 0 && i < last_index || i == last_index {
                    self.newline(depth.saturating_sub(1));
                }
            } else if i > 0 || has_leading_newline && allow_leading_newline {
                self.output.push('\n');
                for _ in 0..indent {
                    self.output.push(' ');
                }
            }
            self.format_item(item, 0, depth);
            if !is_empty_line && i == last_index && has_leading_newline && starts_indented {
                self.newline(depth.saturating_sub(1));
            }
        }
    }
    fn format_modifier(&mut self, modifier: &Sp<Modifier>, depth: usize) {
        match &modifier.value {
            Modifier::Primitive(prim) => self.format_primitive(*prim, &modifier.span),
            Modifier::Ref(r) => self.format_ref(r),
            Modifier::Macro(mac) => {
                self.func(&mac.func.value, depth);
                if let Some(span) = &mac.caret_span {
                    self.push(span, "^");
                }
                self.push(&mac.ident.span, &mac.ident.value);
            }
        }
    }
    fn push(&mut self, span: &CodeSpan, formatted: &str) {
        let start = end_loc(&self.output);
        self.output.push_str(formatted);
        let end = end_loc(&self.output);
        self.glyph_map.push((span.clone(), (start, end)));
    }
    fn format_comments(&mut self, comments: &Comments, depth: usize) {
        for line in &comments.lines {
            self.push(&line.span, &format!("# {}", line.value));
            self.newline(depth);
        }
        for (sem, span) in &comments.semantic {
            self.push(span, &sem.to_string());
            self.newline(depth);
        }
    }
    fn eval_output_comment(&mut self, index: usize) -> Vec<Vec<Value>> {
        let values = self.output_comments.get_or_insert_with(|| {
            if !self.eval_output_comments {
                return HashMap::new();
            }
            let mut env = Uiua::with_backend(FormatterBackend(self.config.backend.clone()))
                .with_execution_limit(Duration::from_secs(1));

            let enabled = env.rt.backend.set_output_enabled(false);
            let res = env.compile_run(|comp| {
                comp.print_diagnostics(false)
                    .mode(RunMode::All)
                    .pre_eval_mode(PreEvalMode::Lazy)
                    .load_str_src(&self.inputs.get(&self.src), self.src.clone())
            });
            env.rt.backend.set_output_enabled(enabled);

            let mut values = env.rt.output_comments;
            if let Err(e) = res {
                let next = (0..).take_while(|i| values.contains_key(i)).count();
                values.insert(next, vec![vec![e.to_string().into()]]);
            }
            values
        });
        values.remove(&index).unwrap_or_default()
    }
    fn func(&mut self, func: &Func, depth: usize) {
        let start_indent = (self.output.rsplit('\n').next()).map_or(0, |line| line.chars().count());

        let double_nest = self.output.ends_with(['(', '{', '[']);

        self.output.push('(');

        // Signature
        if let Some(sig) = &func.signature {
            let trailing_space = func.lines.len() <= 1
                && !(func.word_lines().flatten()).any(|word| word_is_multiline(&word.value));
            self.format_signature(sig.value, trailing_space);
            if func.lines.is_empty() {
                self.output.pop();
            }
        }

        let extra_newline = func.lines.len() > 1
            && start_indent > self.config.multiline_indent * (depth + 1)
            && !func.lines.first().is_some_and(|item| item.is_empty_line());
        if extra_newline {
            self.newline(depth + 1);
        }

        let depth = depth + 1
            - ((double_nest && func.word_lines().next().is_some_and(|line| line.is_empty()))
                as usize);
        self.format_inner_items(&func.lines, true, depth);
        if double_nest {
            while self.output.chars().rev().take_while(|&c| c == ' ').count() >= start_indent {
                self.output.pop();
            }
        }
        if extra_newline && !func.lines.last().is_some_and(|item| item.is_empty_line()) {
            self.newline(depth.saturating_sub(1));
        }
        self.output.push(')');
    }
    fn subscript(&mut self, sub: &Sp<Subscript>) {
        self.push(&sub.span, &sub.value.to_string());
    }
    fn pack(&mut self, pack: &FunctionPack, depth: usize) {
        if let Some(down_span) = &pack.down_span {
            self.push(down_span, "↓");
        }

        let start_indent = (self.output.lines().last()).map_or(0, |line| line.chars().count());
        let indent = self.config.multiline_indent * depth;

        self.output.push(match pack.is_array {
            None => '(',
            Some(false) => '[',
            Some(true) => '{',
        });

        if pack.branches.iter().all(|br| !br.value.is_multiline()) {
            // If all branches are single-line, we can put multiple branches on the same line
            let mut rows: Vec<Vec<&Sp<Func>>> = Vec::new();
            for br in &pack.branches {
                if rows.last_mut().is_some_and(|line| {
                    line.last()
                        .is_some_and(|br2| br.span.start.line == br2.span.start.line)
                }) {
                    rows.last_mut().unwrap().push(br);
                } else {
                    rows.push(vec![br]);
                }
            }
            let row_count = rows.len();
            for (i, row) in rows.into_iter().enumerate() {
                for (j, br) in row.into_iter().enumerate() {
                    let mut lines = &*br.value.lines;
                    while lines.first().is_some_and(Item::is_empty_line) {
                        lines = &lines[1..];
                    }

                    if (i, j) == (0, 0) {
                        let add_leading_newline =
                            row_count > 1 && (start_indent > indent + 1 || lines.is_empty());
                        if add_leading_newline {
                            self.newline(depth + (!lines.is_empty()) as usize);
                        }
                    } else {
                        if j > 0 && row_count > 1 {
                            self.output.push(' ');
                        }
                        self.output.push('|');
                        if row_count > 1 {
                            self.output.push(' ');
                        }
                    }
                    if let Some(sig) = &br.value.signature {
                        self.format_signature(sig.value, lines.len() <= 1);
                        if lines.is_empty() {
                            self.output.pop();
                        }
                    }
                    self.format_inner_items(lines, false, depth + 1);
                }
                if row_count > 1
                    && i < row_count - 1
                    && !self.output.trim_end_matches(' ').ends_with('\n')
                {
                    self.newline(depth);
                }
            }
        } else {
            // If any branch is multiline, we put all branches on separate lines
            let any_multiline = pack.branches.iter().any(|br| {
                br.value.lines.len() > 1
                    || br.value.lines.len() >= 2 && br.value.lines.iter().any(Item::is_empty_line)
                    || br.value.lines.iter().any(item_is_end_of_line)
            });

            for (i, br) in pack.branches.iter().enumerate() {
                let mut lines = &*br.value.lines;

                while lines
                    .first()
                    .is_some_and(|item| matches!(item, Item::Words(lines) if lines.is_empty()))
                {
                    lines = &lines[1..];
                }

                if i == 0 {
                    let add_leading_newline =
                        any_multiline && (start_indent > indent + 1 || lines.is_empty());
                    if add_leading_newline {
                        self.newline(depth + (!lines.is_empty()) as usize);
                    }
                } else {
                    self.output.push('|');
                    if any_multiline {
                        self.output.push(' ');
                    }
                }
                if let Some(sig) = &br.value.signature {
                    self.format_signature(sig.value, any_multiline || lines.len() <= 1);
                    if lines.is_empty() {
                        self.output.pop();
                    }
                }
                // Remove trailing empty lines from last branch
                if i == pack.branches.len() - 1
                    && lines.last().is_some_and(Item::is_empty_line)
                    && lines.iter().nth_back(1).is_some_and(Item::is_empty_line)
                    && !(lines.iter().nth_back(1)).is_some_and(item_is_end_of_line)
                {
                    lines = &lines[..lines.len() - 1];
                }
                self.format_inner_items(lines, false, depth + 1);
                if any_multiline
                    && i < pack.branches.len() - 1
                    && (br.value.lines.last()).is_some_and(|item| !item.is_empty_line())
                    && !self.output.trim_end_matches(' ').ends_with('\n')
                {
                    self.newline(depth);
                }
            }
        }
        self.output.push(match pack.is_array {
            None => ')',
            Some(false) => ']',
            Some(true) => '}',
        });
    }
}

fn words_are_multiline(words: &[Sp<Word>]) -> bool {
    if let Some((last, words)) = words.split_last() {
        words.iter().any(|word| word_is_multiline(&word.value))
            || !last.value.is_end_of_line() && word_is_multiline(&last.value)
    } else {
        false
    }
}

fn item_is_end_of_line(item: &Item) -> bool {
    item.words_or(true, |words| {
        (words.iter().filter(|word| word.value.is_code()).next_back())
            .is_some_and(|word| word.value.is_end_of_line())
    })
}

fn item_is_multiline(item: &Item) -> bool {
    item.words_or(true, words_are_multiline)
}

pub(crate) fn word_is_multiline(word: &Word) -> bool {
    match word {
        Word::Number(..) => false,
        Word::Char(_) => false,
        Word::Label(_) => false,
        Word::String(_) => false,
        Word::FormatString(_) => false,
        Word::MultilineString(_) => true,
        Word::MultilineFormatString(_) => true,
        Word::Ref(_) => false,
        Word::IncompleteRef { .. } => false,
        Word::Strand(_) => false,
        Word::Array(arr) => {
            arr.lines.len() > 1
                || (arr.lines.iter()).any(|item| item.words_or(true, words_are_multiline))
        }
        Word::Func(func) => {
            func.lines.len() > 1
                || (func.lines.iter()).any(|item| item.words_or(true, words_are_multiline))
        }
        Word::InlineMacro(InlineMacro { func, .. }) => {
            func.value.lines.len() > 1
                || (func.value.lines.iter()).any(|item| item.words_or(true, words_are_multiline))
        }
        Word::Pack(pack) => pack.branches.iter().any(|br| {
            br.value.lines.len() > 1
                || (br.value.lines.iter()).any(|item| item.words_or(true, words_are_multiline))
        }),
        Word::Primitive(_) => false,
        Word::Modified(m) => {
            m.operands.iter().any(|word| word_is_multiline(&word.value))
                || match &m.modifier.value {
                    Modifier::Macro(mac) => {
                        mac.func.value.lines.len() > 1
                            || (mac.func.value.lines.iter())
                                .any(|item| item.words_or(true, words_are_multiline))
                    }
                    _ => false,
                }
        }
        Word::Placeholder(_) => false,
        Word::Subscripted(sub) => word_is_multiline(&sub.word.value),
        Word::Comment(_) => true,
        Word::Spaces => false,
        Word::BreakLine => true,
        Word::FlipLine => false,
        Word::SemanticComment(_) => true,
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

#[derive(Default)]
struct FormatterBackend(pub Option<Arc<dyn SysBackend>>);

impl FormatterBackend {
    fn sys(&self) -> &dyn SysBackend {
        if let Some(s) = &self.0 {
            s.as_ref()
        } else {
            native()
        }
    }
}

impl SysBackend for FormatterBackend {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
    fn print_str_stdout(&self, _: &str) -> Result<(), String> {
        Ok(())
    }
    fn print_str_stderr(&self, _: &str) -> Result<(), String> {
        Ok(())
    }
    fn print_str_trace(&self, _: &str) {}
    fn show(&self, _: Value) -> Result<(), String> {
        Ok(())
    }
    #[cfg(feature = "image")]
    fn show_image(&self, _: image::DynamicImage, _: Option<&str>) -> Result<(), String> {
        Ok(())
    }
    fn show_gif(&self, _: Vec<u8>, _: Option<&str>) -> Result<(), String> {
        Ok(())
    }
    fn allow_thread_spawning(&self) -> bool {
        true
    }
    fn file_exists(&self, path: &str) -> bool {
        self.sys().file_exists(path)
    }
    fn is_file(&self, path: &str) -> Result<bool, String> {
        self.sys().is_file(path)
    }
    fn list_dir(&self, path: &str) -> Result<Vec<String>, String> {
        self.sys().list_dir(path)
    }
    fn file_read_all(&self, path: &Path) -> Result<Vec<u8>, String> {
        self.sys().file_read_all(path)
    }
    fn file_write_all(&self, path: &Path, contents: &[u8]) -> Result<(), String> {
        self.sys().file_write_all(path, contents)
    }
    fn open_file(&self, path: &Path, write: bool) -> Result<Handle, String> {
        self.sys().open_file(path, write)
    }
    fn close(&self, handle: Handle) -> Result<(), String> {
        self.sys().close(handle)
    }
    fn read(&self, handle: Handle, len: usize) -> Result<Vec<u8>, String> {
        self.sys().read(handle, len)
    }
    fn read_all(&self, handle: Handle) -> Result<Vec<u8>, String> {
        self.sys().read_all(handle)
    }
    fn read_until(&self, handle: Handle, delim: &[u8]) -> Result<Vec<u8>, String> {
        self.sys().read_until(handle, delim)
    }
    fn write(&self, handle: Handle, contents: &[u8]) -> Result<(), String> {
        self.sys().write(handle, contents)
    }
    fn var(&self, name: &str) -> Option<String> {
        self.sys().var(name)
    }
    fn tcp_connect(&self, addr: &str) -> Result<Handle, String> {
        self.sys().tcp_connect(addr)
    }
    fn tls_connect(&self, addr: &str) -> Result<Handle, String> {
        self.sys().tls_connect(addr)
    }
    fn timezone(&self) -> Result<f64, String> {
        self.sys().timezone()
    }
    fn breakpoint(&self, _: &Uiua) -> Result<bool, String> {
        Ok(true)
    }
    fn audio_sample_rate(&self) -> u32 {
        self.sys().audio_sample_rate()
    }
    fn clipboard(&self) -> Result<String, String> {
        self.sys().clipboard()
    }
    fn load_git_module(&self, url: &str, target: crate::GitTarget) -> Result<PathBuf, String> {
        self.sys().load_git_module(url, target)
    }
}

#[cfg(not(feature = "native_sys"))]
struct NoSys;
#[cfg(not(feature = "native_sys"))]
impl SysBackend for NoSys {
    fn any(&self) -> &dyn Any {
        self
    }
    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

fn native() -> &'static dyn SysBackend {
    #[cfg(feature = "native_sys")]
    {
        &crate::NativeSys
    }
    #[cfg(not(feature = "native_sys"))]
    {
        &NoSys
    }
}

#[test]
#[cfg(test)]
fn formatter_idempotence() {
    let input = "\
⊃(|)
⊃(+|-|×|÷)
F ← (
  ⊃(+
  | -)
)
G ← (
  ∘
  ∘ # hi
)
⊃(+
| - # x
)
∘⊃(
  +
| - # x
)
⊃(+
| -
)∘
⊃(+
| -)
(1 2) (
  5
)
(1
 2
) (
  5
)
(1
 2
 3
)
[1
 2
 3
]
(1
 2
 3)
[1
 2
 3]
⊃(1
| 2
)
Foo(
  Bar(
    Baz
    Qux
  )Mix
)Dir

x ← 2

┌─╴Foo
  F ← (
    1
  )
  G ← (
    2
  )
└─╴
∘⊃(
| 1
| 2
| 3
)
⊃(
| 1
| 2
| 3
)
⊃(1
| 2
| 3
)
∘∘(
  (∘
  )
)
(
  ∘∘∘(+)
)
1/10
x ← [1_2
     3_4]
x ← [
  1_2
  3_4
]
x ← [
  1_2
  3_4
]
x ← {1_2
     3_4}
x ← {
  1_2
  3_4
}
x ← {
  1_2
  3_4
}
∘∘[°$ a
    $ b
]
F ← |2 +
F ← (|2
  +
)
+,
∘ M! =
~ \"x\" ~ A B
~ \"x\"
  ~ A B
~ \"x\"
  ~ A B
  ~ C D
┌─╴A
  F ← 2

  G ← 3 # hi
└─╴
┌─╴A
  F ← 2

  G ← 3
└─╴
~ \"example\"

F ← 5

[(
  1 2
  3 4
)]
[(1 2
  3 4
)]

⊃(∘ | ¯ | ±
| ⌵ | ∿ | √
| ⁅ | ⌊ | ⌈
)

∘⊃(
  ∘ | ¯ | ±
| ⌵ | ∿ | √
| ⁅ | ⌊ | ⌈
)

F ← (
  (
    1
    ⊃(2
    | 3)
  )
)
";
    let formatted = format_str(input, &FormatConfig::default()).unwrap().output;
    if formatted != input {
        const N: usize = 50;
        let offset = formatted
            .chars()
            .zip(input.chars())
            .position(|(a, b)| a != b)
            .unwrap_or_else(|| formatted.chars().count().min(input.chars().count()))
            .saturating_sub(N / 2);
        panic!(
            "Formatting non-idempotent:\n\
            input:     {:?}\n\
            formatted: {:?}\n",
            input.chars().skip(offset).take(N).collect::<String>(),
            formatted.chars().skip(offset).take(N).collect::<String>(),
        );
    }
}

#[test]
#[cfg(test)]
fn formatter_correctness() {
    let input = "\
F,1
\\\\25cb
";
    let output = "\
F₁
○
";
    let formatted = format_str(input, &FormatConfig::default()).unwrap().output;
    assert_eq!(formatted, output);
}
