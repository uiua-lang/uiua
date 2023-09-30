//! Functions for formatting Uiua code.

use std::{env, fs, path::Path};

use crate::{
    ast::*, function::Signature, grid_fmt::GridFmt, lex::Sp, parse::parse, UiuaError, UiuaResult,
};

#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Whether to add a trailing newline to the output.
    ///
    /// Default: `true`
    pub trailing_newline: bool,
    /// Whether to add a space after the `#` in comments.
    ///
    /// Default: `true`
    pub comment_space_after_hash: bool,
    /// The number of spaces to indent multiline arrays and functions
    ///
    /// Default: `2`
    pub multiline_indent: usize,
    /// Override multiline formatting to be always compact or always not.
    ///
    /// Default: `None`
    ///
    /// If `None`, then multiline arrays and functions that start on or before `multiline_compact_threshold` will be compact, and those that start after will not be.
    pub compact_multiline: Option<bool>,
    /// The number of characters on line preceding a multiline array or function, at or before which the multiline will be compact.
    ///
    /// Default: `10`
    pub multiline_compact_threshold: usize,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            trailing_newline: true,
            comment_space_after_hash: true,
            multiline_indent: 2,
            compact_multiline: None,
            multiline_compact_threshold: 10,
        }
    }
}

pub fn format<P: AsRef<Path>>(input: &str, path: P, config: &FormatConfig) -> UiuaResult<String> {
    format_impl(input, Some(path.as_ref()), config)
}
pub fn format_str(input: &str, config: &FormatConfig) -> UiuaResult<String> {
    format_impl(input, None, config)
}

pub fn format_items(items: &[Item], config: &FormatConfig) -> String {
    let mut formatter = Formatter {
        config,
        output: String::new(),
    };
    formatter.format_items(items);
    let mut output = formatter.output;
    while output.ends_with('\n') {
        output.pop();
    }
    if config.trailing_newline && !output.trim().is_empty() {
        output.push('\n');
    }
    output
}

fn format_impl(input: &str, path: Option<&Path>, config: &FormatConfig) -> UiuaResult<String> {
    let (items, errors) = parse(input, path);
    if errors.is_empty() {
        Ok(format_items(&items, config))
    } else {
        Err(errors.into())
    }
}

pub fn format_file<P: AsRef<Path>>(path: P, config: &FormatConfig) -> UiuaResult<String> {
    let path = path.as_ref();
    let input =
        fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e.into()))?;
    let formatted = format(&input, path, config)?;
    if formatted == input {
        return Ok(formatted);
    }
    let dont_write = env::var("UIUA_NO_FORMAT").is_ok_and(|val| val == "1");
    if !dont_write {
        fs::write(path, &formatted).map_err(|e| UiuaError::Format(path.to_path_buf(), e.into()))?;
    }
    Ok(formatted)
}

struct Formatter<'a> {
    config: &'a FormatConfig,
    output: String,
}

impl<'a> Formatter<'a> {
    fn format_items(&mut self, items: &[Item]) {
        for item in items {
            self.format_item(item);
            self.output.push('\n');
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
                self.output.push_str(" ← ");
                if let Some(sig) = &binding.signature {
                    self.format_signature(sig.value);
                }
                self.format_words(&binding.words, true, 0);
            }
            Item::ExtraNewlines(_) => {}
        }
    }
    fn format_signature(&mut self, sig: Signature) {
        self.output.push('|');
        self.output.push_str(&sig.args.to_string());
        if sig.outputs != 1 {
            self.output.push('.');
            self.output.push_str(&sig.outputs.to_string());
        }
        self.output.push(' ');
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
                    self.format_signature(sig.value);
                }
                self.format_multiline_words(&func.lines, false, depth + 1);
                self.output.push(')');
            }
            Word::Primitive(prim) => {
                self.output.push_str(&prim.to_string());
                if prim.is_modifier() {
                    self.output.push('^');
                }
            }
            Word::Modified(m) => {
                self.output.push_str(&m.modifier.value.to_string());
                self.format_words(&m.operands, true, depth);
                if m.terminated {
                    self.output.push('^');
                }
            }
            Word::Spaces => self.output.push(' '),
            Word::Comment(comment) => {
                self.output.push('#');
                if self.config.comment_space_after_hash {
                    self.output.push(' ');
                }
                self.output.push_str(comment);
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
        if lines.len() == 1
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
            && self.config.compact_multiline.unwrap_or_else(|| {
                start_line_pos <= self.config.multiline_compact_threshold
                    || curr_line.starts_with(' ')
            })
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
