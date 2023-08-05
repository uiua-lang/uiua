//! Functions for formatting Uiua code.

use std::{fs, path::Path};

use crate::{ast::*, lex::Sp, parse::parse, UiuaError, UiuaResult};

pub fn format_items(items: &[Item]) -> String {
    let mut output = String::new();
    format_items_impl(&mut output, items);
    output
}

pub fn format<P: AsRef<Path>>(input: &str, path: P) -> UiuaResult<String> {
    format_impl(input, Some(path.as_ref()))
}
pub fn format_str(input: &str) -> UiuaResult<String> {
    format_impl(input, None)
}

fn format_impl(input: &str, path: Option<&Path>) -> UiuaResult<String> {
    let (items, errors) = parse(input, path);
    if errors.is_empty() {
        Ok(format_items(&items))
        // Ok(input.into())
    } else {
        Err(errors.into())
    }
}

pub fn format_file<P: AsRef<Path>>(path: P) -> UiuaResult<String> {
    let path = path.as_ref();
    let input = fs::read_to_string(path).map_err(|e| UiuaError::Load(path.to_path_buf(), e))?;
    let formatted = format(&input, path)?;
    if formatted == input {
        return Ok(formatted);
    }
    fs::write(path, &formatted).map_err(|e| UiuaError::Format(path.to_path_buf(), e))?;
    Ok(formatted)
}

fn format_items_impl(output: &mut String, items: &[Item]) {
    for item in items {
        format_item(output, item);
        output.push('\n');
    }
}

fn format_item(output: &mut String, item: &Item) {
    match item {
        Item::Scoped { items, test } => {
            let delim = if *test { "~~~" } else { "---" };
            output.push_str(delim);
            output.push('\n');
            format_items_impl(output, items);
            output.push_str(delim);
        }
        Item::Words(w) => {
            format_words(output, w);
        }
        Item::Binding(binding) => {
            output.push_str(&binding.name.value.0);
            output.push_str(" ← ");
            format_words(output, &binding.words);
        }
        Item::Newlines => {}
    }
}

fn format_words(output: &mut String, words: &[Sp<Word>]) {
    for word in trim_spaces(words) {
        format_word(output, word);
    }
}

fn format_word(output: &mut String, word: &Sp<Word>) {
    match &word.value {
        Word::Number(s, _) => output.push_str(&s.replace(['-', '`'], "¯")),
        Word::Char(c) => output.push_str(&format!("{:?}", c)),
        Word::String(s) => output.push_str(&format!("{:?}", s)),
        Word::FormatString(_) => output.push_str(word.span.as_str()),
        Word::MultilineString(lines) => {
            let curr_line_pos = if output.ends_with('\n') {
                0
            } else {
                output.lines().last().unwrap_or_default().chars().count()
            };
            for (i, line) in lines.iter().enumerate() {
                if i > 0 {
                    output.push('\n');
                    for _ in 0..curr_line_pos {
                        output.push(' ');
                    }
                }
                output.push_str(line.span.as_str());
            }
        }
        Word::Ident(ident) => output.push_str(&ident.0),
        Word::Strand(items) => {
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    output.push('_');
                }
                format_word(output, item);
            }
        }
        Word::Array(items) => {
            output.push('[');
            format_multiline_words(output, items);
            output.push(']');
        }
        Word::Func(func) => {
            output.push('(');
            format_multiline_words(output, &func.body);
            output.push(')');
        }
        Word::Dfn(dfn) => {
            output.push('{');
            format_multiline_words(output, &dfn.body);
            output.push('}');
        }
        Word::Primitive(prim) => output.push_str(&prim.to_string()),
        Word::Modified(m) => {
            output.push_str(&m.modifier.value.to_string());
            format_words(output, &m.words);
        }
        Word::Spaces => output.push(' '),
        Word::Comment(comment) => {
            output.push_str("# ");
            output.push_str(comment);
        }
    }
}

fn format_multiline_words(output: &mut String, lines: &[Vec<Sp<Word>>]) {
    if lines.len() <= 1 {
        format_words(output, &lines[0]);
    } else {
        let curr_line = output.lines().last().unwrap_or_default();
        let curr_line_pos = if output.ends_with('\n') {
            0
        } else {
            curr_line.chars().count()
        };
        let compact = curr_line_pos <= 10 || curr_line.starts_with(' ');
        let indent = if compact { curr_line_pos } else { 2 };
        for (i, line) in lines.iter().enumerate() {
            if i > 0 || !compact {
                output.push('\n');
                if !line.is_empty() {
                    for _ in 0..indent {
                        output.push(' ');
                    }
                }
            }
            format_words(output, line);
        }
        if !compact {
            output.push('\n');
        }
    }
}

fn trim_spaces(words: &[Sp<Word>]) -> &[Sp<Word>] {
    let mut start = 0;
    for word in words {
        if let Word::Spaces = word.value {
            start += 1;
        } else {
            break;
        }
    }
    let mut end = words.len();
    for word in words.iter().rev() {
        if let Word::Spaces = word.value {
            end -= 1;
        } else {
            break;
        }
    }
    if start >= end {
        return &[];
    }
    &words[start..end]
}
