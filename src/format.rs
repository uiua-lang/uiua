use std::{fs, path::Path};

use crate::{ast::*, lex::Sp, parse::parse, UiuaError, UiuaResult};

pub fn format_items(items: &[Item]) -> String {
    let mut output = String::new();
    for item in items {
        format_item(&mut output, item);
    }
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

fn format_item(output: &mut String, item: &Item) {
    match item {
        Item::Scoped { items, test } => {
            let delim = if *test { "~~~" } else { "---" };
            output.push_str(delim);
            output.push('\n');
            for item in items {
                format_item(output, item);
            }
            output.push_str(delim);
        }
        Item::Words(w, comment) => {
            format_words(output, w);
            if let Some(comment) = comment {
                output.push_str("  # ");
                output.push_str(comment);
            }
        }
        Item::Binding(binding, comment) => {
            output.push_str(&binding.name.value.0);
            output.push_str(" ← ");
            format_words(output, &binding.words);
            if let Some(comment) = comment {
                output.push_str("  # ");
                output.push_str(comment);
            }
        }
        Item::Comment(comment) => {
            output.push_str("# ");
            output.push_str(comment);
        }
        Item::Newlines => {}
    }
    output.push('\n');
}

fn format_words(output: &mut String, words: &[Sp<Word>]) {
    for word in words {
        format_word(output, &word.value);
    }
}

fn format_word(output: &mut String, word: &Word) {
    match word {
        Word::Number(n) => {
            if let Some(n) = n.strip_prefix('-') {
                output.push('¯');
                output.push_str(n);
            } else {
                output.push_str(n);
            }
        }
        Word::Char(c) => output.push_str(&format!("{:?}", c)),
        Word::String(s) => output.push_str(&format!("{:?}", s)),
        Word::Ident(ident) => output.push_str(&ident.0),
        Word::Strand(items) => {
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    output.push('_');
                }
                format_word(output, &item.value);
            }
        }
        Word::Array(items) => {
            output.push('[');
            format_words(output, items);
            output.push(']');
        }
        Word::Func(func) => {
            output.push('(');
            format_words(output, &func.body);
            output.push(')');
        }
        Word::Dfn(dfn) => {
            output.push('{');
            format_words(output, &dfn.body);
            output.push('}');
        }
        Word::Primitive(prim) => output.push_str(&prim.to_string()),
        Word::Modified(m) => {
            output.push_str(&m.modifier.value.to_string());
            format_words(output, &m.words);
        }
        Word::Spaces => output.push(' '),
    }
}
