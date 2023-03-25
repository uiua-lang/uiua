use std::{fs, iter::once, path::Path};

use crate::{ast::*, ops::Primitive, parse::parse, UiuaError, UiuaResult};

pub fn format_items(items: &[Item]) -> String {
    let mut output = String::new();
    for item in items {
        let mut line = String::new();
        match item {
            Item::Words(w) => {
                for node in words(w.iter().map(|w| &w.value).by_ref()) {
                    reduce(&mut line, &node);
                }
            }
            Item::Binding(binding) => {
                line.push_str(&binding.name.value.0);
                line.push_str(" = ");
                for node in words(binding.words.iter().map(|w| &w.value).by_ref()) {
                    reduce(&mut line, &node);
                }
            }
            Item::Comment(comment) => {
                line.push_str("# ");
                line.push_str(comment);
            }
            Item::Newlines => line.push('\n'),
        }
        output.push_str(&line);
        output.push('\n');
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

#[derive(Debug)]
enum FormatNode {
    Unit(String),
    Call(String, Vec<FormatNode>),
    Strand(Vec<FormatNode>),
    Delim(char, char, Vec<FormatNode>),
}

fn is_literal_char(c: char) -> bool {
    c.is_alphabetic() && c != 'ⁿ' || c.is_ascii_digit() || "\"'".contains(c)
}

fn space_between(a: char, b: char) -> bool {
    if a == ' ' || ".,])}".contains(b) {
        return false;
    }
    is_literal_char(a) || (a.is_ascii_digit() && (b.is_alphabetic() || b.is_ascii_digit()))
}

fn space(output: &mut String, s: &str) {
    if let Some(c) = output.chars().last() {
        if c == ' ' && s == " " {
            return;
        }
        if space_between(c, s.chars().next().unwrap()) {
            output.push(' ');
        }
    }
    if s.starts_with("])}") {
        while output.ends_with(' ') {
            output.pop();
        }
    }
    output.push_str(s);
}

fn reduce(output: &mut String, node: &FormatNode) {
    match node {
        FormatNode::Unit(s) => space(output, s),
        FormatNode::Call(f, args) => {
            space(output, f);
            for arg in args {
                reduce(output, arg);
            }
        }
        FormatNode::Strand(items) => {
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    output.push('_');
                }
                reduce(output, item);
            }
            output.push(' ');
        }
        FormatNode::Delim(start, end, items) => {
            space(output, &start.to_string());
            for item in items {
                reduce(output, item);
            }
            space(output, &end.to_string());
        }
    }
}

fn word_node(iter: &mut dyn Iterator<Item = &Word>) -> Option<FormatNode> {
    let word = iter.next()?;
    Some(match word {
        Word::Number(n) => FormatNode::Unit({
            if let Some(n) = n.strip_prefix('-') {
                format!("¯{}", n)
            } else {
                n.to_string()
            }
        }),
        Word::Char(c) => FormatNode::Unit(format!("'{c}'")),
        Word::String(s) => FormatNode::Unit(format!("{s:?}")),
        Word::Ident(ident) => {
            if !ident.is_capitalized() {
                if let Some(prim) = Primitive::from_name(ident.as_str()) {
                    if prim.ascii().is_some() || prim.unicode().is_some() {
                        if let Some(args) = prim.args() {
                            let mut arg_nodes = Vec::new();
                            for _ in 0..args {
                                arg_nodes.extend(word_node(iter));
                            }
                            return Some(FormatNode::Call(prim.to_string(), arg_nodes));
                        }
                    }
                }
            }
            FormatNode::Unit(ident.to_string())
        }
        Word::Strand(items) => {
            let mut nodes = Vec::new();
            for item in items {
                nodes.extend(word_node(&mut once(&item.value)));
            }
            FormatNode::Strand(nodes)
        }
        Word::Array(items) => {
            FormatNode::Delim('[', ']', words(items.iter().map(|i| &i.value).by_ref()))
        }
        Word::Func(func) => {
            FormatNode::Delim('(', ')', words(func.body.iter().map(|i| &i.value).by_ref()))
        }
        Word::RefFunc(rfunc) => FormatNode::Delim(
            '{',
            '}',
            words(rfunc.body.iter().map(|i| &i.value).by_ref()),
        ),
        Word::Primitive(prim) => {
            if prim.ascii().is_some() || prim.unicode().is_some() {
                if let Some(args) = prim.args() {
                    let mut arg_nodes = Vec::new();
                    for _ in 0..args {
                        arg_nodes.extend(word_node(iter));
                    }
                    return Some(FormatNode::Call(prim.to_string(), arg_nodes));
                }
            }
            FormatNode::Unit(prim.to_string())
        }
        Word::Modified(m) => FormatNode::Call(
            m.modifier.value.to_string(),
            words(m.words.iter().map(|i| &i.value).by_ref()),
        ),
    })
}

fn words(iter: &mut dyn Iterator<Item = &Word>) -> Vec<FormatNode> {
    let mut nodes = Vec::new();
    while let Some(word) = word_node(iter) {
        nodes.push(word);
    }
    nodes
}
