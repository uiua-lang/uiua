use std::{
    fmt::{self, Write},
    fs,
    path::Path,
};

use crate::{
    ast::*,
    lex::{is_basically_alphabetic, is_basically_alphanumeric},
    ops::Primitive,
    parse::parse,
    UiuaError, UiuaResult,
};

pub fn format_items(items: &[Item]) -> String {
    let mut output = String::new();
    for item in items {
        let mut line = String::new();
        match item {
            Item::Words(w) => {
                for node in words(w.iter().map(|w| &w.value)) {
                    reduce(&mut line, &node);
                }
            }
        }
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

fn space_between(a: char, b: char) -> bool {
    a.is_alphabetic() && b.is_alphabetic() || a.is_ascii_digit() && b.is_ascii_digit()
}

fn formatted(output: &mut String, s: &str) {
    if let Some(c) = output.chars().last() {
        if space_between(c, s.chars().next().unwrap()) {
            output.push(' ');
        }
    }
    output.push_str(s);
}

fn reduce(output: &mut String, node: &FormatNode) {
    match node {
        FormatNode::Unit(s) => formatted(output, s),
        FormatNode::Call(f, args) => {
            formatted(output, f);
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    formatted(output, " ");
                }
                reduce(output, arg);
            }
        }
        FormatNode::Strand(items) => {
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    formatted(output, "_");
                }
                reduce(output, item);
            }
        }
        FormatNode::Delim(start, end, items) => {
            formatted(output, &start.to_string());
            for item in items {
                reduce(output, item);
            }
            formatted(output, &end.to_string());
        }
    }
}

fn words<'a, I: Iterator<Item = &'a Word>>(mut iter: I) -> Vec<FormatNode> {
    let mut nodes = Vec::new();
    while let Some(word) = iter.next() {
        match word {
            Word::Number(n) => nodes.push(FormatNode::Unit(n.to_string())),
            Word::Char(c) => nodes.push(FormatNode::Unit(format!("'{c}'"))),
            Word::String(s) => nodes.push(FormatNode::Unit(format!("{s:?}"))),
            Word::Ident(ident) => {
                if !ident.is_capitalized() {
                    if let Some(prim) = Primitive::from_name(ident.as_str()) {
                        if prim.ascii().is_some() || prim.unicode().is_some() {
                            if let Some(args) = prim.args() {
                                let args = words(iter.by_ref().take(args as usize));
                                nodes.push(FormatNode::Call(prim.to_string(), args));
                                continue;
                            }
                        }
                    }
                }
                nodes.push(FormatNode::Unit(ident.to_string()));
            }
            Word::Strand(items) => {
                nodes.push(FormatNode::Strand(words(items.iter().map(|i| &i.value))))
            }
            Word::Array(items) => nodes.push(FormatNode::Delim(
                '[',
                ']',
                words(items.iter().map(|i| &i.value)),
            )),
            Word::Func(func) => nodes.push(FormatNode::Delim(
                '(',
                ')',
                words(func.body.iter().map(|i| &i.value)),
            )),
            Word::RefFunc(rfunc) => nodes.push(FormatNode::Delim(
                '{',
                '}',
                words(rfunc.body.iter().map(|i| &i.value)),
            )),
            Word::Primitive(prim) => {
                if prim.ascii().is_some() || prim.unicode().is_some() {
                    if let Some(args) = prim.args() {
                        let args = words(iter.by_ref().take(args as usize));
                        nodes.push(FormatNode::Call(prim.to_string(), args));
                        continue;
                    }
                }
                nodes.push(FormatNode::Unit(prim.to_string()));
            }
            Word::Modified(m) => nodes.push(FormatNode::Call(
                m.modifier.to_string(),
                words(m.words.iter().map(|i| &i.value)),
            )),
        }
    }
    nodes
}

struct FormatState {
    pub string: String,
    was_strand: bool,
    was_primitive: bool,
    override_space: bool,
}

impl FormatState {
    fn push<T: fmt::Display>(&mut self, t: T) {
        self.was_strand = false;
        self.was_primitive = false;
        write!(&mut self.string, "{t}").unwrap();
    }
    fn space_if(&mut self, f: impl Fn(char) -> bool) {
        if self.override_space {
            self.override_space = false;
            return;
        }
        if self.was_strand || self.string.ends_with(f) {
            self.push(' ');
        }
    }
    fn space_if_alphanumeric(&mut self) {
        self.space_if(is_basically_alphanumeric);
    }
    fn space_if_alphabetic(&mut self) {
        self.space_if(is_basically_alphabetic);
    }
    fn prepare_for_constant(&mut self) -> bool {
        let val = self.was_primitive;
        self.was_primitive = false;
        val
    }
}

trait Format {
    fn format(&self, state: &mut FormatState);
}

impl Format for Item {
    fn format(&self, state: &mut FormatState) {
        match self {
            Item::Words(words) => {
                for word in words {
                    word.value.format(state);
                }
            }
            Item::Binding(l) => l.format(state),
            Item::Comment(comment) => {
                state.push("# ");
                state.push(comment);
            }
            Item::Newlines => {}
        }
        state.push('\n');
    }
}

impl Format for Binding {
    fn format(&self, state: &mut FormatState) {
        state.push(&self.name.value);
        state.push(" = ");
        for word in &self.words {
            word.value.format(state);
        }
    }
}

impl Format for Word {
    fn format(&self, state: &mut FormatState) {
        match self {
            Word::Number(f) => {
                let space = state.prepare_for_constant();
                state.space_if_alphanumeric();
                state.space_if(|c| c == '¯');
                if let Some(f) = f.strip_prefix('-') {
                    state.push('¯');
                    state.push(f);
                } else {
                    state.push(f);
                }
                if space {
                    state.push(' ');
                }
            }
            Word::Char(c) => {
                let space = state.prepare_for_constant();
                state.space_if_alphanumeric();
                state.push(&format!("{c:?}"));
                if space {
                    state.push(' ');
                }
            }
            Word::String(s) => {
                let space = state.prepare_for_constant();
                state.space_if_alphanumeric();
                if state.string.ends_with('"') {
                    state.push(' ');
                }
                state.push('"');
                for c in s.chars() {
                    state.push(c);
                }
                state.push('"');
                if space {
                    state.push(' ');
                }
            }
            Word::Ident(ident) => {
                if !ident.is_capitalized() {
                    if let Some(prim) = Primitive::from_name(ident.as_str()) {
                        if prim.ascii().is_some() || prim.unicode().is_some() {
                            return prim.format(state);
                        }
                    }
                }
                state.space_if_alphabetic();
                state.push(ident);
            }
            Word::Strand(items) => {
                state.was_primitive = false;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        state.push('_');
                    }
                    item.value.format(state);
                }
                state.was_strand = true;
            }
            Word::Array(items) => {
                state.push('[');
                for item in items {
                    item.value.format(state);
                }
                state.push(']');
            }
            Word::Func(f) => {
                state.push('(');
                for (i, word) in f.body.iter().enumerate() {
                    if i == f.body.len() - 1 {
                        state.was_primitive = false;
                    }
                    word.value.format(state);
                }
                state.push(')');
            }
            Word::RefFunc(f) => {
                state.push('{');
                for (i, word) in f.body.iter().enumerate() {
                    if i == f.body.len() - 1 {
                        state.was_primitive = false;
                    }
                    word.value.format(state);
                }
                state.push('}');
            }
            Word::Primitive(prim) => prim.format(state),
            Word::Modified(m) => {
                m.modifier.value.format(state);
                state.override_space = true;
                for word in &m.words {
                    word.value.format(state);
                }
            }
        }
    }
}

impl Format for Primitive {
    fn format(&self, state: &mut FormatState) {
        match self {
            Primitive::Dup => {
                state.space_if(|c: char| c.is_ascii_digit());
                state.push(self);
                return;
            }
            Primitive::Flip => {
                state.push(self);
                return;
            }
            _ => {}
        }
        state.space_if_alphabetic();
        let s = self.to_string();
        if s.starts_with(is_basically_alphabetic) {
            state.space_if_alphanumeric();
        }
        state.push(s);
        state.was_primitive = true;
    }
}
