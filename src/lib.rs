/*!
The Uiua programming language

This currently exists as a library only to reserve the name on crates.io.
The current API should be considered deeply unstable.
*/

#![allow(clippy::single_match, clippy::needless_range_loop)]

mod algorithm;
pub mod array;
pub mod ast;
mod check;
mod cowslice;
mod error;
pub mod format;
pub mod function;
mod grid_fmt;
pub mod lex;
pub mod lsp;
pub mod parse;
pub mod primitive;
#[doc(hidden)]
pub mod profile;
pub mod run;
mod sys;
pub mod value;

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

pub use {error::*, run::Uiua, sys::*};

#[derive(Debug, Clone)]
pub struct Ident(Arc<str>);

impl Ident {
    pub fn as_str(&self) -> &str {
        &self.0
    }
    fn lower_chars(&self) -> impl Iterator<Item = char> + '_ {
        lower_chars(&self.0)
    }
}

fn lower_chars(s: &str) -> impl Iterator<Item = char> + '_ {
    s.chars().flat_map(|c| c.to_lowercase())
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> From<&'a str> for Ident {
    fn from(s: &'a str) -> Self {
        Ident(Arc::from(s))
    }
}

impl From<String> for Ident {
    fn from(s: String) -> Self {
        Ident(Arc::from(s))
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.lower_chars().eq(other.lower_chars())
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.lower_chars().partial_cmp(other.lower_chars())
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        self.lower_chars().cmp(other.lower_chars())
    }
}

impl Hash for Ident {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lower_chars().for_each(|c| c.hash(state))
    }
}

impl PartialEq<str> for Ident {
    fn eq(&self, other: &str) -> bool {
        self.lower_chars().eq(lower_chars(other))
    }
}

impl<'a> PartialEq<&'a str> for Ident {
    fn eq(&self, other: &&'a str) -> bool {
        self.lower_chars().eq(lower_chars(other))
    }
}

#[test]
fn suite() {
    for entry in std::fs::read_dir("tests").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|s| s == "ua") {
            if let Err(e) = Uiua::with_native_sys().load_file(&path) {
                panic!("Test failed in {}:\n{}", path.display(), e.show(true));
            }
        }
    }
}

#[test]
fn site() {
    let mut threads = Vec::new();
    for entry in std::fs::read_dir("site/src").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        for line in std::fs::read_to_string(&path).unwrap().lines() {
            if let Some(code) = line
                .trim()
                .strip_prefix(r#"<Editor example=""#)
                .and_then(|line| line.strip_suffix(r#""/>"#))
            {
                let code = code
                    .replace("\\\"", "\"")
                    .replace("\\\\", "\\")
                    .replace("\\n", "\n");
                if code.contains(crate::sys::SysOp::AudioPlay.name()) {
                    continue;
                }
                threads.push((
                    path.to_path_buf(),
                    code.clone(),
                    std::thread::spawn(move || Uiua::with_native_sys().load_str(&code).map(drop)),
                ));
            }
        }
    }
    assert!(threads.len() > 50);
    for (path, code, thread) in threads {
        if let Err(e) = thread.join().unwrap() {
            panic!(
                "Test failed in {}\n{}\n{}",
                path.display(),
                code,
                e.show(true)
            );
        }
    }
}
