/*!
The Uiua programming language

This is the crate so you can use Uiua as a Rust library. If you just want to write programs in Uiua, you can check out [uiua.org](https://uiua.org) or the [GitHub repo](https://github.com/uiua-lang/uiua).

# Usage

The main entry point is the [`Uiua`] struct, which is the Uiua runtime. It must be created with a [`SysBackend`]. [`Uiua::with_native_sys`] is a convenient way to create a Uiua runtime that uses the same backend as the Uiua CLI, though keep in mind it gives full access to the filesystem and TCP sockets and so probably shouldn't be used in a sandboxed environment.

[`Value`] is the generic value type. It wraps one of four [`Array`] types.

You can run code with [`Uiua::load_str`] or [`Uiua::load_file`].
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.load_str("&p + 1 2").unwrap();
```
You can push values onto the stack with [`Uiua::push`]. When you're done, you can get the results with [`Uiua::pop`], [`Uiua::take_stack`], or one of numerous pop+conversion convenience functions.
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.push(1);
uiua.push(2);
uiua.load_str("+").unwrap();
let res = uiua.pop_int().unwrap();
assert_eq!(res, 3);
```
You can create and bind Rust functions with [`Uiua::create_function`], [`Uiua::bind_function`], and [`Uiua::create_bind_function`]
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();

uiua.create_bind_function("MyAdd", (2, 1), |uiua| {
    let a = uiua.pop_num()?;
    let b = uiua.pop_num()?;
    uiua.push(a + b);
    Ok(())
}).unwrap();

uiua.load_str("MyAdd 2 3").unwrap();
let res = uiua.pop_num().unwrap();
assert_eq!(res, 5.0);
```
You can format Uiua code with the [`mod@format`] module.
```rust
use uiua::format::*;

let input = "resh3_4rang12";
let config = FormatConfig::default().with_trailing_newline(false);
let formatted = format_str(input, &config).unwrap().output;
assert_eq!(formatted, "↯3_4⇡12");
```

# Features

The `uiua` crate has the following feature flags:
- `bytes`: Enables a byte array type. This type is semantically equivalent to a numeric array, but takes up less space. It is returned by certain file and network functions, as well as some comparison functions.
- `audio`: Enables audio features in the [`NativeSys`] backend.
*/

#![allow(clippy::single_match, clippy::needless_range_loop)]
#![warn(missing_docs)]

mod algorithm;
mod array;
pub mod ast;
mod boxed;
mod check;
mod compile;
mod complex;
mod cowslice;
mod error;
pub mod format;
mod function;
mod grid_fmt;
mod lex;
mod lsp;
mod parse;
mod primitive;
#[doc(hidden)]
pub mod profile;
mod run;
#[cfg(feature = "stand")]
#[doc(hidden)]
pub mod stand;
mod sys;
mod sys_native;
mod value;

use std::sync::Arc;

pub use self::{
    array::*,
    boxed::*,
    error::*,
    function::*,
    lex::is_ident_char,
    lsp::*,
    lsp::{spans, SpanKind},
    parse::{parse, ParseError},
    primitive::*,
    run::*,
    sys::*,
    sys_native::*,
    value::*,
};
#[cfg(feature = "complex")]
pub use complex::*;
#[cfg(not(feature = "complex"))]
use complex::*;

/// A Uiua identifier
pub type Ident = Arc<str>;

#[test]
fn suite() {
    for entry in std::fs::read_dir("tests").unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|s| s == "ua") {
            let mut env = Uiua::with_native_sys();
            if let Err(e) = env.load_file(&path) {
                panic!("Test failed in {}:\n{}", path.display(), e.report());
            } else if let Some(diag) = env.take_diagnostics().into_iter().next() {
                panic!("Test failed in {}:\n{}", path.display(), diag.report());
            }
        }
    }
}

#[test]
fn no_dbgs() {
    fn recurse_dirs(dir: &std::path::Path, f: &impl Fn(&std::path::Path)) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.to_string_lossy().contains("target") {
                continue;
            }
            if path.is_dir() {
                recurse_dirs(&path, f);
            } else {
                f(&path);
            }
        }
    }
    recurse_dirs(std::path::Path::new("."), &|path| {
        if path.extension().is_some_and(|ext| ext == "rs") {
            if path.canonicalize().unwrap() == std::path::Path::new(file!()).canonicalize().unwrap()
            {
                return;
            }
            let contents = std::fs::read_to_string(path).unwrap();
            if contents.contains("dbg!") {
                panic!("File {} contains a dbg! macro", path.display());
            }
        }
    });
}
