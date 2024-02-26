/*!
The Uiua programming language

This is the crate so you can use Uiua as a Rust library. If you just want to write programs in Uiua, you can check out [uiua.org](https://uiua.org) or the [GitHub repo](https://github.com/uiua-lang/uiua).

# Usage

The `uiua` crate is set up primarily to be installed as a binary. For this reason, when using it as a library, you'll likely want to disable default features.

This disables some of the features that many users are used to having, so to re-enable things like regex and media encoding, you can enable the `batteries` feature.
```toml
# Cargo.toml
[dependencies]
uiua = { version = "*", default-features = false, features = ["batteries"] }
```

The main entry point is the [`Uiua`] struct, which is the Uiua runtime. It must be created with a [`SysBackend`]. [`Uiua::with_native_sys`] is a convenient way to create a Uiua runtime that uses the same backend as the Uiua CLI, though keep in mind it gives full access to the filesystem and TCP sockets and so probably shouldn't be used in a sandboxed environment.

[`Value`] is the generic value type. It wraps one of four [`Array`] types.

You can run Uiua code with [`Uiua::run_str`] or [`Uiua::run_file`].
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.run_str("&p + 1 2").unwrap();
```
You can push values onto the stack with [`Uiua::push`]. When you're done, you can get the results with [`Uiua::pop`], [`Uiua::take_stack`], or one of numerous pop+conversion convenience functions.
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.push(1);
uiua.push(2);
uiua.run_str("+").unwrap();
let res = uiua.pop_int().unwrap();
assert_eq!(res, 3);
```

Sometimes, you need to configure the compiler before running.

You can create a new compiler with [`Compiler::new`]. Strings or files can be compiled with [`Compiler::load_str`] or [`Compiler::load_file`] respectively.
You can get the compiled assembly with [`Compiler::finish`] and run it with [`Uiua::run_asm`].
```rust
use uiua::*;

let mut comp = Compiler::new();
comp.print_diagnostics(true);
let asm = comp.load_str("+ 3 5").unwrap().finish();

let mut uiua = Uiua::with_native_sys();
uiua.run_asm(&asm).unwrap();
let res = uiua.pop_int().unwrap();
assert_eq!(res, 8);
```

This can be shortened a bit with [`Uiua::compile_run`].
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.compile_run(|comp| {
    comp.print_diagnostics(true).load_str("+ 3 5")
});
```

You can create and bind Rust functions with [`Compiler::create_function`], [`Compiler::bind_function`], and [`Compiler::create_bind_function`]
```rust
use uiua::*;

let mut comp = Compiler::new();
comp.create_bind_function("MyAdd", (2, 1), |uiua| {
    let a = uiua.pop_num()?;
    let b = uiua.pop_num()?;
    uiua.push(a + b);
    Ok(())
}).unwrap();
comp.load_str("MyAdd 2 3").unwrap();
let asm = comp.finish();

let mut uiua = Uiua::with_native_sys();
uiua.run_asm(asm).unwrap();
let res = uiua.pop_num().unwrap();
assert_eq!(res, 5.0);
```

Bindings can be retrieved with [`Uiua::bound_values`] or [`Uiua::bound_functions`].
```rust
use uiua::*;

let mut uiua = Uiua::with_native_sys();
uiua.run_str("
    X ← 5
    F ← +1
").unwrap();

let x = uiua.bound_values().remove("X").unwrap();
assert_eq!(x.as_int(&uiua, "").unwrap(), 5);

let f = uiua.bound_functions().remove("F").unwrap();
let mut comp = Compiler::new().with_assembly(uiua.take_asm());
comp.create_bind_function("AddTwo", (1, 1), move |uiua| {
    uiua.call(f.clone())?;
    uiua.call(f.clone())
}).unwrap();
comp.load_str("AddTwo 3").unwrap();
uiua.run_asm(comp.finish()).unwrap();
let res = uiua.pop_int().unwrap();
assert_eq!(res, 5);
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

The `uiua` crate has the following noteable feature flags:
- `batteries`: Enables the following features:
    - `regex`: Enables the `regex` function
    - `image`: Enables image encoding and decoding
    - `gif`: Enables GIF encoding and decoding
    - `audio_encode`: Enables audio encoding and decoding
- `bytes`: Enables a byte array type. This type is semantically equivalent to a numeric array, but takes up less space. It is returned by certain file and network functions, as well as some comparison functions.
- `native_sys`: Enables the [`NativeSys`] backend. This is the default backend used by the interpreter.
- `audio`: Enables audio features in the [`NativeSys`] backend.
- `https`: Enables the `&httpsw` system function
- `invoke`: Enables the `&invk` system function
- `trash`: Enables the `&ftr` system function
- `raw_mode`: Enables the `&raw` system function
*/

#![allow(clippy::single_match, clippy::needless_range_loop)]
#![warn(missing_docs)]

mod algorithm;
mod array;
mod assembly;
pub mod ast;
mod boxed;
mod check;
mod compile;
mod complex;
mod cowslice;
mod error;
mod ffi;
pub mod format;
mod function;
mod grid_fmt;
mod lex;
pub mod lsp;
mod optimize;
mod parse;
mod primitive;
#[doc(hidden)]
pub mod profile;
mod run;
mod shape;
#[cfg(feature = "stand")]
#[doc(hidden)]
pub mod stand;
mod sys;
#[cfg(feature = "native_sys")]
mod sys_native;
mod value;

#[allow(unused_imports)]
pub use self::{
    array::*,
    assembly::*,
    boxed::*,
    compile::*,
    error::*,
    ffi::*,
    function::*,
    lex::is_ident_char,
    lex::*,
    lsp::{spans, SpanKind},
    parse::{ident_modifier_args, parse, ParseError},
    primitive::*,
    run::*,
    shape::*,
    sys::*,
    value::*,
};

#[cfg(feature = "native_sys")]
pub use self::sys_native::*;

pub use complex::*;
use ecow::EcoString;

/// The Uiua version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// A Uiua identifier
pub type Ident = EcoString;

#[cfg(test)]
mod tests {
    use std::path::*;
    fn test_files(filter: impl Fn(&Path) -> bool) -> impl Iterator<Item = PathBuf> {
        std::fs::read_dir("tests")
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(move |path| {
                path.is_file() && path.extension().is_some_and(|s| s == "ua") && filter(path)
            })
    }

    #[test]
    #[cfg(feature = "native_sys")]
    fn suite() {
        use super::*;
        for path in test_files(|path| {
            !(path.file_stem().unwrap())
                .to_string_lossy()
                .contains("error")
        }) {
            let code = std::fs::read_to_string(&path).unwrap();
            let mut env = Uiua::with_native_sys();
            let mut comp = Compiler::new();
            if let Err(e) = comp
                .load_str_src(&code, &path)
                .and_then(|comp| env.run_asm(&comp.finish()))
            {
                panic!("Test failed in {}:\n{}", path.display(), e.report());
            }
            if let Some(diag) = comp
                .take_diagnostics()
                .into_iter()
                .find(|d| d.kind < DiagnosticKind::Advice)
            {
                panic!("Test failed in {}:\n{}", path.display(), diag.report());
            }
            if env.run_str("◌").is_ok() {
                panic!("{} had a non-empty stack", path.display());
            }
        }
    }

    #[test]
    #[cfg(feature = "native_sys")]
    fn errors() {
        use super::*;
        for path in test_files(|path| {
            (path.file_stem().unwrap())
                .to_string_lossy()
                .contains("error")
        }) {
            let mut code = std::fs::read_to_string(&path).unwrap();
            if code.contains('\r') {
                code = code.replace('\r', "");
            }
            for section in code.split("\n\n") {
                let mut env = Uiua::with_native_sys();
                let mut comp = Compiler::new();
                let res = comp
                    .load_str_src(section, &path)
                    .and_then(|comp| env.run_asm(&comp.finish()));
                if res.is_ok() {
                    panic!(
                        "Test succeeded when it should have failed in {}:\n{}",
                        path.display(),
                        section
                    );
                }
            }
        }
    }

    #[test]
    fn lsp_spans() {
        use super::*;
        for path in test_files(|_| true) {
            let code = std::fs::read_to_string(&path).unwrap();
            spans(&code);
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
                if path.canonicalize().unwrap()
                    == std::path::Path::new(file!()).canonicalize().unwrap()
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
}
