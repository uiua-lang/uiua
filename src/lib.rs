pub mod ast;
pub mod builtin;
pub mod lex;
pub mod list;
pub mod parse;
pub mod transpile;
pub mod types;

use std::{
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
};

use mlua::Lua;

use lex::Sp;
use transpile::{TranspileError, Transpiler};

#[derive(Debug)]
pub enum RuntimeError {
    Load(PathBuf, io::Error),
    Transpile(Vec<Sp<TranspileError>>),
    NoMain,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            RuntimeError::Transpile(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            RuntimeError::NoMain => write!(f, "no main function found"),
        }
    }
}

impl From<Vec<Sp<TranspileError>>> for RuntimeError {
    fn from(errors: Vec<Sp<TranspileError>>) -> Self {
        Self::Transpile(errors)
    }
}

impl Error for RuntimeError {}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

pub struct Runtime {
    pub lua: Lua,
    pub transpiler: Transpiler,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Self {
            lua: Lua::new(),
            transpiler: Transpiler::new(),
        };
        rt.initialize_builtins();
        rt
    }
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult {
        let path = path.as_ref();
        let input =
            fs::read_to_string(path).map_err(|e| RuntimeError::Load(path.to_path_buf(), e))?;
        self.transpiler.transpile(&input, path)?;
        Ok(())
    }
    pub fn lua_code(&self) -> &str {
        &self.transpiler.code
    }
    pub fn run(&mut self) -> RuntimeResult {
        self.lua
            .load(&self.transpiler.code)
            .exec()
            .unwrap_or_else(|e| {
                eprintln!("{e}");
            });
        if let Ok(main) = self.lua.globals().get::<_, mlua::Function>("main") {
            main.call::<_, ()>(()).unwrap_or_else(|e| {
                eprintln!("{e}");
            });
            Ok(())
        } else {
            Err(RuntimeError::NoMain)
        }
    }
}
