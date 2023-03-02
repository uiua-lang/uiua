pub mod ast;
pub mod builtin;
pub mod check;
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

use check::{CheckError, Checker};

use lex::Sp;

#[derive(Debug)]
pub enum RuntimeError {
    Load(PathBuf, io::Error),
    Check(Vec<Sp<CheckError>>),
    NoMain,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            RuntimeError::Check(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            RuntimeError::NoMain => write!(f, "no main function found"),
        }
    }
}

impl From<Vec<Sp<CheckError>>> for RuntimeError {
    fn from(errors: Vec<Sp<CheckError>>) -> Self {
        Self::Check(errors)
    }
}

impl Error for RuntimeError {}

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

pub struct Runtime {
    pub checker: Checker,
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

impl Runtime {
    pub fn new() -> Self {
        let mut rt = Self {
            checker: Checker::default(),
        };
        rt.initialize_builtins();
        rt
    }
    pub fn load_file<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult {
        let path = path.as_ref();
        let input =
            fs::read_to_string(path).map_err(|e| RuntimeError::Load(path.to_path_buf(), e))?;
        self.checker.load(&input, path)?;
        Ok(())
    }
}
