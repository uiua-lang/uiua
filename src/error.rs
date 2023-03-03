use std::{error::Error, fmt, io, path::PathBuf};

use crate::{compile::CompileError, lex::Sp};

#[derive(Debug)]
pub enum UiuaError {
    Load(PathBuf, io::Error),
    Compile(Vec<Sp<CompileError>>),
    Run(Sp<String>),
    NotEnoughArgsOnEntry(usize, usize),
}

impl fmt::Display for UiuaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiuaError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            UiuaError::Compile(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaError::Run(e) => write!(f, "{e}"),
            UiuaError::NotEnoughArgsOnEntry(expected, got) => {
                write!(
                    f,
                    "note enough arguments passed to entry function: \
                    expected {expected} args, got {got}"
                )
            }
        }
    }
}

impl From<Sp<String>> for UiuaError {
    fn from(value: Sp<String>) -> Self {
        Self::Run(value)
    }
}

impl From<Vec<Sp<CompileError>>> for UiuaError {
    fn from(errors: Vec<Sp<CompileError>>) -> Self {
        Self::Compile(errors)
    }
}

impl Error for UiuaError {}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;
