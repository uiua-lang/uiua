use std::{error::Error, fmt, io, path::PathBuf};

use crate::{check::CheckError, lex::Sp};

#[derive(Debug)]
pub enum UiuaError {
    Load(PathBuf, io::Error),
    Check(Vec<Sp<CheckError>>),
    Run(Sp<String>),
}

impl fmt::Display for UiuaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiuaError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            UiuaError::Check(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaError::Run(e) => write!(f, "{e}"),
        }
    }
}

impl From<Sp<String>> for UiuaError {
    fn from(value: Sp<String>) -> Self {
        Self::Run(value)
    }
}

impl From<Vec<Sp<CheckError>>> for UiuaError {
    fn from(errors: Vec<Sp<CheckError>>) -> Self {
        Self::Check(errors)
    }
}

impl Error for UiuaError {}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;
