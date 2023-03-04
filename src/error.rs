use std::{error::Error, fmt, io, path::PathBuf};

use crate::{
    ast::FunctionId,
    compile::CompileError,
    lex::{Sp, Span},
};

#[derive(Debug)]
pub enum UiuaError {
    Load(PathBuf, io::Error),
    Compile(Vec<Sp<CompileError>>),
    Run {
        error: Box<RuntimeError>,
        trace: Vec<TraceFrame>,
    },
}

#[derive(Debug, Clone)]
pub struct TraceFrame {
    pub id: FunctionId,
    pub span: Span,
}

pub type RuntimeError = Sp<String>;
pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

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
            UiuaError::Run { error, trace } => {
                writeln!(f, "{error}")?;
                for frame in trace {
                    writeln!(f, "  in {} at {}", frame.id, frame.span)?;
                }
                Ok(())
            }
        }
    }
}

impl From<Vec<Sp<CompileError>>> for UiuaError {
    fn from(errors: Vec<Sp<CompileError>>) -> Self {
        Self::Compile(errors)
    }
}

impl Error for UiuaError {}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;
