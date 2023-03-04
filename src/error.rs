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

#[derive(Debug, Clone, PartialEq, Eq)]
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
                write!(f, "Error at {}: {}", error.span, error.value)?;
                let last = TraceFrame {
                    id: FunctionId::Named("".into()),
                    span: Span::builtin(),
                };
                let mut last = &last;
                let mut repetitions = 1;
                let max_id_length = trace
                    .iter()
                    .map(|frame| frame.id.to_string().len())
                    .max()
                    .unwrap_or(0);
                let max_span_length = trace
                    .iter()
                    .map(|frame| frame.span.to_string().len())
                    .max()
                    .unwrap_or(0);
                for frame in trace {
                    if frame == last {
                        repetitions += 1;
                    } else {
                        if repetitions > 1 {
                            writeln!(f, " (x {repetitions})")?;
                            repetitions = 0;
                        } else {
                            writeln!(f)?;
                        }
                        write!(
                            f,
                            "  in {:max_id_length$} at {:max_span_length$}",
                            frame.id.to_string(),
                            frame.span,
                        )?;
                        last = frame;
                    }
                }
                if repetitions > 1 {
                    write!(f, " (x {repetitions})")?;
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
