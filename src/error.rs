use std::{error::Error, fmt, io, path::PathBuf};

use crate::{
    compile::CompileError,
    function::FunctionId,
    lex::{Sp, Span},
    parse::ParseError,
};

#[derive(Debug)]
pub enum UiuaError {
    Load(PathBuf, io::Error),
    Format(PathBuf, io::Error),
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
            UiuaError::Format(path, e) => {
                write!(f, "failed to format {}: {e}", path.to_string_lossy())
            }
            UiuaError::Compile(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaError::Run { error, trace } => {
                match &error.span {
                    Span::Code(span) => write!(f, "Error at {}: {}", span, error.value)?,
                    Span::Builtin => write!(f, "Error: {}", error.value)?,
                }
                let last = TraceFrame {
                    id: FunctionId::Named("".into()),
                    span: Span::Builtin,
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
                    .map(|frame| match &frame.span {
                        Span::Code(span) => span.to_string().len(),
                        Span::Builtin => 0,
                    })
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
                        match &frame.span {
                            Span::Code(span) => write!(
                                f,
                                "  in {:max_id_length$} at {:max_span_length$}",
                                frame.id.to_string(),
                                span
                            )?,
                            Span::Builtin => {
                                write!(f, "  in {:max_id_length$}", frame.id.to_string())?
                            }
                        }
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

impl From<Vec<Sp<ParseError>>> for UiuaError {
    fn from(errors: Vec<Sp<ParseError>>) -> Self {
        Self::Compile(errors.into_iter().map(|sp| sp.map(Into::into)).collect())
    }
}

impl Error for UiuaError {}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;
