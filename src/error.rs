use std::{
    collections::HashMap,
    convert::Infallible,
    error::Error,
    fmt, fs, io,
    path::{Path, PathBuf},
    sync::Arc,
};

use ariadne::{Color, Config, Label, Report, ReportKind, Source};

use crate::{
    example_ua,
    function::FunctionId,
    lex::{CodeSpan, Sp, Span},
    parse::ParseError,
    value::Value,
};

#[derive(Debug, Clone)]
pub enum UiuaError {
    Load(PathBuf, Arc<io::Error>),
    Format(PathBuf, Arc<io::Error>),
    Parse(Vec<Sp<ParseError>>),
    Run(Sp<String, Span>),
    Traced {
        error: Box<Self>,
        trace: Vec<TraceFrame>,
    },
    Throw(Box<Value>, Span),
    Break(usize, Span),
    Timeout(Span),
    Fill(Box<Self>),
}

pub type UiuaResult<T = ()> = Result<T, UiuaError>;

impl From<Sp<String, Span>> for UiuaError {
    fn from(value: Sp<String, Span>) -> Self {
        Self::Run(value)
    }
}

impl From<Sp<String>> for UiuaError {
    fn from(value: Sp<String>) -> Self {
        Self::Run(value.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceFrame {
    pub id: FunctionId,
    pub span: Span,
}

impl fmt::Display for UiuaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UiuaError::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            UiuaError::Format(path, e) => {
                write!(f, "failed to format {}: {e}", path.to_string_lossy())
            }
            UiuaError::Parse(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaError::Run(error) => write!(f, "{error}"),
            UiuaError::Traced { error, trace } => {
                write!(f, "{error}")?;
                format_trace(f, trace)
            }
            UiuaError::Throw(value, span) => write!(f, "{span}: {value}"),
            UiuaError::Break(_, span) => write!(f, "{span}: break outside of loop"),
            UiuaError::Timeout(_) => write!(f, "Maximum execution time exceeded"),
            UiuaError::Fill(error) => error.fmt(f),
        }
    }
}

impl UiuaError {
    pub fn message(&self) -> String {
        match self {
            UiuaError::Traced { error, .. } => error.message(),
            error => error.to_string(),
        }
    }
    pub fn value(self) -> Value {
        match self {
            UiuaError::Throw(value, _) => *value,
            UiuaError::Traced { error, .. } => error.value(),
            error => error.message().into(),
        }
    }
    pub fn break_data(self) -> Result<(usize, Span), Self> {
        match self {
            UiuaError::Traced { error, trace } => {
                error.break_data().map_err(|error| UiuaError::Traced {
                    error: Box::new(error),
                    trace,
                })
            }
            UiuaError::Break(n, span) => Ok((n, span)),
            error => Err(error),
        }
    }
    /// Check if the error is fill-related
    pub(crate) fn is_fill(&self) -> bool {
        match self {
            UiuaError::Traced { error, .. } => error.is_fill(),
            UiuaError::Fill(_) => true,
            _ => false,
        }
    }
    /// Mark the error as fill-related
    pub(crate) fn fill(self) -> Self {
        UiuaError::Fill(Box::new(self))
    }
}

fn format_trace<F: fmt::Write>(f: &mut F, trace: &[TraceFrame]) -> fmt::Result {
    let last = TraceFrame {
        id: FunctionId::Named("".into()),
        span: Span::Builtin,
    };
    let mut last = &last;
    let mut repetitions = 1;
    let max_id_length = trace
        .iter()
        .filter(|frame| frame.span != Span::Builtin)
        .map(|frame| frame.id.to_string().chars().count())
        .max()
        .unwrap_or(0);
    let max_span_length = trace
        .iter()
        .map(|frame| match &frame.span {
            Span::Code(span) => span.to_string().chars().count(),
            Span::Builtin => 0,
        })
        .max()
        .unwrap_or(0);
    for frame in trace {
        if frame.id == FunctionId::Main {
            continue;
        }
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
                Span::Builtin => write!(f, "  in {:max_id_length$}", frame.id.to_string())?,
            }
            last = frame;
        }
    }
    if repetitions > 1 {
        write!(f, " (x {repetitions})")?;
    }
    Ok(())
}

impl From<Vec<Sp<ParseError>>> for UiuaError {
    fn from(errors: Vec<Sp<ParseError>>) -> Self {
        Self::Parse(errors)
    }
}

impl Error for UiuaError {}

impl From<Infallible> for UiuaError {
    fn from(value: Infallible) -> Self {
        match value {}
    }
}

impl UiuaError {
    pub fn show(&self, color: bool) -> String {
        let kind = ReportKind::Error;
        match self {
            UiuaError::Parse(errors) => report(
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
                kind,
                color,
            ),
            UiuaError::Run(error) => report([(&error.value, error.span.clone())], kind, color),
            UiuaError::Traced { error, trace } => {
                let mut s = error.show(color);
                format_trace(&mut s, trace).unwrap();
                s
            }
            UiuaError::Throw(message, span) => report([(&message, span.clone())], kind, color),
            UiuaError::Break(_, span) => {
                report([("break outside of loop", span.clone())], kind, color)
            }
            UiuaError::Timeout(span) => report(
                [("Maximum execution time exceeded", span.clone())],
                kind,
                color,
            ),
            UiuaError::Fill(error) => error.show(color),
            UiuaError::Load(..) | UiuaError::Format(..) => self.to_string(),
        }
    }
}

/// A message to be displayed to the user that is not an error
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
    pub kind: DiagnosticKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticKind {
    Warning,
    Advice,
    Style,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, span: impl Into<Span>, kind: DiagnosticKind) -> Self {
        Self {
            message: message.into(),
            span: span.into(),
            kind,
        }
    }
    pub fn show(&self, color: bool) -> String {
        report(
            [(&self.message, self.span.clone())],
            match self.kind {
                DiagnosticKind::Warning => ReportKind::Warning,
                DiagnosticKind::Advice => ReportKind::Advice,
                DiagnosticKind::Style => ReportKind::Custom("Style", Color::Green),
            },
            color,
        )
    }
}

fn report<I, T>(errors: I, mut kind: ReportKind, color: bool) -> String
where
    I: IntoIterator<Item = (T, Span)>,
    T: ToString,
{
    let config = Config::default().with_color(color);
    let color = if color {
        match kind {
            ReportKind::Error => Color::Red,
            ReportKind::Warning => Color::Yellow,
            ReportKind::Advice => Color::Fixed(147),
            ReportKind::Custom(_, col) => col,
        }
    } else {
        if let ReportKind::Custom(_, col) = &mut kind {
            *col = Color::Unset;
        }
        Color::Unset
    };
    let mut buffer = Vec::new();
    let mut chache = None;
    for (message, span) in errors {
        if let Span::Code(span) = span {
            let cache = chache.get_or_insert_with(|| Cache {
                input: Source::from(&span.input),
                files: HashMap::new(),
            });
            let report = Report::<CodeSpan>::build(kind, span.path.clone(), span.start.char_pos)
                .with_message(message)
                .with_label(Label::new(span.clone()).with_color(color))
                .with_config(config)
                .finish();
            let _ = report.write(cache, &mut buffer);
        } else {
            if !buffer.ends_with(b"\n") {
                buffer.push(b'\n');
            }
            buffer.extend(message.to_string().into_bytes());
        }
    }
    let s = String::from_utf8_lossy(&buffer);
    let s = s.trim();
    s.lines()
        .filter(|line| {
            ![
                "│",
                "\u{1b}[38;5;246m│\u{1b}[0m",
                "\u{1b}[38;5;240m  │\u{1b}[0m",
            ]
            .contains(&line.trim())
        })
        .collect::<Vec<_>>()
        .join("\n")
}

type SourceId = Option<Arc<Path>>;

impl ariadne::Span for CodeSpan {
    type SourceId = SourceId;
    fn source(&self) -> &Self::SourceId {
        &self.path
    }
    fn start(&self) -> usize {
        self.start.char_pos
    }
    fn end(&self) -> usize {
        self.end.char_pos
    }
}

struct Cache {
    input: Source,
    files: HashMap<SourceId, Source>,
}

impl ariadne::Cache<SourceId> for Cache {
    fn fetch(&mut self, id: &SourceId) -> Result<&Source, Box<dyn fmt::Debug + '_>> {
        match id {
            Some(path) => {
                if !self.files.contains_key(id) {
                    let text = fs::read_to_string(path)
                        .or_else(|e| {
                            if path.to_string_lossy() == "example.ua" {
                                Ok(example_ua(|ex| ex.clone()))
                            } else {
                                Err(e)
                            }
                        })
                        .map_err(|e| Box::new(e) as Box<dyn fmt::Debug>)?;
                    let source = Source::from(text);
                    self.files.insert(id.clone(), source);
                }
                Ok(&self.files[id])
            }
            None => Ok(&self.input),
        }
    }
    fn display<'a>(&self, id: &'a SourceId) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(match id {
            Some(path) => Box::new(path.display()),
            None => Box::<String>::default(),
        })
    }
}
