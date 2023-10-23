use std::{convert::Infallible, error::Error, fmt, io, iter::repeat, path::PathBuf, sync::Arc};

use crate::{
    function::FunctionId,
    lex::{Sp, Span},
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
            UiuaError::Break(_, span) => write!(f, "{span}: Break amount exceeded loop depth"),
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
        match self {
            UiuaError::Parse(errors) => report(
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
                ReportKind::Error,
                color,
            ),
            UiuaError::Run(error) => report(
                [(&error.value, error.span.clone())],
                ReportKind::Error,
                color,
            ),
            UiuaError::Traced { error, trace } => {
                let mut s = error.show(color);
                format_trace(&mut s, trace).unwrap();
                s
            }
            UiuaError::Throw(message, span) => {
                report([(&message, span.clone())], ReportKind::Error, color)
            }
            UiuaError::Break(_, span) => report(
                [("Break amount exceeded loop depth", span.clone())],
                ReportKind::Error,
                color,
            ),
            UiuaError::Timeout(span) => report(
                [("Maximum execution time exceeded", span.clone())],
                ReportKind::Error,
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
            ReportKind::Diagnostic(self.kind),
            color,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportKind {
    Error,
    Diagnostic(DiagnosticKind),
}

fn report<I, T>(errors: I, kind: ReportKind, color: bool) -> String
where
    I: IntoIterator<Item = (T, Span)>,
    T: fmt::Display,
{
    use colored::*;
    let color = |s: &str| {
        if color {
            let color = match kind {
                ReportKind::Error => Color::Red,
                ReportKind::Diagnostic(DiagnosticKind::Warning) => Color::Yellow,
                ReportKind::Diagnostic(DiagnosticKind::Advice) => Color::TrueColor {
                    r: 40,
                    g: 150,
                    b: 255,
                },
                ReportKind::Diagnostic(DiagnosticKind::Style) => Color::Green,
            };
            s.color(color)
        } else {
            s.into()
        }
    };
    let mut report = String::new();
    for (message, span) in errors {
        let kind_str = match kind {
            ReportKind::Error => "Error",
            ReportKind::Diagnostic(DiagnosticKind::Warning) => "Warning",
            ReportKind::Diagnostic(DiagnosticKind::Advice) => "Advice",
            ReportKind::Diagnostic(DiagnosticKind::Style) => "Style",
        };
        report.push_str(&format!("{}: {}", color(kind_str), message));
        if let Span::Code(span) = span {
            report.push('\n');
            report.push_str("  at ");
            if let Some(path) = &span.path {
                report.push_str(&path.display().to_string());
                report.push(':');
            }
            report.push_str(&format!("{}:{}", span.start.line, span.start.col));
            report.push('\n');
            let line_prefix = format!("{} | ", span.start.line);
            report.push_str(&line_prefix);
            let line = span.input.lines().nth(span.start.line - 1).unwrap_or("");
            let start_char_pos = span.start.col - 1;
            let end_char_pos = if span.start.line == span.end.line {
                span.end.char_pos
            } else {
                line.chars().count()
            };
            for (i, c) in line.chars().enumerate() {
                if i >= start_char_pos && i < end_char_pos {
                    report.push_str(&color(&c.to_string()).to_string());
                } else {
                    report.push(c);
                }
            }
            report.push('\n');
            report.extend(repeat(' ').take(line_prefix.chars().count()));
            for i in 0..line.chars().count() {
                if i >= start_char_pos && i < end_char_pos {
                    report.push_str(&color("─").to_string());
                } else {
                    report.push(' ');
                }
            }
        }
    }
    report
}
