use std::{convert::Infallible, error::Error, fmt, io, path::PathBuf, sync::Arc};

use colored::*;

use crate::{
    function::FunctionId,
    lex::{Sp, Span},
    parse::ParseError,
    value::Value,
};

/// An error produced when running a Uiua program
#[derive(Debug, Clone)]
pub enum UiuaError {
    /// An error occurred while loading a file
    Load(PathBuf, Arc<io::Error>),
    /// An error occurred while formatting a file
    Format(PathBuf, Arc<io::Error>),
    /// An error occurred while parsing a file
    Parse(Vec<Sp<ParseError>>),
    /// An error occurred while compiling or executing a program
    Run(Sp<String, Span>),
    /// An error with a trace attached
    Traced {
        /// The error itself
        error: Box<Self>,
        /// The stack trace
        trace: Vec<TraceFrame>,
    },
    /// An error thrown by `assert`
    Throw(Box<Value>, Span),
    /// Control flow for `break`
    Break(usize, Span),
    /// Maximum execution time exceeded
    Timeout(Span),
    /// A wrapper marking this error as being fill-related
    Fill(Box<Self>),
}

/// Uiua's result type
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

/// A frame in a trace
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceFrame {
    /// The function that was called
    pub id: FunctionId,
    /// The span of the call
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
                for line in format_trace(trace) {
                    writeln!(f, "{line}")?;
                }
                Ok(())
            }
            UiuaError::Throw(value, span) => write!(f, "{span}: {value}"),
            UiuaError::Break(_, span) => write!(f, "{span}: Break amount exceeded loop depth"),
            UiuaError::Timeout(_) => write!(f, "Maximum execution time exceeded"),
            UiuaError::Fill(error) => error.fmt(f),
        }
    }
}

impl UiuaError {
    /// Get the message of the error
    pub fn message(&self) -> String {
        match self {
            UiuaError::Traced { error, .. } => error.message(),
            error => error.to_string(),
        }
    }
    /// Get the value of the error if it was thrown by `assert`
    pub fn value(self) -> Value {
        match self {
            UiuaError::Throw(value, _) => *value,
            UiuaError::Traced { error, .. } => error.value(),
            error => error.message().into(),
        }
    }
    pub(crate) fn break_data(self) -> Result<(usize, Span), Self> {
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

fn format_trace(trace: &[TraceFrame]) -> Vec<String> {
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
    let mut lines = Vec::new();
    for frame in trace {
        if frame.id == FunctionId::Main {
            continue;
        }
        lines.push(match &frame.span {
            Span::Code(span) => format!(
                "  in {:max_id_length$} at {:max_span_length$}",
                frame.id.to_string(),
                span
            ),
            Span::Builtin => format!("  in {:max_id_length$}", frame.id.to_string()),
        });
    }
    lines
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
    /// Get a rich-text report for the error
    pub fn report(&self) -> Report {
        let kind = ReportKind::Error;
        match self {
            UiuaError::Parse(errors) => Report::new_multi(
                kind,
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
            ),
            UiuaError::Run(error) => Report::new_multi(kind, [(&error.value, error.span.clone())]),
            UiuaError::Traced { error, trace } => error.report().trace(trace),
            UiuaError::Throw(message, span) => Report::new_multi(kind, [(&message, span.clone())]),
            UiuaError::Break(_, span) => {
                Report::new_multi(kind, [("Break amount exceeded loop depth", span.clone())])
            }
            UiuaError::Timeout(span) => {
                Report::new_multi(kind, [("Maximum execution time exceeded", span.clone())])
            }
            UiuaError::Fill(error) => error.report(),
            UiuaError::Load(..) | UiuaError::Format(..) => Report::new(kind, self.to_string()),
        }
    }
}

/// A message to be displayed to the user that is not an error
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic {
    /// The span of the message
    pub span: Span,
    /// The message itself
    pub message: String,
    /// What kind of diagnostic this is
    pub kind: DiagnosticKind,
}

/// Kinds of non-error diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticKind {
    /// Something that really needs to be fixed
    Warning,
    /// Something that should be fixed for performance reasons
    Advice,
    /// Bad code style
    Style,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(message: impl Into<String>, span: impl Into<Span>, kind: DiagnosticKind) -> Self {
        Self {
            message: message.into(),
            span: span.into(),
            kind,
        }
    }
    /// Get a rich-text report for the diagnostic
    pub fn report(&self) -> Report {
        Report::new_multi(
            ReportKind::Diagnostic(self.kind),
            [(&self.message, self.span.clone())],
        )
    }
}

/// Kinds of reports
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReportKind {
    /// An error
    Error,
    /// A diagnostic
    Diagnostic(DiagnosticKind),
}

impl ReportKind {
    /// Get the string that prefixes the formatted report
    pub fn str(&self) -> &'static str {
        match self {
            ReportKind::Error => "Error",
            ReportKind::Diagnostic(DiagnosticKind::Warning) => "Warning",
            ReportKind::Diagnostic(DiagnosticKind::Advice) => "Advice",
            ReportKind::Diagnostic(DiagnosticKind::Style) => "Style",
        }
    }
}

/// A text fragment of a report
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReportFragment {
    /// Just plain text
    Plain(String),
    /// Text colored according to the report kind
    Colored(String),
    /// Faint text
    Faint(String),
    /// Even fainter text
    Fainter(String),
    /// A newline
    Newline,
}

/// A rich-text error/diagnostic report
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Report {
    /// What kind of report this is
    pub kind: ReportKind,
    /// The rich-text fragments of the report
    pub fragments: Vec<ReportFragment>,
    /// Whether to color the report with ANSI escape codes when converting it to a string
    ///
    /// Defaults to `true`
    pub color: bool,
}

impl Report {
    /// Change whether to color the report with ANSI escape codes when converting it to a string
    ///
    /// Defaults to `true`
    pub fn color(mut self, color: bool) -> Self {
        self.color = color;
        self
    }
    /// Add a trace to the report
    pub fn trace(mut self, trace: &[TraceFrame]) -> Self {
        for line in format_trace(trace) {
            self.fragments.push(ReportFragment::Newline);
            self.fragments.push(ReportFragment::Plain(line));
        }
        self
    }
    /// Create a new report
    pub fn new(kind: ReportKind, message: impl Into<String>) -> Self {
        let fragments = vec![
            ReportFragment::Colored(kind.str().into()),
            ReportFragment::Plain(": ".into()),
            ReportFragment::Plain(message.into()),
        ];
        Self {
            kind,
            fragments,
            color: true,
        }
    }
    /// Create a new report with multiple messages
    pub fn new_multi<I, T>(kind: ReportKind, errors: I) -> Self
    where
        I: IntoIterator<Item = (T, Span)>,
        T: fmt::Display,
    {
        let mut fragments = Vec::new();
        for (message, span) in errors {
            fragments.push(ReportFragment::Colored(kind.str().into()));
            fragments.push(ReportFragment::Plain(": ".into()));
            for (i, line) in message.to_string().lines().enumerate() {
                if i > 0 {
                    fragments.push(ReportFragment::Newline);
                    fragments.push(ReportFragment::Plain("  ".into()));
                }
                fragments.push(ReportFragment::Plain(line.into()));
            }
            if let Span::Code(span) = span {
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Fainter("  at ".into()));
                if let Some(path) = &span.path {
                    fragments.push(ReportFragment::Fainter(format!("{}:", path.display())));
                }
                fragments.push(ReportFragment::Fainter(format!(
                    "{}:{}",
                    span.start.line, span.start.col
                )));
                fragments.push(ReportFragment::Newline);
                let line_prefix = format!("{} | ", span.start.line);
                fragments.push(ReportFragment::Plain(line_prefix.clone()));
                let line = span.input.lines().nth(span.start.line - 1).unwrap_or("");
                let start_char_pos = span.start.col - 1;
                let end_char_pos = if span.start.line == span.end.line {
                    span.end.col - 1
                } else {
                    line.chars().count()
                };
                let pre_color: String = line.chars().take(start_char_pos).collect();
                let color: String = line
                    .chars()
                    .skip(start_char_pos)
                    .take(end_char_pos - start_char_pos)
                    .collect();
                let post_color: String = line.chars().skip(end_char_pos).collect();
                fragments.push(ReportFragment::Faint(pre_color));
                fragments.push(ReportFragment::Colored(color));
                fragments.push(ReportFragment::Faint(post_color));
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Plain(
                    " ".repeat(line_prefix.chars().count()),
                ));
                fragments.push(ReportFragment::Plain(" ".repeat(start_char_pos)));
                fragments.push(ReportFragment::Colored(
                    "─".repeat(end_char_pos - start_char_pos),
                ));
            }
        }
        Self {
            kind,
            fragments,
            color: true,
        }
    }
}

impl fmt::Display for Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for frag in &self.fragments {
            match frag {
                ReportFragment::Plain(s)
                | ReportFragment::Faint(s)
                | ReportFragment::Fainter(s) => write!(f, "{s}")?,
                ReportFragment::Colored(s) => {
                    if self.color {
                        let s = s.color(match self.kind {
                            ReportKind::Error => Color::Red,
                            ReportKind::Diagnostic(DiagnosticKind::Warning) => Color::Yellow,
                            ReportKind::Diagnostic(DiagnosticKind::Style) => Color::Green,
                            ReportKind::Diagnostic(DiagnosticKind::Advice) => Color::TrueColor {
                                r: 50,
                                g: 150,
                                b: 255,
                            },
                        });
                        write!(f, "{s}")?
                    } else {
                        write!(f, "{s}")?
                    }
                }
                ReportFragment::Newline => writeln!(f)?,
            }
        }
        Ok(())
    }
}
