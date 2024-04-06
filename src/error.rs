use std::{convert::Infallible, error::Error, fmt, io, path::PathBuf, sync::Arc};

use colored::*;

use crate::{
    function::FunctionId,
    lex::{Sp, Span},
    parse::ParseError,
    value::Value,
    CodeSpan, InputSrc, Inputs,
};

/// An error produced when running a Uiua program
#[derive(Debug, Clone)]
#[must_use]
pub enum UiuaError {
    /// An error occurred while loading a file
    Load(PathBuf, Arc<io::Error>),
    /// An error occurred while formatting a file
    Format(PathBuf, Arc<io::Error>),
    /// An error occurred while parsing a file
    Parse(Vec<Sp<ParseError>>, Box<Inputs>),
    /// An error occurred while compiling or executing a program
    Run(Sp<String, Span>, Box<Inputs>),
    /// An error with a trace attached
    Traced {
        /// The error itself
        error: Box<Self>,
        /// The stack trace
        trace: Vec<TraceFrame>,
    },
    /// An error thrown by `assert`
    Throw(Box<Value>, Span, Box<Inputs>),
    /// Maximum execution time exceeded
    Timeout(Span, Box<Inputs>),
    /// A wrapper marking this error as being fill-related
    Fill(Box<Self>),
    /// A pattern match failed
    PatternMatch(Span, Box<Inputs>),
    /// The interpreter panicked
    Panic(String),
    /// Multiple errors
    Multi(Vec<Self>),
}

/// Uiua's result type
pub type UiuaResult<T = ()> = Result<T, UiuaError>;

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
            UiuaError::Parse(errors, _) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaError::Run(error, _) => write!(f, "{error}"),
            UiuaError::Traced { error, trace } => {
                write!(f, "{error}")?;
                for line in format_trace(trace) {
                    writeln!(f, "{line}")?;
                }
                Ok(())
            }
            UiuaError::Throw(value, span, _) => write!(f, "{span}: {value}"),
            UiuaError::Timeout(..) => write!(f, "Maximum execution time exceeded"),
            UiuaError::Fill(error) => error.fmt(f),
            UiuaError::PatternMatch(span, _) => write!(f, "{span}: Pattern match failed"),
            UiuaError::Panic(message) => message.fmt(f),
            UiuaError::Multi(errors) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
        }
    }
}

impl UiuaError {
    pub(crate) fn inner(&self) -> &Self {
        match self {
            UiuaError::Traced { error, .. } => error.inner(),
            UiuaError::Fill(error) => error.inner(),
            error => error,
        }
    }
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
            UiuaError::Throw(value, ..) => *value,
            UiuaError::Traced { error, .. } => error.value(),
            error => error.message().into(),
        }
    }
    /// Check if the error is fill-related
    pub fn is_fill(&self) -> bool {
        match self {
            UiuaError::Traced { error, .. } => error.is_fill(),
            UiuaError::Fill(_) => true,
            _ => false,
        }
    }
    /// Check if the error is a pattern match failure
    pub fn is_pattern_match(&self) -> bool {
        match self {
            UiuaError::Traced { error, .. } => error.is_pattern_match(),
            UiuaError::PatternMatch(..) => true,
            _ => false,
        }
    }
    /// Mark the error as fill-related
    pub(crate) fn fill(self) -> Self {
        UiuaError::Fill(Box::new(self))
    }
    /// Add a span to the trace of the error
    pub fn trace(mut self, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: FunctionId::Anonymous(span.clone()),
            span: Span::Code(span),
        };
        if let UiuaError::Traced { trace, .. } = &mut self {
            trace.push(frame);
            self
        } else {
            UiuaError::Traced {
                error: Box::new(self),
                trace: vec![frame],
            }
        }
    }
    pub(crate) fn trace_macro(mut self, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: FunctionId::Macro(span.clone()),
            span: Span::Code(span),
        };
        if let UiuaError::Traced { trace, .. } = &mut self {
            trace.push(frame);
            self
        } else {
            UiuaError::Traced {
                error: Box::new(self),
                trace: vec![frame],
            }
        }
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
            UiuaError::Parse(errors, inputs) => Report::new_multi(
                kind,
                inputs,
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
            ),
            UiuaError::Run(error, inputs) => {
                Report::new_multi(kind, inputs, [(&error.value, error.span.clone())])
            }
            UiuaError::Traced { error, trace } => error.report().trace(trace),
            UiuaError::Throw(message, span, inputs) => {
                Report::new_multi(kind, inputs, [(&message, span.clone())])
            }
            UiuaError::Timeout(span, inputs) => Report::new_multi(
                kind,
                inputs,
                [("Maximum execution time exceeded", span.clone())],
            ),
            UiuaError::Fill(error) => error.report(),
            UiuaError::PatternMatch(span, inputs) => {
                Report::new_multi(kind, inputs, [("Pattern match failed", span.clone())])
            }
            UiuaError::Panic(message) => Report::new(kind, message),
            UiuaError::Load(..) | UiuaError::Format(..) => Report::new(kind, self.to_string()),
            UiuaError::Multi(errors) => {
                let mut fragments = Vec::new();
                for (i, error) in errors.iter().enumerate() {
                    if i > 0 {
                        fragments.push(ReportFragment::Newline);
                    }
                    fragments.extend(error.report().fragments);
                }
                Report {
                    kind,
                    fragments,
                    color: true,
                }
            }
        }
    }
}

/// A message to be displayed to the user that is not an error
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The span of the message
    pub span: CodeSpan,
    /// The message itself
    pub message: String,
    /// What kind of diagnostic this is
    pub kind: DiagnosticKind,
    /// The inputs of the program
    pub inputs: Inputs,
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.message == other.message
    }
}

impl Eq for Diagnostic {}

impl PartialOrd for Diagnostic {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Diagnostic {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.span.cmp(&other.span)
    }
}

/// Kinds of non-error diagnostics
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticKind {
    /// Bad code style
    Style,
    /// Something that should be fixed for performance reasons
    Advice,
    /// Something that really needs to be fixed
    Warning,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl Diagnostic {
    /// Create a new diagnostic
    pub fn new(
        message: impl Into<String>,
        span: CodeSpan,
        kind: DiagnosticKind,
        inputs: Inputs,
    ) -> Self {
        Self {
            message: message.into(),
            span,
            kind,
            inputs,
        }
    }
    /// Get a rich-text report for the diagnostic
    pub fn report(&self) -> Report {
        Report::new_multi(
            ReportKind::Diagnostic(self.kind),
            &self.inputs,
            [(&self.message, self.span.clone().into())],
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
        let message = message.into();
        let mut fragments = vec![
            ReportFragment::Colored(kind.str().into()),
            ReportFragment::Plain(": ".into()),
        ];
        if message.lines().count() > 1 {
            fragments.push(ReportFragment::Newline);
        }
        fragments.push(ReportFragment::Plain(message));
        Self {
            kind,
            fragments,
            color: true,
        }
    }
    /// Create a new report with multiple messages
    pub fn new_multi<I, T>(kind: ReportKind, inputs: &Inputs, errors: I) -> Self
    where
        I: IntoIterator<Item = (T, Span)>,
        T: fmt::Display,
    {
        let mut fragments = Vec::new();
        for (i, (message, span)) in errors.into_iter().enumerate() {
            if i > 0 {
                fragments.push(ReportFragment::Newline);
            }
            fragments.push(ReportFragment::Colored(kind.str().into()));
            fragments.push(ReportFragment::Plain(": ".into()));
            let message = message.to_string();
            for (i, line) in message.lines().enumerate() {
                if i > 0 || message.lines().count() > 1 {
                    fragments.push(ReportFragment::Newline);
                    fragments.push(ReportFragment::Plain("  ".into()));
                }
                fragments.push(ReportFragment::Plain(line.into()));
            }
            if let Span::Code(mut span) = span {
                while let InputSrc::Macro(inner) = span.src {
                    span = *inner;
                }
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Fainter("  at ".into()));
                if let InputSrc::File(path) = &span.src {
                    fragments.push(ReportFragment::Fainter(format!("{}:", path.display())));
                }
                fragments.push(ReportFragment::Fainter(format!(
                    "{}:{}",
                    span.start.line, span.start.col
                )));
                fragments.push(ReportFragment::Newline);
                let line_prefix = format!("{} | ", span.start.line);
                fragments.push(ReportFragment::Plain(line_prefix.clone()));
                let input = inputs.get(&span.src);
                let line = input
                    .lines()
                    .nth(span.start.line as usize - 1)
                    .unwrap_or("");
                let start_char_pos = span.start.col - 1;
                let end_char_pos = if span.start.line == span.end.line {
                    span.end.col - 1
                } else {
                    line.chars().count() as u16
                };
                let pre_color: String = line.chars().take(start_char_pos as usize).collect();
                let color: String = line
                    .chars()
                    .skip(start_char_pos as usize)
                    .take(end_char_pos.saturating_sub(start_char_pos).max(1) as usize)
                    .collect();
                let post_color: String = line.chars().skip(end_char_pos as usize).collect();
                fragments.push(ReportFragment::Faint(pre_color));
                fragments.push(ReportFragment::Colored(color));
                fragments.push(ReportFragment::Faint(post_color));
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Plain(
                    " ".repeat(line_prefix.chars().count()),
                ));
                fragments.push(ReportFragment::Plain(" ".repeat(start_char_pos as usize)));
                fragments.push(ReportFragment::Colored(
                    "─".repeat(end_char_pos.saturating_sub(start_char_pos).max(1) as usize),
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
