use std::{convert::Infallible, error::Error, fmt, io, mem::take, path::PathBuf, sync::Arc};

use colored::*;

use crate::{
    function::FunctionId,
    lex::{Sp, Span},
    parse::ParseError,
    value::Value,
    CodeSpan, InputSrc, Inputs,
};

/// An error produced when running/compiling/formatting a Uiua program
#[derive(Debug, Clone)]
#[must_use]
pub struct UiuaError {
    /// The kind of error
    pub kind: UiuaErrorKind,
    /// A stack trace of the error
    pub trace: Vec<TraceFrame>,
    /// Whether the error is fill-related
    pub is_fill: bool,
    /// Whether the error can escape a single `try`
    pub is_case: bool,
    /// Bundled errors
    pub multi: Vec<Self>,
    /// Additional info about the error
    pub infos: Vec<(String, Option<Span>)>,
}

/// The kind of an error produced when running/compiling/formatting a Uiua program
#[derive(Debug, Clone)]
pub enum UiuaErrorKind {
    /// An error occurred while loading a file
    Load(PathBuf, Arc<io::Error>),
    /// An error occurred while formatting a file
    Format(PathBuf, Arc<io::Error>),
    /// An error occurred while parsing a file
    Parse(Vec<Sp<ParseError>>, Box<Inputs>),
    /// An error occurred while compiling or executing a program
    Run(Sp<String, Span>, Box<Inputs>),
    /// An error thrown by `assert`
    Throw(Box<Value>, Span, Box<Inputs>),
    /// Maximum execution time exceeded
    Timeout(Span, Box<Inputs>),
    /// The compiler panicked
    CompilerPanic(String),
    /// The program was interrupted
    Interrupted,
    /// Some tests failed
    TestsFailed(usize, usize),
}

impl UiuaErrorKind {
    /// Turn the error kind into an error
    pub fn error(self) -> UiuaError {
        self.into()
    }
}

impl From<UiuaErrorKind> for UiuaError {
    fn from(kind: UiuaErrorKind) -> Self {
        Self {
            kind,
            trace: Vec::new(),
            is_fill: false,
            is_case: false,
            multi: Vec::new(),
            infos: Vec::new(),
        }
    }
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
        match &self.kind {
            UiuaErrorKind::Load(path, e) => {
                write!(f, "failed to load {}: {e}", path.to_string_lossy())
            }
            UiuaErrorKind::Format(path, e) => {
                write!(f, "failed to format {}: {e}", path.to_string_lossy())
            }
            UiuaErrorKind::Parse(errors, _) => {
                for error in errors {
                    writeln!(f, "{error}")?;
                }
                Ok(())
            }
            UiuaErrorKind::Run(error, _) => write!(f, "{error}"),
            UiuaErrorKind::Throw(value, span, _) => write!(f, "{span}: {value}"),
            UiuaErrorKind::Timeout(..) => write!(f, "Maximum execution time exceeded"),
            UiuaErrorKind::CompilerPanic(message) => message.fmt(f),
            UiuaErrorKind::Interrupted => write!(f, "# Program interrupted"),
            UiuaErrorKind::TestsFailed(successes, failures) => {
                let total = successes + failures;
                write!(
                    f,
                    "{}/{} test{} passed, {}/{} failed",
                    successes,
                    total,
                    if total == 1 { "" } else { "s" },
                    failures,
                    total
                )
            }
        }
    }
}

impl UiuaError {
    /// Attach some info to the error
    pub fn with_info(mut self, info: impl IntoIterator<Item = (String, Option<Span>)>) -> Self {
        self.infos.extend(info);
        self
    }
    /// Get the value of the error if it was thrown by `assert`
    pub fn value(self) -> Value {
        match self.kind {
            UiuaErrorKind::Throw(value, ..) => *value,
            _ => self.to_string().into(),
        }
    }
    /// Turn the error into a multi-error
    pub fn into_multi(mut self) -> Vec<Self> {
        let mut multi = take(&mut self.multi);
        multi.insert(0, self);
        multi
    }
    /// Create an error from multiple errors
    pub fn from_multi(multi: impl IntoIterator<Item = Self>) -> Self {
        let mut iter = multi.into_iter();
        let mut error = iter
            .next()
            .unwrap_or_else(|| UiuaErrorKind::CompilerPanic("Unknown error".into()).error());
        error.multi.extend(iter);
        error
    }
    /// Mark the error as fill-related
    pub(crate) fn fill(mut self) -> Self {
        self.is_fill = true;
        self
    }
    /// Add a span to the trace of the error
    pub fn trace(mut self, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: FunctionId::Anonymous(span.clone()),
            span: Span::Code(span),
        };
        self.trace.push(frame);
        self
    }
    pub(crate) fn trace_macro(mut self, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: FunctionId::Macro(span.clone()),
            span: Span::Code(span),
        };
        self.trace.push(frame);
        self
    }
    pub(crate) fn track_caller(&mut self, new_span: impl Into<Span>) {
        self.trace.clear();
        match &mut self.kind {
            UiuaErrorKind::Run(message, _) => message.span = new_span.into(),
            UiuaErrorKind::Throw(_, span, _) => *span = new_span.into(),
            _ => {}
        }
    }
    /// Make a Load error
    pub fn load(path: PathBuf, error: io::Error) -> Self {
        UiuaErrorKind::Load(path, Arc::new(error)).into()
    }
    /// Make a Format error
    pub fn format(path: PathBuf, error: io::Error) -> Self {
        UiuaErrorKind::Format(path, Arc::new(error)).into()
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
    let mut lines: Vec<String> = Vec::new();
    let mut i = 0;
    'outer: while i < trace.len() {
        let frame = &trace[i];
        if frame.id == FunctionId::Main {
            i += 1;
            continue;
        }
        // Look for cycles
        for n in 1..=4 {
            if i >= n
                && i + n < trace.len()
                && trace[i - n..][..n]
                    .iter()
                    .zip(&trace[i..][..n])
                    .all(|(a, b)| a.id == b.id)
            {
                for (i, line) in lines.iter_mut().rev().take(n).rev().enumerate() {
                    let sep = match (n, i) {
                        (1, _) => " ×",
                        (_, 0) => " ┬×",
                        (n, i) if i == n - 1 => " ┘",
                        _ => " ┤",
                    };
                    if let Some((msg, n)) = line
                        .rsplit_once(sep)
                        .and_then(|(a, b)| b.parse::<usize>().ok().map(|n| (a, n)))
                    {
                        *line = format!("{msg}{sep}{}", n + 1);
                    } else {
                        if !line.ends_with(sep) {
                            line.push_str(sep);
                        }
                        if i == 0 {
                            line.push('2');
                        }
                    }
                }
                i += n;
                continue 'outer;
            }
        }
        lines.push(match &frame.span {
            Span::Code(span) => format!(
                "  in {:max_id_length$} at {:max_span_length$}",
                frame.id.to_string(),
                span
            ),
            Span::Builtin => format!("  in {:max_id_length$}", frame.id.to_string()),
        });
        i += 1;
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
        let mut report = match &self.kind {
            UiuaErrorKind::Parse(errors, inputs) => Report::new_multi(
                kind,
                inputs,
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
            ),
            UiuaErrorKind::Run(error, inputs) => {
                Report::new_multi(kind, inputs, [(&error.value, error.span.clone())])
            }
            UiuaErrorKind::Throw(message, span, inputs) => {
                Report::new_multi(kind, inputs, [(&message, span.clone())])
            }
            UiuaErrorKind::Timeout(span, inputs) => Report::new_multi(
                kind,
                inputs,
                [("Maximum execution time exceeded", span.clone())],
            ),
            UiuaErrorKind::CompilerPanic(message) => Report::new(kind, message),
            UiuaErrorKind::Load(..) | UiuaErrorKind::Format(..) => {
                Report::new(kind, self.to_string())
            }
            UiuaErrorKind::Interrupted => {
                return Report {
                    fragments: vec![ReportFragment::Plain(self.to_string())],
                    color: true,
                }
            }
            UiuaErrorKind::TestsFailed(successes, failures) => {
                let total = successes + failures;
                return Report {
                    fragments: vec![
                        ReportFragment::Colored(
                            format!(
                                "{}/{} test{} passed",
                                successes,
                                total,
                                if total == 1 { "" } else { "s" }
                            ),
                            DiagnosticKind::Info.into(),
                        ),
                        ReportFragment::Plain(", ".into()),
                        ReportFragment::Colored(
                            format!("{}/{} failed", failures, total),
                            ReportKind::Error,
                        ),
                    ],
                    color: true,
                };
            }
        };
        report = report.trace(&self.trace);
        let default_inputs = Inputs::default();
        let inputs = match &self.kind {
            UiuaErrorKind::Parse(_, inputs)
            | UiuaErrorKind::Run(_, inputs)
            | UiuaErrorKind::Throw(_, _, inputs)
            | UiuaErrorKind::Timeout(_, inputs) => inputs,
            _ => &default_inputs,
        };
        for (info, span) in &self.infos {
            if let Some(span) = span {
                report.fragments.extend(
                    Report::new_multi(DiagnosticKind::Info.into(), inputs, [(info, span.clone())])
                        .fragments,
                );
            } else {
                report.fragments.push(ReportFragment::Newline);
                report.fragments.push(ReportFragment::Colored(
                    "Info".into(),
                    DiagnosticKind::Info.into(),
                ));
                report.fragments.push(ReportFragment::Plain(": ".into()));
                report.fragments.push(ReportFragment::Plain(info.into()));
            }
        }
        for error in &self.multi {
            report.fragments.push(ReportFragment::Newline);
            report.fragments.extend(error.report().fragments);
        }
        report
    }
}

/// A message to be displayed to the user that is not an error
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// The span of the message
    pub span: Span,
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
    /// Informational message
    Info,
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
        message: String,
        span: impl Into<Span>,
        kind: DiagnosticKind,
        inputs: Inputs,
    ) -> Self {
        Self {
            message,
            span: span.into(),
            kind,
            inputs,
        }
    }
    /// Get a rich-text report for the diagnostic
    pub fn report(&self) -> Report {
        Report::new_multi(
            ReportKind::Diagnostic(self.kind),
            &self.inputs,
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

impl From<DiagnosticKind> for ReportKind {
    fn from(kind: DiagnosticKind) -> Self {
        ReportKind::Diagnostic(kind)
    }
}

impl ReportKind {
    /// Get the string that prefixes the formatted report
    pub fn str(&self) -> &'static str {
        match self {
            ReportKind::Error => "Error",
            ReportKind::Diagnostic(DiagnosticKind::Warning) => "Warning",
            ReportKind::Diagnostic(DiagnosticKind::Advice) => "Advice",
            ReportKind::Diagnostic(DiagnosticKind::Style) => "Style",
            ReportKind::Diagnostic(DiagnosticKind::Info) => "Info",
        }
    }
}

/// A text fragment of a report
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReportFragment {
    /// Just plain text
    Plain(String),
    /// Text colored according to the report kind
    Colored(String, ReportKind),
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
            ReportFragment::Colored(kind.str().into(), kind),
            ReportFragment::Plain(": ".into()),
        ];
        if message.lines().count() > 1 {
            fragments.push(ReportFragment::Newline);
        }
        fragments.push(ReportFragment::Plain(message));
        Self {
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
            fragments.push(ReportFragment::Colored(kind.str().into(), kind));
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
                fragments.push(ReportFragment::Colored(color, kind));
                fragments.push(ReportFragment::Faint(post_color));
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Plain(
                    " ".repeat(line_prefix.chars().count()),
                ));
                fragments.push(ReportFragment::Plain(" ".repeat(start_char_pos as usize)));
                fragments.push(ReportFragment::Colored(
                    "─".repeat(end_char_pos.saturating_sub(start_char_pos).max(1) as usize),
                    kind,
                ));
            }
        }
        Self {
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
                ReportFragment::Colored(s, kind) => {
                    if self.color {
                        let s = s.color(match kind {
                            ReportKind::Error => Color::Red,
                            ReportKind::Diagnostic(DiagnosticKind::Warning) => Color::Yellow,
                            ReportKind::Diagnostic(DiagnosticKind::Style) => Color::Green,
                            ReportKind::Diagnostic(DiagnosticKind::Advice) => Color::TrueColor {
                                r: 50,
                                g: 150,
                                b: 255,
                            },
                            ReportKind::Diagnostic(DiagnosticKind::Info) => Color::BrightCyan,
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
