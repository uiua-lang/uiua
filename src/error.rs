use std::{convert::Infallible, error::Error, fmt, io, mem::take, path::PathBuf, sync::Arc};

use crate::{
    CodeSpan, DiagnosticKind, Ident, Inputs, Report, ReportFragment, ReportKind, Sp, Span,
    function::FunctionId, parse::ParseError, value::Value,
};

/// An error produced when running/compiling/formatting a Uiua program
#[derive(Debug, Clone)]
#[must_use]
pub struct UiuaError {
    /// The kind of error
    pub kind: Box<UiuaErrorKind>,
    /// Additional error data
    pub meta: Box<ErrorMeta>,
}

/// Additional data attached to a Uiua error
#[derive(Debug, Clone)]
pub struct ErrorMeta {
    /// A stack trace of the error
    pub trace: Vec<TraceFrame>,
    /// Whether the error is fill-related
    pub is_fill: bool,
    /// Whether the error can escape a single `try`
    pub is_case: bool,
    /// Bundled errors
    pub multi: Vec<UiuaError>,
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
    Run {
        /// The error message
        message: Sp<String, Span>,
        /// Associated information
        info: Vec<Sp<String, Span>>,
        /// The inputs
        inputs: Box<Inputs>,
    },
    /// An error thrown by `assert`
    Throw(Box<Value>, Span, Box<Inputs>),
    /// Maximum execution time exceeded
    Timeout(Span, Box<Inputs>),
    /// The compiler panicked
    CompilerPanic(String),
    /// The program was interrupted
    Interrupted,
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
            kind: kind.into(),
            meta: ErrorMeta {
                trace: Vec::new(),
                is_fill: false,
                is_case: false,
                multi: Vec::new(),
                infos: Vec::new(),
            }
            .into(),
        }
    }
}

/// Uiua's result type
pub type UiuaResult<T = ()> = Result<T, UiuaError>;

/// A frame in a trace
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TraceFrame {
    /// The function that was called
    pub id: Option<FunctionId>,
    /// The span of the call
    pub span: Span,
}

impl fmt::Display for UiuaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.kind {
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
            UiuaErrorKind::Run { message: error, .. } => write!(f, "{error}"),
            UiuaErrorKind::Throw(value, span, _) => write!(f, "{span}: {value}"),
            UiuaErrorKind::Timeout(..) => write!(f, "Maximum execution time exceeded"),
            UiuaErrorKind::CompilerPanic(message) => message.fmt(f),
            UiuaErrorKind::Interrupted => write!(f, "# Program interrupted"),
        }
    }
}

impl UiuaError {
    /// Attach some info to the error
    pub fn with_info(mut self, info: impl IntoIterator<Item = (String, Option<Span>)>) -> Self {
        self.meta.infos.extend(info);
        self
    }
    /// Get the value of the error if it was thrown by `assert`
    pub fn value(self) -> Value {
        match *self.kind {
            UiuaErrorKind::Throw(value, ..) => *value,
            _ => self.to_string().into(),
        }
    }
    /// Turn the error into a multi-error
    pub fn into_multi(mut self) -> Vec<Self> {
        let mut multi = take(&mut self.meta.multi);
        multi.insert(0, self);
        multi
    }
    /// Create an error from multiple errors
    pub fn from_multi(multi: impl IntoIterator<Item = Self>) -> Self {
        let mut iter = multi.into_iter();
        let mut error = iter
            .next()
            .unwrap_or_else(|| UiuaErrorKind::CompilerPanic("Unknown error".into()).error());
        error.meta.multi.extend(iter);
        error
    }
    /// Mark the error as fill-related
    pub(crate) fn fill(mut self) -> Self {
        self.meta.is_fill = true;
        self
    }
    /// Add a span to the trace of the error
    pub fn trace(mut self, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: None,
            span: Span::Code(span),
        };
        self.meta.trace.push(frame);
        self
    }
    pub(crate) fn trace_macro(mut self, name: Option<Ident>, span: CodeSpan) -> Self {
        let frame = TraceFrame {
            id: Some(FunctionId::Macro(name, span.clone())),
            span: Span::Code(span),
        };
        self.meta.trace.push(frame);
        self
    }
    pub(crate) fn track_caller(&mut self, new_span: impl Into<Span>) {
        self.meta.trace.clear();
        match &mut *self.kind {
            UiuaErrorKind::Run { message, .. } => message.span = new_span.into(),
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
        .map(|frame| (frame.id.as_ref()).map_or(0, |id| id.to_string().chars().count()))
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
        if frame.id == Some(FunctionId::Main) {
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
        lines.push(match (&frame.id, &frame.span) {
            (Some(id), Span::Code(span)) => {
                format!("  in {id:max_id_length$} at {span:max_span_length$}")
            }
            (Some(id), Span::Builtin) => format!("  in {id:max_id_length$}"),
            (None, Span::Code(span)) => {
                format!("  at {span:max_span_length$}")
            }
            (None, Span::Builtin) => {
                i += 1;
                continue;
            }
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
        let mut report = match &*self.kind {
            UiuaErrorKind::Parse(errors, inputs) => Report::new_multi(
                kind,
                inputs,
                errors
                    .iter()
                    .map(|error| (error.value.to_string(), error.span.clone().into())),
            ),
            UiuaErrorKind::Run {
                message,
                info,
                inputs,
            } => {
                let mut report =
                    Report::new_multi(kind, inputs, [(&message.value, message.span.clone())]);
                for info in info {
                    report.fragments.push(ReportFragment::Newline);
                    report.fragments.extend(
                        Report::new_multi(
                            DiagnosticKind::Info.into(),
                            inputs,
                            [(&info.value, info.span.clone())],
                        )
                        .fragments,
                    );
                }
                report
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
                };
            }
        };
        report = report_trace(report, &self.meta.trace);
        let default_inputs = Inputs::default();
        let inputs = match &*self.kind {
            UiuaErrorKind::Parse(_, inputs)
            | UiuaErrorKind::Run { inputs, .. }
            | UiuaErrorKind::Throw(_, _, inputs)
            | UiuaErrorKind::Timeout(_, inputs) => inputs,
            _ => &default_inputs,
        };
        for (info, span) in &self.meta.infos {
            report.fragments.push(ReportFragment::Newline);
            if let Some(span) = span {
                report.fragments.extend(
                    Report::new_multi(DiagnosticKind::Info.into(), inputs, [(info, span.clone())])
                        .fragments,
                );
            } else {
                report.fragments.push(ReportFragment::Colored(
                    "Info".into(),
                    DiagnosticKind::Info.into(),
                ));
                report.fragments.push(ReportFragment::Plain(": ".into()));
                report.fragments.push(ReportFragment::Plain(info.into()));
            }
        }
        for error in &self.meta.multi {
            report.fragments.push(ReportFragment::Newline);
            report.fragments.extend(error.report().fragments);
        }
        report
    }
}

fn report_trace(mut report: Report, trace: &[TraceFrame]) -> Report {
    for line in format_trace(trace) {
        report.fragments.push(ReportFragment::Newline);
        report.fragments.push(ReportFragment::Plain(line));
    }
    report
}
