use std::fmt;

use colored::{Color, Colorize};
use serde::{Deserialize, Serialize};

use crate::{InputSrc, Inputs, Span};

/// A message to be displayed to the user that is not an error
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
pub enum ReportFragment {
    /// Just plain text
    Plain(String),
    /// Text colored according to the report kind
    Colored {
        text: String,
        kind: ReportKind,
    },
    /// Faint text
    Faint(String),
    /// Even fainter text
    Fainter(String),
    /// A newline
    Newline,
}

impl ReportFragment {
    /// Create a colored report fragment
    pub fn colored(text: impl Into<String>, kind: ReportKind) -> Self {
        Self::Colored {
            text: text.into(),
            kind,
        }
    }
}

/// A rich-text error/diagnostic report
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "camelCase")]
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
    /// Create a new report
    pub fn new(kind: ReportKind, message: impl Into<String>) -> Self {
        let message = message.into();
        let mut fragments = vec![
            ReportFragment::colored(kind.str(), kind),
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
            fragments.push(ReportFragment::colored(kind.str(), kind));
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
                fragments.push(ReportFragment::colored(color, kind));
                fragments.push(ReportFragment::Faint(post_color));
                fragments.push(ReportFragment::Newline);
                fragments.push(ReportFragment::Plain(
                    " ".repeat(line_prefix.chars().count()),
                ));
                fragments.push(ReportFragment::Plain(" ".repeat(start_char_pos as usize)));
                fragments.push(ReportFragment::colored(
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
    /// A report that tests have finished
    pub fn tests(successes: usize, failures: usize, not_run: usize) -> Self {
        let mut fragments = if successes == 0 && not_run == 0 {
            if failures == 0 {
                vec![]
            } else {
                vec![ReportFragment::colored(
                    match failures {
                        1 => "Test failed".into(),
                        2 => "Both tests failed".into(),
                        _ => format!("All {failures} tests failed"),
                    },
                    ReportKind::Error,
                )]
            }
        } else {
            let mut fragments = vec![ReportFragment::colored(
                match (successes, failures) {
                    (1, 0) if not_run == 0 => "Test passed".into(),
                    (2, 0) if not_run == 0 => "Both tests passed".into(),
                    (suc, 0) if not_run == 0 => format!("All {suc} tests passed"),
                    (suc, _) => {
                        format!("{suc} test{} passed", if suc == 1 { "" } else { "s" })
                    }
                },
                DiagnosticKind::Info.into(),
            )];
            if failures > 0 {
                fragments.extend([
                    ReportFragment::Plain(", ".into()),
                    ReportFragment::colored(format!("{failures} failed"), ReportKind::Error),
                ])
            }
            fragments
        };
        if not_run > 0 {
            if fragments.is_empty() {
                fragments.push(ReportFragment::colored(
                    format!("0 of {not_run} tests ran"),
                    ReportKind::Error,
                ));
            } else {
                fragments.push(ReportFragment::Plain(", ".into()));
                fragments.push(ReportFragment::colored(
                    format!("{not_run} didn't run"),
                    DiagnosticKind::Warning.into(),
                ));
            }
        }
        Report {
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
                ReportFragment::Colored { text: s, kind } => {
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
