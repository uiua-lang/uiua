use std::{fmt, path::PathBuf};

use dashmap::DashMap;
use ecow::{EcoString, EcoVec};
use serde::*;

use crate::{CodeSpan, InputSrc, IntoInputSrc};

/// A repository of code strings input to the compiler
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Inputs {
    /// A map of file paths to their string contents
    #[serde(skip_serializing_if = "DashMap::is_empty")]
    pub files: DashMap<PathBuf, EcoString>,
    /// A list of input strings without paths
    #[serde(skip_serializing_if = "EcoVec::is_empty")]
    pub strings: EcoVec<EcoString>,
    /// A map of spans to macro strings
    #[serde(skip)]
    pub macros: DashMap<CodeSpan, EcoString>,
}

impl Inputs {
    /// Add an input source
    pub fn add_src(&mut self, src: impl IntoInputSrc, input: impl Into<EcoString>) -> InputSrc {
        let src = src.into_input_src(self.strings.len());
        match &src {
            InputSrc::File(path) => {
                self.files.insert(path.to_path_buf(), input.into());
            }
            InputSrc::Str(i) => {
                while self.strings.len() <= *i {
                    self.strings.push(EcoString::default());
                }
                self.strings.make_mut()[*i] = input.into();
            }
            InputSrc::Macro(span) => {
                self.macros.insert((**span).clone(), input.into());
            }
            InputSrc::Literal(_) => {}
        }
        src
    }
    /// Get an input string
    pub fn get(&self, src: &InputSrc) -> EcoString {
        match src {
            InputSrc::File(path) => self
                .files
                .get(&**path)
                .unwrap_or_else(|| panic!("File {path:?} not found"))
                .clone(),
            InputSrc::Str(index) => self
                .strings
                .get(*index)
                .unwrap_or_else(|| panic!("String {index} not found"))
                .clone(),
            InputSrc::Macro(span) => self
                .macros
                .get(span)
                .unwrap_or_else(|| panic!("Macro at {span} not found"))
                .clone(),
            InputSrc::Literal(s) => s.clone(),
        }
    }
    /// Get an input string and perform an operation on it
    #[track_caller]
    pub fn get_with<T>(&self, src: &InputSrc, f: impl FnOnce(&str) -> T) -> T {
        match src {
            InputSrc::File(path) => {
                if let Some(src) = self.files.get(&**path) {
                    f(&src)
                } else {
                    panic!(
                        "File {} not found. Available sources are {}",
                        path.display(),
                        self.available_srcs()
                    )
                }
            }
            InputSrc::Str(index) => {
                if let Some(src) = self.strings.get(*index) {
                    f(src)
                } else {
                    panic!(
                        "String {} not found. Available sources are {}",
                        index,
                        self.available_srcs()
                    )
                }
            }
            InputSrc::Macro(span) => {
                if let Some(src) = self.macros.get(span) {
                    f(src.value())
                } else {
                    panic!(
                        "Macro at {} not found. Available sources are {}",
                        span,
                        self.available_srcs()
                    )
                }
            }
            InputSrc::Literal(s) => f(s),
        }
    }
    fn available_srcs(&self) -> String {
        (self.files.iter().map(|e| e.key().display().to_string()))
            .chain(self.strings.iter().map(|i| format!("string {i}")))
            .collect::<Vec<_>>()
            .join(", ")
    }
    /// Get an input string and perform an operation on it
    pub fn try_get_with<T>(&self, src: &InputSrc, f: impl FnOnce(&str) -> T) -> Option<T> {
        match src {
            InputSrc::File(path) => self.files.get(&**path).map(|src| f(&src)),
            InputSrc::Str(index) => self.strings.get(*index).map(|src| f(src)),
            InputSrc::Macro(span) => self.macros.get(span).map(|src| f(&src)),
            InputSrc::Literal(s) => Some(f(s)),
        }
    }
}

/// Character counts for a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct BindingCounts {
    /// The number of characters
    pub char: usize,
    /// The number of SBCS bytes
    pub sbcs: usize,
}

impl fmt::Display for BindingCounts {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} character{}",
            self.char,
            if self.char == 1 { "" } else { "s" }
        )?;
        if self.sbcs != self.char {
            write!(f, " ({} SBCS)", self.sbcs)?;
        }
        Ok(())
    }
}
