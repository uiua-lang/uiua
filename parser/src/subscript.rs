use std::fmt;

use serde::*;

use crate::SUBSCRIPT_DIGITS;

/// A subscripts
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(default)]
pub struct Subscript<N = NumericSubscript> {
    /// The numeric part of the subscript
    #[serde(skip_serializing_if = "Option::is_none")]
    pub num: Option<N>,
    /// The sided part of the subscript
    #[serde(skip_serializing_if = "Option::is_none")]
    pub side: Option<SidedSubscript>,
}

impl<N> Default for Subscript<N> {
    fn default() -> Self {
        Self {
            num: None,
            side: None,
        }
    }
}

impl From<i32> for Subscript {
    fn from(i: i32) -> Self {
        Subscript {
            num: Some(i.into()),
            side: None,
        }
    }
}

/// The numeric part of a subscript
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum NumericSubscript {
    /// Only a negative sign
    NegOnly,
    /// The number is too large to be represented
    TooLarge,
    /// A valid number
    #[serde(untagged)]
    N(i32),
}

impl From<i32> for NumericSubscript {
    fn from(i: i32) -> Self {
        NumericSubscript::N(i)
    }
}

/// The sided part of a subscript
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SidedSubscript {
    /// The side
    pub side: SubSide,
    /// An additional quantifying number
    #[serde(skip_serializing_if = "Option::is_none")]
    pub n: Option<usize>,
}

/// A sided subscript
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum SubSide {
    Left,
    Right,
}

impl Subscript {
    /// Make a pure numeric subscript
    pub fn numeric(i: i32) -> Self {
        Self {
            num: Some(NumericSubscript::N(i)),
            side: None,
        }
    }
    /// Check if the subscript is useable
    pub fn is_useable(&self) -> bool {
        matches!(self.num, Some(NumericSubscript::N(_))) || self.side.is_some()
    }
    /// Get the numeric part of the subscript as an integer, if it exists
    pub fn n(&self) -> Option<i32> {
        self.num.and_then(|n| match n {
            NumericSubscript::N(n) => Some(n),
            _ => None,
        })
    }
}

impl<N> Subscript<N> {
    /// Map the numeric part of the subscript
    pub fn map_num<M>(self, f: impl FnOnce(N) -> M) -> Subscript<M> {
        Subscript {
            num: self.num.map(f),
            side: self.side,
        }
    }
}

impl fmt::Display for SubSide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SubSide::Left => write!(f, "⌞"),
            SubSide::Right => write!(f, "⌟"),
        }
    }
}

impl fmt::Display for NumericSubscript {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumericSubscript::NegOnly => write!(f, "₋"),
            NumericSubscript::N(n) => {
                if *n < 0 {
                    write!(f, "₋")?;
                }
                for c in n.abs().to_string().chars() {
                    write!(f, "{}", SUBSCRIPT_DIGITS[(c as u32 as u8 - b'0') as usize])?;
                }
                Ok(())
            }
            NumericSubscript::TooLarge => write!(f, "…"),
        }
    }
}

impl fmt::Display for SidedSubscript {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.side.fmt(f)?;
        if let Some(n) = self.n {
            for c in n.to_string().chars() {
                write!(f, "{}", SUBSCRIPT_DIGITS[(c as u32 as u8 - b'0') as usize])?;
            }
        }
        Ok(())
    }
}

impl<N: fmt::Display> fmt::Display for Subscript<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.num.is_none() && self.side.is_none() {
            return write!(f, ",");
        };
        if let Some(num) = &self.num {
            num.fmt(f)?;
        }
        if let Some(side) = self.side {
            side.fmt(f)?;
        }
        Ok(())
    }
}
