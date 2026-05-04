use std::{fmt, ops::Neg};

use ecow::EcoString;
use serde::*;

use crate::{FormatSubscript, SUBSCRIPT_DIGITS};

/// A subscripts
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(default)]
pub struct Subscript<N = NumericSubscript<SubscriptNumber>> {
    /// The numeric part of the subscript
    #[serde(skip_serializing_if = "Option::is_none")]
    pub num: Option<N>,
    /// The sided part of the subscript
    #[serde(skip_serializing_if = "Option::is_none")]
    pub side: Option<SidedSubscript>,
}

pub type SubscriptToken = Subscript<NumericSubscriptToken>;

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

impl From<i32> for Subscript<i32> {
    fn from(i: i32) -> Self {
        Subscript {
            num: Some(i),
            side: None,
        }
    }
}

impl From<u32> for Subscript<u32> {
    fn from(i: u32) -> Self {
        Subscript {
            num: Some(i),
            side: None,
        }
    }
}

impl<N> From<SubSide> for Subscript<N> {
    fn from(side: SubSide) -> Self {
        Subscript {
            num: None,
            side: Some(SidedSubscript { side, n: None }),
        }
    }
}

/// The numeric part of a subscript
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", content = "value", rename_all = "snake_case")]
pub enum NumericSubscript<I = SubscriptNumber> {
    /// Only a negative sign
    NegOnly,
    /// The number is too large to be represented
    TooLarge(EcoString),
    /// A valid number
    #[serde(untagged)]
    N(I),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SubscriptNumber {
    Int(i32),
    I,
    NegI,
    R,
    NegR,
}
impl SubscriptNumber {
    /// Get the integer
    pub fn as_int(&self) -> Option<i32> {
        match self {
            SubscriptNumber::Int(i) => Some(*i),
            _ => None,
        }
    }
    /// Get a mutable reference to the integer
    pub fn as_int_mut(&mut self) -> Option<&mut i32> {
        match self {
            SubscriptNumber::Int(i) => Some(i),
            _ => None,
        }
    }
}

impl Neg for SubscriptNumber {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            SubscriptNumber::Int(i) => SubscriptNumber::Int(-i),
            SubscriptNumber::I => SubscriptNumber::NegI,
            SubscriptNumber::NegI => SubscriptNumber::I,
            SubscriptNumber::R => SubscriptNumber::NegR,
            SubscriptNumber::NegR => SubscriptNumber::R,
        }
    }
}

impl From<i32> for SubscriptNumber {
    fn from(i: i32) -> Self {
        SubscriptNumber::Int(i)
    }
}

pub type NumericSubscriptToken = NumericSubscript<Option<SubscriptNumber>>;

impl<I> From<I> for NumericSubscript<I> {
    fn from(i: I) -> Self {
        NumericSubscript::N(i)
    }
}

impl From<i32> for NumericSubscriptToken {
    fn from(i: i32) -> Self {
        NumericSubscript::N(Some(i.into()))
    }
}

impl From<i32> for NumericSubscript {
    fn from(i: i32) -> Self {
        NumericSubscript::N(i.into())
    }
}

impl From<Subscript> for SubscriptToken {
    fn from(sub: Subscript) -> Self {
        sub.map_num(|n| n.map(Some))
    }
}

impl From<i32> for SubscriptToken {
    fn from(i: i32) -> Self {
        Subscript::<NumericSubscript>::from(i).into()
    }
}

impl<I> NumericSubscript<I> {
    pub fn map<J>(self, f: impl FnOnce(I) -> J) -> NumericSubscript<J> {
        match self {
            NumericSubscript::NegOnly => NumericSubscript::NegOnly,
            NumericSubscript::TooLarge(s) => NumericSubscript::TooLarge(s),
            NumericSubscript::N(n) => NumericSubscript::N(f(n)),
        }
    }
}

impl<I: Clone> NumericSubscript<I> {
    pub fn i(&self) -> Option<I> {
        match self {
            NumericSubscript::N(i) => Some(i.clone()),
            _ => None,
        }
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

impl<I: Clone> Subscript<NumericSubscript<I>> {
    /// Make a pure numeric subscript
    pub fn numeric(i: I) -> Self {
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
    pub fn n(&self) -> Option<I> {
        self.num.as_ref().and_then(|n| match n {
            NumericSubscript::N(n) => Some(n.clone()),
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

impl fmt::Display for SubscriptNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SubscriptNumber::Int(n) => FormatSubscript(*n).fmt(f),
            SubscriptNumber::I => write!(f, "ᵢ"),
            SubscriptNumber::NegI => write!(f, "₋ᵢ"),
            SubscriptNumber::R => write!(f, "ᵣ"),
            SubscriptNumber::NegR => write!(f, "₋ᵣ"),
        }
    }
}

impl fmt::Display for NumericSubscript {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumericSubscript::NegOnly => write!(f, "₋"),
            NumericSubscript::N(n) => n.fmt(f),
            NumericSubscript::TooLarge(s) => s.fmt(f),
        }
    }
}

impl fmt::Display for NumericSubscriptToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NumericSubscript::NegOnly => write!(f, "₋"),
            NumericSubscript::N(n) => {
                if let Some(n) = n {
                    n.fmt(f)
                } else {
                    write!(f, "ₙ")
                }
            }
            NumericSubscript::TooLarge(s) => s.fmt(f),
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
