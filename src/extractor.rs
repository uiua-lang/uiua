use std::{fmt, sync::Arc};

use ecow::EcoVec;
use serde::*;

use crate::{Boxed, SigNode, Uiua, UiuaResult, Value};

pub fn extractor(ex: Extractor, op: Option<Arc<SigNode>>, env: &mut Uiua) -> UiuaResult {
    let target = env.pop("extractor target")?;
    match extract(&ex, target) {
        Ok((extracted, _)) => {
            env.push(extracted);
            Ok(())
        }
        Err((e, target)) => {
            if let Some(op) = op {
                if op.sig.args() > 0 {
                    env.push(target);
                }
                env.exec(op)
            } else {
                Err(env.error(match e {
                    ExtractionError::NoMatch => "Extractor found no match in the list".into(),
                    ExtractionError::Type(expected, found) => {
                        format!(
                            "Extractor {ex} expects {} but the array is {}",
                            expected.name(),
                            found.name()
                        )
                    }
                    ExtractionError::WrongRank(expected, found) => format!(
                        "Extractor {ex} expects rank {expected} \
                        but the array is rank {found}"
                    ),
                    ExtractionError::WrongDim(i, expected, found) => format!(
                        "Extractor {ex} expected axis {i} to have \
                        length {expected}, it is {found}"
                    ),
                }))
            }
        }
    }
}

fn extract(
    ex: &Extractor,
    target: Value,
) -> Result<(Value, Option<Value>), (ExtractionError, Value)> {
    match target {
        Value::Box(mut arr) if arr.rank() == 1 => {
            for (i, Boxed(val)) in arr.data.iter().enumerate() {
                if extract_base(ex, val).is_ok() {
                    let val = val.clone();
                    arr.remove_row(i);
                    return Ok((val, Some(arr.into())));
                }
            }
            let val = arr.into();
            match extract_base(ex, &val) {
                Ok(()) => Ok((val, None)),
                Err(_) => Err((ExtractionError::NoMatch, val)),
            }
        }
        val => match extract_base(ex, &val) {
            Ok(()) => Ok((val, None)),
            Err(e) => Err((e, val)),
        },
    }
}

fn extract_base(ex: &Extractor, val: &Value) -> Result<(), ExtractionError> {
    if let Some(expected) = ex.ty {
        let found = match val {
            Value::Byte(_) | Value::Num(_) => ExtractorType::Real,
            Value::Complex(_) => ExtractorType::Complex,
            Value::Char(_) => ExtractorType::Char,
            Value::Box(_) => ExtractorType::Box,
        };
        if expected != found {
            return Err(ExtractionError::Type(expected, found));
        }
    }
    if let Some(ExtractorShape(dims)) = &ex.shape {
        if dims.iter().all(|dim| matches!(dim, ExtractorDim::Dim(_))) {
            if dims.len() != val.shape.len() {
                return Err(ExtractionError::WrongRank(dims.len(), val.shape.len()));
            }
            for (i, dim) in dims.iter().enumerate() {
                let &ExtractorDim::Dim(dim) = dim else {
                    unreachable!()
                };
                if dim != val.shape[i] {
                    return Err(ExtractionError::WrongDim(i, dim, val.shape[i]));
                }
            }
        }
    }
    Ok(())
}

enum ExtractionError {
    NoMatch,
    Type(ExtractorType, ExtractorType),
    WrongRank(usize, usize),
    WrongDim(usize, usize, usize),
}

/// A value extractor
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Extractor {
    /// The shape of the extractor
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shape: Option<ExtractorShape>,
    /// The type of the extractor
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ty: Option<ExtractorType>,
}
impl fmt::Debug for Extractor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for Extractor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "⌁")?;
        if let Some(shape) = &self.shape {
            shape.fmt(f)?;
        }
        if let Some(ty) = &self.ty {
            ty.fmt(f)?;
        }
        Ok(())
    }
}

/// A type for an extractor
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum ExtractorType {
    #[serde(rename = "r")]
    Real,
    #[serde(rename = "a")]
    Char,
    #[serde(rename = "b")]
    Box,
    #[serde(rename = "c")]
    Complex,
}
impl fmt::Debug for ExtractorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for ExtractorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractorType::Real => write!(f, "ℝ"),
            ExtractorType::Char => write!(f, "@"),
            ExtractorType::Box => write!(f, "□"),
            ExtractorType::Complex => write!(f, "ℂ"),
        }
    }
}
impl ExtractorType {
    /// Get a word name for the type
    pub fn name(&self) -> &'static str {
        match self {
            ExtractorType::Real => "numbers",
            ExtractorType::Char => "characters",
            ExtractorType::Box => "boxes",
            ExtractorType::Complex => "complexes",
        }
    }
}

/// A shape for an extractor
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ExtractorShape(pub EcoVec<ExtractorDim>);
impl fmt::Debug for ExtractorShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for ExtractorShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            write!(f, "∙")
        } else {
            for (i, dim) in self.0.iter().enumerate() {
                if i > 0 {
                    write!(f, "×")?;
                }
                dim.fmt(f)?;
            }
            Ok(())
        }
    }
}

/// A dimension in an extractor shape
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ExtractorDim {
    /// Any dimension size
    #[serde(rename = "W")]
    Wildcard,
    /// Multiple wildcards
    #[serde(rename = "M")]
    MultiWildcard,
    /// An exact dimension
    #[serde(untagged)]
    Dim(usize),
}
impl fmt::Debug for ExtractorDim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for ExtractorDim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractorDim::Wildcard => write!(f, "*"),
            ExtractorDim::MultiWildcard => write!(f, "⁑"),
            ExtractorDim::Dim(d) => d.fmt(f),
        }
    }
}

/// What to do when an extractor completes
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub enum ExtractorEnd {
    #[default]
    Error,
    Or,
}

impl fmt::Display for ExtractorEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractorEnd::Error => Ok(()),
            ExtractorEnd::Or => write!(f, "."),
        }
    }
}
