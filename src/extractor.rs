use std::{fmt, sync::Arc};

use ecow::EcoVec;
use serde::*;
use tinyvec::TinyVec;

use crate::{Boxed, Ident, SigNode, Uiua, UiuaResult, Value};

pub fn extractor(ex: Extractor, op: Option<Arc<SigNode>>, env: &mut Uiua) -> UiuaResult {
    let target = env.pop("extractor target")?;
    match extract(&ex, target) {
        Ok((extracted, target)) => {
            if let Some(op) = op {
                if op.sig.args() > 1 {
                    let n = op.sig.args() - 1;
                    _ = env.remove_n(n, n)?;
                }
            }
            env.push(extracted);
            if let Some(target) = target {
                if ex.preserve {
                    env.push(target);
                } else if target.row_count() != 0 {
                    return Err(env.error("▷ extractor left array with items left over"));
                }
            } else if ex.preserve {
                return Err(env.error("◁ extractor was called on non box list"));
            }
            Ok(())
        }
        Err((e, target)) => {
            if let Some(op) = op.filter(|_| !e.hard) {
                if op.sig.args() > 0 {
                    env.push(target.clone());
                }
                env.exec(op)?;
                if let Some(label) = &ex.label {
                    let mut default = env.pop("default value")?;
                    default.meta.set_label(label.clone().into());
                    env.push(default);
                }
                if ex.preserve {
                    env.push(target);
                }
                Ok(())
            } else {
                Err(env.error(match e.kind {
                    ExErrorKind::NoListMatch => "Extractor found no match in the list".into(),
                    ExErrorKind::NoFragMatch => "No extractor fragment matches".into(),
                    ExErrorKind::Type(found) => {
                        let mut s = format!("Extractor {ex} expects ");
                        let types = unique_list(ex.frags.iter().filter_map(|f| f.ty));
                        format_list(&mut s, &types, |ty| ty.name());
                        s.push_str(", but the array is ");
                        s.push_str(found.name());
                        s
                    }
                    ExErrorKind::WrongRank(found) => {
                        let mut s = format!("Extractor {ex} expects rank ");
                        let ranks = unique_list(
                            (ex.frags.iter())
                                .filter_map(|f| f.shape.as_ref())
                                .map(|s| s.0.len()),
                        );
                        format_list(&mut s, &ranks, |rank| rank.to_string());
                        s.push_str(", but the array is rank ");
                        s.push_str(&found.to_string());
                        s
                    }
                    ExErrorKind::WrongDim(i, found) => {
                        let mut s = format!("Extractor {ex} expects axis {i} to have length ");
                        let lengths = unique_list(
                            (ex.frags.iter())
                                .filter_map(|f| f.shape.as_ref())
                                .filter_map(|s| s.0.get(i))
                                .filter_map(|d| match d {
                                    ExtractorDim::Dim(i) => Some(*i),
                                    _ => None,
                                }),
                        );
                        format_list(&mut s, &lengths, |len| len.to_string());
                        s.push_str(", but it is ");
                        s.push_str(&found.to_string());
                        s
                    }
                }))
            }
        }
    }
}

fn unique_list<T>(iter: impl IntoIterator<Item = T>) -> TinyVec<[T; 4]>
where
    T: Default + PartialEq,
{
    let mut list = TinyVec::<[_; 4]>::new();
    for item in iter {
        if !list.contains(&item) {
            list.push(item);
        }
    }
    list
}

fn format_list<T, S>(string: &mut String, items: &[T], format: impl Fn(&T) -> S)
where
    S: AsRef<str>,
{
    match items {
        [] => {}
        [item] => string.push_str(format(item).as_ref()),
        [a, b] => {
            string.push_str(format(a).as_ref());
            string.push_str(" or ");
            string.push_str(format(b).as_ref());
        }
        [items @ .., last] => {
            for item in items {
                string.push_str(format(item).as_ref());
                string.push_str(", ");
            }
            string.push_str("or ");
            string.push_str(format(last).as_ref());
        }
    }
}

fn extract(ex: &Extractor, target: Value) -> Result<(Value, Option<Value>), (ExError, Value)> {
    match target {
        Value::Box(mut arr) if arr.rank() == 1 => {
            // Extract by label
            if let Some(label) = &ex.label {
                for (i, Boxed(val)) in arr.data.iter().enumerate().rev() {
                    let label_matches = match &val.meta.label {
                        Some(vl) => label == vl,
                        None => label.is_empty(),
                    };
                    if label_matches {
                        return match extract_frags(ex, val) {
                            Err(e) if !label.is_empty() => Err((e.hard(true), arr.into())),
                            _ => {
                                let val = val.clone();
                                arr.remove_row(i);
                                Ok((val, Some(arr.into())))
                            }
                        };
                    }
                }
                if ex.frags.is_empty() {
                    return Err((ExErrorKind::NoListMatch.hard(false), arr.into()));
                }
            }
            // Extract by fragment
            for (i, Boxed(val)) in arr.data.iter().enumerate().rev() {
                if val.meta.label.is_some() && ex.label.is_some() {
                    continue;
                }
                if extract_frags(ex, val).is_ok() {
                    let val = val.clone();
                    arr.remove_row(i);
                    return Ok((val, Some(arr.into())));
                }
            }
            let val = arr.into();
            if ex.label.is_some() {
                Err((ExErrorKind::NoListMatch.hard(false), val))
            } else {
                match extract_frags(ex, &val) {
                    Ok(()) => Ok((val, None)),
                    Err(_) => Err((ExErrorKind::NoListMatch.hard(false), val)),
                }
            }
        }
        val => match extract_frags(ex, &val) {
            Ok(()) => Ok((val, None)),
            Err(e) => Err((e.hard(false), val)),
        },
    }
}

fn extract_frags(ex: &Extractor, val: &Value) -> Result<(), ExErrorKind> {
    let mut type_error = None;
    let mut shape_error = None;
    for frag in &ex.frags {
        match extract_frag(frag, val) {
            Ok(()) => return Ok(()),
            Err(e @ ExErrorKind::Type(_)) => type_error.get_or_insert(e),
            Err(e @ (ExErrorKind::WrongRank(_) | ExErrorKind::WrongDim(..))) => {
                shape_error.get_or_insert(e)
            }
            _ => continue,
        };
    }
    let error = if ex.shape_is_homogeneous() {
        shape_error.or(type_error)
    } else if ex.type_is_homogeneous() {
        type_error.or(shape_error)
    } else {
        type_error.or(shape_error).map(|_| ExErrorKind::NoFragMatch)
    };
    match error {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn extract_frag(frag: &ExtractorFrag, val: &Value) -> Result<(), ExErrorKind> {
    if let Some(expected) = frag.ty {
        let found = match val {
            Value::Byte(_) | Value::Num(_) => ExtractorType::Real,
            Value::Complex(_) => ExtractorType::Complex,
            Value::Char(_) => ExtractorType::Char,
            Value::Box(_) => ExtractorType::Box,
        };
        if expected != found {
            return Err(ExErrorKind::Type(found));
        }
    }
    if let Some(ExtractorShape(dims)) = &frag.shape {
        if let Some(ExtractorDim::MultiWildcard) = dims.first() {
            for ((i, dim), s) in dims.iter().enumerate().rev().zip(val.shape.iter().rev()) {
                if let ExtractorDim::Dim(dim) = dim {
                    if dim != s {
                        return Err(ExErrorKind::WrongDim(i, *s));
                    }
                }
            }
        } else if dims.len() != val.shape.len() {
            return Err(ExErrorKind::WrongRank(val.shape.len()));
        } else {
            for ((i, dim), s) in dims.iter().enumerate().zip(&val.shape) {
                if let ExtractorDim::Dim(dim) = dim {
                    if dim != s {
                        return Err(ExErrorKind::WrongDim(i, *s));
                    }
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
struct ExError {
    kind: ExErrorKind,
    hard: bool,
}

#[derive(Debug)]
enum ExErrorKind {
    NoListMatch,
    NoFragMatch,
    Type(ExtractorType),
    WrongRank(usize),
    WrongDim(usize, usize),
}
impl ExErrorKind {
    fn hard(self, hard: bool) -> ExError {
        ExError { kind: self, hard }
    }
}

/// A value extractor
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct Extractor {
    /// The label
    pub label: Option<Ident>,
    /// The fragments
    pub frags: EcoVec<ExtractorFrag>,
    /// Whether to preserve the extracted-from array on the stack
    pub preserve: bool,
}
impl fmt::Debug for Extractor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for Extractor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.preserve {
            write!(f, "◁")
        } else {
            write!(f, "▷")
        }?;
        if let Some(label) = &self.label {
            write!(f, "{label}")?;
            if self.frags.first().is_some_and(|frag| {
                frag.shape.as_ref().is_some_and(|s| s.0.is_empty())
                    || frag.shape.is_none() && frag.ty.is_some()
            }) {
                write!(f, ".")?;
            }
        }
        for (i, frag) in self.frags.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            frag.fmt(f)?;
        }
        Ok(())
    }
}
impl Extractor {
    /// Create a new extractor
    pub fn new(
        frags: impl IntoIterator<Item = ExtractorFrag>,
        preserve: bool,
        label: impl Into<Option<Ident>>,
    ) -> Self {
        Extractor {
            frags: frags.into_iter().collect(),
            preserve,
            label: label.into(),
        }
    }
    fn type_is_homogeneous(&self) -> bool {
        let mut iter = self.frags.iter().map(|f| f.ty);
        let Some(first) = iter.next() else {
            return true;
        };
        iter.all(|ty| ty == first)
    }
    fn shape_is_homogeneous(&self) -> bool {
        let mut iter = self.frags.iter().map(|f| f.shape.as_ref());
        let Some(first) = iter.next() else {
            return true;
        };
        iter.all(|s| s == first)
    }
}

/// A value extractor fragment
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ExtractorFrag {
    /// The shape of the extractor
    #[serde(skip_serializing_if = "Option::is_none")]
    pub shape: Option<ExtractorShape>,
    /// The type of the extractor
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ty: Option<ExtractorType>,
}
impl fmt::Debug for ExtractorFrag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}
impl fmt::Display for ExtractorFrag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
#[allow(missing_docs)]
pub enum ExtractorType {
    #[default]
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
            write!(f, "s")
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
    OrElse,
}

impl fmt::Display for ExtractorEnd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractorEnd::Error => Ok(()),
            ExtractorEnd::Or => write!(f, "."),
            ExtractorEnd::OrElse => write!(f, "?"),
        }
    }
}
