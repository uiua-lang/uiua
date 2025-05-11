use std::{collections::HashMap, fmt};

use enum_iterator::Sequence;
use once_cell::sync::Lazy;

use crate::{ast::NumWord, Complex, SysOp};

use super::Primitive;

static ALIASES: Lazy<HashMap<Primitive, &[&str]>> = Lazy::new(|| {
    [
        (Primitive::Identity, &["id"] as &[_]),
        (Primitive::Gap, &["ga"]),
        (Primitive::Pop, &["po"]),
        (Primitive::Fix, &["fx"]),
        (Primitive::Box, &["bx"]),
        (Primitive::IndexOf, &["idx"]),
        (Primitive::ProgressiveIndexOf, &["pidx"]),
        (Primitive::Switch, &["sw"]),
        (Primitive::Stencil, &["st", "win"]),
        (Primitive::Floor, &["flr", "flor"]),
        (Primitive::Range, &["ran"]),
        (Primitive::Partition, &["par"]),
        (Primitive::Dup, &["dup"]),
        (Primitive::Deshape, &["flat"]),
        (Primitive::Ne, &["ne", "neq"]),
        (Primitive::Eq, &["eq"]),
        (Primitive::Lt, &["lt"]),
        (Primitive::Le, &["le", "leq"]),
        (Primitive::Gt, &["gt"]),
        (Primitive::Ge, &["ge", "geq"]),
        (Primitive::Utf8, &["utf", "utf__8"]),
        (Primitive::First, &["fst"]),
        (Primitive::Last, &["lst"]),
        (Primitive::Slf, &["slf"]),
        (Primitive::Select, &["sel"]),
        (Primitive::Voxels, &["project"]),
    ]
    .into()
});

/// A numeric syntax component that can be parsed along with primitive names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum NumComponent {
    /// Euler's number
    E,
    /// The real complex unit
    R,
    /// The imaginary complex unit
    I,
}

/// A parsed primitive component
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum PrimComponent {
    /// A primitive
    Prim(Primitive),
    /// A numeric component
    Num(NumComponent),
}

impl From<Primitive> for PrimComponent {
    fn from(prim: Primitive) -> Self {
        PrimComponent::Prim(prim)
    }
}

impl From<NumComponent> for PrimComponent {
    fn from(num: NumComponent) -> Self {
        PrimComponent::Num(num)
    }
}

impl PrimComponent {
    /// Get the name of this component
    pub fn name(&self) -> &'static str {
        match self {
            PrimComponent::Prim(prim) => prim.name(),
            PrimComponent::Num(num) => num.name(),
        }
    }
    /// Try to parse a component from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        (Primitive::from_format_name(name).map(Into::into))
            .or_else(|| NumComponent::from_format_name(name).map(Into::into))
    }
}

impl NumComponent {
    /// Get the name of this numeric component
    pub fn name(&self) -> &'static str {
        use NumComponent::*;
        match self {
            E => "e",
            R => "r",
            I => "i",
        }
    }
    /// Get the number value
    pub fn value(&self) -> NumWord {
        use NumComponent::*;
        match self {
            E => std::f64::consts::E.into(),
            R => Complex::ONE.into(),
            I => Complex::I.into(),
        }
    }
    /// Try to parse a numeric component from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        use NumComponent::*;
        Some(match name {
            "e" => E,
            "r" => R,
            "i" => I,
            _ => return None,
        })
    }
}

impl fmt::Display for NumComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Display for PrimComponent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimComponent::Prim(prim) => prim.fmt(f),
            PrimComponent::Num(num) => num.fmt(f),
        }
    }
}

impl Primitive {
    /// Get the short aliases for this primitive
    pub fn aliases(&self) -> &'static [&'static str] {
        ALIASES.get(self).copied().unwrap_or_default()
    }
    /// Try to parse a primitive from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        if name.chars().any(char::is_uppercase) {
            return None;
        }
        if name.len() < 2 {
            return None;
        }
        static REVERSE_ALIASES: Lazy<HashMap<&'static str, Primitive>> = Lazy::new(|| {
            ALIASES
                .iter()
                .flat_map(|(prim, aliases)| aliases.iter().map(|&s| (s, *prim)))
                .collect()
        });
        if let Some(prim) = REVERSE_ALIASES.get(name) {
            return Some(*prim);
        }
        if let Some(prim) = Primitive::non_deprecated().find(|p| p.name() == name) {
            return Some(prim);
        }
        if let Some(prim) = Primitive::all().find(|p| p.glyph().is_none() && p.name() == name) {
            return Some(prim);
        }
        if let Some(prim) = SysOp::ALL.iter().find(|s| s.name() == name) {
            return Some(Primitive::Sys(*prim));
        }
        if name.len() < 3 {
            return None;
        }
        let mut matching = Primitive::non_deprecated()
            .filter(|p| p.glyph().is_some() && p.name().starts_with(name));
        let res = matching.next()?;
        let exact_match = res.name() == name;
        (exact_match || matching.next().is_none()).then_some(res)
    }
    /// The list of strings where each character maps to an entire primitive
    pub fn multi_aliases() -> &'static [(&'static str, &'static [(Primitive, &'static str)])] {
        use Primitive::*;
        &[
            ("kork", &[(Keep, "k"), (On, "o"), (Rows, "r"), (Keep, "k")]),
            ("rkok", &[(Rows, "r"), (Keep, "k"), (On, "o"), (Keep, "k")]),
            ("awm", &[(Assert, "a"), (With, "w"), (Match, "m")]),
            ("dor", &[(Div, "d"), (On, "o"), (Range, "r")]),
            (
                "pbbn",
                &[(Partition, "p"), (Box, "b"), (By, "b"), (Ne, "n")],
            ),
            (
                "ppbn",
                &[(Partition, "p"), (Parse, "p"), (By, "b"), (Ne, "n")],
            ),
            (
                "pibn",
                &[(Partition, "p"), (Identity, "i"), (By, "b"), (Ne, "n")],
            ),
            ("kbn", &[(Keep, "k"), (By, "b"), (Ne, "n")]),
            ("ftd", &[(Fork, "f"), (Take, "t"), (Drop, "d")]),
            ("fdt", &[(Fork, "f"), (Drop, "d"), (Take, "t")]),
            ("dnod", &[(Drop, "d"), (Neg, "n"), (On, "o"), (Drop, "d")]),
            (
                "adnoad",
                &[
                    (Anti, "a"),
                    (Drop, "d"),
                    (Neg, "n"),
                    (On, "o"),
                    (Anti, "a"),
                    (Drop, "d"),
                ],
            ),
            ("perf", &[(Dip, "p"), (Pop, "e"), (Under, "r"), (Now, "f")]),
            ("wrench", &[(Sub, "wr"), (By, "en"), (Not, "ch")]),
        ]
    }
    /// Look up a multi-alias from [`Self::multi_aliases`]
    pub fn get_multi_alias(name: &str) -> Option<&'static [(Primitive, &'static str)]> {
        Self::multi_aliases()
            .iter()
            .find(|(alias, _)| *alias == name)
            .map(|(_, aliases)| *aliases)
    }
}

/// Try to parse multiple primitives from the concatenation of their name prefixes
pub fn split_name(name: &str) -> Option<Vec<(PrimComponent, &str)>> {
    let mut indices: Vec<usize> = name.char_indices().map(|(i, _)| i).collect();
    indices.push(name.len());
    // Forward parsing
    let mut prims = Vec::new();
    let mut start = 0;
    'outer: loop {
        if start == indices.len() {
            return Some(prims);
        }
        let start_index = indices[start];
        for len in (1..=indices.len() - start).rev() {
            let end_index = indices.get(start + len).copied().unwrap_or(name.len());
            if start_index == end_index {
                continue;
            }
            let sub_name = &name[start_index..end_index];
            // Normal primitive matching
            if let Some(p) = PrimComponent::from_format_name(sub_name) {
                if sub_name.len() > 1 || p.name().len() == 1 {
                    prims.push((p, sub_name));
                    start += len;
                    continue 'outer;
                }
            }
            // Greek
            if sub_name.chars().count() == 1 {
                for prim in [Primitive::Eta, Primitive::Pi, Primitive::Tau] {
                    if sub_name.chars().next() == prim.glyph() {
                        prims.push((prim.into(), sub_name));
                        start += len;
                        continue 'outer;
                    }
                }
            }
            if sub_name.len() == 1 {
                continue;
            }
            // 1-letter planet notation
            if sub_name
                .strip_prefix('f')
                .unwrap_or(sub_name)
                .strip_suffix(['i', 'p'])
                .unwrap_or(sub_name)
                .chars()
                .all(|c| "gd".contains(c))
                && sub_name != "fi"
            {
                for (i, c) in sub_name.char_indices() {
                    let prim = match c {
                        'f' => Primitive::Fork,
                        'g' => Primitive::Gap,
                        'd' => Primitive::Dip,
                        'i' => Primitive::Identity,
                        'p' => Primitive::Pop,
                        _ => unreachable!(),
                    };
                    prims.push((prim.into(), &sub_name[i..i + 1]))
                }
                start += len;
                continue 'outer;
            }
            // Dip fix
            if sub_name
                .strip_suffix('f')
                .unwrap_or(sub_name)
                .chars()
                .all(|c| c == 'd')
            {
                for (i, c) in sub_name.char_indices() {
                    let prim = match c {
                        'd' => Primitive::Dip,
                        'f' => Primitive::Fix,
                        _ => unreachable!(),
                    };
                    prims.push((prim.into(), &sub_name[i..i + 1]))
                }
                start += len;
                continue 'outer;
            }
            // Aliases
            if let Some(ps) = Primitive::get_multi_alias(sub_name) {
                prims.extend(ps.iter().map(|(p, s)| ((*p).into(), *s)));
                start += len;
                continue 'outer;
            }
        }
        break;
    }
    // Backward parsing
    prims.clear();
    let mut end = indices.len() - 1;
    'outer: loop {
        if end == 0 {
            prims.reverse();
            return Some(prims);
        }
        let end_index = indices[end];
        for len in (1..=end).rev() {
            let start_index = indices.get(end - len).copied().unwrap_or(0);
            let sub_name = &name[start_index..end_index];
            // Normal primitive matching
            if let Some(p) = Primitive::from_format_name(sub_name) {
                if sub_name.len() > 1 || p.name().len() == 1 {
                    prims.push((p.into(), sub_name));
                    end -= len;
                    continue 'outer;
                }
            }
            // Greek
            if sub_name.chars().count() == 1 {
                for prim in [Primitive::Eta, Primitive::Pi, Primitive::Tau] {
                    if sub_name.chars().next() == prim.glyph() {
                        prims.push((prim.into(), sub_name));
                        start += len;
                        continue 'outer;
                    }
                }
            }
            if sub_name.len() == 1 {
                continue;
            }
            // 1-letter planet notation
            if sub_name
                .strip_prefix('f')
                .unwrap_or(sub_name)
                .strip_suffix(['i', 'p'])
                .unwrap_or(sub_name)
                .chars()
                .all(|c| "gd".contains(c))
                && sub_name != "fi"
            {
                for (i, c) in sub_name.char_indices().rev() {
                    let prim = match c {
                        'f' => Primitive::Fork,
                        'g' => Primitive::Gap,
                        'd' => Primitive::Dip,
                        'i' => Primitive::Identity,
                        'p' => Primitive::Pop,
                        _ => unreachable!(),
                    };
                    prims.push((prim.into(), &sub_name[i..i + 1]))
                }
                end -= len;
                continue 'outer;
            }
            // Dip fix
            if sub_name
                .strip_suffix('f')
                .unwrap_or(sub_name)
                .chars()
                .all(|c| c == 'd')
            {
                for (i, c) in sub_name.char_indices().rev() {
                    let prim = match c {
                        'd' => Primitive::Dip,
                        'f' => Primitive::Fix,
                        _ => unreachable!(),
                    };
                    prims.push((prim.into(), &sub_name[i..i + 1]))
                }
                end -= len;
                continue 'outer;
            }
            // Aliases
            if let Some(ps) = Primitive::get_multi_alias(sub_name) {
                prims.extend(ps.iter().rev().map(|(p, s)| ((*p).into(), *s)));
                end -= len;
                continue 'outer;
            }
            // Greek
            if sub_name.chars().count() == 1 {
                for prim in [Primitive::Eta, Primitive::Pi, Primitive::Tau] {
                    if sub_name.chars().next() == prim.glyph() {
                        prims.push((prim.into(), sub_name));
                        end -= len;
                        continue 'outer;
                    }
                }
            }
        }
        break;
    }
    None
}
