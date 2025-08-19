use std::{collections::HashMap, fmt, sync::LazyLock};

use enum_iterator::Sequence;

use crate::{ast::NumWord, Complex, SysOp};

use super::Primitive;

static ALIASES: LazyLock<HashMap<Primitive, &[&str]>> = LazyLock::new(|| {
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
        (Primitive::Utf8, &["utf"]),
        (Primitive::First, &["fst"]),
        (Primitive::Last, &["lst"]),
        (Primitive::Slf, &["slf"]),
        (Primitive::Select, &["sel"]),
        (Primitive::Parse, &["prs"]),
        (Primitive::Rand, &["rnd"]),
        (Primitive::Voxels, &["project"]),
        (Primitive::Evert, &["ev"]),
    ]
    .into()
});

/// A numeric syntax component that can be parsed along with primitive names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum NumComponent {
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
    /// Subscript 0
    Sub0,
    /// Subscript 2
    Sub2,
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
            PrimComponent::Sub0 => "₀",
            PrimComponent::Sub2 => "₂",
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
            R => "r",
            I => "i",
        }
    }
    /// Get the number value
    pub fn value(&self) -> NumWord {
        use NumComponent::*;
        match self {
            R => Complex::ONE.into(),
            I => Complex::I.into(),
        }
    }
    /// Try to parse a numeric component from a name prefix
    pub fn from_format_name(name: &str) -> Option<Self> {
        use NumComponent::*;
        Some(match name {
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
            PrimComponent::Sub0 | PrimComponent::Sub2 => self.name().fmt(f),
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
        static REVERSE_ALIASES: LazyLock<HashMap<&'static str, Primitive>> = LazyLock::new(|| {
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
    pub fn multi_aliases() -> &'static [(&'static str, &'static [(PrimComponent, &'static str)])] {
        use Primitive::*;
        macro_rules! alias {
            ($(($s:ident, $prim:expr)),* $(,)*) => {
                (
                    concat!($(stringify!($s)),*),
                    &[$((PrimComponent::Prim($prim), stringify!($s))),*]
                )
            }
        }
        &[
            alias!((a, Assert), (w, With), (m, Match)),
            alias!((d, Div), (o, On), (r, Range)),
            alias!((p, Partition), (b, Box), (b, By), (n, Ne)),
            alias!((p, Partition), (p, Parse), (b, By), (n, Ne)),
            alias!((p, Partition), (i, Identity), (b, By), (n, Ne)),
            alias!((k, Keep), (b, By), (n, Ne)),
            alias!((f, Fork), (t, Take), (d, Drop)),
            alias!((f, Fork), (d, Drop), (t, Take)),
            alias!((d, Drop), (n, Neg), (o, On), (d, Drop)),
            alias!(
                (a, Anti),
                (d, Drop),
                (n, Neg),
                (o, On),
                (a, Anti),
                (d, Drop)
            ),
            alias!((p, Dip), (e, Pop), (r, Under), (f, Now)),
            alias!((s, Un), (et, By)),
            alias!((wr, Sub), (en, By), (ch, Not)),
            alias!((sel, Select), (first, First)),
            alias!((l, Un), (og, Exp)),
            (
                "kork",
                &[
                    (PrimComponent::Prim(Keep), "kor"),
                    (PrimComponent::Sub2, "k"),
                ],
            ),
            (
                "each",
                &[
                    (PrimComponent::Prim(Rows), "eac"),
                    (PrimComponent::Sub0, "h"),
                ],
            ),
        ]
    }
    /// Look up a multi-alias from [`Self::multi_aliases`]
    pub fn get_multi_alias(name: &str) -> Option<&'static [(PrimComponent, &'static str)]> {
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
            let unforked = sub_name.strip_prefix('f').unwrap_or(sub_name);
            if unforked
                .strip_suffix(['i', 'p', 'f'])
                .unwrap_or(unforked)
                .chars()
                .all(|c| "gd".contains(c))
            {
                for (i, c) in sub_name.char_indices() {
                    let prim = match c {
                        'f' if i == 0 => Primitive::Fork,
                        'f' => Primitive::Fix,
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
            // Aliases
            if let Some(ps) = Primitive::get_multi_alias(sub_name) {
                prims.extend(ps.iter().map(|(p, s)| (*p, *s)));
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
                        end -= len;
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
                prims.extend(ps.iter().rev().map(|(p, s)| (*p, *s)));
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
