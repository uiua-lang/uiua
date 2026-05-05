use std::fmt;

use serde::*;

#[cfg(feature = "multivector")]
pub use crate::multivector::*;

/// A geometric algebra flavor
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub enum Flavor {
    /// Cl(n, 0, 0)
    #[default]
    #[serde(rename = "v")]
    Vanilla,
    /// Cl(n, 0, 1)
    #[serde(rename = "p")]
    Projective,
    /// Cl(n+1, 1, 0)
    #[serde(rename = "c")]
    Conformal,
    /// Cl(1, n, 0)
    #[serde(rename = "s")]
    Spacetime,
    /// Cl(a, b, c)
    #[serde(rename = "cl")]
    Cl(u8, u8, u8),
}
impl Flavor {
    pub const NULL: Self = Flavor::Cl(0, 0, u8::MAX);
    #[doc(hidden)]
    pub fn metric(&self, mut index: u8) -> i32 {
        match self {
            Flavor::Vanilla => 1,
            Flavor::Projective => index.min(1) as i32,
            Flavor::Conformal => 2 * index.min(1) as i32 - 1,
            Flavor::Spacetime => 1 - 2 * index.min(1) as i32,
            &Flavor::Cl(_p1, n1, z) => {
                if index < z {
                    return 0;
                }
                index -= z;
                if index < n1 {
                    return -1;
                }
                1
            }
        }
    }
    pub fn try_from_str(s: &str) -> Option<Result<Self, &'static str>> {
        Some(Ok(match s {
            "VGA" => Flavor::Vanilla,
            "PGA" => Flavor::Projective,
            "CGA" => Flavor::Conformal,
            "SGA" => Flavor::Spacetime,
            s => {
                let s = s.strip_prefix("Cl ")?;
                let mut iter = s.split_whitespace();
                let mut next = || iter.next().and_then(|s| s.parse::<u8>().ok());
                let fl = next()
                    .and_then(|p1| next().and_then(|n1| next().map(|z| Flavor::Cl(p1, n1, z))))
                    .filter(|_| iter.next().is_none())
                    .ok_or("Invalid GA spec");
                match fl {
                    Ok(fl) => fl,
                    Err(e) => return Some(Err(e)),
                }
            }
        }))
    }
}

pub const MAX_DIMS: u8 = 15;
pub const MAX_BLADES: u8 = 225;

impl fmt::Display for Flavor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Flavor::Vanilla => write!(f, "VGA"),
            Flavor::Projective => write!(f, "PGA"),
            Flavor::Conformal => write!(f, "CGA"),
            Flavor::Spacetime => write!(f, "SGA"),
            Flavor::Cl(p1, n1, z) => write!(f, "Cl {p1} {n1} {z}"),
        }
    }
}
