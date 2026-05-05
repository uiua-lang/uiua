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
    /// Cl(0, 0, n)
    #[serde(rename = "n")]
    Null,
}
impl Flavor {
    #[doc(hidden)]
    pub fn metric(&self, index: u8) -> i32 {
        match self {
            Flavor::Vanilla => 1,
            Flavor::Null => 0,
            Flavor::Projective => index.min(1) as i32,
            Flavor::Conformal => 2 * index.min(1) as i32 - 1,
            Flavor::Spacetime => 1 - 2 * index.min(1) as i32,
        }
    }
}

pub const MAX_DIMS: u8 = 15;
pub const MAX_BLADES: u8 = 225;
