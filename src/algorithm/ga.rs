//! Geometric Algebra

use serde::*;

use crate::is_default;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[serde(default)]
pub struct GeoSpace {
    #[serde(skip_serializing_if = "Option::is_none", rename = "d")]
    pub dims: Option<u8>,
    #[serde(skip_serializing_if = "is_default", rename = "f")]
    pub flavor: GaFlavor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum GaFlavor {
    #[default]
    #[serde(rename = "v")]
    Vanilla,
    #[serde(rename = "p")]
    Projective,
}

impl GaFlavor {
    fn metric(&self) -> [f64; 3] {
        match self {
            GaFlavor::Vanilla => [1.0, 1.0, 1.0],
            GaFlavor::Projective => [1.0, 1.0, 0.0],
        }
    }
}
