use std::{cell::RefCell, collections::HashMap, fmt, iter::repeat_n};

use serde::*;

#[cfg(feature = "multivector")]
pub use crate::multivector::*;

pub const MAX_DIMS: u8 = 15;
pub const MAX_BLADES: u8 = 225;

/// Get the size of a grade for some number of dimensions
pub fn grade_size(dims: u8, grade: u8) -> usize {
    fn combinations(n: usize, k: usize) -> f64 {
        if k > n {
            return 0.0;
        }
        (1..=k.min(n - k))
            .map(|i| (n + 1 - i) as f64 / i as f64)
            .product::<f64>()
            .round()
    }
    combinations(dims as usize, grade as usize) as usize
}

/// Iterate over the grades of each blade
pub fn blade_grades(dims: u8) -> impl Iterator<Item = u8> {
    (0..=dims).flat_map(move |i| repeat_n(i, grade_size(dims, i)))
}

type MaskTables = (&'static [usize], &'static [usize]);
#[doc(hidden)]
pub fn mask_tables(dims: u8) -> MaskTables {
    thread_local! {
        static CACHE: RefCell<HashMap<u8, MaskTables>> = Default::default();
    }
    CACHE.with(move |r| {
        *r.borrow_mut().entry(dims).or_insert_with(|| {
            let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
            for d in (0..dims).rev() {
                let dim_mask = 1 << d;
                mask_table.sort_by_key(|&a| u32::MAX - (a & dim_mask).count_ones());
            }
            mask_table.sort_by_key(|&a| a.count_ones());
            let mut inv_mask_table = vec![0; 1usize << dims];
            for (i, &v) in mask_table.iter().enumerate() {
                inv_mask_table[v] = i;
            }
            (mask_table.leak(), inv_mask_table.leak())
        })
    })
}

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
