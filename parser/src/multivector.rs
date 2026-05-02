use std::ops::*;

use ecow::EcoVec;

#[derive(Debug, Clone, Default)]
pub struct Multivector {
    coefs: EcoVec<f64>,
    flavor: Flavor,
    dims: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Flavor {
    #[default]
    Vanilla,
    Projective,
}
impl Flavor {
    pub fn metric(&self, index: usize) -> usize {
        match self {
            Flavor::Vanilla => 1,
            Flavor::Projective => {
                if index == 0 {
                    0
                } else {
                    1
                }
            }
        }
    }
}

impl Multivector {
    pub fn from_all(coefs: impl Into<EcoVec<f64>>, flavor: Flavor) -> Self {
        let coefs = coefs.into();
        assert!(
            coefs.len().max(1).is_power_of_two(),
            "Must have power of 2 number of coefficients, but there are {}",
            coefs.len()
        );
        let dims = (coefs.len().max(1) as f64).log2() as u8;
        Multivector {
            coefs,
            flavor,
            dims,
        }
    }
    pub fn from_even(coefs: impl Into<EcoVec<f64>>, flavor: Flavor) -> Self {
        let coefs = coefs.into();
        assert!(
            coefs.len().max(1).is_power_of_two(),
            "Must have power of 2 number of coefficients, but there are {}",
            coefs.len()
        );
        let dims = (coefs.len().max(1) as f64).log2() as u8 + 1;
        Multivector {
            coefs,
            flavor,
            dims,
        }
    }
}

impl MulAssign for Multivector {
    fn mul_assign(&mut self, rhs: Self) {
        self.product_impl(rhs, false)
    }
}

impl Multivector {
    fn product_impl(&mut self, rhs: Self, dot: bool) {
        let dims = self.dims;
        let mask_table = mask_table(dims);
        let mut rev_mask_table = vec![0; 1usize << dims];
        for (i, &v) in mask_table.iter().enumerate() {
            rev_mask_table[v] = i;
        }
        for i in 0..1usize << dims {
            let i_mask = mask_table[i];
            for j in 0..1usize << dims {
                let j_mask = mask_table[j];
                let (sign, metric) = blade_sign_and_metric(dims, self.flavor, dot, i_mask, j_mask);
                if metric == 0.0 {
                    continue;
                }
                let k_mask = j_mask ^ i_mask;
                let k = rev_mask_table[k_mask];
                let (Some(ai), Some(aj), Some(ci)) = (asel[j], bsel[i], csel[k]) else {
                    continue;
                };
            }
        }
    }
}

fn mask_table(dims: u8) -> Vec<usize> {
    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by_key(|&a| a.count_ones());
    mask_table
}
fn blade_sign_and_metric(dims: u8, metrics: Flavor, dot: bool, a: usize, b: usize) -> (i32, f64) {
    let mut sign = 1;
    if dims >= 3 {
        let ab = a ^ b;
        for i in [a, b, ab] {
            if (i ^ (i >> 1)).count_ones() == dims as u32 {
                sign = -sign;
            }
        }
    }
    let mut metric = (!dot || a == 0 || b == 0 || a & b == a || a & b == b) as u8 as f64;
    for i in 0..dims {
        let bit_i = 1 << i;
        if a & bit_i != 0 {
            // Count how many set bits in b are below bit_i
            let lower_bits = b & (bit_i - 1);
            if lower_bits.count_ones() % 2 == 1 {
                sign = -sign;
            }
        }
        if (a & bit_i != 0) && (b & bit_i != 0) {
            metric *= metrics.get(i as usize) as f64;
        }
    }
    (sign, metric)
}
