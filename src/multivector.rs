#![allow(missing_docs)]

use std::{cmp::Ordering, fmt, sync::Arc};

use serde::*;

use crate::{grid_fmt::GridFmt, Complex};

#[derive(Serialize, Deserialize)]
#[serde(from = "Rep", into = "Rep")]
pub struct Multivector(Complex);

impl Default for Multivector {
    fn default() -> Self {
        Self::COMPLEX_0
    }
}

struct Mv {
    dims: u8,
    data: Vec<f64>,
}

impl Multivector {
    pub const COMPLEX_0: Self = Self(Complex::ZERO);
    pub const COMPLEX_1: Self = Self(Complex::ONE);
    pub const COMPLEX_I: Self = Self(Complex::I);
    pub fn new_multi(dims: u8, data: impl Into<Vec<f64>>) -> Self {
        let data = data.into();
        if dims == 2 && data.len() == 2 {
            return Complex::new(data[0], data[1]).into();
        }
        let mv = Arc::new(Mv { dims, data });
        let nanbox = ptr_to_nanbox(Arc::into_raw(mv) as *const _);
        Multivector(Complex::new(nanbox, 0.0))
    }
    pub fn new_complex(re: f64, im: f64) -> Self {
        Complex::new(re, im).into()
    }
    fn ptr(&self) -> Option<*const Mv> {
        nanbox_to_ptr(self.0.re).map(|p| p as *const Mv)
    }
    fn mv(&self) -> Option<&Mv> {
        self.ptr().map(|p| unsafe { &*p })
    }
    fn op_bin<T>(
        &self,
        other: &Self,
        cc: impl FnOnce(Complex, Complex) -> T,
        mm: impl FnOnce(&Mv, &Mv) -> T,
        cm: impl FnOnce(Complex, &Mv) -> T,
        mc: impl FnOnce(&Mv, Complex) -> T,
    ) -> T {
        match (self.mv(), other.mv()) {
            (Some(a), Some(b)) => mm(a, b),
            (None, None) => cc(self.0, other.0),
            (None, Some(m)) => cm(self.0, m),
            (Some(m), None) => mc(m, self.0),
        }
    }
    pub fn dims(&self) -> u8 {
        self.mv().map_or(2, |mv| mv.dims)
    }
    pub fn data(&self) -> &[f64] {
        if let Some(mv) = self.mv() {
            &mv.data
        } else {
            unsafe { &*(&self.0 as *const Complex as *const [f64; 2]) }
        }
    }
    pub fn get(&self) -> Result<Complex, (u8, &[f64])> {
        if let Some(mv) = self.mv() {
            Err((mv.dims, &mv.data))
        } else {
            Ok(self.0)
        }
    }
}

impl From<u8> for Multivector {
    fn from(val: u8) -> Self {
        (val as f64).into()
    }
}

impl From<f64> for Multivector {
    fn from(val: f64) -> Self {
        Complex::from(val).into()
    }
}

impl From<Complex> for Multivector {
    fn from(mut val: Complex) -> Self {
        if val.re.is_nan() {
            val.re = f64::NAN; // Ensure only canonical NaN
        }
        Self(val)
    }
}

impl PartialEq for Multivector {
    fn eq(&self, other: &Self) -> bool {
        self.op_bin(
            other,
            |a, b| a == b,
            |a, b| a.dims == b.dims && a.data == b.data,
            |a, b| b.dims == 2 && a.re == b.data[0] && a.im == b.data[1],
            |a, b| a.dims == 2 && a.data[0] == b.re && a.data[1] == b.im,
        )
    }
}

impl Eq for Multivector {}

impl PartialOrd for Multivector {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let dim_cmp = self.dims().cmp(&other.dims());
        if dim_cmp != Ordering::Equal {
            return Some(dim_cmp);
        }
        self.data().partial_cmp(other.data())
    }
}

impl Clone for Multivector {
    fn clone(&self) -> Self {
        if let Some(p) = self.ptr() {
            // Safety
            unsafe { Arc::increment_strong_count(p) }
        }
        Self(self.0)
    }
}

impl Drop for Multivector {
    fn drop(&mut self) {
        if let Some(p) = self.ptr() {
            unsafe { Arc::decrement_strong_count(p) }
        }
    }
}

impl fmt::Debug for Multivector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Multivector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get() {
            Ok(c) => c.fmt(f),
            Err((dims, data)) => {
                let dim_offset = 1; // TODO: Make dependent on metrics
                let mask_table = mask_table(dims);
                let mut s = String::new();
                for (i, &n) in data.iter().enumerate() {
                    if n == 0.0 {
                        continue;
                    }
                    if s.is_empty() {
                        if n < 0.0 {
                            s.push('-');
                        }
                    } else {
                        s.push_str(if n > 0.0 { " + " } else { " - " });
                    }
                    let mask = mask_table[i];
                    if n.abs() != 1.0 || mask == 0 {
                        let n_grid = n.abs().fmt_grid(Default::default());
                        s.extend(n_grid.into_iter().next().unwrap());
                    }
                    if mask == 0 {
                        continue;
                    }
                    s.push('e');
                    for j in 0..dims {
                        if mask & (1 << j) != 0 {
                            s.push(crate::SUBSCRIPT_DIGITS[j as usize + dim_offset]);
                        }
                    }
                    if dims > 2 && (mask ^ (mask >> 1)).count_ones() == dims as u32 {
                        let a = s.pop().unwrap();
                        let b = s.pop().unwrap();
                        s.push(a);
                        s.push(b);
                    }
                }
                s.fmt(f)
            }
        }
    }
}

fn mask_table(dims: u8) -> Vec<usize> {
    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by_key(|&a| a.count_ones());
    mask_table
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
enum Rep {
    Complex(f64, f64),
    Mv(u8, Vec<f64>),
}

impl From<Rep> for Multivector {
    fn from(rep: Rep) -> Self {
        match rep {
            Rep::Complex(re, im) => Self::new_complex(re, im),
            Rep::Mv(dims, data) => Self::new_multi(dims, data),
        }
    }
}

impl From<Multivector> for Rep {
    fn from(m: Multivector) -> Self {
        match m.get() {
            Ok(c) => Rep::Complex(c.re, c.im),
            Err((dims, data)) => Rep::Mv(dims, data.to_vec()),
        }
    }
}

const EXP_ALL_ONES: u64 = 0x7FF;
const QNAN_BIT: u64 = 1u64 << 51;
const PAYLOAD_MASK: u64 = QNAN_BIT - 1;
const FRAC_MASK: u64 = (1u64 << 52) - 1;

fn nanbox_to_ptr(val: f64) -> Option<*const u8> {
    let bits = val.to_bits();
    let exp_all_ones = (bits >> 52) & EXP_ALL_ONES == EXP_ALL_ONES;
    if !exp_all_ones {
        return None;
    }
    let frac = bits & FRAC_MASK;
    if frac & QNAN_BIT != 0 {
        Some((frac & PAYLOAD_MASK) as *const u8)
    } else {
        None
    }
}

fn ptr_to_nanbox(ptr: *const u8) -> f64 {
    let p = (ptr as u64) & PAYLOAD_MASK;
    debug_assert_eq!((ptr as u64) >> 51, 0, "pointer exceeds 51-bit payload");
    f64::from_bits((EXP_ALL_ONES << 52) | QNAN_BIT | p)
}
