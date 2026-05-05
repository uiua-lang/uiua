use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt,
    iter::{self, repeat_n},
    mem::{replace, take},
    ops::*,
    slice,
};

use ecow::{EcoVec, eco_vec};
use serde::*;

use crate::{Complex, ga::Flavor};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Multivector {
    #[serde(default, skip_serializing_if = "EcoVec::is_empty", rename = "c")]
    coefs: EcoVec<f64>,
    /// The geometric algebra flavor
    #[serde(default, skip_serializing_if = "is_vanilla", rename = "f")]
    pub flavor: Flavor,
}

fn is_vanilla(flavor: &Flavor) -> bool {
    matches!(flavor, Flavor::Vanilla)
}

impl Multivector {
    /// The the maximum number of dimensions
    ///
    /// # Panics
    /// Panics if `d < self.dims()`
    pub fn set_dims(&mut self, dims: u8) {
        let low_dims = self.dims();
        self.coefs
            .extend(repeat_n(0.0, (1 << dims) - (1 << low_dims)));
        let slice = self.coefs.make_mut();
        let mut b_left = 0;
        for d in 0..=low_dims {
            let ai = grade_size(low_dims, d);
            let bi = grade_size(dims, d);
            slice[b_left + ai..].rotate_right(bi - ai);
            b_left += bi;
        }
    }
    fn conform(&mut self, other: &mut Self) {
        match self.dims().cmp(&other.dims()) {
            Ordering::Equal => {}
            Ordering::Greater => other.set_dims(self.dims()),
            Ordering::Less => self.set_dims(other.dims()),
        }
    }
    /// Get the maximum number of dimensions
    pub fn dims(&self) -> u8 {
        (self.coefs.len().max(1) as f64).log2() as u8
    }
    /// Create a multivector from the real and imaginary parts of a complex number
    pub fn complex(re: impl Into<f64>, im: impl Into<f64>) -> Self {
        Self::all([re.into(), 0.0, 0.0, im.into()])
    }
    /// Create a multivector from all of its coefficients
    pub fn all(coefs: impl Into<EcoVec<f64>>) -> Self {
        let coefs = coefs.into();
        assert!(
            coefs.len().max(1).is_power_of_two(),
            "Must have power of 2 number of coefficients, but there are {}",
            coefs.len()
        );
        Multivector {
            coefs,
            flavor: Flavor::Vanilla,
        }
    }
    /// Create a multivector from its vector coefficients
    pub fn vector(vector: impl Into<EcoVec<f64>>) -> Self {
        let mut coefs = vector.into();
        let dims = coefs.len();
        coefs.insert(0, 0.0);
        coefs.extend(repeat_n(0.0, (1 << dims) - 1 - dims));
        Multivector {
            coefs,
            flavor: Flavor::Vanilla,
        }
    }
    /// Create a multivector from its n-1 blade coefficients
    pub fn n_1_blades(blades: impl Into<EcoVec<f64>>) -> Self {
        let blades = blades.into();
        let len = blades.len();
        let mut mv = Self::vector(blades);
        mv.coefs.make_mut().rotate_left(len + 1);
        mv
    }
    /// Create a multivector from some low-grade, even, or all blades
    pub fn blades_left(dims: u8, blades: impl Into<EcoVec<f64>>) -> Result<Self, EcoVec<f64>> {
        Self::blades_impl(dims, blades.into(), false)
    }
    /// Create a multivector from some high-grade, even, or all blades
    pub fn blades_right(dims: u8, blades: impl Into<EcoVec<f64>>) -> Result<Self, EcoVec<f64>> {
        Self::blades_impl(dims, blades.into(), true)
    }
    fn blades_impl(dims: u8, mut blades: EcoVec<f64>, odd: bool) -> Result<Self, EcoVec<f64>> {
        let blade_count = 1 << dims;
        Ok(if blades.len() == blade_count {
            Self::all(blades)
        } else if blades.len() * 2 == blade_count {
            blades.extend(repeat_n(0.0, blade_count / 2));
            let slice = blades.make_mut();
            let mut left = 0;
            for d in 0..=dims {
                let i = grade_size(dims, d);
                if d % 2 != odd as u8 {
                    slice[left..].rotate_right(i)
                }
                left += i;
            }
            Self::all(blades)
        } else {
            let (start, end) = if odd {
                ((dims as f32 / 2.0).floor() as u8, dims)
            } else {
                (0, (dims as f32 / 2.0).ceil() as u8)
            };
            let mut left: usize = (0..start).map(|d| grade_size(dims, d)).sum();
            for d in start..end {
                let grade_size = grade_size(dims, d);
                if grade_size == blades.len() {
                    blades.extend(repeat_n(0.0, blade_count - grade_size));
                    blades.make_mut().rotate_right(left);
                    return Ok(Self::all(blades));
                }
                left += grade_size;
            }
            return Err(blades);
        })
    }
    /// Create a unit pseudoscalar
    pub fn pseudo_unit(dims: u8) -> Self {
        Self::pseudoscalar(dims, 1.0)
    }
    /// Create a pseudoscalar
    pub fn pseudoscalar(dims: u8, n: f64) -> Self {
        let mut coefs = eco_vec![0.0; 1 << dims];
        *coefs.make_mut().last_mut().unwrap() = n;
        Self::all(coefs)
    }
    /// Set the flavor
    pub fn flavor(mut self, flavor: Flavor) -> Self {
        self.flavor = flavor;
        self
    }
    /// Make this multivector use Projective Geometric Algebra
    pub fn pga(self) -> Self {
        self.flavor(Flavor::Projective)
    }
    /// Try to get the multivector as a scalar
    pub fn as_scalar(&self) -> Option<f64> {
        (!self.coefs.is_empty() && self.coefs.iter().skip(1).all(|&c| c == 0.0)).then(|| self[0])
    }
    /// Try to get the multivector as a complex number
    pub fn as_complex(&self) -> Option<Complex> {
        (self.coefs.len() == 4 && self[1] == 0.0 && self[2] == 0.0)
            .then(|| Complex::new(self[0], self[3]))
    }
    pub fn iter(&self) -> iter::Copied<slice::Iter<'_, f64>> {
        self.coefs.iter().copied()
    }
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, f64> {
        self.coefs.make_mut().iter_mut()
    }
    /// Geometrically dual the multivector
    pub fn dual(&mut self) {
        let flavor = take(&mut self.flavor);
        let dims = self.dims();
        *self *= Self::pseudo_unit(dims);
        self.flavor = flavor;
    }
    /// Get the geometric dual of the multivector
    pub fn dualed(mut self) -> Self {
        self.dual();
        self
    }
    /// Geometrically antidual the multivector
    pub fn antidual(&mut self) {
        let flavor = take(&mut self.flavor);
        let dims = self.dims();
        let v = replace(self, Self::pseudo_unit(dims));
        *self *= v;
        self.flavor = flavor;
    }
    /// Get the geometric antidual of the multivector
    pub fn antidualed(mut self) -> Self {
        self.antidual();
        self
    }
    /// Geometrically reverse the multivector
    pub fn reverse(&mut self) {
        for (g, c) in blade_grades(self.dims()).zip(self) {
            if g / 2 % 2 == 1 {
                *c = -*c;
            }
        }
    }
    /// Geometrically inverse reverse the multivector
    pub fn inv_reverse(&mut self) {
        for (g, c) in blade_grades(self.dims()).zip(self) {
            if g / 2 % 2 == 0 {
                *c = -*c;
            }
        }
    }
    /// Get the geometric reverse of the multivector
    pub fn reversed(mut self) -> Self {
        self.reverse();
        self
    }
    /// Get the geometric inverse reverse of the multivector
    pub fn inv_reversed(mut self) -> Self {
        self.inv_reverse();
        self
    }
    /// Get the magnitude of the multivector
    pub fn magnitude(self) -> f64 {
        (self.clone() * self.reversed())[0]
    }
    /// Normalize the multivector
    pub fn normalize(&mut self) {
        let mag = self.clone().magnitude();
        *self /= mag;
    }
    /// Get the normalized multivector
    pub fn normalized(mut self) -> Self {
        self.normalize();
        self
    }
    /// Get the rotor from this multivector to another
    pub fn rotor(self, to: Self) -> Self {
        ((self * to.reversed()).normalized() + 1.0).normalized()
    }
    pub fn inner_product(mut self, other: Self) -> Self {
        self.product_impl(other, true);
        self
    }
    pub fn wedge_product(mut self, mut other: Self) -> Self {
        self.conform(&mut other);
        let flavor = replace(&mut self.flavor, Flavor::Null);
        self.product_impl(other, false);
        self.flavor = flavor;
        self
    }
    pub fn regressive_product(self, other: Self) -> Self {
        self.dualed().wedge_product(other.dualed()).dualed()
    }
    fn product_impl(&mut self, rhs: Self, dot: bool) {
        let (a, mut b) = (self, rhs);
        a.conform(&mut b);
        let dims = a.dims();
        let mut new_coefs = eco_vec![0.0; a.coefs.len()];
        mask_tables(dims, |mask_table, rev_mask_table| {
            let slice = new_coefs.make_mut();
            for (bi, &b_mask) in mask_table.iter().enumerate() {
                for (ai, &a_mask) in mask_table.iter().enumerate() {
                    let (sign, metric) = blade_sign_and_metric(dims, a.flavor, dot, a_mask, b_mask);
                    if metric == 0.0 {
                        continue;
                    }
                    let c_mask = a_mask ^ b_mask;
                    let ci = rev_mask_table[c_mask];
                    // println!(
                    //     "sign: {}, metric: {}, a: {}, b: {}",
                    //     sign as f64, metric, a[ai], b[bi]
                    // );
                    slice[ci] += sign as f64 * metric * a[ai] * b[bi];
                }
            }
        });
        a.coefs = new_coefs;
    }
}

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

fn blade_grades(dims: u8) -> impl Iterator<Item = u8> {
    (0..=dims).flat_map(move |i| repeat_n(i, grade_size(dims, i)))
}

#[allow(clippy::type_complexity)]
#[doc(hidden)]
pub fn mask_tables<F, T>(dims: u8, f: F) -> T
where
    F: FnOnce(&[usize], &[usize]) -> T,
{
    thread_local! {
        static CACHE: RefCell<HashMap<u8, (Vec<usize>, Vec<usize>)>> = Default::default();
    }
    CACHE.with(move |r| {
        let mut cache = r.borrow_mut();
        let (mask_table, rev_mask_table) = cache.entry(dims).or_insert_with(|| {
            let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
            mask_table.sort_by_key(|&a| a.count_ones());
            let mut rev_mask_table = vec![0; 1usize << dims];
            for (i, &v) in mask_table.iter().enumerate() {
                rev_mask_table[v] = i;
            }
            (mask_table, rev_mask_table)
        });
        f(mask_table, rev_mask_table)
    })
}

fn blade_sign_and_metric(dims: u8, flavor: Flavor, dot: bool, a: usize, b: usize) -> (i32, f64) {
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
            metric *= flavor.metric(i as usize) as f64;
        }
    }
    (sign, metric)
}

fn eq(a: f64, b: f64) -> bool {
    a == b || a.is_nan() && b.is_nan()
}

impl PartialEq for Multivector {
    fn eq(&self, other: &Self) -> bool {
        self.as_scalar() == other.as_scalar()
            || self.coefs.len() == other.coefs.len()
                && self.coefs.iter().zip(&other.coefs).all(|(a, b)| eq(*a, *b))
    }
}

impl Eq for Multivector {}

impl PartialOrd for Multivector {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Multivector {
    fn cmp(&self, other: &Self) -> Ordering {
        if let Some(a) = self.as_scalar()
            && let Some(b) = other.as_scalar()
        {
            return (a.partial_cmp(&b)).unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()));
        }
        self.coefs.len().cmp(&other.coefs.len()).then_with(|| {
            self.iter()
                .zip(other)
                .map(|(a, b)| {
                    a.partial_cmp(&b)
                        .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
                })
                .find(|&o| o != Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        })
    }
}

impl PartialEq<[f64]> for Multivector {
    fn eq(&self, other: &[f64]) -> bool {
        self.coefs == other
    }
}
impl<const N: usize> PartialEq<[f64; N]> for Multivector {
    fn eq(&self, other: &[f64; N]) -> bool {
        self.coefs == other
    }
}

impl Neg for Multivector {
    type Output = Self;
    fn neg(mut self) -> Self::Output {
        for c in &mut self {
            *c = -*c;
        }
        self
    }
}

impl AddAssign<f64> for Multivector {
    fn add_assign(&mut self, rhs: f64) {
        if self.coefs.is_empty() {
            self.coefs = eco_vec![rhs]
        } else {
            self[0] += rhs
        }
    }
}
impl Add<f64> for Multivector {
    type Output = Self;
    fn add(mut self, rhs: f64) -> Self::Output {
        self += rhs;
        self
    }
}

impl AddAssign for Multivector {
    fn add_assign(&mut self, mut rhs: Self) {
        if let Some(b) = rhs.as_scalar() {
            *self += b;
        } else {
            self.conform(&mut rhs);
            for (a, b) in self.coefs.make_mut().iter_mut().zip(rhs.coefs) {
                *a += b;
            }
        }
    }
}
impl Add for Multivector {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        self += rhs;
        self
    }
}

impl SubAssign for Multivector {
    fn sub_assign(&mut self, mut rhs: Self) {
        if !self.coefs.is_empty()
            && let Some(b) = rhs.as_scalar()
        {
            self[0] -= b;
        } else {
            self.conform(&mut rhs);
            for (a, b) in self.coefs.make_mut().iter_mut().zip(rhs.coefs) {
                *a -= b;
            }
        }
    }
}
impl Sub for Multivector {
    type Output = Self;
    fn sub(mut self, rhs: Self) -> Self::Output {
        self -= rhs;
        self
    }
}

impl MulAssign for Multivector {
    fn mul_assign(&mut self, rhs: Self) {
        if let Some(b) = rhs.as_scalar() {
            for a in self {
                *a *= b;
            }
        } else {
            self.product_impl(rhs, false)
        }
    }
}
impl Mul for Multivector {
    type Output = Self;
    fn mul(mut self, rhs: Self) -> Self::Output {
        self *= rhs;
        self
    }
}

impl Div for Multivector {
    type Output = Result<Self, InvalidDivisor>;
    fn div(mut self, rhs: Self) -> Self::Output {
        if let Some(b) = rhs.as_scalar() {
            for a in &mut self {
                *a /= b;
            }
            Ok(self)
        } else if let Some((a, b)) = self.as_complex().zip(rhs.as_complex()) {
            Ok((a / b).into())
        } else {
            Err(InvalidDivisor(rhs))
        }
    }
}

impl DivAssign<f64> for Multivector {
    fn div_assign(&mut self, rhs: f64) {
        for a in self {
            *a /= rhs;
        }
    }
}
impl Div<f64> for Multivector {
    type Output = Self;
    fn div(mut self, rhs: f64) -> Self::Output {
        self /= rhs;
        self
    }
}

#[derive(Debug, Clone)]
pub struct InvalidDivisor(pub Multivector);

impl Index<usize> for Multivector {
    type Output = f64;
    fn index(&self, index: usize) -> &Self::Output {
        &self.coefs[index]
    }
}
impl IndexMut<usize> for Multivector {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.coefs.make_mut()[index]
    }
}
impl<'a> IntoIterator for &'a Multivector {
    type Item = f64;
    type IntoIter = iter::Copied<slice::Iter<'a, f64>>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<'a> IntoIterator for &'a mut Multivector {
    type Item = &'a mut f64;
    type IntoIter = slice::IterMut<'a, f64>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl From<u8> for Multivector {
    fn from(u: u8) -> Self {
        (u as f64).into()
    }
}
impl From<f64> for Multivector {
    fn from(f: f64) -> Self {
        Multivector::all([f])
    }
}
impl From<Complex> for Multivector {
    fn from(c: Complex) -> Self {
        Multivector::complex(c.re, c.im)
    }
}

impl fmt::Display for Multivector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let dims = self.dims();
        let dim_offset = (self.flavor.metric(0) != 0) as usize;
        mask_tables(dims, |mask_table, _| -> fmt::Result {
            let mut wrote = false;
            for (i, n) in self.iter().enumerate() {
                if n == 0.0 {
                    continue;
                }
                if wrote {
                    if n > 0.0 {
                        write!(f, " + ")?;
                    } else {
                        write!(f, " - ")?;
                    }
                    wrote = true;
                } else {
                    if n < 0.0 {
                        write!(f, "-")?;
                        wrote = true;
                    }
                }
                let mask = mask_table[i];
                if n.abs() != 1.0 || mask == 0 {
                    write!(f, "{}", n.abs())?;
                    wrote = true;
                }
                if mask == 0 {
                    continue;
                }
                write!(f, "e")?;
                wrote = true;
                if dims > 2 && (mask ^ (mask >> 1)).count_ones() == dims as u32 {
                    for j in (0..dims).rev() {
                        if mask & (1 << j) != 0 {
                            write!(f, "{}", crate::SUBSCRIPT_DIGITS[j as usize + dim_offset])?;
                        }
                    }
                } else {
                    for j in 0..dims {
                        if mask & (1 << j) != 0 {
                            write!(f, "{}", crate::SUBSCRIPT_DIGITS[j as usize + dim_offset])?;
                        }
                    }
                }
            }
            Ok(())
        })
    }
}

#[cfg(test)]
mod test {
    use super::Multivector as Mv;

    #[test]
    fn conform() {
        let mut a = Mv::all([1.0, 2.0, 3.0, 4.0]);
        a.set_dims(3);
        assert_eq!(a, [1.0, 2.0, 3.0, 0.0, 4.0, 0.0, 0.0, 0.0]);

        let mut a = Mv::all([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]);
        a.set_dims(4);
        assert_eq!(
            a,
            [
                1.0, 2.0, 3.0, 4.0, 0.0, 5.0, 6.0, 7.0, 0.0, 0.0, 0.0, 8.0, 0.0, 0.0, 0.0, 0.0
            ]
        );
    }

    #[test]
    fn product() {
        assert_eq!(
            Mv::complex(1, 2) * Mv::complex(3, 4),
            [-5.0, 0.0, 0.0, 10.0],
        );
        assert_eq!(
            Mv::vector([1.0, 2.0]) * Mv::vector([3.0, 4.0]),
            [11.0, 0.0, 0.0, -2.0],
        );
        assert_eq!(
            Mv::vector([1.0, 2.0, 3.0]) * Mv::vector([4.0, 5.0, 6.0]),
            [32.0, 0.0, 0.0, 0.0, -3.0, 6.0, -3.0, 0.0],
        );
    }

    #[test]
    fn dual() {
        assert_eq!(
            Mv::all([1.0, 2.0, 3.0, 4.0]).dualed(),
            [-4.0, -3.0, 2.0, 1.0],
        );
        assert_eq!(
            Mv::all([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]).dualed(),
            [-8.0, -7.0, -6.0, -5.0, 4.0, 3.0, 2.0, 1.0],
        );
    }

    #[test]
    fn reverse() {
        assert_eq!(
            Mv::all([1.0, 2.0, 3.0, 4.0]).reversed(),
            [1.0, 2.0, 3.0, -4.0],
        );
        assert_eq!(
            Mv::all([1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]).reversed(),
            [1.0, 2.0, 3.0, 4.0, -5.0, -6.0, -7.0, -8.0],
        );
    }
}
