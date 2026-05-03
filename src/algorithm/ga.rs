//! Geometric Algebra

use std::{fmt, iter::repeat_n, mem::take};

use ecow::eco_vec;
use serde::*;

use crate::{
    Array, Shape, SubSide, Uiua, UiuaResult, Value, algorithm::pervade::derive_new_shape,
    grid_fmt::GridFmt,
};

type GaResult<T = ()> = Result<T, String>;

impl Value {
    pub(crate) fn make_multivector(
        self,
        met: Metrics,
        dims: Option<u8>,
        blade_side: SubSide,
    ) -> GaResult<Array<f64>> {
        let mut arr = match self {
            Value::Num(arr) => arr,
            Value::Byte(arr) => arr.convert(),
            Value::Complex(arr) => {
                let elem_count = arr.element_count();
                let mut data = eco_vec![0.0; elem_count * 4];
                let slice = data.make_mut();
                for (i, c) in arr.data.into_iter().enumerate() {
                    slice[i] = c.re;
                    slice[elem_count * 3 + i] = c.im;
                }
                let mut shape = arr.shape;
                shape.insert(0, 4);
                let mut arr = Array::new(shape, data);
                arr.meta.ga_metrics = Some(met);
                return Ok(arr);
            }
            val => {
                return Err(format!(
                    "Cannot make multivector from {} array",
                    val.type_name()
                ));
            }
        };
        if arr.meta.ga_metrics.is_some() {
            return Ok(arr);
        }
        arr.meta.take_sorted_flags();
        arr.untranspose();
        let coef_count = arr.row_count();
        match (coef_count, dims, blade_side) {
            (0 | 1, None, _) => {}
            (0 | 1, Some(d), SubSide::Left) => pad_blades(d, 0, &mut arr)?,
            (0 | 1, Some(d), SubSide::Right) => pad_blades(d, d, &mut arr)?,
            (n, None, SubSide::Left) => pad_blades(n as u8, 1, &mut arr)?,
            (n, None, SubSide::Right) => pad_blades(n as u8, n as u8, &mut arr)?,
            (n, Some(d), side) => {
                if n == 1 << d {
                    // Full multivector
                } else if n == even_odd_blade_count(d, side == SubSide::Right) {
                    // Even or odd blades
                    pad_blades_even_odd(d, side == SubSide::Right, &mut arr)?
                } else {
                    // Single blades
                    let (start, end) = match side {
                        SubSide::Left => (0, (d as f32 / 2.0).floor() as u8),
                        SubSide::Right => ((d as f32 / 2.0).ceil() as u8, d),
                    };
                    (start..=end)
                        .find(|&i| grade_size(d, i) == n)
                        .map(|i| pad_blades(d, i, &mut arr))
                        .transpose()?
                        .ok_or_else(|| {
                            format!("{n} is not a valid blade size for {d} dimensions")
                        })?
                }
            }
        };
        arr.meta.ga_metrics = Some(met);
        arr.validate();
        Ok(arr)
    }
}

pub(crate) fn monadic_metrics(val: &mut Value) -> Option<GaResult<(Metrics, Array<f64>)>> {
    let metrics = val.meta.ga_metrics?;
    Some(
        take(val)
            .make_multivector(metrics, None, SubSide::Left)
            .map(|arr| (metrics, arr)),
    )
}

#[allow(clippy::type_complexity)]
pub(crate) fn dyadic_metrics(
    a: &mut Value,
    b: &mut Value,
) -> Option<GaResult<(Metrics, Array<f64>, Array<f64>)>> {
    match (a.meta.ga_metrics, b.meta.ga_metrics) {
        (None, None) => None,
        (Some(am), None) => Some(
            take(a)
                .make_multivector(am, None, SubSide::Left)
                .and_then(|a| {
                    take(b)
                        .make_multivector(am, Some(a.ga_dims()), SubSide::Left)
                        .map(|b| (am, a, b))
                }),
        ),
        (None, Some(_)) => Some(dyadic_metrics(b, a)?.map(|(m, b, a)| (m, a, b))),
        (Some(am), Some(bm)) => Some({
            if am != bm {
                Err("incompatible metrics".into())
            } else if a.row_count() != b.row_count() {
                Err("different number of dimensions".into())
            } else {
                take(a)
                    .make_multivector(am, None, SubSide::Left)
                    .and_then(|a| {
                        take(b)
                            .make_multivector(bm, None, SubSide::Left)
                            .map(|b| (am, a, b))
                    })
            }
        }),
    }
}

fn pad_blades(dims: u8, grade: u8, arr: &mut Array<f64>) -> GaResult {
    if dims > MAX_DIMS {
        return Err(format!(
            "{dims} dimensions is too many for the geometric algebra system"
        ));
    }
    if grade > dims {
        return Err(format!(
            "Cannot pad grade {grade} blades in {dims} dimensions"
        ));
    }
    // Validate size
    let correct_size: usize = grade_size(dims, grade);
    let row_len = arr.row_len();
    let size = arr.row_count();
    if size != correct_size {
        return Err(format!(
            "{dims}D multivector should have {correct_size} \
            grade-{grade} blades, but the array has {size}"
        ));
    }

    let full_size = 1usize << dims;
    let mut new_shape = arr.ga_semi_shape();
    new_shape.insert(0, full_size);
    arr.data.extend_repeat(&0.0, (full_size - size) * row_len);
    let slice = arr.data.as_mut_slice();
    let left_size: usize = (0..grade).map(|g| grade_size(dims, g)).sum();
    slice.rotate_right(left_size * row_len);
    arr.shape = new_shape;
    Ok(())
}

fn pad_blades_even_odd(dims: u8, odd: bool, arr: &mut Array<f64>) -> GaResult {
    if dims > MAX_DIMS {
        return Err(format!(
            "{dims} dimensions is too many for the geometric algebra system"
        ));
    }
    // Validate size
    let correct_size = even_odd_blade_count(dims, odd);
    let row_len = arr.row_len();
    let size = arr.row_count();
    if size != correct_size {
        return Err(format!(
            "{dims}D multivector should have {correct_size} \
            {}-graded blades, but the array has {size}",
            if odd { "odd" } else { "even" }
        ));
    }
    let full_size = 1usize << dims;
    let mut new_shape = arr.ga_semi_shape();
    new_shape.insert(0, full_size);
    let mut new_data = eco_vec![0.0; new_shape.elements()];
    let slice = new_data.make_mut();
    let mut dst_start = 0;
    let mut src_start = 0;
    for d in 0..=dims {
        let g = grade_size(dims, d);
        if d % 2 == odd as u8 {
            slice[dst_start * row_len..][..g * row_len]
                .copy_from_slice(&arr.data[src_start * row_len..][..g * row_len]);
            src_start += g;
        }
        dst_start += g;
    }
    arr.data = new_data.into();
    arr.shape = new_shape;
    Ok(())
}

impl Array<f64> {
    fn ga_dims(&self) -> u8 {
        (self.row_count() as f32).log2() as u8
    }
    fn ga_semi_shape(&self) -> Shape {
        self.shape.row()
    }
}

use super::{
    pervade::{self, InfalliblePervasiveFn, bin_pervade_mut, bin_pervade_recursive},
    tuples::combinations,
};

pub const MAX_DIMS: u8 = Metrics::COUNT as u8;

fn grade_size(dims: u8, grade: u8) -> usize {
    combinations(dims as usize, grade as usize, false) as usize
}

fn even_odd_blade_count(dims: u8, odd: bool) -> usize {
    (0..=dims)
        .filter(|&i| i % 2 == odd as u8)
        .map(|i| grade_size(dims, i))
        .sum()
}

/// Iterator over the grades of each blade in a multivector of the given number of dimensions
fn blade_grades(dims: u8) -> impl Iterator<Item = u8> {
    (0..=dims).flat_map(move |i| repeat_n(i, grade_size(dims, i)))
}

pub fn reverse(mut arr: Array<f64>, _: Metrics, _: &Uiua) -> UiuaResult<Array<f64>> {
    for (g, slice) in blade_grades(arr.ga_dims()).zip(arr.row_slices_mut()) {
        if g / 2 % 2 == 1 {
            for f in slice {
                *f = -*f;
            }
        }
    }
    Ok(arr)
}

fn pseudo(dims: u8) -> Array<f64> {
    let mut data = eco_vec![0.0; 1 << dims];
    *data.make_mut().last_mut().unwrap() = 1.0;
    data.into()
}

pub fn dual(arr: Array<f64>, _: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    product(pseudo(arr.ga_dims()), arr, Metrics::VANILLA, env)
}

pub fn magnitude(arr: Array<f64>, met: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    let rev = reverse(arr.clone(), met, env)?;
    let prod = product(rev, arr, met, env)?;
    let mut arr = prod.first(env)?;
    for v in arr.data.as_mut_slice() {
        *v = v.abs().sqrt();
    }
    Ok(arr)
}

pub fn normalize(mut arr: Array<f64>, met: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    fn div(num: f64, denom: f64) -> f64 {
        if denom == 0.0 { 0.0 } else { num / denom }
    }
    let mag = magnitude(arr.clone(), met, env)?;
    bin_pervade_mut(mag, &mut arr, false, env, |a, b| div(b, a))?;
    Ok(arr)
}

pub fn add(mut a: Array<f64>, mut b: Array<f64>, _: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    let am = take(&mut a.meta.ga_metrics);
    let bm = take(&mut b.meta.ga_metrics);
    let Value::Num(mut arr) = Value::from(a).add(b.into(), env)? else {
        unreachable!()
    };
    arr.meta.ga_metrics = am.or(bm);
    Ok(arr)
}

pub fn sub(mut a: Array<f64>, mut b: Array<f64>, _: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    let am = take(&mut a.meta.ga_metrics);
    let bm = take(&mut b.meta.ga_metrics);
    let Value::Num(mut arr) = Value::from(a).sub(b.into(), env)? else {
        unreachable!()
    };
    arr.meta.ga_metrics = am.or(bm);
    Ok(arr)
}

pub fn rotor(a: Array<f64>, b: Array<f64>, met: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    // |1+|ab̃||
    let revb = reverse(b, met, env)?;
    let prod = product(revb, a, met, env)?;
    let mut norm = normalize(prod, met, env)?;
    let row_len = norm.row_len();
    for n in &mut norm.data.as_mut_slice()[..row_len] {
        *n += 1.0;
    }
    normalize(norm, met, env)
}

pub fn div(mut a: Array<f64>, mut b: Array<f64>, _: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    if a.rank() == 1 && a.data.iter().skip(1).all(|&n| n == 0.0) {
        a = a.first(env)?;
    } else if a.rank() > 0 {
        return Err(env.error(format!(
            "Only scalar division is supported,
            but the arrays are shape {} and {}",
            a.shape, b.shape
        )));
    }
    bin_pervade_mut(a, &mut b, false, env, pervade::div::num_num)?;
    Ok(b)
}

pub fn inner_product(
    a: Array<f64>,
    b: Array<f64>,
    met: Metrics,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    product_impl(a, b, met, true, env)
}

pub fn wedge_product(
    a: Array<f64>,
    b: Array<f64>,
    _: Metrics,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    product_impl(a, b, Metrics::NULL, false, env)
}

pub fn regressive_product(
    _a: Array<f64>,
    _b: Array<f64>,
    _met: Metrics,
    _env: &Uiua,
) -> UiuaResult<Array<f64>> {
    todo!()
}

pub fn product(a: Array<f64>, b: Array<f64>, met: Metrics, env: &Uiua) -> UiuaResult<Array<f64>> {
    product_impl(a, b, met, false, env)
}
fn product_impl(
    a: Array<f64>,
    b: Array<f64>,
    met: Metrics,
    dot: bool,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    assert_eq!(a.row_count(), b.row_count());
    let size = a.row_count();
    let dims = a.ga_dims();
    let asemi = a.ga_semi_shape();
    let bsemi = b.ga_semi_shape();
    let mut csemi = derive_new_shape(&asemi, &bsemi, Err(""), Err("")).map_err(|e| env.error(e))?;
    let mut c_data = eco_vec![0.0; size * csemi.elements()];

    if csemi.contains(&0) {
        csemi.prepend(size);
        return Ok(Array::new(csemi, c_data));
    }

    let a_row_len = asemi.elements();
    let b_row_len = bsemi.elements();
    let c_row_len = csemi.elements();
    let a = a.data.as_slice();
    let b = b.data.as_slice();
    let c_slice = c_data.make_mut();
    let mut temp = vec![0.0; c_row_len];

    let mask_table = mask_table(dims);
    let mut rev_mask_table = vec![0; 1usize << dims];
    for (i, &v) in mask_table.iter().enumerate() {
        rev_mask_table[v] = i;
    }

    // println!(
    //     "mask table: {:?}",
    //     (mask_table.iter())
    //         .map(|&v| blade_name(dims, v))
    //         .collect::<Vec<_>>()
    // );

    let mul = InfalliblePervasiveFn::new(pervade::mul::num_num);
    for i in 0..1usize << dims {
        if dims > 5 {
            env.respect_execution_limit()?;
        }
        let i_mask = mask_table[i];
        for j in 0..1usize << dims {
            let j_mask = mask_table[j];
            let (sign, metric) = blade_sign_and_metric(dims, met, dot, i_mask, j_mask);
            if metric == 0.0 {
                continue;
            }
            let k_mask = j_mask ^ i_mask;
            let k = rev_mask_table[k_mask];
            // println!(
            //     "i: {:<4}, j: {:<4}, k: {:<4}, sign: {sign}, metric: {metric}",
            //     blade_name(dims, i_mask),
            //     blade_name(dims, j_mask),
            //     blade_name(dims, k_mask),
            // );
            let a = &a[j * a_row_len..][..a_row_len];
            let b = &b[i * b_row_len..][..b_row_len];
            bin_pervade_recursive((a, &asemi), (b, &bsemi), &mut temp, None, None, mul, env)?;
            if sign == -1 {
                for v in &mut temp {
                    *v = -*v;
                }
            }
            if metric != 1.0 {
                for v in &mut temp {
                    *v *= metric;
                }
            }
            for (c, t) in c_slice[k * c_row_len..][..c_row_len].iter_mut().zip(&temp) {
                *c += *t;
            }
        }
    }

    csemi.prepend(size);
    let mut arr = Array::new(csemi, c_data);
    arr.meta.ga_metrics = Some(met);
    Ok(arr)
}

fn mask_table(dims: u8) -> Vec<usize> {
    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by_key(|&a| a.count_ones());
    mask_table
}

fn blade_sign_and_metric(dims: u8, met: Metrics, dot: bool, a: usize, b: usize) -> (i32, f64) {
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
            metric *= met.get(i as usize) as f64;
        }
    }
    (sign, metric)
}

#[allow(dead_code)]
fn blade_name(dims: u8, mask: usize) -> String {
    let mut s = String::new();
    for i in 0..dims {
        if mask & (1 << i) != 0 {
            if s.is_empty() {
                s.push('e');
            }
            s.push(crate::SUBSCRIPT_DIGITS[i as usize]);
        }
    }
    if s.is_empty() { 1.to_string() } else { s }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Metrics(u32);
impl Default for Metrics {
    fn default() -> Self {
        Metrics::VANILLA
    }
}
impl Metrics {
    pub const COUNT: usize = size_of::<u32>() << 3 >> 1;
    pub const VANILLA: Self = Self::all(1);
    pub const PROJECTIVE: Self = {
        let mut metrics = Self::all(1);
        metrics.set(0, 1);
        metrics
    };
    pub const CONFORMAL: Self = {
        let mut metrics = Self::all(1);
        metrics.set(0, -1);
        metrics
    };
    pub const SPACETIME: Self = Self::all(-1);
    pub const NULL: Self = Self::all(0);
    pub const fn all(val: i8) -> Self {
        let mut metrics = Self(0);
        let mut i = 0;
        while i < Self::COUNT {
            metrics.set(i, val);
            i += 1;
        }
        metrics
    }
    pub const fn get(&self, index: usize) -> i8 {
        let bits = (self.0 >> (2 * index)) & 0b11;
        match bits {
            0b00 => 1,
            0b01 => 0,
            0b10 => -1,
            _ => unreachable!(),
        }
    }
    pub const fn set(&mut self, index: usize, val: i8) {
        let bits = match val {
            0 => 0b01,
            -1 => 0b10,
            _ => 0b00,
        };
        self.0 &= !(0b11 << (2 * index));
        self.0 |= bits << (2 * index);
    }
}
impl fmt::Debug for Metrics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in 0..Self::COUNT {
            let val = self.get(i);
            write!(f, "{val}")?;
        }
        Ok(())
    }
}

pub fn format(arr: &Array<f64>, met: Metrics) -> Vec<Vec<char>> {
    let dims = arr.ga_dims();
    let mask_table = mask_table(dims);
    let row_len = arr.row_len();
    let row_count = arr.row_count();
    let mut strings = Vec::with_capacity(row_len);
    let dim_offset = (met.get(0) != 0) as usize;
    for i in 0..row_len {
        let mut s = Vec::new();
        for j in 0..row_count {
            let n = arr.data[j * row_len + i];
            if n == 0.0 {
                continue;
            }
            if s.is_empty() {
                if n < 0.0 {
                    s.push('-');
                }
            } else {
                s.extend(if n > 0.0 { " + " } else { " - " }.chars());
            }
            let mask = mask_table[j];
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
                let (a, b) = (s.len() - 1, s.len() - 2);
                s.swap(a, b);
            }
        }
        strings.push(s);
    }
    strings
}
