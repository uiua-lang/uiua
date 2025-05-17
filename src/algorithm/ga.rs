//! Geometric Algebra

use std::{array, fmt, iter::repeat_n};

use ecow::eco_vec;
use serde::*;

use crate::{
    algorithm::pervade::derive_new_shape, grid_fmt::GridFmt, is_default, Array, Shape, Uiua,
    UiuaResult, Value,
};

/// Specification for the kind of geometric algebra to perform
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
#[serde(default)]
pub struct Spec {
    #[serde(skip_serializing_if = "Option::is_none", rename = "d")]
    pub dims: Option<u8>,
    #[serde(skip_serializing_if = "is_default", rename = "m")]
    pub metrics: Metrics,
}

fn ga_arg(value: Value, env: &Uiua) -> UiuaResult<(Array<f64>, Shape, usize)> {
    if value.shape.is_empty() {
        return Err(env.error("Geometric algebra arguments must be at least rank 1"));
    }
    let mut arr = match value {
        Value::Byte(arr) => arr.convert(),
        Value::Num(arr) => arr,
        val => {
            return Err(env.error(format!(
                "Cannot do geometric algebra on {}",
                val.type_name_plural()
            )))
        }
    };
    let mut semishape = arr.shape.clone();
    let blade_count = semishape.pop().unwrap();
    arr.transpose_depth(0, -1);
    Ok((arr, semishape, blade_count))
}

fn ga_arg_no_transpose(value: Value, env: &Uiua) -> UiuaResult<(Array<f64>, Shape, usize)> {
    if value.shape.is_empty() {
        return Err(env.error("Geometric algebra arguments must be at least rank 1"));
    }
    let arr = match value {
        Value::Byte(arr) => arr.convert(),
        Value::Num(arr) => arr,
        val => {
            return Err(env.error(format!(
                "Cannot do geometric algebra on {}",
                val.type_name_plural()
            )))
        }
    };
    let mut semishape = arr.shape.clone();
    let blade_count = semishape.pop().unwrap();
    Ok((arr, semishape, blade_count))
}

/// Mapping from coefficient index to array index
type Sel = Vec<Option<usize>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Even,
    Full,
}
use Mode::*;

use super::{
    pervade::{self, bin_pervade_recursive, InfalliblePervasiveFn},
    tuples::combinations,
};

pub const MAX_DIMS: u8 = Metrics::COUNT as u8;
const MAX_SIZE: usize = 1usize << (MAX_DIMS - 1);

fn derive_dims_mode(dims: Option<u8>, size: usize, env: &Uiua) -> UiuaResult<(u8, Mode)> {
    Ok(if let Some(dims) = dims {
        if dims > MAX_DIMS {
            return Err(env.error(format!(
                "{dims} is too many dimensions. Why would you need that many?"
            )));
        }
        let full_size = 1usize << dims;
        let even_size = 1usize << dims.saturating_sub(1);
        if size == full_size {
            (dims, Full)
        } else if size == even_size {
            (dims, Even)
        } else {
            return Err(env.error(format!(
                "{size} is not a valid array size \
                for geometric algebra in {dims} dimensions"
            )));
        }
    } else {
        if !size.is_power_of_two() {
            return Err(env.error(format!(
                "{size} is not a valid array size for geometric algebra"
            )));
        }
        if size > MAX_SIZE {
            return Err(env.error(format!("{size} is too large for geometric algebra")));
        }
        let dims = (size as f64).log(2.0).round() as u8 + 1;
        (dims, Even)
    })
}

fn dim_selector(dims: u8, elem_size: usize, env: &Uiua) -> UiuaResult<Sel> {
    let (_, this_mode) = derive_dims_mode(Some(dims), elem_size, env)?;
    Ok(match this_mode {
        Even => {
            let mut i = 0;
            blade_grades(dims)
                .map(|g| {
                    (g % 2 == 0).then(|| {
                        i += 1;
                        i - 1
                    })
                })
                .collect()
        }
        Full => (0..(1usize << dims)).map(Some).collect(),
    })
}

fn grade_size(dims: u8, grade: u8) -> usize {
    combinations(dims as usize, grade as usize, false) as usize
}

fn blade_grades(dims: u8) -> impl Iterator<Item = u8> {
    (0..=dims).flat_map(move |i| repeat_n(i, grade_size(dims, i)))
}

type Init<const N: usize> = ([Array<f64>; N], [Shape; N], [Sel; N], u8, usize);
fn init<const N: usize>(spec: Spec, vals: [Value; N], env: &Uiua) -> UiuaResult<Init<N>> {
    let mut arrs = array::from_fn(|_| Array::default());
    let mut sizes = [0; N];
    let mut semis = array::from_fn(|_| Shape::default());
    let mut sels = array::from_fn(|_| Vec::new());
    let mut max_size = 0;
    for (i, val) in vals.into_iter().enumerate() {
        let (arr, semi, size) = ga_arg(val, env)?;
        max_size = max_size.max(size);
        arrs[i] = arr;
        semis[i] = semi;
        sizes[i] = size;
    }
    let (dims, _) = derive_dims_mode(spec.dims, max_size, env)?;
    for i in 0..N {
        sels[i] = dim_selector(dims, sizes[i], env)?;
    }
    Ok((arrs, semis, sels, dims, max_size))
}

fn to_complex(dims: Option<u8>, a: Value) -> Result<Array<f64>, Value> {
    if a.shape.last() != Some(&2) || dims.unwrap_or(2) != 2 {
        Err(a)
    } else {
        match a {
            Value::Num(arr) => Ok(arr),
            Value::Byte(arr) => Ok(arr.convert()),
            val => Err(val),
        }
    }
}

fn fast_dyadic_complex(
    dims: Option<u8>,
    a: Value,
    b: Value,
    re: impl Fn(f64, f64, f64, f64) -> f64,
    im: impl Fn(f64, f64, f64, f64) -> f64,
) -> UiuaResult<Result<Array<f64>, [Value; 2]>> {
    if a.shape.last() != Some(&2) || b.shape.last() != Some(&2) || dims.unwrap_or(2) != 2 {
        return Ok(Err([a, b]));
    }
    let mut a = match to_complex(dims, a) {
        Ok(arr) => arr,
        Err(val) => return Ok(Err([val, b])),
    };
    let mut b = match to_complex(dims, b) {
        Ok(arr) => arr,
        Err(val) => return Ok(Err([a.into(), val])),
    };
    Ok(if a.data.is_copy_of(&b.data) {
        drop(b);
        for chunk in a.data.as_mut_slice().chunks_exact_mut(2) {
            let [ar, ai] = [chunk[0], chunk[1]];
            let r = re(ar, ai, ar, ai);
            let i = im(ar, ai, ar, ai);
            chunk[0] = r;
            chunk[1] = i;
        }
        Ok(a)
    } else {
        match (&*a.shape, &*b.shape) {
            (ash, bsh) if ash == bsh => {
                for (a, b) in a
                    .data
                    .chunks_exact(2)
                    .zip(b.data.as_mut_slice().chunks_exact_mut(2))
                {
                    let [ar, ai, br, bi] = [a[0], a[1], b[0], b[1]];
                    let r = re(ar, ai, br, bi);
                    let i = im(ar, ai, br, bi);
                    b[0] = r;
                    b[1] = i;
                }
                Ok(b)
            }
            (_, [2]) => {
                let [br, bi] = [b.data[0], b.data[1]];
                for a in a.data.as_mut_slice().chunks_exact_mut(2) {
                    let [ar, ai] = [a[0], a[1]];
                    let r = re(ar, ai, br, bi);
                    let i = im(ar, ai, br, bi);
                    a[0] = r;
                    a[1] = i;
                }
                Ok(a)
            }
            ([2], _) => {
                let [ar, ai] = [a.data[0], a.data[1]];
                for b in b.data.as_mut_slice().chunks_exact_mut(2) {
                    let [br, bi] = [b[0], b[1]];
                    let r = re(ar, ai, br, bi);
                    let i = im(ar, ai, br, bi);
                    b[0] = r;
                    b[1] = i;
                }
                Ok(b)
            }
            _ => Err([a.into(), b.into()]),
        }
    })
}

pub fn reverse(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let ([mut arr], [_], [sel], dims, _) = init(spec, [val], env)?;
    arr = reverse_impl(dims, arr, &sel);
    arr.transpose();
    Ok(arr)
}

fn reverse_impl(dims: u8, mut arr: Array<f64>, sel: &Sel) -> Array<f64> {
    for (i, g) in blade_grades(dims).enumerate() {
        if let Some(i) = sel[i] {
            if g / 2 % 2 == 1 {
                for v in arr.row_slice_mut(i) {
                    *v = -*v;
                }
            }
        }
    }
    arr.meta.take_sorted_flags();
    arr
}

pub fn magnitude(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let val = match to_complex(spec.dims, val) {
        Ok(mut arr) => {
            let slice = arr.data.as_mut_slice();
            for i in 0..slice.len() / 2 {
                let [re, im] = [slice[i * 2], slice[i * 2 + 1]];
                slice[i] = (re * re + im * im).sqrt();
            }
            let new_len = slice.len() / 2;
            arr.data.truncate(new_len);
            arr.shape.pop();
            arr.meta.take_sorted_flags();
            arr.validate();
            return Ok(arr);
        }
        Err(val) => val,
    };

    let ([arr], [semi], [sel], dims, size) = init(spec, [val], env)?;
    let rev = reverse_impl(dims, arr.clone(), &sel);
    let ab = [arr, rev];
    let semi = [semi.clone(), semi];
    let sel = [sel.clone(), sel];
    let prod = product_impl(spec.metrics, ab, &semi, &sel, dims, size, env)?;
    let mut arr = prod.first(env)?;
    for v in arr.data.as_mut_slice() {
        *v = v.abs().sqrt();
    }
    Ok(arr)
}

pub fn sqrt(_spec: Spec, _val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    Err(env.error("Geometric square root is not implemented"))
}

pub fn pad_blades(spec: Spec, grade: u8, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let Some(dims) = spec.dims else {
        return Err(env.error("Blade padding requires a specified number of dimensions"));
    };
    if grade > dims {
        return Err(env.error(format!(
            "Cannot pad grade {grade} blades in {dims} dimensions"
        )));
    }
    let (arr, semi, size) = ga_arg_no_transpose(val, env)?;
    let correct_size = grade_size(dims, grade);
    if size != correct_size {
        return Err(env.error(format!(
            "{dims}D multivector should have {correct_size} \
            grade-{grade} blades, but the array has {size}"
        )));
    }
    let left_size = (0..grade).map(|g| grade_size(dims, g)).sum::<usize>();
    let full_size = 1usize << dims;
    let mut new_shape = semi;
    new_shape.push(full_size);
    let mut new_data = eco_vec![0.0; new_shape.elements()];
    let slice = new_data.make_mut();

    for (src, dst) in (arr.data.chunks_exact(size)).zip(slice.chunks_exact_mut(full_size)) {
        dst[left_size..][..size].copy_from_slice(src);
    }

    Ok(Array::new(new_shape, new_data))
}

pub fn extract_blades(spec: Spec, grade: u8, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let Some(dims) = spec.dims else {
        return Err(env.error("Blade padding requires a specified number of dimensions"));
    };
    if grade > dims {
        return Err(env.error(format!(
            "Cannot extract grade {grade} blades in {dims} dimensions"
        )));
    }
    todo!()
}

pub fn add(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let ab = match fast_dyadic_complex(
        spec.dims,
        a,
        b,
        |ar, _, br, _| ar + br,
        |_, ai, _, bi| ai + bi,
    )? {
        Ok(res) => return Ok(res),
        Err(ab) => ab,
    };

    let ([a, b], [asemi, bsemi], [a_sel, b_sel], _, size) = init(spec, ab, env)?;

    let mut csemi = derive_new_shape(&asemi, &bsemi, Err(""), Err(""), env)?;
    let mut c_data = eco_vec![0.0; size * csemi.elements()];

    if csemi.contains(&0) {
        csemi.push(size);
        return Ok(Array::new(csemi, c_data));
    }

    let a_row_len = asemi.elements();
    let b_row_len = bsemi.elements();
    let c_row_len = csemi.elements();
    let a = a.data.as_slice();
    let b = b.data.as_slice();
    let c_slice = c_data.make_mut();

    let add = InfalliblePervasiveFn::new(pervade::add::num_num);
    for i in 0..size {
        match (a_sel[i], b_sel[i]) {
            (Some(ai), Some(bi)) => {
                let a = &a[ai * a_row_len..][..a_row_len];
                let b = &b[bi * b_row_len..][..b_row_len];
                let c = &mut c_slice[i * c_row_len..][..c_row_len];
                bin_pervade_recursive((a, &asemi), (b, &bsemi), c, None, None, add, env)?;
            }
            (Some(ai), None) => {
                let a = &a[ai * a_row_len..][..a_row_len];
                for c in c_slice[i * c_row_len..][..c_row_len].chunks_exact_mut(a_row_len) {
                    c.copy_from_slice(a);
                }
            }
            (None, Some(bi)) => {
                let b = &b[bi * b_row_len..][..b_row_len];
                for c in c_slice[i * c_row_len..][..c_row_len].chunks_exact_mut(b_row_len) {
                    c.copy_from_slice(b);
                }
            }
            _ => {}
        }
    }

    csemi.prepend(size);
    let mut result = Array::new(csemi, c_data);
    result.transpose();
    Ok(result)
}

pub fn product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let [a, b] = match fast_dyadic_complex(
        spec.dims,
        a,
        b,
        |ar, ai, br, bi| ar * br - ai * bi,
        |ar, ai, br, bi| ar * bi + ai * br,
    )? {
        Ok(res) => return Ok(res),
        Err(ab) => ab,
    };

    let (ab, semi, sel, dims, size) = init(spec, [a, b], env)?;
    let mut res = product_impl(spec.metrics, ab, &semi, &sel, dims, size, env)?;
    res.transpose();
    Ok(res)
}
fn product_impl(
    metrics: Metrics,
    ab: [Array<f64>; 2],
    semi: &[Shape; 2],
    sel: &[Sel; 2],
    dims: u8,
    size: usize,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let ([a, b], [asemi, bsemi], [a_sel, b_sel]) = (ab, semi, sel);

    let c_sel = dim_selector(dims, size, env)?;
    let mut csemi = derive_new_shape(asemi, bsemi, Err(""), Err(""), env)?;
    let mut c_data = eco_vec![0.0; size * csemi.elements()];

    // println!("dims: {dims}, metrics: {metrics:?}, size: {size}");
    // println!("a_sel: {a_sel:?}");
    // println!("b_sel: {b_sel:?}");
    // println!("c_sel: {c_sel:?}");

    if csemi.contains(&0) {
        csemi.push(size);
        return Ok(Array::new(csemi, c_data));
    }

    let a_row_len = asemi.elements();
    let b_row_len = bsemi.elements();
    let c_row_len = csemi.elements();
    let a = a.data.as_slice();
    let b = b.data.as_slice();
    let c_slice = c_data.make_mut();
    let mut temp = vec![0.0; c_row_len];

    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by(|a, b| a.count_ones().cmp(&b.count_ones()).then_with(|| a.cmp(b)));
    let mut rev_mask_table = vec![0; 1usize << dims];
    for (i, &v) in mask_table.iter().enumerate() {
        rev_mask_table[v] = i;
    }

    let mul = InfalliblePervasiveFn::new(pervade::mul::num_num);
    for i in 0..1usize << dims {
        let i_mask = mask_table[i];
        for j in 0..1usize << dims {
            let j_mask = mask_table[j];
            let (sign, metric) = blade_sign_and_metric(i_mask, j_mask, dims, metrics);
            if metric == 0.0 {
                continue;
            }
            let k_mask = i_mask ^ j_mask;
            let k = rev_mask_table[k_mask];
            let (Some(ai), Some(aj), Some(ci)) = (a_sel[i], b_sel[j], c_sel[k]) else {
                continue;
            };
            // println!(
            //     "i: {:<4}, j: {:<4}, k: {:<4}, sign: {sign}, metric: {metric}",
            //     blade_name(dims, i_mask),
            //     blade_name(dims, j_mask),
            //     blade_name(dims, k_mask),
            // );
            let a = &a[ai * a_row_len..][..a_row_len];
            let b = &b[aj * b_row_len..][..b_row_len];
            bin_pervade_recursive((a, asemi), (b, bsemi), &mut temp, None, None, mul, env)?;
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
            for (c, t) in c_slice[ci * c_row_len..][..c_row_len].iter_mut().zip(&temp) {
                *c += *t;
            }
        }
    }

    csemi.prepend(size);
    Ok(Array::new(csemi, c_data))
}

fn blade_sign_and_metric(a: usize, b: usize, dims: u8, metrics: Metrics) -> (i32, f64) {
    let mut sign = 1;
    let mut metric = 1.0;
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
    if s.is_empty() {
        1.to_string()
    } else {
        s
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
pub struct Metrics(u32);
impl Metrics {
    pub const COUNT: usize = 16;
    pub const VANILLA: Self = Self(0);
    pub fn all(val: i8) -> Self {
        let mut metrics = Self(0);
        for i in 0..Self::COUNT {
            metrics.set(i, val);
        }
        metrics
    }
    pub fn get(&self, index: usize) -> i8 {
        let bits = (self.0 >> (2 * index)) & 0b11;
        match bits {
            0b00 => 1,
            0b01 => 0,
            0b10 => -1,
            _ => unreachable!(),
        }
    }
    pub fn set(&mut self, index: usize, val: i8) {
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

pub fn metrics_from_val(val: &Value) -> Result<Metrics, String> {
    if val.rank() > 2 {
        return Err(format!(
            "Metrics array must be rank 0 or 1, but its rank is {}",
            val.rank()
        ));
    }
    if val.row_count() > Metrics::COUNT {
        return Err(format!(
            "Metrics array must have at most {} elements, but it has {}",
            Metrics::COUNT,
            val.row_count()
        ));
    }
    Ok(match val {
        Value::Num(arr) => {
            if let Some(m) = arr.data.iter().find(|&v| ![1.0, 0.0, -1.0].contains(v)) {
                return Err(format!(
                    "Metrics may only be 1, 0, or ¯1, but the array contains {}",
                    m.grid_string(false)
                ));
            }
            if arr.rank() == 0 {
                Metrics::all(arr.data[0] as i8)
            } else {
                let mut metrics = Metrics::default();
                for (i, v) in arr.data.iter().enumerate() {
                    metrics.set(i, *v as i8);
                }
                metrics
            }
        }
        Value::Byte(arr) => {
            if let Some(m) = arr.data.iter().find(|&v| ![1, 0].contains(v)) {
                return Err(format!(
                    "Metrics may only be 1, 0, or ¯1, but the array contains {m}"
                ));
            }
            if arr.rank() == 0 {
                Metrics::all(arr.data[0] as i8)
            } else {
                let mut metrics = Metrics::default();
                for (i, v) in arr.data.iter().enumerate() {
                    metrics.set(i, *v as i8);
                }
                metrics
            }
        }
        val => {
            return Err(format!(
                "Metrics array must be numbers, but it is {}",
                val.type_name_plural()
            ))
        }
    })
}
