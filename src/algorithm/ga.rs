//! Geometric Algebra

use std::iter::repeat_n;

use ecow::eco_vec;
use serde::*;

use crate::{
    algorithm::pervade::derive_new_shape, is_default, Array, Shape, Uiua, UiuaResult, Value,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
#[serde(default)]
pub struct GaSpace {
    #[serde(skip_serializing_if = "Option::is_none", rename = "d")]
    pub dims: Option<u8>,
    #[serde(skip_serializing_if = "is_default", rename = "f")]
    pub flavor: GaFlavor,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub enum GaFlavor {
    #[default]
    #[serde(rename = "v")]
    Vanilla,
    #[serde(rename = "p")]
    Projective,
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

/// Mapping from coefficient index to array index
type Sel = Vec<Option<usize>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GaMode {
    Even,
    Full,
}
use GaMode::*;

use super::{
    pervade::{self, bin_pervade_recursive, InfalliblePervasiveFn},
    tuples::combinations,
};

const MAX_DIMS: u8 = 11;
const MAX_SIZE: usize = 1usize << (MAX_DIMS - 1);

fn derive_dims_mode(dims: Option<u8>, size: usize, env: &Uiua) -> UiuaResult<(u8, GaMode)> {
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
    if this_mode == Full {
        return Ok((0..(1usize << dims)).map(Some).collect());
    }
    let mut results = Vec::with_capacity(1usize << dims);
    let mut i = 0;
    for d in 0..=dims {
        let n = combinations(dims as usize, d as usize, false) as usize;
        if d % 2 == 0 {
            for _ in 0..n {
                results.push(Some(i));
                i += 1;
            }
        } else {
            results.extend(repeat_n(None, n));
        }
    }
    Ok(results)
}

fn metrics(flavor: GaFlavor, dims: u8) -> Vec<f64> {
    match flavor {
        GaFlavor::Vanilla => vec![1.0; dims as usize],
        GaFlavor::Projective => {
            let mut metrics = vec![1.0; dims as usize];
            if let Some(m) = metrics.first_mut() {
                *m = 0.0;
            }
            metrics
        }
    }
}

pub fn product(space: GaSpace, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let flavor = space.flavor;
    let (a, asemi, a_size) = ga_arg(a, env)?;
    let (b, bsemi, b_size) = ga_arg(b, env)?;
    let size = a_size.max(b_size);
    let (dims, _) = derive_dims_mode(space.dims, size, env)?;
    let metrics = metrics(flavor, dims);
    let a_sel = dim_selector(dims, a_size, env)?;
    let b_sel = dim_selector(dims, b_size, env)?;
    let c_sel = dim_selector(dims, size, env)?;

    let mut csemi = derive_new_shape(&asemi, &bsemi, Err(""), Err(""), env)?;
    let mut c_data = eco_vec![0.0; size * csemi.elements() ];

    // println!("flavor: {flavor:?}, dims: {dims}");
    // println!("mode: {mode:?}, size: {size}, metrics: {metrics:?}");
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

    for i in 0..1usize << dims {
        let i_mask = mask_table[i];
        for j in 0..1usize << dims {
            let j_mask = mask_table[j];
            let (sign, metric) = blade_sign_and_metric(i_mask, j_mask, dims, &metrics);
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
            bin_pervade_recursive(
                (a, &asemi),
                (b, &bsemi),
                &mut temp,
                None,
                None,
                InfalliblePervasiveFn::new(pervade::mul::num_num),
                env,
            )?;
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
    let mut result = Array::new(csemi, c_data);
    result.transpose();
    Ok(result)
}

fn blade_sign_and_metric(a: usize, b: usize, dims: u8, metrics: &[f64]) -> (i32, f64) {
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
            metric *= metrics[i as usize];
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
