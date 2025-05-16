//! Geometric Algebra

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
    let elem_size = *value.shape.last().unwrap();
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
    semishape.pop();
    Ok((arr, semishape, elem_size))
}

/// Mapping from coefficient index to array index
type Sel = &'static [Option<usize>];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GaDims {
    D2 = 2,
    D3 = 3,
}
use GaDims::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GaMode {
    Even,
    Full,
}
use GaMode::*;

fn get_dim(selector: Sel, dim: usize, data: &[f64]) -> f64 {
    selector[dim].map_or(0.0, |i| data[i])
}

fn derive_dims_mode(dims: Option<u8>, size: usize, env: &Uiua) -> UiuaResult<(GaDims, GaMode)> {
    Ok(match (dims, size) {
        (Some(2), 2) => (D2, Even),
        (Some(2), 4) => (D2, Full),
        (Some(3), 4) => (D3, Even),
        (Some(3), 8) => (D3, Full),
        (None, 2) => (D2, Even),
        (None, 4) => (D3, Even),
        (None, n) => {
            return Err(env.error(format!(
                "{n} is not a valid number of multivector components"
            )))
        }
        (Some(d @ (2 | 3)), n) => {
            return Err(env.error(format!(
                "{n} is not a valid number of multivector components in {d} dimensions"
            )))
        }
        (Some(d), _) => {
            return Err(env.error(format!(
                "{d} dimensions for geometric algebra is not supported"
            )))
        }
    })
}

fn dim_selector(dims: GaDims, mode: GaMode, elem_size: usize, env: &Uiua) -> UiuaResult<Sel> {
    const fn s(n: usize) -> Option<usize> {
        Some(n)
    }
    const N: Option<usize> = None;
    let (_, this_mode) = derive_dims_mode(Some(dims as u8), elem_size, env)?;
    Ok(match (dims, mode, this_mode) {
        (D2, Even, Even) => &const { [s(0), s(0)] },
        (D2, Full, Even) => &const { [s(0), N, N, s(1)] },
        (D2, Full, Full) => &const { [s(0), s(1), s(2), s(3)] },
        (D2, Even, Full) => unreachable!(),
        (D3, Even, Even) => &const { [s(0), s(1), s(2), s(3)] },
        (D3, Full, Even) => &const { [s(0), N, N, N, s(1), s(2), s(3), N] },
        (D3, Full, Full) => &const { [s(0), s(1), s(2), s(3), s(4), s(5), s(6), s(7)] },
        (D3, Even, Full) => unreachable!(),
    })
}

fn translate_index(from: &Shape, to: &Shape, index: usize) -> usize {
    let mut result = 0;
    let mut from_stride = 1;
    let mut to_stride = 1;
    for (&f, &t) in from.iter().zip(to).rev() {
        let in_from = index / from_stride % f;
        let in_to = in_from % t;
        result += in_to * to_stride;
        from_stride *= f;
        to_stride *= t;
    }
    result
}

pub fn product(space: GaSpace, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    fn e2(a: &[f64], b: &[f64], out: &mut [f64]) {
        let [a0, a1] = [0, 1].map(|i| a[i]);
        let [b0, b1] = [0, 1].map(|i| b[i]);
        out[0] = a0 * b0 - a1 * b1; // scalar
        out[1] = a0 * b1 + a1 * b0; // e12
    }
    fn f2(asel: Sel, a: &[f64], bsel: Sel, b: &[f64], out: &mut [f64]) {
        let [a0, a1, a2, a3] = [0, 1, 2, 3].map(|i| get_dim(asel, i, a));
        let [b0, b1, b2, b3] = [0, 1, 2, 3].map(|i| get_dim(bsel, i, b));
        out[0] = a0 * b0 + a1 * b1 + a2 * b2 - a3 * b3; // scalar
        out[1] = a0 * b1 + a1 * b0 - a2 * b3 + a3 * b2; // e1
        out[2] = a0 * b2 + a2 * b0 + a1 * b3 - a3 * b1; // e2
        out[3] = a0 * b3 + a3 * b0 + a1 * b2 - a2 * b1; // e12
    }
    fn e3(a: &[f64], b: &[f64], out: &mut [f64]) {
        let [a0, a1, a2, a3] = [0, 1, 2, 3].map(|i| a[i]);
        let [b0, b1, b2, b3] = [0, 1, 2, 3].map(|i| b[i]);
        out[0] = a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3; // scalar
        out[1] = a0 * b1 + a1 * b0 - a2 * b3 + a3 * b2; // e12
        out[2] = a0 * b2 + a2 * b0 - a3 * b1 + a1 * b3; // e23
        out[3] = a0 * b3 + a3 * b0 - a1 * b2 + a2 * b1; // e31
    }
    fn f3(asel: Sel, a: &[f64], bsel: Sel, b: &[f64], out: &mut [f64]) {
        let [a0, a1, a2, a3, a4, a5, a6, a7] =
            [0, 1, 2, 3, 4, 5, 6, 7].map(|i| get_dim(asel, i, a));
        let [b0, b1, b2, b3, b4, b5, b6, b7] =
            [0, 1, 2, 3, 4, 5, 6, 7].map(|i| get_dim(bsel, i, b));

        out[0] = a0 * b0 + a1 * b1 + a2 * b2 + a3 * b3 - a4 * b4 - a5 * b5 - a6 * b6 - a7 * b7; // scalar
        out[1] = a0 * b1 + a1 * b0 - a2 * b6 + a3 * b5 + a4 * b3 - a5 * b7 + a6 * b2 + a7 * b5; // e1
        out[2] = a0 * b2 + a2 * b0 + a1 * b6 - a3 * b4 - a4 * b7 + a5 * b1 - a6 * b1 + a7 * b4; // e2
        out[3] = a0 * b3 + a3 * b0 - a1 * b5 + a2 * b4 + a4 * b2 - a5 * b1 + a6 * b7 + a7 * b6; // e3
        out[4] = a0 * b4 + a4 * b0 + a1 * b3 - a2 * b7 - a3 * b1 + a5 * b6 - a6 * b5 + a7 * b2; // e23
        out[5] = a0 * b5 + a5 * b0 + a2 * b1 - a3 * b7 - a1 * b2 + a6 * b4 - a4 * b6 + a7 * b3; // e31
        out[6] = a0 * b6 + a6 * b0 + a3 * b2 - a1 * b7 - a2 * b3 + a4 * b5 - a5 * b4 + a7 * b1; // e12
        out[7] = a0 * b7 + a7 * b0 + a1 * b4 + a2 * b5 + a3 * b6 + a4 * b1 + a5 * b2 + a6 * b3;
        // e123
    }
    dyadic(space, a, b, env, e2, f2, e3, f3)
}

#[allow(clippy::too_many_arguments)]
fn dyadic(
    space: GaSpace,
    a: Value,
    b: Value,
    env: &Uiua,
    e2: impl DyFnEven,
    f2: impl DyFnFull,
    e3: impl DyFnEven,
    f3: impl DyFnFull,
) -> UiuaResult<Array<f64>> {
    let (a, asemi, a_size) = ga_arg(a, env)?;
    let (b, bsemi, b_size) = ga_arg(b, env)?;
    let size = a_size.max(b_size);
    let (dims, mode) = derive_dims_mode(space.dims, size, env)?;
    let a_sel = dim_selector(dims, mode, a_size, env)?;
    let b_sel = dim_selector(dims, mode, b_size, env)?;

    let mut new_shape = derive_new_shape(&asemi, &bsemi, Err(""), Err(""), env)?;
    let mut new_data = eco_vec![0.0; new_shape.elements() * size];
    if new_shape.contains(&0) {
        new_shape.push(size);
        return Ok(Array::new(new_shape, new_data));
    }

    let a = a.data.as_slice();
    let b = b.data.as_slice();

    macro_rules! go {
        ($f:expr $(, $a_sel:ident, $b_sel:ident)?) => {
            for (i, chunk) in new_data.make_mut().chunks_exact_mut(size).enumerate() {
                let ai = translate_index(&new_shape, &asemi, i);
                let bi = translate_index(&new_shape, &bsemi, i);
                let a = &a[ai * a_size..][..a_size];
                let b = &b[bi * b_size..][..b_size];
                $f.call($($a_sel,)? a, $($b_sel,)? b, chunk);
            }
        };
    }

    match (dims, mode) {
        (D2, Even) => go!(e2),
        (D2, Full) => go!(f2, a_sel, b_sel),
        (D3, Even) => go!(e3),
        (D3, Full) => go!(f3, a_sel, b_sel),
    }

    new_shape.push(size);
    Ok(Array::new(new_shape, new_data))
}

trait DyFnEven {
    fn call(&self, a: &[f64], b: &[f64], out: &mut [f64]);
}

impl<F> DyFnEven for F
where
    F: Fn(&[f64], &[f64], &mut [f64]),
{
    fn call(&self, a: &[f64], b: &[f64], out: &mut [f64]) {
        self(a, b, out)
    }
}

trait DyFnFull {
    fn call(&self, asel: Sel, a: &[f64], bsel: Sel, b: &[f64], out: &mut [f64]);
}

impl<F> DyFnFull for F
where
    F: Fn(Sel, &[f64], Sel, &[f64], &mut [f64]),
{
    fn call(&self, asel: Sel, a: &[f64], bsel: Sel, b: &[f64], out: &mut [f64]) {
        self(asel, a, bsel, b, out)
    }
}
