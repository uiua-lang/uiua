//! Geometric Algebra

use std::{
    array, fmt,
    iter::{once, repeat_n},
};

use ecow::{eco_vec, EcoVec};
use serde::*;

use crate::{
    algorithm::pervade::derive_new_shape, grid_fmt::GridFmt, is_default, Array, Boxed, Shape, Uiua,
    UiuaResult, Value,
};

pub fn resolve_specs(a: &Value, b: &Value, env: &Uiua) -> UiuaResult<Option<Spec>> {
    let dims = match (a.meta.ga_spec.dims, b.meta.ga_spec.dims) {
        (Some(a), Some(b)) => a.max(b),
        _ => return Ok(None),
    };
    if a.meta.ga_spec.metrics != b.meta.ga_spec.metrics {
        return Err(env.error("Multivectors have incompatible metrics"));
    }
    Ok(Some(Spec {
        dims: Some(dims),
        metrics: a.meta.ga_spec.metrics,
    }))
}

fn resolve_specs_impl(a: Spec, b: Spec, env: &Uiua) -> UiuaResult<Spec> {
    let dims = match (a.dims, b.dims) {
        (Some(a), Some(b)) => Some(a.max(b)),
        (Some(d), _) | (_, Some(d)) => Some(d),
        (None, None) => None,
    };
    if a.metrics != b.metrics {
        return Err(env.error("Multivectors have incompatible metrics"));
    }
    Ok(Spec {
        dims,
        metrics: a.metrics,
    })
}

impl Value {
    /// Make a value a geometric algebra multivector
    pub fn geometric(mut self, dims_or_blades: &Value, env: &Uiua) -> UiuaResult<Self> {
        if !matches!(self, Value::Byte(_) | Value::Num(_)) {
            return Err(env.error(format!(
                "GA multivector must be numbers, but it is {}",
                self.type_name_plural()
            )));
        }
        if dims_or_blades.rank() == 0 {
            let dims = dims_or_blades.as_byte(env, "GA dims must be a small positive integer")?;
            let spec = Spec {
                dims: Some(dims),
                metrics: Metrics::EUCLIDEAN,
            };
            self.meta.ga_spec = spec;
            Ok(self)
        } else {
            let spec = Spec {
                dims: None,
                metrics: Metrics::EUCLIDEAN,
            };
            pad_blades(spec, dims_or_blades, self, env).map(Into::into)
        }
    }
}

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

impl Spec {
    pub const DEFAULT: Self = Self {
        dims: None,
        metrics: Metrics::NULL,
    };
}

fn ga_arg(value: Value, env: &Uiua) -> UiuaResult<(Array<f64>, Shape, usize)> {
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
    let blade_count = semishape.pop().unwrap_or(1);
    Ok((arr, semishape, blade_count))
}

/// Mapping from coefficient index to array index
type Sel = Vec<Option<usize>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
enum Mode {
    Scalar,
    Vector,
    Even,
    #[default]
    Full,
}
use Mode::*;
impl Mode {
    fn size(self, dims: u8) -> usize {
        match self {
            Scalar => 1,
            Vector => dims as usize,
            Even => 1 << dims.saturating_sub(1),
            Full => 1 << dims,
        }
    }
    fn combine(self, other: Self, vector_hint: VectorHint) -> Self {
        match (self, other) {
            (Scalar, Scalar) => Scalar,
            (Scalar, m) | (m, Scalar) => match (vector_hint, m) {
                (SameSize, Vector) => Full,
                _ => m,
            },
            (Full, _) | (_, Full) => Full,
            (Vector, Vector) => match vector_hint {
                SameSize => Vector,
                Rotor => Even,
                ExpandFull => Full,
            },
            (Even, Vector) | (Vector, Even) => Full,
            (Even, Even) => Even,
        }
    }
}

/// What to do with the size of a vector input
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VectorHint {
    /// Keep the size of the vector
    SameSize,
    /// Form a rotor
    Rotor,
    /// Expand to the full multivector
    ExpandFull,
}
use VectorHint::*;

use super::{
    pervade::{self, bin_pervade_mut, bin_pervade_recursive, InfalliblePervasiveFn},
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
        } else if size == dims as usize {
            (dims, Vector)
        } else if size == 1 {
            (dims, Scalar)
        } else {
            return Err(env.error(format!(
                "{size} is not a valid array size \
                for geometric algebra in {dims} dimension{}",
                if dims == 1 { "" } else { "s" }
            )));
        }
    } else {
        if size > MAX_SIZE {
            return Err(env.error(format!("{size} is too large for geometric algebra")));
        }
        if size.is_power_of_two() {
            ((size as f64).log2().round() as u8 + 1, Even)
        } else {
            (size as u8, Vector)
        }
    })
}

fn dim_selector(dims: u8, elem_size: usize, env: &Uiua) -> UiuaResult<(Sel, Mode)> {
    let (_, this_mode) = derive_dims_mode(Some(dims), elem_size, env)?;
    let sel = match this_mode {
        Full => (0..1 << dims).map(Some).collect(),
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
        Vector => {
            let mut sel = vec![None; 1 << dims];
            for i in 0..dims as usize {
                sel[i + 1] = Some(i);
            }
            sel
        }
        Scalar => once(Some(0))
            .chain(repeat_n(None, (1 << dims) - 1))
            .collect(),
    };
    Ok((sel, this_mode))
}

fn grade_size(dims: u8, grade: u8) -> usize {
    combinations(dims as usize, grade as usize, false) as usize
}

fn blade_grades(dims: u8) -> impl Iterator<Item = u8> {
    (0..=dims).flat_map(move |i| repeat_n(i, grade_size(dims, i)))
}

#[derive(Clone, Default)]
struct Arg {
    arr: Array<f64>,
    semi: Shape,
    sel: Sel,
    mode: Mode,
}
fn init_arr<const N: usize>(
    vals: [Value; N],
    vector_hint: VectorHint,
    env: &Uiua,
) -> UiuaResult<(u8, usize, [Arg; N])> {
    let mut args = array::from_fn(|_| Arg::default());
    let mut sizes = [0; N];
    let mut max_size = 0;
    let mut spec = None;
    for (i, val) in vals.into_iter().enumerate() {
        let spec = spec.get_or_insert(val.meta.ga_spec);
        *spec = resolve_specs_impl(*spec, val.meta.ga_spec, env)?;
        let (arr, semi, size) = ga_arg(val, env)?;
        max_size = max_size.max(size);
        args[i].arr = arr;
        args[i].semi = semi;
        sizes[i] = size;
    }
    let spec = spec.unwrap();
    let (dims, _) = derive_dims_mode(spec.dims, max_size, env)?;
    for i in 0..N {
        let (sel, mode) = dim_selector(dims, sizes[i], env)?;
        args[i].sel = sel;
        args[i].mode = mode;
    }
    let size = args
        .iter()
        .map(|a| a.mode)
        .reduce(|a, b| a.combine(b, vector_hint))
        .unwrap_or_default()
        .size(dims);
    Ok((dims, size, args))
}
fn init(val: Value, vector_hint: VectorHint, env: &Uiua) -> UiuaResult<(u8, usize, Arg)> {
    let (dims, size, [arg]) = init_arr([val], vector_hint, env)?;
    Ok((dims, size, arg))
}
impl Arg {
    fn map(self, f: impl FnOnce(Self) -> Array<f64>) -> Self {
        let (semi, sel, mode) = (self.semi.clone(), self.sel.clone(), self.mode);
        let arr = f(self);
        Self {
            arr,
            semi,
            sel,
            mode,
        }
    }
    fn from_not_transposed(dims: u8, arr: Array<f64>, env: &Uiua) -> UiuaResult<Self> {
        let mut semi = arr.shape.clone();
        let size = semi.pop().unwrap_or(1);
        let (sel, mode) = dim_selector(dims, size, env)?;
        Ok(Arg {
            arr,
            semi,
            sel,
            mode,
        })
    }
}
impl fmt::Debug for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Arg({:?}, {:?}, ", self.arr, self.semi)?;
        for i in &self.sel {
            match i {
                Some(i) => write!(f, "{i}"),
                None => write!(f, "_"),
            }?
        }
        write!(f, ")")
    }
}

fn is_complex(dims: u8, a: &Array<f64>) -> bool {
    a.shape.last() == Some(&2) && dims == 2
}

fn fast_monadic_complex(
    mut arr: Array<f64>,
    f: impl Fn(f64, f64) -> [f64; 2],
) -> Result<Array<f64>, Array<f64>> {
    if arr.shape.last() != Some(&2) || arr.meta.ga_spec.dims.unwrap_or(2) != 2 {
        return Err(arr);
    }
    arr.meta.take_sorted_flags();
    for chunk in arr.data.as_mut_slice().chunks_exact_mut(2) {
        let [r, i] = [chunk[0], chunk[1]];
        let [r, i] = f(r, i);
        chunk[0] = r;
        chunk[1] = i;
    }
    Ok(arr)
}

fn fast_dyadic_complex(
    dims: Option<u8>,
    mut a: Array<f64>,
    mut b: Array<f64>,
    f: impl Fn(f64, f64, f64, f64) -> [f64; 2],
) -> Result<Array<f64>, [Array<f64>; 2]> {
    if a.shape.last() != Some(&2) || b.shape.last() != Some(&2) || dims.unwrap_or(2) != 2 {
        return Err([a, b]);
    }
    a.meta.take_sorted_flags();
    b.meta.take_sorted_flags();
    if a.data.is_copy_of(&b.data) {
        drop(b);
        for chunk in a.data.as_mut_slice().chunks_exact_mut(2) {
            let [ar, ai] = [chunk[0], chunk[1]];
            let [r, i] = f(ar, ai, ar, ai);
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
                    let [r, i] = f(ar, ai, br, bi);
                    b[0] = r;
                    b[1] = i;
                }
                Ok(b)
            }
            (_, [2]) => {
                let [br, bi] = [b.data[0], b.data[1]];
                for a in a.data.as_mut_slice().chunks_exact_mut(2) {
                    let [ar, ai] = [a[0], a[1]];
                    let [r, i] = f(ar, ai, br, bi);
                    a[0] = r;
                    a[1] = i;
                }
                Ok(a)
            }
            ([2], _) => {
                let [ar, ai] = [a.data[0], a.data[1]];
                for b in b.data.as_mut_slice().chunks_exact_mut(2) {
                    let [br, bi] = [b[0], b[1]];
                    let [r, i] = f(ar, ai, br, bi);
                    b[0] = r;
                    b[1] = i;
                }
                Ok(b)
            }
            _ => Err([a, b]),
        }
    }
}

pub fn reverse(val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, _, arg) = init(val, SameSize, env)?;
    Ok(reverse_impl_not_transposed(dims, arg))
}

fn reverse_impl_not_transposed(dims: u8, mut arg: Arg) -> Array<f64> {
    let size = arg.arr.shape.last().copied().unwrap_or(1);
    let slice = arg.arr.data.as_mut_slice();
    for (i, g) in blade_grades(dims).enumerate() {
        if let Some(i) = arg.sel[i] {
            if g / 2 % 2 == 1 {
                for v in slice.chunks_exact_mut(size) {
                    v[i] = -v[i];
                }
            }
        }
    }
    arg.arr.meta.take_sorted_flags();
    arg.arr
}

fn reverse_impl_transposed(dims: u8, mut arg: Arg) -> Array<f64> {
    for (i, g) in blade_grades(dims).enumerate() {
        if let Some(i) = arg.sel[i] {
            if g / 2 % 2 == 1 {
                for v in arg.arr.row_slice_mut(i) {
                    *v = -*v;
                }
            }
        }
    }
    arg.arr.meta.take_sorted_flags();
    arg.arr
}

fn pseudo(dims: u8, env: &Uiua) -> UiuaResult<Arg> {
    let mut pseudoscalar = eco_vec![0.0; 1 << dims];
    *pseudoscalar.make_mut().last_mut().unwrap() = 1.0;
    Arg::from_not_transposed(dims, pseudoscalar.into(), env)
}

pub fn dual(val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, _, arg) = init(val, ExpandFull, env)?;
    let mode = arg.mode;
    let pseudoscalar = pseudo(dims, env)?;
    let semi = arg.semi.clone();
    let mut arr = dual_impl(dims, pseudoscalar, arg, env)?;
    if mode == Vector && dims % 2 == 1 {
        let grades: Vec<u8> = (0..=dims).filter(|&d| d % 2 == 0).collect();
        arr = extract_blades_impl(dims, 1 << dims, arr, semi, &grades, env)?;
    }
    Ok(arr)
}

fn dual_impl(dims: u8, pseu: Arg, arg: Arg, env: &Uiua) -> UiuaResult<Array<f64>> {
    product_impl_not_transposed(dims, Metrics::EUCLIDEAN, 1 << dims, false, pseu, arg, env)
}

pub fn magnitude(val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let metrics = val.meta.ga_spec.metrics;
    let (dims, _, arg) = init(val, ExpandFull, env)?;
    magnitude_impl(dims, metrics, arg, env)
}

fn magnitude_impl(dims: u8, metrics: Metrics, mut arg: Arg, env: &Uiua) -> UiuaResult<Array<f64>> {
    if is_complex(dims, &arg.arr) {
        let slice = arg.arr.data.as_mut_slice();
        for i in 0..slice.len() / 2 {
            let [re, im] = [slice[i * 2], slice[i * 2 + 1]];
            slice[i] = (re * re + im * im).sqrt();
        }
        let new_len = slice.len() / 2;
        arg.arr.data.truncate(new_len);
        arg.arr.shape.pop();
        arg.arr.meta.take_sorted_flags();
        arg.arr.validate();
        return Ok(arg.arr);
    }

    arg.arr.untranspose();
    let rev = arg.clone().map(|arg| reverse_impl_transposed(dims, arg));
    let prod = product_impl_transposed(dims, metrics, 1 << dims, false, rev, arg, env)?;
    let mut arr = prod.first(env)?;
    for v in arr.data.as_mut_slice() {
        *v = v.abs().sqrt();
    }
    Ok(arr)
}

pub fn normalize(val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let metrics = val.meta.ga_spec.metrics;
    let (dims, _, arg) = init(val, Rotor, env)?;
    normalize_impl_not_transposed(dims, metrics, arg, env)
}

fn normalize_impl_not_transposed(
    dims: u8,
    metrics: Metrics,
    arg: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let mut arr = arg.arr.clone();
    let mag = magnitude_impl(dims, metrics, arg, env)?;
    bin_pervade_mut(mag, &mut arr, false, env, |a, b| div(b, a))?;
    Ok(arr)
}

fn div(num: f64, denom: f64) -> f64 {
    if denom == 0.0 {
        0.0
    } else {
        num / denom
    }
}

pub fn sqrt(val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (arr, ..) = ga_arg(val, env)?;
    fast_monadic_complex(arr, |re, im| {
        if im == 0.0 {
            if re >= 0.0 {
                [re.sqrt(), 0.0]
            } else {
                [0.0, re.abs().sqrt()]
            }
        } else {
            let r = (re * re + im * im).sqrt().sqrt();
            let theta = im.atan2(re) / 2.0;
            [r * theta.cos(), r * theta.sin()]
        }
    })
    .map_err(|_| env.error("Geometric square root is only implemented for complexes"))
}

pub fn subtract(a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    add(a.scalar_neg(env)?, b, env)
}

pub fn add(a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [mut a, mut b]) = init_arr([a, b], SameSize, env)?;

    // println!("a: {a}, semi: {asemi}, sel: {a_sel:?}");
    // println!("b: {b}, semi: {bsemi}, sel: {b_sel:?}");
    // println!("size: {size}");

    let [a_arr, b_arr] = match fast_dyadic_complex(Some(dims), a.arr, b.arr, |ar, ai, br, bi| {
        [ar + br, ai + bi]
    }) {
        Ok(res) => return Ok(res),
        Err(ab) => ab,
    };
    a.arr = a_arr;
    b.arr = b_arr;

    a.arr.untranspose();
    b.arr.untranspose();

    let (csel, _) = dim_selector(dims, size, env)?;
    let mut csemi = derive_new_shape(&a.semi, &b.semi, Err(""), Err(""), env)?;
    let mut c_data = eco_vec![0.0; size * csemi.elements()];

    if csemi.contains(&0) {
        csemi.push(size);
        return Ok(Array::new(csemi, c_data));
    }

    let a_row_len = a.semi.elements();
    let b_row_len = b.semi.elements();
    let c_row_len = csemi.elements();
    let a_slice = a.arr.data.as_slice();
    let b_slice = b.arr.data.as_slice();
    let c_slice = c_data.make_mut();

    let add = InfalliblePervasiveFn::new(pervade::scalar_add::num_num);
    for i in 0..1usize << dims {
        let Some(ci) = csel[i] else {
            continue;
        };
        match (a.sel[i], b.sel[i]) {
            (Some(ai), Some(bi)) => {
                let asl = &a_slice[ai * a_row_len..][..a_row_len];
                let bsl = &b_slice[bi * b_row_len..][..b_row_len];
                let c = &mut c_slice[ci * c_row_len..][..c_row_len];
                bin_pervade_recursive((asl, &a.semi), (bsl, &b.semi), c, None, None, add, env)?;
            }
            (Some(ai), None) => {
                let asl = &a_slice[ai * a_row_len..][..a_row_len];
                for c in c_slice[ci * c_row_len..][..c_row_len].chunks_exact_mut(a_row_len) {
                    c.copy_from_slice(asl);
                }
            }
            (None, Some(bi)) => {
                let bsl = &b_slice[bi * b_row_len..][..b_row_len];
                for c in c_slice[ci * c_row_len..][..c_row_len].chunks_exact_mut(b_row_len) {
                    c.copy_from_slice(bsl);
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

pub fn rotor(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    // |1+|ab̃||
    let (dims, size, [a, b]) = init_arr([a, b], Rotor, env)?;
    let revb = b.map(|b| reverse_impl_not_transposed(dims, b));
    let prod = product_impl_not_transposed(dims, spec.metrics, size, false, revb, a, env)?;
    let prod = Arg::from_not_transposed(dims, prod, env)?;
    let mut norm = normalize_impl_not_transposed(dims, spec.metrics, prod, env)?;
    for chunk in norm.data.as_mut_slice().chunks_exact_mut(size) {
        chunk[0] += 1.0;
    }
    let arg = Arg::from_not_transposed(dims, norm, env)?;
    normalize_impl_not_transposed(dims, spec.metrics, arg, env)
}

pub fn divide(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (a, ..) = ga_arg(a, env)?;
    let (b, ..) = ga_arg(b, env)?;
    Ok(
        match fast_dyadic_complex(spec.dims, a, b, |ar, ai, br, bi| {
            let denom = ar * ar + ai * ai;
            [(ar * br + ai * bi) / denom, (ar * bi - ai * br) / denom]
        }) {
            Ok(res) => res,
            Err([a, mut b]) => {
                if a.rank() >= b.rank() {
                    return Err(env.error(format!(
                        "Only scalar division on multivectors and complex division \
                        are supported, but the arrays are shape {} and {}",
                        a.shape, b.shape
                    )));
                }
                bin_pervade_mut(a, &mut b, false, env, pervade::div::num_num)?;
                b
            }
        },
    )
}

pub fn sandwich(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr([a, b], Rotor, env)?;
    let (amode, bmode) = (a.mode, b.mode);
    let rev_a = a.clone().map(|a| reverse_impl_not_transposed(dims, a));
    let ab = product_impl_not_transposed(dims, spec.metrics, size, false, b, a, env)?;
    let ab = Arg::from_not_transposed(dims, ab, env)?;
    let mut res = product_impl_not_transposed(dims, spec.metrics, size, false, rev_a, ab, env)?;
    if let (Vector, Even) | (Even, Vector) = (amode, bmode) {
        extract_single(&mut res, 1, dims as usize);
    }
    Ok(res)
}

pub fn inner_product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr([a, b], ExpandFull, env)?;
    product_impl_not_transposed(dims, spec.metrics, size, true, a, b, env)
}

pub fn wedge_product(a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr([a, b], Rotor, env)?;
    product_impl_not_transposed(dims, Metrics::NULL, size, false, a, b, env)
}

pub fn regressive_product(a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, _, [a, b]) = init_arr([a, b], ExpandFull, env)?;
    let pseudoscalar = pseudo(dims, env)?;
    let modes = (a.mode, b.mode);
    let adual =
        Arg::from_not_transposed(dims, dual_impl(dims, pseudoscalar.clone(), a, env)?, env)?;
    let bdual =
        Arg::from_not_transposed(dims, dual_impl(dims, pseudoscalar.clone(), b, env)?, env)?;
    let wedge =
        product_impl_not_transposed(dims, Metrics::NULL, 1 << dims, false, adual, bdual, env)?;
    let arg = Arg::from_not_transposed(dims, wedge, env)?;
    let mut arr = dual_impl(dims, pseudoscalar, arg, env)?;
    if modes == (Even, Even) {
        extract_vectors(dims, &mut arr);
    }
    Ok(arr)
}

pub fn product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr([a, b], Rotor, env)?;
    product_impl_not_transposed(dims, spec.metrics, size, false, a, b, env)
}
fn product_impl_not_transposed(
    dims: u8,
    metrics: Metrics,
    size: usize,
    dot: bool,
    mut a: Arg,
    mut b: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    // Scalar case
    if a.arr.rank() == 0 || b.arr.rank() == 0 {
        bin_pervade_mut(a.arr, &mut b.arr, false, env, pervade::scalar_mul::num_num)?;
        b.arr.meta.take_sorted_flags();
        return Ok(b.arr);
    }

    // Fast case for complex numbers
    let [a_arr, b_arr] = match fast_dyadic_complex(Some(dims), a.arr, b.arr, |ar, ai, br, bi| {
        [ar * br - ai * bi, ar * bi + ai * br]
    }) {
        Ok(res) => return Ok(res),
        Err(ab) => ab,
    };
    a.arr = a_arr;
    b.arr = b_arr;

    a.arr.untranspose();
    b.arr.untranspose();
    let mut res = product_impl_transposed(dims, metrics, size, dot, a, b, env)?;
    res.transpose();
    res.meta.take_sorted_flags();
    Ok(res)
}
fn product_impl_transposed(
    dims: u8,
    metrics: Metrics,
    size: usize,
    dot: bool,
    a: Arg,
    b: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let Arg {
        arr: a,
        semi: asemi,
        sel: asel,
        ..
    } = a;
    let Arg {
        arr: b,
        semi: bsemi,
        sel: bsel,
        ..
    } = b;

    let (csel, _) = dim_selector(dims, size, env)?;
    let mut csemi = derive_new_shape(&asemi, &bsemi, Err(""), Err(""), env)?;
    let mut c_data = eco_vec![0.0; size * csemi.elements()];

    // println!("dims: {dims}, metrics: {metrics:?}, size: {size}");
    // println!("a_sel: {asel:?}");
    // println!("b_sel: {bsel:?}");
    // println!("c_sel: {csel:?}");

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

    let mul = InfalliblePervasiveFn::new(pervade::scalar_mul::num_num);
    for i in 0..1usize << dims {
        if dims > 5 {
            env.respect_execution_limit()?;
        }
        let i_mask = mask_table[i];
        for j in 0..1usize << dims {
            let j_mask = mask_table[j];
            let (sign, metric) = blade_sign_and_metric(dims, metrics, dot, i_mask, j_mask);
            if metric == 0.0 {
                continue;
            }
            let k_mask = j_mask ^ i_mask;
            let k = rev_mask_table[k_mask];
            let (Some(ai), Some(aj), Some(ci)) = (asel[j], bsel[i], csel[k]) else {
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
            for (c, t) in c_slice[ci * c_row_len..][..c_row_len].iter_mut().zip(&temp) {
                *c += *t;
            }
        }
    }

    csemi.prepend(size);
    Ok(Array::new(csemi, c_data))
}

fn mask_table(dims: u8) -> Vec<usize> {
    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by_key(|&a| a.count_ones());
    mask_table
}

fn blade_sign_and_metric(dims: u8, metrics: Metrics, dot: bool, a: usize, b: usize) -> (i32, f64) {
    let mut sign = 1;
    if dims >= 3 {
        let ab = a ^ b;
        for i in [a, b, ab] {
            if (i ^ (i >> 1)).count_ones() == dims as u32 {
                sign = -sign;
            }
        }
    }
    let mut metric = (!dot || a == 0 || b == 0 || a & b != 0) as u8 as f64;
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Metrics(u32);
impl Default for Metrics {
    fn default() -> Self {
        Metrics::EUCLIDEAN
    }
}
impl Metrics {
    pub const COUNT: usize = size_of::<u32>() << 3 >> 1;
    pub const EUCLIDEAN: Self = Self::all(1);
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
                if let Some(last) = arr.data.last() {
                    for i in arr.data.len()..Metrics::COUNT {
                        metrics.set(i, *last as i8);
                    }
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
                if let Some(last) = arr.data.last() {
                    for i in arr.data.len()..Metrics::COUNT {
                        metrics.set(i, *last as i8);
                    }
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

pub fn pad_blades(spec: Spec, grades: &Value, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let grades = grades.as_bytes(env, "Grades must be a list of natural numbers")?;
    let dims = (spec.dims).unwrap_or_else(|| grades.iter().copied().max().unwrap_or(2));
    // Process grades
    for &grade in &*grades {
        if grade > dims {
            return Err(env.error(format!(
                "Cannot pad grade {grade} blades in {dims} dimensions"
            )));
        }
    }
    // Process arg
    let (arr, semi, size) = ga_arg(val, env)?;
    // Validate size
    let correct_size: usize = grades.iter().map(|&grade| grade_size(dims, grade)).sum();
    if size != correct_size {
        return Err(env.error(if let [grade] = *grades {
            format!(
                "{dims}D multivector should have {correct_size} \
                grade-{grade} blades, but the array has {size}"
            )
        } else {
            format!(
                "{dims}D multivector should have {correct_size} \
                blades for the given selector, but the array has {size}"
            )
        }));
    }
    if (grades.iter().enumerate()).any(|(i, grade)| grades[i + 1..].contains(grade)) {
        return Err(env.error("Selected grades must be unique"));
    }

    let full_size = 1usize << dims;
    let mut new_shape = semi;
    new_shape.push(full_size);
    let mut new_data = eco_vec![0.0; new_shape.elements()];
    let slice = new_data.make_mut();

    if let [grade] = *grades {
        let left_size: usize = (0..grade).map(|g| grade_size(dims, g)).sum();
        for (src, dst) in (arr.data.chunks_exact(size)).zip(slice.chunks_exact_mut(full_size)) {
            dst[left_size..][..size].copy_from_slice(src);
        }
    } else {
        let mut left_sizes = Vec::with_capacity(grades.len());
        for &grade in &*grades {
            let left_size: usize = (0..grade).map(|g| grade_size(dims, g)).sum();
            let size = grade_size(dims, grade);
            left_sizes.push((left_size, size));
        }
        for (src, dst) in (arr.data.chunks_exact(size)).zip(slice.chunks_exact_mut(full_size)) {
            let mut offset = 0;
            for &(left_size, size) in &left_sizes {
                dst[left_size..][..size].copy_from_slice(&src[offset..][..size]);
                offset += size;
            }
        }
    }
    let mut arr = Array::new(new_shape, new_data);
    arr.meta.ga_spec = Spec {
        dims: Some(dims),
        metrics: spec.metrics,
    };
    Ok(arr)
}

pub fn extract_blades(spec: Spec, grades: Value, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let Some(dims) = spec.dims else {
        return Err(env.error("Blade padding requires a specified number of dimensions"));
    };
    // Process grades
    let grades = grades.as_bytes(env, "Grades must be a list of natural numbers")?;
    for &grade in &*grades {
        if grade > dims {
            return Err(env.error(format!(
                "Cannot extract grade {grade} blades in {dims} dimensions"
            )));
        }
    }
    // Process arg
    let (arr, semi, size) = ga_arg(val, env)?;
    if grades.is_empty() {
        let mut shape = semi;
        shape.push(0);
        return Ok(Array::new(shape, EcoVec::new()));
    }
    // Validate size
    let full_size = 1usize << dims;
    let half_size = full_size / 2;
    if size != full_size {
        if size == half_size {
            for &grade in &*grades {
                if grade % 2 == 1 {
                    return Err(env.error(format!(
                        "Cannot extract odd grade {grade} blades from \
                        even multivector of size {half_size} in {dims} dimensions"
                    )));
                }
            }
        } else {
            return Err(env.error(format!(
                "{dims}D multivector should have {full_size} \
                or {half_size} blades, but the array has {size}"
            )));
        }
    }
    if (grades.iter().enumerate()).any(|(i, grade)| grades[i + 1..].contains(grade)) {
        return Err(env.error("Selected grades must be unique"));
    }
    extract_blades_impl(dims, size, arr, semi, &grades, env)
}

fn extract_blades_impl(
    dims: u8,
    size: usize,
    mut arr: Array<f64>,
    semi: Shape,
    grades: &[u8],
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let slice = arr.data.as_mut_slice();
    let new_size: usize = grades.iter().map(|&grade| grade_size(dims, grade)).sum();
    let full_size = 1usize << dims;
    let left_size = |grade| {
        if size == full_size {
            (0..grade).map(|g| grade_size(dims, g)).sum::<usize>()
        } else {
            (0..grade)
                .filter(|&g| g % 2 == 0)
                .map(|g| grade_size(dims, g))
                .sum()
        }
    };
    if let [grade] = *grades {
        let left_size = left_size(grade);
        extract_single(&mut arr, left_size, new_size);
    } else if grades.is_sorted() {
        let mut left_sizes = Vec::with_capacity(grades.len());
        for &grade in grades {
            let left_size = left_size(grade);
            let size = grade_size(dims, grade);
            left_sizes.push((left_size, size));
        }
        let arr_size = size;
        for i in 0..semi.elements() {
            let mut offset = 0;
            for &(left_size, size) in &left_sizes {
                let src_start = i * arr_size + left_size;
                let dst_start = i * new_size + offset;
                let src_end = src_start + size;
                slice.copy_within(src_start..src_end, dst_start);
                offset += size;
            }
        }
    } else {
        return Err(env.error("Grades must be sorted"));
    }
    *arr.shape.last_mut().unwrap() = new_size;
    arr.data.truncate(arr.shape.elements());
    arr.validate();
    Ok(arr)
}

fn extract_vectors(dims: u8, arr: &mut Array<f64>) {
    extract_single(arr, 1, dims as usize)
}

fn extract_single(arr: &mut Array<f64>, left_size: usize, new_size: usize) {
    let elems: usize = arr.shape.iter().rev().skip(1).product();
    let size = *arr.shape.last().unwrap();
    let slice = arr.data.as_mut_slice();
    for i in 0..elems {
        let src_start = i * size + left_size;
        let dst_start = i * new_size;
        let src_end = src_start + new_size;
        slice.copy_within(src_start..src_end, dst_start);
    }
    *arr.shape.last_mut().unwrap() = new_size;
    arr.data.truncate(arr.shape.elements());
}

pub fn couple(mut a: Value, mut b: Value, env: &Uiua) -> UiuaResult<Value> {
    match (&a, &b) {
        (Value::Num(_) | Value::Byte(_), Value::Num(_) | Value::Byte(_)) => {}
        (a, b) => {
            return Err(env.error(format!(
                "Cannot geometric couple {} and {} arrays",
                a.type_name(),
                b.type_name()
            )))
        }
    }
    if a.shape.ends_with(&b.shape) || b.shape.ends_with(&a.shape) {
        a.retropose_depth(0);
        b.retropose_depth(0);
        let mut coupled = a.couple_infallible(b, true);
        coupled.retropose_depth(0);
        Ok(coupled)
    } else {
        Err(env.error(format!(
            "Arrays with shapes {} and {} cannot be geometric coupled",
            a.shape, b.shape
        )))
    }
}

pub fn uncouple(mut val: Value, env: &Uiua) -> UiuaResult<(Value, Value)> {
    match val {
        Value::Num(_) | Value::Byte(_) => {}
        val => {
            return Err(env.error(format!(
                "Cannot geometric uncouple {}",
                val.type_name_plural()
            )))
        }
    }
    if val.shape.last().is_none_or(|&d| d == 1) {
        val.shape.pop();
        let imag = Array::<u8>::new(val.shape.clone(), eco_vec![0; val.shape.elements()]);
        return Ok((val, imag.into()));
    }
    let depth = val.rank().saturating_sub(1);
    val.uncouple_depth(depth, env)
}

pub fn parse(_: Spec, _: Value, env: &Uiua) -> UiuaResult<Value> {
    Err(env.error("Geometric parse is not implemented"))
}

pub fn unparse(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Value> {
    let (dims, size, arg) = init(val, ExpandFull, env)?;
    let dim_offset = (spec.metrics.get(0) != 0) as usize;
    if dims as usize + dim_offset > 9 {
        return Err(env.error(format!(
            "Cannot format {dims} dimensional multivector \
            starting at {dim_offset}"
        )));
    }
    let mut formatted = EcoVec::with_capacity(arg.semi.elements());
    let mask_table = mask_table(dims);
    let is_complex = dims == 2 && size == 2;
    for chunk in arg.arr.data.chunks_exact(size) {
        let mut s = EcoVec::new();
        for (i, &sel) in arg.sel.iter().enumerate() {
            let Some(sel) = sel else {
                continue;
            };
            let n = chunk[sel];
            if n == 0.0 {
                continue;
            }
            if s.is_empty() {
                if n < 0.0 {
                    s.push('-');
                }
            } else {
                s.extend(
                    match (n > 0.0, is_complex) {
                        (true, false) => " + ",
                        (true, true) => "+",
                        (false, false) => " - ",
                        (false, true) => "-",
                    }
                    .chars(),
                );
            }
            let mask = mask_table[i];
            if n.abs() != 1.0 || mask == 0 {
                let n_grid = n.abs().fmt_grid(Default::default());
                s.extend(n_grid.into_iter().next().unwrap());
            }
            if mask == 0 {
                continue;
            }
            if is_complex {
                s.push('i');
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
                s.make_mut().swap(a, b);
            }
        }
        formatted.push(Boxed(Value::from(s)))
    }
    Ok(if arg.semi.is_empty() {
        formatted.into_iter().next().unwrap().0
    } else {
        Array::new(arg.semi, formatted).into()
    })
}
