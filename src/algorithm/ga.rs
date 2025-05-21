//! Geometric Algebra

use std::{
    array, fmt,
    iter::{once, repeat_n},
};

use ecow::{eco_vec, EcoVec};
use serde::*;

use crate::{
    algorithm::pervade::derive_new_shape, grid_fmt::GridFmt, is_default, Array, Primitive, Shape,
    Uiua, UiuaResult, Value,
};

macro_rules! ga_op {
    ($(($args:literal, $name:ident)),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        pub enum GaOp {
            $($name,)*
        }

        impl GaOp {
            pub fn args(&self) -> usize {
                match self {
                    $(GaOp::$name => $args,)*
                }
            }
        }
    };
}

ga_op!(
    (1, GeometricMagnitude),
    (1, GeometricNormalize),
    (1, GeometricSqrt),
    (1, GeometricReverse),
    (1, GeometricDual),
    (1, GeometricAdd),
    (1, GeometricSub),
    (2, GeometricProduct),
    (2, GeometricDivide),
    (2, GeometricRotor),
    (2, GeometricSandwich),
    (2, GeometricWedge),
    (2, GeometricRegressive),
    (2, PadBlades),
    (2, ExtractBlades),
);

impl fmt::Display for GaOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use {GaOp::*, Primitive::*};
        match self {
            GeometricMagnitude => write!(f, "{Geometric}{Abs}"),
            GeometricNormalize => write!(f, "{Geometric}{Sign}"),
            GeometricSqrt => write!(f, "{Geometric}{Sqrt}"),
            GeometricReverse => write!(f, "{Geometric}{Neg}"),
            GeometricDual => write!(f, "{Geometric}{Not}"),
            GeometricAdd => write!(f, "{Geometric}{Add}"),
            GeometricSub => write!(f, "{Geometric}{Sub}"),
            GeometricProduct => write!(f, "{Geometric}{Mul}"),
            GeometricDivide => write!(f, "{Geometric}{Div}"),
            GeometricRotor => write!(f, "{Geometric}{Atan}"),
            GeometricSandwich => write!(f, "{Geometric}{Rotate}"),
            GeometricWedge => write!(f, "{Geometric}{Min}"),
            GeometricRegressive => write!(f, "{Geometric}{Max}"),
            PadBlades => write!(f, "{Geometric}{Anti}{Select}"),
            ExtractBlades => write!(f, "{Geometric}{Select}"),
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
    spec: Spec,
    vals: [Value; N],
    vector_hint: VectorHint,
    env: &Uiua,
) -> UiuaResult<(u8, usize, [Arg; N])> {
    let mut args = array::from_fn(|_| Arg::default());
    let mut sizes = [0; N];
    let mut max_size = 0;
    for (i, val) in vals.into_iter().enumerate() {
        let (arr, semi, size) = ga_arg(val, env)?;
        max_size = max_size.max(size);
        args[i].arr = arr;
        args[i].semi = semi;
        sizes[i] = size;
    }
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
fn init(
    spec: Spec,
    val: Value,
    vector_hint: VectorHint,
    env: &Uiua,
) -> UiuaResult<(u8, usize, Arg)> {
    let (dims, size, [arg]) = init_arr(spec, [val], vector_hint, env)?;
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
    fn try_map(self, f: impl FnOnce(Self) -> UiuaResult<Array<f64>>) -> UiuaResult<Self> {
        let (semi, sel, mode) = (self.semi.clone(), self.sel.clone(), self.mode);
        let arr = f(self)?;
        Ok(Self {
            arr,
            semi,
            sel,
            mode,
        })
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

fn fast_dyadic_complex(
    dims: Option<u8>,
    mut a: Array<f64>,
    mut b: Array<f64>,
    re: impl Fn(f64, f64, f64, f64) -> f64,
    im: impl Fn(f64, f64, f64, f64) -> f64,
) -> UiuaResult<Result<Array<f64>, [Array<f64>; 2]>> {
    if a.shape.last() != Some(&2) || b.shape.last() != Some(&2) || dims.unwrap_or(2) != 2 {
        return Ok(Err([a, b]));
    }
    a.meta.take_sorted_flags();
    b.meta.take_sorted_flags();
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
            _ => Err([a, b]),
        }
    })
}

pub fn reverse(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, _, arg) = init(spec, val, SameSize, env)?;
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

pub fn dual(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, _, arg) = init(spec, val, ExpandFull, env)?;
    let pseudoscalar = pseudo(dims, env)?;
    dual_impl(dims, pseudoscalar, arg, env)
}

fn dual_impl(dims: u8, pseu: Arg, arg: Arg, env: &Uiua) -> UiuaResult<Array<f64>> {
    product_impl_not_transposed(dims, Metrics::EUCLIDEAN, 1 << dims, pseu, arg, env)
}

pub fn magnitude(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, arg) = init(spec, val, ExpandFull, env)?;
    magnitude_impl(dims, spec.metrics, size, arg, env)
}

fn magnitude_impl(
    dims: u8,
    metrics: Metrics,
    size: usize,
    mut arg: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
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
    let prod = product_impl_transposed(dims, metrics, size, rev, arg, env)?;
    let mut arr = prod.first(env)?;
    for v in arr.data.as_mut_slice() {
        *v = v.abs().sqrt();
    }
    Ok(arr)
}

pub fn normalize(spec: Spec, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, arg) = init(spec, val, Rotor, env)?;
    normalize_impl_not_transposed(dims, spec.metrics, size, arg, env)
}

fn normalize_impl_not_transposed(
    dims: u8,
    metrics: Metrics,
    size: usize,
    arg: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    let mut arr = arg.arr.clone();
    let mag = magnitude_impl(dims, metrics, size, arg, env)?;
    bin_pervade_mut(mag, &mut arr, false, env, pervade::div::num_num)?;
    Ok(arr)
}

pub fn sqrt(_spec: Spec, _val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    Err(env.error("Geometric square root is not implemented"))
}

pub fn add(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [mut a, mut b]) = init_arr(spec, [a, b], SameSize, env)?;

    // println!("a: {a}, semi: {asemi}, sel: {a_sel:?}");
    // println!("b: {b}, semi: {bsemi}, sel: {b_sel:?}");
    // println!("size: {size}");

    let [a_arr, b_arr] = match fast_dyadic_complex(
        spec.dims,
        a.arr,
        b.arr,
        |ar, _, br, _| ar + br,
        |_, ai, _, bi| ai + bi,
    )? {
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

    let add = InfalliblePervasiveFn::new(pervade::add::num_num);
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
    // |1-b̂â|
    let (dims, size, [a, b]) = init_arr(spec, [a, b], Rotor, env)?;
    let a = a.try_map(|a| normalize_impl_not_transposed(dims, spec.metrics, size, a, env))?;
    let b = b.try_map(|b| normalize_impl_not_transposed(dims, spec.metrics, size, b, env))?;
    let mut arr = product_impl_not_transposed(dims, spec.metrics, size, a, b, env)?;
    for chunk in arr.data.as_mut_slice().chunks_exact_mut(size) {
        for v in &mut *chunk {
            *v = -*v;
        }
        chunk[0] += 1.0;
    }
    let arg = Arg::from_not_transposed(dims, arr, env)?;
    normalize_impl_not_transposed(dims, spec.metrics, size, arg, env)
}

pub fn divide(a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    if a.rank() >= b.rank() {
        return Err(env.error(format!(
            "Only scalar division on multivectors is supported \
            but the arrays are rank {} and {}",
            a.rank(),
            b.rank()
        )));
    }
    let (a, ..) = ga_arg(a, env)?;
    let (mut b, ..) = ga_arg(b, env)?;
    bin_pervade_mut(a, &mut b, false, env, pervade::div::num_num)?;
    Ok(b)
}

pub fn sandwich(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr(spec, [a, b], Rotor, env)?;
    let (amode, bmode) = (a.mode, b.mode);
    let rev_a = a.clone().map(|a| reverse_impl_not_transposed(dims, a));
    let ab = product_impl_not_transposed(dims, spec.metrics, size, b, a, env)?;
    let ab = Arg::from_not_transposed(dims, ab, env)?;
    let mut res = product_impl_not_transposed(dims, spec.metrics, size, rev_a, ab, env)?;
    if let (Vector, Even) | (Even, Vector) = (amode, bmode) {
        extract_single_impl(&mut res, 1, dims as usize);
    }
    Ok(res)
}

pub fn wedge_product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr(spec, [a, b], Rotor, env)?;
    product_impl_not_transposed(dims, Metrics::NULL, size, a, b, env)
}

pub fn regressive_product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr(spec, [a, b], ExpandFull, env)?;
    let pseudoscalar = pseudo(dims, env)?;
    let adual = a.try_map(|a| dual_impl(dims, pseudoscalar.clone(), a, env))?;
    let bdual = b.try_map(|b| dual_impl(dims, pseudoscalar.clone(), b, env))?;
    let wedge = product_impl_not_transposed(dims, Metrics::NULL, size, adual, bdual, env)?;
    let arg = Arg::from_not_transposed(dims, wedge, env)?;
    dual_impl(dims, pseudoscalar, arg, env)
}

pub fn product(spec: Spec, a: Value, b: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let (dims, size, [a, b]) = init_arr(spec, [a, b], Rotor, env)?;
    product_impl_not_transposed(dims, spec.metrics, size, a, b, env)
}
fn product_impl_not_transposed(
    dims: u8,
    metrics: Metrics,
    size: usize,
    mut a: Arg,
    mut b: Arg,
    env: &Uiua,
) -> UiuaResult<Array<f64>> {
    // Scalar case
    if a.arr.rank() == 0 || b.arr.rank() == 0 {
        bin_pervade_mut(a.arr, &mut b.arr, false, env, pervade::mul::num_num)?;
        b.arr.meta.take_sorted_flags();
        return Ok(b.arr);
    }

    // Fast case for complex numbers
    let [a_arr, b_arr] = match fast_dyadic_complex(
        Some(dims),
        a.arr,
        b.arr,
        |ar, ai, br, bi| ar * br - ai * bi,
        |ar, ai, br, bi| ar * bi + ai * br,
    )? {
        Ok(res) => return Ok(res),
        Err(ab) => ab,
    };
    a.arr = a_arr;
    b.arr = b_arr;

    a.arr.untranspose();
    b.arr.untranspose();
    let mut res = product_impl_transposed(dims, metrics, size, a, b, env)?;
    res.transpose();
    res.meta.take_sorted_flags();
    Ok(res)
}
fn product_impl_transposed(
    dims: u8,
    metrics: Metrics,
    size: usize,
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

    let mut mask_table: Vec<usize> = (0..1usize << dims).collect();
    mask_table.sort_by_key(|&a| a.count_ones());
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
    for j in 0..1usize << dims {
        let j_mask = mask_table[j];
        for i in 0..1usize << dims {
            let i_mask = mask_table[i];
            let (sign, metric) = blade_sign_and_metric(j_mask, i_mask, dims, metrics);
            if metric == 0.0 {
                continue;
            }
            let k_mask = i_mask ^ j_mask;
            let k = rev_mask_table[k_mask];
            let (Some(ai), Some(aj), Some(ci)) = (asel[i], bsel[j], csel[k]) else {
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

fn blade_sign_and_metric(a: usize, b: usize, dims: u8, metrics: Metrics) -> (i32, f64) {
    let mut sign = 1;
    let ab = a ^ b;
    if (ab ^ (ab >> 1)).count_ones() == dims as u32 {
        sign = -sign;
    }
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct Metrics(u32);
impl Default for Metrics {
    fn default() -> Self {
        Metrics::EUCLIDEAN
    }
}
impl Metrics {
    pub const COUNT: usize = 11;
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

pub fn pad_blades(spec: Spec, grades: Value, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let Some(dims) = spec.dims else {
        return Err(env.error("Blade padding requires a specified number of dimensions"));
    };
    // Process grades
    let grades = grades.as_bytes(env, "Grades must be a list of natural numbers")?;
    for &grade in &grades {
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
        for grade in grades {
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
    Ok(Array::new(new_shape, new_data))
}

pub fn extract_blades(spec: Spec, grades: Value, val: Value, env: &Uiua) -> UiuaResult<Array<f64>> {
    let Some(dims) = spec.dims else {
        return Err(env.error("Blade padding requires a specified number of dimensions"));
    };
    // Process grades
    let grades = grades.as_bytes(env, "Grades must be a list of natural numbers")?;
    for &grade in &grades {
        if grade > dims {
            return Err(env.error(format!(
                "Cannot extract grade {grade} blades in {dims} dimensions"
            )));
        }
    }
    // Process arg
    let (mut arr, semi, size) = ga_arg(val, env)?;
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
            for &grade in &grades {
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

    let slice = arr.data.as_mut_slice();
    let new_size: usize = grades.iter().map(|&grade| grade_size(dims, grade)).sum();
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
        extract_single_impl(&mut arr, left_size, new_size);
    } else if grades.is_sorted() {
        let mut left_sizes = Vec::with_capacity(grades.len());
        for grade in grades {
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

fn extract_single_impl(arr: &mut Array<f64>, left_size: usize, new_size: usize) {
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
