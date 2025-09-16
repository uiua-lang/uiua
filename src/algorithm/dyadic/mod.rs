//! Algorithms for dyadic array operations

mod combine;
mod search;
mod structure;

use core::f64;
use std::{
    borrow::Cow,
    cmp::Ordering,
    hash::{DefaultHasher, Hash, Hasher},
    iter::{once, repeat_n},
    mem::{replace, swap, take},
};

use bytemuck::allocation::cast_vec;
use ecow::{eco_vec, EcoVec};
use rand::prelude::*;
#[cfg(not(target_arch = "wasm32"))]
use rayon::prelude::*;
use smallvec::SmallVec;

use crate::{
    algorithm::pervade::{self, bin_pervade_recursive, InfalliblePervasiveFn},
    array::*,
    boxed::Boxed,
    cowslice::{cowslice, extend_repeat, CowSlice},
    fill::FillValue,
    grid_fmt::GridFmt,
    val_as_arr,
    value::Value,
    Complex, Shape, Uiua, UiuaResult, RNG,
};

use super::{
    shape_prefixes_match, validate_size, validate_size_of, ArrayCmpSlice, FillContext, SizeError,
};

macro_rules! par_if {
    ($cond:expr, $if_true:expr, $if_false:expr) => {{
        #[cfg(not(target_arch = "wasm32"))]
        {
            if $cond {
                $if_true
            } else {
                $if_false
            }
        }
        #[cfg(target_arch = "wasm32")]
        $if_false
    }};
}

impl Value {
    pub(crate) fn bin_coerce_to_boxes<T, C: FillContext, E: ToString>(
        self,
        other: Self,
        ctx: &C,
        on_success: impl FnOnce(Array<Boxed>, Array<Boxed>, &C) -> Result<T, C::Error>,
        on_error: impl FnOnce(&str, &str) -> E,
    ) -> Result<T, C::Error> {
        match (self, other) {
            (Value::Box(a), Value::Box(b)) => on_success(a, b, ctx),
            (Value::Box(a), b) => on_success(a, b.coerce_to_boxes(), ctx),
            (a, Value::Box(b)) => on_success(a.coerce_to_boxes(), b, ctx),
            (a, b) => Err(ctx.error(on_error(a.type_name(), b.type_name()))),
        }
    }
    pub(crate) fn bin_coerce_to_boxes_mut<T, C: FillContext, E: ToString>(
        &mut self,
        other: Self,
        ctx: &C,
        on_success: impl FnOnce(&mut Array<Boxed>, Array<Boxed>, &C) -> Result<T, C::Error>,
        on_error: impl FnOnce(&str, &str) -> E,
    ) -> Result<T, C::Error> {
        match (self, other) {
            (Value::Box(a), Value::Box(b)) => on_success(a, b, ctx),
            (Value::Box(a), b) => on_success(a, b.coerce_to_boxes(), ctx),
            (a, Value::Box(b)) => {
                let mut a_arr = take(a).coerce_to_boxes();
                let res = on_success(&mut a_arr, b, ctx)?;
                *a = a_arr.into();
                Ok(res)
            }
            (a, b) => Err(ctx.error(on_error(a.type_name(), b.type_name()))),
        }
    }
}

impl Value {
    /// `reshape` this value with another
    pub fn reshape(&mut self, shape: &Self, env: &Uiua) -> UiuaResult {
        let target_shape = shape.as_ints_or_infs(
            env,
            "Shape should be a single integer \
            or a list of integers or infinity",
        )?;
        if shape.rank() == 0 {
            let n = target_shape[0];
            val_as_arr!(self, |a| a.reshape_scalar(n, true, env))
        } else {
            self.reshape_impl(&target_shape, env)
        }
    }
    pub(crate) fn reshape_impl(&mut self, dims: &[Result<isize, bool>], env: &Uiua) -> UiuaResult {
        self.match_fill(env);
        val_as_arr!(self, |a| a.reshape(dims, env))
    }
    pub(crate) fn undo_reshape(&mut self, old_shape: &Self, env: &Uiua) -> UiuaResult {
        if old_shape.as_nat(env, None).is_ok() {
            return Err(env.error("Cannot undo scalar reshape"));
        }
        let orig_shape = old_shape.as_nats(env, "Shape should be a list of integers")?;
        if env.fill().value_for(self).is_some()
            || orig_shape.iter().product::<usize>() == self.shape.iter().product::<usize>()
        {
            let orig_shape_spec: Vec<_> = orig_shape.iter().map(|&d| Ok(d as isize)).collect();
            self.reshape_impl(&orig_shape_spec, env)
        } else {
            Err(env.error(format!(
                "Cannot unreshape array because its old shape was {}, \
                but its new shape is {}, which has a different number of elements",
                FormatShape(&orig_shape),
                self.shape
            )))
        }
    }
    pub(crate) fn reshape_scalar<C: FillContext>(
        &mut self,
        count: Result<isize, bool>,
        use_fill: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        val_as_arr!(self, |a| a.reshape_scalar(count, use_fill, ctx))
    }
}

impl<T: ArrayValue> Array<T> {
    pub(crate) fn reshape_scalar_integer(
        &mut self,
        count: usize,
        fill: Option<FillValue<T>>,
    ) -> Result<(), SizeError> {
        if count == 0 {
            self.data.clear();
            self.shape.prepend(0);
            return Ok(());
        }
        let elem_count = validate_size_of::<T>([count - 1, self.data.len()])?;
        let has_fill = fill.is_some();
        if let Some(fill) = fill {
            self.data.extend_repeat_fill(&fill, elem_count);
        } else {
            self.data.reserve(elem_count);
            let row = self.data.to_vec();
            for _ in 1..count {
                self.data.extend_from_slice(&row);
            }
        }
        self.shape.prepend(count);
        if has_fill {
            self.meta.take_sorted_flags();
        } else {
            self.meta.mark_sorted_up(true);
            self.meta.mark_sorted_down(true);
        }
        self.validate();
        Ok(())
    }
    /// `reshape` this array by replicating it as the rows of a new array
    pub fn reshape_scalar<C: FillContext>(
        &mut self,
        count: Result<isize, bool>,
        use_fill: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        self.meta.take_map_keys();
        match count {
            Ok(count) => {
                if count < 0 {
                    self.reverse();
                }
                let fill = if use_fill {
                    ctx.scalar_fill::<T>().ok()
                } else {
                    None
                };
                self.reshape_scalar_integer(count.unsigned_abs(), fill)
                    .map_err(|e| ctx.error(e))
            }
            Err(rev) => {
                if rev {
                    self.reverse()
                }
                Ok(())
            }
        }
    }
    /// `reshape` the array
    pub fn reshape(&mut self, dims: &[Result<isize, bool>], env: &Uiua) -> UiuaResult {
        let fill = env.scalar_fill::<T>();
        let has_fill = fill.is_ok();
        let was_scalar = self.rank() == 0;
        let axes = derive_shape(&self.shape, dims, has_fill, env)?;
        if (axes.first()).is_none_or(|&d| d.unsigned_abs() != self.row_count()) {
            self.meta.take_map_keys();
        }
        let reversed_axes: Vec<usize> = (axes.iter().enumerate())
            .filter_map(|(i, &s)| if s < 0 { Some(i) } else { None })
            .collect();
        let shape: Shape = axes.iter().map(|&s| s.unsigned_abs()).collect();
        validate_size::<T>(shape.iter().copied(), env)?;
        let target_len: usize = shape.iter().product();
        if self.data.len() < target_len {
            match fill {
                Ok(fill) => {
                    let start = self.data.len();
                    self.data.extend_repeat_fill(&fill, target_len - start);
                }
                Err(e) => {
                    if self.data.is_empty() {
                        if !shape.contains(&0) {
                            return Err(env
                                .error(format!(
                                    "Cannot reshape empty array without a fill value{e}"
                                ))
                                .fill());
                        }
                    } else if self.rank() == 0 {
                        self.data = cowslice![self.data[0].clone(); target_len];
                    } else {
                        let start = self.data.len();
                        let old_data = self.data.clone();
                        self.data.reserve(target_len - self.data.len());
                        let additional = target_len - start;
                        for _ in 0..additional / start {
                            self.data.extend_from_slice(&old_data);
                        }
                        self.data.extend_from_slice(&old_data[..additional % start]);
                    }
                }
            }
        } else {
            self.data.truncate(target_len);
        }
        self.shape = shape;
        for s in reversed_axes {
            self.reverse_depth(s);
        }
        if was_scalar && !has_fill {
            self.meta.mark_sorted_up(true);
            self.meta.mark_sorted_down(true);
        } else {
            self.meta.take_sorted_flags();
        }
        self.validate();
        Ok(())
    }
}

fn derive_shape(
    shape: &[usize],
    dims: &[Result<isize, bool>],
    has_fill: bool,
    env: &Uiua,
) -> UiuaResult<Vec<isize>> {
    let mut inf_count = 0;
    for dim in dims {
        if dim.is_err() {
            inf_count += 1;
        }
    }
    let derive_len = |data_len: usize, other_len: usize| {
        (if has_fill { f32::ceil } else { f32::floor }(data_len as f32 / other_len as f32) as isize)
    };
    Ok(match inf_count {
        0 => dims.iter().map(|dim| dim.unwrap()).collect(),
        1 => {
            if let Err(rev) = dims[0] {
                let rev_mul = if rev { -1 } else { 1 };
                if dims[1..].iter().any(|&dim| dim.is_err()) {
                    return Err(env.error("Cannot reshape array with multiple infinite dimensions"));
                }
                let shape_non_leading_len =
                    dims[1..].iter().flatten().product::<isize>().unsigned_abs();
                if shape_non_leading_len == 0 {
                    return Err(env.error("Cannot reshape array with any 0 non-leading dimensions"));
                }
                let leading_len =
                    rev_mul * derive_len(shape.iter().product(), shape_non_leading_len);
                let mut axes = vec![leading_len];
                axes.extend(dims[1..].iter().flatten());
                axes
            } else if let Err(rev) = *dims.last().unwrap() {
                let rev_mul = if rev { -1 } else { 1 };
                if dims.iter().rev().skip(1).any(|&dim| dim.is_err()) {
                    return Err(env.error("Cannot reshape array with multiple infinite dimensions"));
                }
                let mut axes: Vec<isize> = dims.iter().copied().flatten().collect();
                let shape_non_trailing_len = axes.iter().copied().product::<isize>().unsigned_abs();
                if shape_non_trailing_len == 0 {
                    return Err(
                        env.error("Cannot reshape array with any 0 non-trailing dimensions")
                    );
                }
                let trailing_len =
                    rev_mul * derive_len(shape.iter().product(), shape_non_trailing_len);
                axes.push(trailing_len);
                axes
            } else {
                let inf_index = dims.iter().position(|&dim| dim.is_err()).unwrap();
                let (front, back) = dims.split_at(inf_index);
                let rev = back[0].unwrap_err();
                let rev_mul = if rev { -1 } else { 1 };
                let back = &back[1..];
                let front_len = front.iter().flatten().product::<isize>().unsigned_abs();
                let back_len = back.iter().flatten().product::<isize>().unsigned_abs();
                if front_len == 0 || back_len == 0 {
                    return Err(env.error("Cannot reshape array with any 0 outer dimensions"));
                }
                let middle_len = rev_mul * derive_len(shape.iter().product(), front_len * back_len);
                let mut axes: Vec<isize> = front.iter().copied().flatten().collect();
                axes.push(middle_len);
                axes.extend(back.iter().flatten());
                axes
            }
        }
        n => return Err(env.error(format!("Cannot reshape array with {n} infinite dimensions"))),
    })
}

impl Value {
    /// Apply the given shape to the array by either tiling or filling
    pub fn undo_shape(&mut self, shape: &Value, env: &Uiua) -> UiuaResult {
        self.match_fill(env);

        let axes_input = shape.as_ints_or_infs(env, "Shape should be integers or infinity")?;
        let mut reversed_axes = SmallVec::<[_; 32]>::new();
        let rank_shift = self.shape.len() as isize - axes_input.len() as isize;
        let shape: Shape = axes_input
            .into_iter()
            .enumerate()
            .map(|(i, ax)| match ax {
                Ok(val) => {
                    if val.is_negative() {
                        reversed_axes.push(i);
                    }
                    val.unsigned_abs()
                }
                Err(rev) => {
                    if rev {
                        reversed_axes.push(i);
                    }
                    self.shape
                        .get(i.wrapping_add_signed(rank_shift))
                        .copied()
                        .unwrap_or(1)
                }
            })
            .collect();
        val_as_arr!(self, |a| a.undo_shape(shape, reversed_axes, env))
    }
}

impl<T: ArrayValue> Array<T> {
    /// Apply the given shape to the array by either tiling or filling
    pub fn undo_shape(
        &mut self,
        shape: Shape,
        reversed_axes: impl IntoIterator<Item = usize> + Clone,
        env: &Uiua,
    ) -> UiuaResult {
        if self.shape == shape {
            return Ok(());
        }

        if shape.elements() == 0 {
            *self = Array::new(shape, []);
            return Ok(());
        }

        let rank_surplus = self.rank() as isize - shape.len() as isize;
        match rank_surplus.cmp(&0) {
            // Rank is too high, take the first row repeatedly
            Ordering::Greater => {
                let mut new = self.clone();
                for _ in 0..rank_surplus {
                    new = new.first(env)?;
                }
                *self = new;
            }
            // Rank is too low, add 1-length axes
            Ordering::Less => {
                let mut new_shape = vec![1; rank_surplus.unsigned_abs()];
                new_shape.extend_from_slice(&self.shape);
                self.shape = new_shape.into();
            }
            _ => {}
        }

        // If converting to rank 0, the rank-matching process
        // is sufficient to produce the correct result
        if shape.is_empty() {
            return Ok(());
        }

        for ax in reversed_axes.clone() {
            self.reverse_depth(ax);
        }

        if let Err(e) = env.array_fill::<T>() {
            // The case where the target shape is empty is handled at the start of the function
            if self.shape.elements() == 0 {
                return Err(env.error(format!(
                    "Cannot set non-empty shape of empty array without a fill value{e}"
                )));
            }
            let ishape: EcoVec<_> = shape.iter().map(|v| *v as isize).collect();
            let range_data: EcoVec<_> = match super::monadic::range(&ishape, 0, env)? {
                Ok(a) => a.iter().map(|v| *v as isize).collect(),
                Err(a) => a.iter().map(|v| *v as isize).collect(),
            };

            let data: EcoVec<_> = range_data
                .chunks(shape.len())
                .map(|idx| {
                    let idx = idx
                        .iter()
                        .zip(&self.shape)
                        .map(|(a, b)| a.rem_euclid(*b as isize));
                    let i = self
                        .shape
                        .i_dims_to_flat(idx)
                        .expect("Tiling index was out of bounds");
                    self.data[i].clone()
                })
                .collect();
            *self = Array::new(shape, data);
        } else {
            let dims: EcoVec<_> = shape.iter().map(|v| Ok(*v as isize)).collect();
            *self = self.clone().take(&dims, env)?;
        }

        for ax in reversed_axes {
            self.reverse_depth(ax);
        }

        Ok(())
    }
}

impl Value {
    /// `rerank` this value with another
    pub fn rerank(&mut self, rank: &Self, env: &Uiua) -> UiuaResult {
        self.meta.take_map_keys();
        let irank = rank.as_int(env, "Rank must be an integer")?;
        let shape = &mut self.shape;
        let rank = irank.unsigned_abs();
        if irank >= 0 {
            // Positive rank
            if rank >= shape.len() {
                for _ in 0..rank - shape.len() + 1 {
                    shape.prepend(1);
                }
            } else {
                let mid = shape.len() - rank;
                let new_first_dim: usize = shape[..mid].iter().product();
                *shape = once(new_first_dim)
                    .chain(shape[mid..].iter().copied())
                    .collect();
            }
        } else {
            // Negative rank
            if rank > shape.len() {
                return Err(env.error(format!(
                    "Negative rerank has magnitude {}, which is greater \
                    than the array's rank {}",
                    rank,
                    shape.len()
                )));
            }
            let new_first_dim: usize = shape[..rank].iter().product();
            *shape = once(new_first_dim)
                .chain(shape[rank..].iter().copied())
                .collect();
        }
        self.validate();
        Ok(())
    }
    pub(crate) fn undo_rerank(
        &mut self,
        rank: &Self,
        orig_shape: &Shape,
        env: &Uiua,
    ) -> UiuaResult {
        if self.rank() == 0 {
            if let Value::Box(arr) = self {
                arr.data.as_mut_slice()[0]
                    .0
                    .undo_rerank(rank, orig_shape, env)?;
            }
            return Ok(());
        }
        let irank = rank.as_int(env, "Rank must be an integer")?;
        if irank == 0 {
            return self.undo_deshape(None, orig_shape, env);
        }
        let rank = irank.unsigned_abs();
        let new_shape: Shape = if irank >= 0 {
            // Positive rank
            (orig_shape.iter())
                .take(orig_shape.len().saturating_sub(rank))
                .chain((self.shape.iter()).skip((rank + 1).saturating_sub(orig_shape.len()).max(1)))
                .copied()
                .collect()
        } else {
            // Negative rank
            (orig_shape.iter().take(rank))
                .chain(self.shape.iter().skip(1))
                .copied()
                .collect()
        };
        if validate_size::<u8>(new_shape.iter().copied(), env)? != self.shape.elements() {
            return Ok(());
        }
        self.shape = new_shape;
        self.validate();
        Ok(())
    }
}

impl Value {
    /// Use this value as counts to `keep` another
    pub fn keep(self, kept: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = self.as_nums(
            env,
            "Keep amount must be a positive real number \
            or list of natural numbers",
        )?;
        Ok(if self.rank() == 0 {
            match kept {
                Value::Num(a) => a.keep_scalar_real(counts[0], env)?.into(),
                Value::Byte(a) => a.convert::<f64>().keep_scalar_real(counts[0], env)?.into(),
                Value::Complex(a) => a.keep_scalar_real(counts[0], env)?.into(),
                Value::Char(a) => a.keep_scalar_real(counts[0], env)?.into(),
                Value::Box(a) => a.keep_scalar_real(counts[0], env)?.into(),
            }
        } else {
            val_as_arr!(kept, |a| a.keep_list(&counts, env)?.into())
        })
    }
    pub(crate) fn unkeep(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        val_as_arr!(self, |a| a.unkeep(env).map(|(a, b)| (a, b.into())))
    }
    pub(crate) fn anti_keep(self, mut kept: Self, env: &Uiua) -> UiuaResult<Self> {
        kept.match_fill(env);
        let counts = self.as_nums(env, "Keep amount must be a list of natural numbers")?;
        Ok(if self.rank() == 0 {
            if counts[0] == 0.0 {
                return Err(env.error("Scalar anti keep cannot be 0"));
            }
            let count = 1.0 / counts[0];
            match kept {
                Value::Num(a) => a.keep_scalar_real(count, env)?.into(),
                Value::Byte(a) => a.convert::<f64>().keep_scalar_real(count, env)?.into(),
                Value::Complex(a) => a.keep_scalar_real(count, env)?.into(),
                Value::Char(a) => a.keep_scalar_real(count, env)?.into(),
                Value::Box(a) => a.keep_scalar_real(count, env)?.into(),
            }
        } else {
            val_as_arr!(kept, |a| a.anti_keep(&counts, env)?.into())
        })
    }
    pub(crate) fn undo_keep(self, kept: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = self.as_nums(
            env,
            "Keep amount must be a positive real number \
            or list of natural numbers",
        )?;
        if self.rank() == 0 {
            let count = counts[0];
            if count == 0.0 {
                return Err(env.error("Cannot invert scalar keep of 0 rows"));
            }
            let recip = 1.0 / count;
            Ok(val_as_arr!(kept, |a| a
                .keep_scalar_real(recip, env)?
                .into()))
        } else {
            kept.generic_bin_into(
                into,
                |a, b| a.undo_keep(&counts, b, env).map(Into::into),
                |a, b| a.undo_keep(&counts, b, env).map(Into::into),
                |a, b| a.undo_keep(&counts, b, env).map(Into::into),
                |a, b| a.undo_keep(&counts, b, env).map(Into::into),
                |a, b| a.undo_keep(&counts, b, env).map(Into::into),
                |a, b| {
                    env.error(format!(
                        "Cannot unkeep {} array with {} array",
                        a.type_name(),
                        b.type_name()
                    ))
                },
            )
        }
    }
    pub(crate) fn multikeep(&self, target: Self, dims: usize, env: &Uiua) -> UiuaResult<Self> {
        let count = self.as_num(env, "Count must be a scalar number")?;
        if count < 0.0 {
            return Err(env.error(format!(
                "Count must be non-negative, but it is {}",
                count.grid_string(false)
            )));
        }
        val_as_arr!(target, |arr| arr
            .multikeep(dims, count, env)
            .map(Into::into))
    }
}

impl<T: ArrayValue> Array<T> {
    /// `keep` this array by replicating it as the rows of a new array
    pub fn keep_scalar_integer(mut self, count: usize, env: &Uiua) -> UiuaResult<Self> {
        let elem_count = validate_size::<T>([count, self.data.len()], env)?;
        // Scalar kept
        if self.rank() == 0 {
            self.shape.push(count);
            let value = self.data[0].clone();
            self.data.clear();
            unsafe {
                self.data
                    .extend_from_trusted((0..count).map(|_| value.clone()))
            };
            self.validate();
            return Ok(self);
        }
        Ok(match count {
            // Keep nothing
            0 => {
                self.data = CowSlice::new();
                self.shape[0] = 0;
                self
            }
            // Keep 1 is a no-op
            1 => self,
            // Keep ≥2 is a repeat
            _ => {
                let mut new_data = EcoVec::with_capacity(elem_count);
                for row in self.row_slices() {
                    for _ in 0..count {
                        new_data.extend_from_slice(row);
                    }
                }
                self.shape[0] *= count;
                self.data = new_data.into();
                self.validate();
                self
            }
        })
    }
    /// `keep` this array with a real-valued scalar
    pub fn keep_scalar_real(mut self, count: f64, env: &Uiua) -> UiuaResult<Self> {
        let abs_count = count.abs();
        if abs_count.is_infinite() {
            return Err(env.error("Cannot keep an infinite amount"));
        }
        if abs_count.fract() == 0.0 && count >= 0.0 {
            return self.keep_scalar_integer(abs_count as usize, env);
        }
        let new_row_count = validate_size::<T>(
            [(abs_count * self.row_count() as f64).round() as usize],
            env,
        )?;
        let row_len = self.row_len();
        let mut new_data = EcoVec::with_capacity(new_row_count * row_len);
        let delta = 1.0 / abs_count;
        for k in 0..new_row_count {
            let t = k as f64 * delta;
            let fract = t.fract();
            let src_row = if fract <= f64::EPSILON || fract >= 1.0 - f64::EPSILON {
                t.round() as usize
            } else {
                t.floor() as usize
            };
            new_data.extend_from_slice(&self.data[src_row * row_len..][..row_len]);
        }
        if count < 0.0 {
            new_data.make_mut().reverse();
        }
        if self.shape.is_empty() {
            self.shape.push(new_row_count);
        } else {
            self.shape[0] = new_row_count;
        }
        self.data = new_data.into();
        self.validate();
        Ok(self)
    }
    pub(crate) fn multikeep(mut self, dims: usize, count: f64, env: &Uiua) -> UiuaResult<Self> {
        let dims = dims.min(self.rank());
        for d in (0..dims).rev() {
            let mut rows = Vec::new();
            for row in self.depth_rows(d) {
                rows.push(row.keep_scalar_real(count, env)?);
            }
            let mut new_shape = Shape::from(&self.shape[..d]);
            self = Array::from_row_arrays(rows, env)?;
            new_shape.extend(self.shape.iter().copied().skip(1));
            self.shape = new_shape;
            self.validate();
        }
        Ok(self)
    }
    /// `keep` this array with some counts
    pub fn keep_list(mut self, counts: &[f64], env: &Uiua) -> UiuaResult<Self> {
        if counts.iter().any(|&n| n.fract() != 0.0) {
            return Err(env.error("Keep amount must be a list of integers"));
        }
        self.meta.take_map_keys();
        let counts = pad_keep_counts(counts, self.row_count(), false, env)?;
        let sorted_flags = self.meta.take_sorted_flags();
        if self.rank() == 0 {
            if counts.len() != 1 {
                return Err(env.error("Scalar array can only be kept with a single number"));
            }
            let count = counts[0].max(0.0) as usize;
            let mut new_data = EcoVec::with_capacity(count);
            for _ in 0..count {
                new_data.push(self.data[0].clone());
            }
            self = new_data.into();
        } else {
            let mut all_bools = true;
            let mut true_count = 0;
            let mut sum: f64 = 0.0;
            for &n in counts.iter() {
                sum += n;
                match n.max(0.0) as usize {
                    0 => {}
                    1 => true_count += 1,
                    _ => all_bools = false,
                }
            }
            let row_len = self.row_len();
            if all_bools {
                let data = self.data.as_mut_slice();
                let mut dest = 0;
                for (r, &b) in counts.iter().enumerate() {
                    if b == 1.0 {
                        let src_start = r * row_len;
                        if src_start != dest {
                            for i in 0..row_len {
                                data[dest + i] = data[src_start + i].clone();
                            }
                        }
                        dest += row_len;
                    }
                }
                self.data.truncate(dest);
                self.shape[0] = true_count;
            } else {
                let elem_count = validate_size::<T>([sum as usize, row_len], env)?;
                let mut new_data = CowSlice::with_capacity(elem_count);
                let mut new_len = 0;
                if row_len > 0 {
                    for (n, r) in counts.iter().zip(self.data.chunks_exact(row_len)) {
                        let n = n.max(0.0) as usize;
                        new_len += n;
                        for _ in 0..n {
                            new_data.extend_from_slice(r);
                        }
                    }
                } else {
                    new_len = counts.iter().map(|&n| n.max(0.0) as usize).sum();
                }
                self.data = new_data;
                self.shape[0] = new_len;
            }
        }
        self.meta.or_sorted_flags(sorted_flags);
        self.validate();
        Ok(self)
    }
    fn unkeep(mut self, env: &Uiua) -> UiuaResult<(Value, Self)> {
        self.meta.take_map_keys();
        if self.rank() == 0 {
            return Err(env.error("Cannot unkeep scalar array"));
        }
        let row_len = self.row_len();
        let row_count = self.row_count();
        let data = self.data.as_mut_slice();
        let mut counts = EcoVec::new();
        let mut dest = 0;
        let mut rep = 0;
        for r in 1..row_count {
            let rep_slice = &data[rep * row_len..(rep + 1) * row_len];
            let row_slice = &data[r * row_len..(r + 1) * row_len];
            if ArrayCmpSlice(rep_slice) != ArrayCmpSlice(row_slice) {
                counts.push((r - rep) as f64);
                dest += 1;
                for i in 0..row_len {
                    data[dest * row_len + i] = data[r * row_len + i].clone();
                }
                rep = r;
            }
        }
        if rep < row_count {
            counts.push((row_count - rep) as f64);
            dest += 1;
        }
        self.data.truncate(dest * row_len);
        self.shape[0] = dest;
        self.validate();
        Ok((counts.into(), self))
    }
    fn anti_keep(&self, counts: &[f64], env: &Uiua) -> UiuaResult<Self> {
        if counts.iter().any(|&n| n != 0.0 && n != 1.0) {
            return Err(env.error("Anti keep amount must be a list of booleans"));
        }
        let trues = counts.iter().filter(|&&n| n == 1.0).count();
        if trues == 0 {
            return Err(env.error("Cannot anti keep with all 0 counts"));
        }
        let falses = counts.iter().filter(|&&n| n == 0.0).count();
        let target_len = self.row_count().max(
            (self.row_count() as f64 * (trues + falses) as f64 / trues as f64).floor() as usize,
        );
        let counts = pad_keep_counts(counts, target_len, true, env)?;
        let mut fill: Option<T> = None;
        let mut new_data = EcoVec::with_capacity(counts.len());
        let mut rows = self.row_slices();
        for &count in counts.iter() {
            if count == 0.0 {
                let fill = if let Some(fill) = &fill {
                    fill
                } else {
                    let f = env.scalar_fill::<T>().map(|fv| fv.value).map_err(|e| {
                        env.error(format!("Anti keep with 0s requires a fill value{e}"))
                    })?;
                    fill = Some(f);
                    fill.as_ref().unwrap()
                };
                extend_repeat(&mut new_data, fill, self.row_len());
            } else if let Some(row) = rows.next() {
                new_data.extend_from_slice(row);
            } else {
                let fill = if let Some(fill) = &fill {
                    fill
                } else {
                    let f = env.scalar_fill::<T>().map(|fv| fv.value).map_err(|e| {
                        env.error(format!(
                            "Anti keep ran out of rows so it \
                            requires a fill value{e}"
                        ))
                    })?;
                    fill = Some(f);
                    fill.as_ref().unwrap()
                };
                extend_repeat(&mut new_data, fill, self.row_len());
            }
        }
        let mut new_shape = self.shape.clone();
        *new_shape.row_count_mut() = counts.len();
        Ok(Array::new(new_shape, new_data))
    }
    fn undo_keep(self, counts: &[f64], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        if into.rank() == 0 {
            return Err(env.error("Cannot undo keep of scalar array"));
        }
        let from = self;
        let counts = pad_keep_counts(counts, into.row_count(), false, env)?;
        let mut true_count = 0;
        for &count in counts.iter() {
            if count > 1.0 {
                return Err(env.error("Cannot invert keep with non-boolean counts"));
            }
            if count == 1.0 {
                true_count += 1;
            }
        }
        let into_row_len = into.row_len();
        match from.rank().cmp(&into.rank()) {
            Ordering::Equal => {
                // Normal same-rank case
                if from.row_count() != true_count {
                    return Err(env.error(format!(
                        "Attempted to undo keep, but the length of \
                        the kept array changed from {true_count} to {}",
                        from.row_count()
                    )));
                }
                if !from.shape.iter().skip(1).eq(into.shape.iter().skip(1)) {
                    let mut original_shape = into.shape.row();
                    original_shape.prepend(from.row_count());
                    return Err(env.error(format!(
                        "Attempted to undo keep, but the shape of \
                        the kept array changed from {} to {}",
                        original_shape, from.shape
                    )));
                }
                let mut from_rows = from.row_slices();
                for (&count, into_slice) in
                    (counts.iter()).zip(into.data.as_mut_slice().chunks_exact_mut(into_row_len))
                {
                    if count < 1.0 {
                        continue;
                    }
                    into_slice.clone_from_slice(from_rows.next().expect(
                        "number of true counts was verified \
                        to match from row count",
                    ))
                }
            }
            Ordering::Less => {
                // If the from rank is reduced, the from array is copied to each row (or subrow) of the into array
                if !into.shape.ends_with(&from.shape) {
                    return Err(env.error(format!(
                        "Cannot undo keep of array with shape {} \
                        into array with shape {}",
                        from.shape, into.shape
                    )));
                }
                let n: usize = into.shape[1..into.rank() - from.rank()].iter().product();
                let from_elem_count = from.element_count();
                for (&count, into_slice) in counts
                    .iter()
                    .zip(into.data.as_mut_slice().chunks_exact_mut(into_row_len))
                {
                    if count < 1.0 {
                        continue;
                    }
                    for i in 0..n {
                        let start = i * from_elem_count;
                        into_slice[start..start + from_elem_count].clone_from_slice(&from.data);
                    }
                }
            }
            Ordering::Greater => {
                // If the from rank is increased, the *into* array is copied to each row (or subrow) of the *from* array
                let target_dim = from.rank().saturating_sub(into.rank());
                if from.shape[target_dim] != true_count {
                    return Err(env.error(format!(
                        "Cannot undo keep because the axis corresponding to \
                        the kept array changed from {true_count} to {} in shape {}",
                        from.shape[target_dim], from.shape,
                    )));
                }
                if from.shape[target_dim + 1..] != into.shape[1..] {
                    return Err(env.error(format!(
                        "Cannot undo keep because the original shape {} \
                        and the new shape {} do not have matching suffixes",
                        into.shape, from.shape,
                    )));
                }
                let new_shape: Shape = from
                    .shape
                    .iter()
                    .take(from.rank() - into.rank())
                    .copied()
                    .chain(into.shape.iter().copied())
                    .collect();
                let mut new_rows = EcoVec::with_capacity(new_shape.elements());
                for row in from.row_slices() {
                    let mut from_subrows = row.chunks_exact(into.row_len());
                    for (&count, into_slice) in counts.iter().zip(into.row_slices()) {
                        if count < 1.0 {
                            new_rows.extend_from_slice(into_slice);
                        } else {
                            new_rows.extend_from_slice(from_subrows.next().expect(
                                "number of true counts was verified \
                                to match from row count",
                            ));
                        }
                    }
                }
                into = Array::new(new_shape, new_rows);
            }
        }
        into.meta.take_sorted_flags();
        Ok(into)
    }
}

pub(super) fn pad_keep_counts<'a>(
    counts: &'a [f64],
    len: usize,
    unfill: bool,
    env: &Uiua,
) -> UiuaResult<Cow<'a, [f64]>> {
    let mut amount = Cow::Borrowed(counts);
    match amount.len().cmp(&len) {
        Ordering::Equal => {}
        Ordering::Less => match if unfill {
            env.unfill().num_array()
        } else {
            env.either_array_fill()
        } {
            Ok(fill) => {
                if let Some(n) = fill.value.data.iter().find(|&&n| n.fract() != 0.0) {
                    return Err(env.error(format!(
                        "Fill value for keep must be an array of \
                        integers, but one of the values is {n}"
                    )));
                }
                match fill.value.rank() {
                    0 => {
                        let fill_val = fill.value.data[0];
                        let amount = amount.to_mut();
                        let count = len - amount.len();
                        amount.extend(repeat_n(fill_val, count));
                        if fill.is_left() {
                            amount.rotate_right(count);
                        }
                    }
                    1 => {
                        let amount = amount.to_mut();
                        let count = len - amount.len();
                        amount.extend((fill.value.data.iter().copied().cycle()).take(count));
                        if fill.is_left() {
                            amount.rotate_right(count);
                        }
                    }
                    _ => {
                        return Err(env.error(format!(
                            "Fill value for keep must be a scalar or a 1D array, \
                            but it has shape {}",
                            fill.value.shape
                        )));
                    }
                }
            }
            Err(e) if counts.is_empty() => {
                return Err(env.error(format!(
                    "Cannot keep array of length {len} with array of length 0{e}",
                )))
            }
            Err(_) => {
                let amount = amount.to_mut();
                for i in 0..len - amount.len() {
                    amount.push(amount[i % amount.len()]);
                }
            }
        },
        Ordering::Greater => {
            let Cow::Borrowed(amount) = &mut amount else {
                unreachable!()
            };
            *amount = &amount[..len];
        }
    }
    Ok(amount)
}

impl Value {
    /// Use this value to `rotate` another
    pub fn rotate(&self, rotated: &mut Self, env: &Uiua) -> UiuaResult {
        self.rotate_depth(rotated, 0, true, env)
    }
    pub(crate) fn anti_rotate(&self, rotated: &mut Self, env: &Uiua) -> UiuaResult {
        self.rotate_depth(rotated, 0, false, env)
    }
    pub(crate) fn rotate_depth(
        &self,
        rotated: &mut Self,
        depth: usize,
        forward: bool,
        env: &Uiua,
    ) -> UiuaResult {
        if self.row_count() == 0 {
            return Ok(());
        }
        let by_ints = || -> UiuaResult<_> {
            let mut ints =
                self.as_integer_array(env, "Rotation amount must be an array of integers")?;
            if !forward {
                for i in ints.data.as_mut_slice() {
                    *i = -*i;
                }
            }
            Ok(ints)
        };
        rotated.match_fill(env);
        match rotated {
            Value::Num(b) => b.rotate_depth(by_ints()?, depth, forward, env)?,
            Value::Byte(b) => b.rotate_depth(by_ints()?, depth, forward, env)?,
            Value::Complex(b) => b.rotate_depth(by_ints()?, depth, forward, env)?,
            Value::Char(b) => b.rotate_depth(by_ints()?, depth, forward, env)?,
            Value::Box(a) => a.rotate_depth(by_ints()?, depth, forward, env)?,
        }
        rotated.meta.take_sorted_flags();
        Ok(())
    }
}

impl<T: ArrayValue> Array<T> {
    /// `rotate` this array by the given amount
    pub fn rotate(&mut self, by: Array<isize>, forward: bool, env: &Uiua) -> UiuaResult {
        self.rotate_depth(by, 0, forward, env)
    }
    pub(crate) fn rotate_depth(
        &mut self,
        by: Array<isize>,
        depth: usize,
        forward: bool,
        env: &Uiua,
    ) -> UiuaResult {
        if !forward && env.scalar_unfill::<T>().is_ok() {
            return Err(env.error("Cannot invert filled rotation"));
        }
        let fill = env.scalar_fill::<T>().ok();

        // Expand the array if doing multiple rotations
        if self.rank() > by.rank() && self.shape.contains(&1) {
            let fixed_dims = self
                .shape
                .iter()
                .take(by.rank())
                .take_while(|&&d| d == 1)
                .count();
            let expansion: usize = by.shape[..fixed_dims].iter().product();
            let new_shape: Shape = (by.shape.iter().take(fixed_dims))
                .chain(&self.shape[fixed_dims..])
                .copied()
                .collect();
            self.reshape_scalar(Ok(expansion as isize), false, env)?;
            self.shape = new_shape;
        }
        if by.rank().saturating_sub(depth) > 1 {
            for &bd in by.shape.iter().rev().skip(1) {
                self.reshape_scalar(Ok(bd as isize), false, env)?;
            }
        }
        if self.rank() < depth {
            for &bd in by.shape.iter().skip(depth.saturating_sub(1)) {
                self.reshape_scalar(Ok(bd as isize), false, env)?;
            }
            self.transpose();
        }

        // Handles depth and fixed axes
        fn recur<T: Clone>(
            bysh: &[usize],
            by: &[isize],
            shape: &[usize],
            rotated: &mut [T],
            depth: usize,
            fill: Option<&T>,
            env: &Uiua,
        ) -> UiuaResult {
            if by.is_empty() || rotated.is_empty() {
                return Ok(());
            }
            // println!("depth: {depth}, bysh: {bysh:?}, shape: {shape:?}");
            match bysh {
                [] if depth == 0 => rotate_maybe_fill(by, shape, rotated, fill),
                [_] if depth == 0 => {
                    if by.len() > shape.len() {
                        return Err(env.error(format!(
                            "Cannot rotate rank {} array with index of length {}",
                            shape.len(),
                            by.len()
                        )));
                    }
                    rotate_maybe_fill(by, shape, rotated, fill);
                }
                [] => {
                    let shape = &shape[shape.len().min(1)..];
                    let row_len = shape.iter().product::<usize>();
                    for chunk in rotated.chunks_exact_mut(row_len) {
                        recur(&[], by, shape, chunk, depth.saturating_sub(1), fill, env)?;
                    }
                }
                [1, rest @ ..] => {
                    let shape = &shape[1..];
                    let row_len = shape.iter().product::<usize>();
                    for chunk in rotated.chunks_exact_mut(row_len) {
                        recur(rest, by, shape, chunk, depth.saturating_sub(1), fill, env)?;
                    }
                }
                [b, rest @ ..] => {
                    if *b != shape[0] {
                        return Err(env.error(format!(
                            "Shapes {} and {} are not compatible for rotation",
                            FormatShape(bysh),
                            FormatShape(shape)
                        )));
                    }
                    let by_row_len = rest.iter().product::<usize>();
                    let shape = &shape[1..];
                    let row_len = shape.iter().product::<usize>();
                    for (by, rot) in by
                        .chunks_exact(by_row_len)
                        .zip(rotated.chunks_exact_mut(row_len))
                    {
                        recur(rest, by, shape, rot, depth.saturating_sub(1), fill, env)?;
                    }
                }
            }
            Ok(())
        }

        recur(
            &by.shape,
            &by.data,
            &self.shape,
            self.data.as_mut_slice(),
            depth,
            fill.as_ref().map(|fv| &fv.value),
            env,
        )?;

        if fill.is_some() {
            self.meta.reset_flags();
        }
        if by.rank().saturating_sub(depth) > 1 {
            self.meta.take_map_keys();
        } else if depth == 0 {
            if let Some(keys) = self.meta.map_keys_mut() {
                let by = by.data[0];
                keys.rotate(by);
            }
        }
        self.meta.take_sorted_flags();
        Ok(())
    }
}

fn rotate_maybe_fill<T: Clone>(by: &[isize], shape: &[usize], data: &mut [T], fill: Option<&T>) {
    rotate(by, shape, data);
    if let Some(fill) = fill {
        fill_shift(by, shape, data, fill.clone());
    }
}

fn rotate<T>(by: &[isize], shape: &[usize], data: &mut [T]) {
    if by.is_empty() || shape.is_empty() {
        return;
    }
    let row_count = shape[0];
    if row_count == 0 {
        return;
    }
    let row_len = shape[1..].iter().product();
    let offset = by[0];
    let mid = (row_count as isize + offset).rem_euclid(row_count as isize) as usize;
    let (left, right) = data.split_at_mut(mid * row_len);
    left.reverse();
    right.reverse();
    data.reverse();
    let index = &by[1..];
    let shape = &shape[1..];
    if index.is_empty() || shape.is_empty() {
        return;
    }
    for cell in data.chunks_mut(row_len) {
        rotate(index, shape, cell);
    }
}

fn fill_shift<T: Clone>(by: &[isize], shape: &[usize], data: &mut [T], fill: T) {
    if by.is_empty() || shape.is_empty() {
        return;
    }
    let row_count = shape[0];
    if row_count == 0 {
        return;
    }
    let offset = by[0];
    let row_len: usize = shape[1..].iter().product();
    if offset != 0 {
        let abs_offset = offset.unsigned_abs() * row_len;
        let data_len = data.len();
        if offset > 0 {
            for val in &mut data[data_len.saturating_sub(abs_offset)..] {
                *val = fill.clone();
            }
        } else {
            for val in &mut data[..abs_offset.min(data_len)] {
                *val = fill.clone();
            }
        }
    }
    let index = &by[1..];
    let shape = &shape[1..];
    if index.is_empty() || shape.is_empty() {
        return;
    }
    for cell in data.chunks_mut(row_len) {
        fill_shift(index, shape, cell, fill.clone());
    }
}

impl Value {
    pub(crate) fn undo_windows(&self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let size_array = self.as_integer_array(env, "Window size must be an integer array")?;
        if !matches!(&*size_array.shape, [1, _]) {
            return Err(env.error("Only chunking windows can be undone"));
        }
        Ok(val_as_arr!(from, |a| a
            .undo_chunks(&size_array.data, env)?
            .into()))
    }
}

impl<T: ArrayValue> Array<T> {
    pub(crate) fn undo_chunks(mut self, isize_spec: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if isize_spec.contains(&0) {
            return Err(env.error("Chunk size cannot be zero"));
        }
        let n = isize_spec.len();
        if self.rank() < n * 2 {
            return Ok(self);
        }

        if n >= 2 {
            // Some of the most deranged code I've ever written
            let mut data = EcoVec::with_capacity(self.element_count());
            for d in 0..n - 1 {
                let r_count: usize = self.shape[..n - d - 1].iter().product();
                let r_len: usize = self.shape[n - d - 1..].iter().product();
                let a_len: usize = self.shape[n - d..].iter().product();
                let b_len: usize = self.shape[n + n - (2 * d + 1)..].iter().product();
                for r in 0..r_count {
                    for i in 0..self.shape[n - d..n + n - (2 * d + 1)]
                        .iter()
                        .product::<usize>()
                    {
                        for j in 0..self.shape[n - d - 1] {
                            let start = r * r_len + j * a_len + i * b_len;
                            let slice = &self.data[start..][..b_len];
                            data.extend_from_slice(slice);
                        }
                    }
                }
                data = replace(&mut self.data, data.into()).into();
                data.clear();
                self.shape[n + n - (2 * d + 1)] *= self.shape[n - (d + 1)];
                self.shape.remove(n - (d + 1));
            }
        }

        self.shape[1] *= self.shape[0];
        self.shape.remove(0);
        self.validate();

        Ok(self)
    }
}

impl Value {
    pub(crate) fn matrix_div(&self, other: &Self, env: &Uiua) -> UiuaResult<Array<f64>> {
        match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.matrix_div(b, env),
            (Value::Num(a), Value::Byte(b)) => a.matrix_div(&b.convert_ref(), env),
            (Value::Byte(a), Value::Num(b)) => a.convert_ref().matrix_div(b, env),
            (Value::Byte(a), Value::Byte(b)) => a.convert_ref().matrix_div(&b.convert_ref(), env),
            _ => Err(env.error(format!(
                "Cannot matrix divide {} by {}",
                other.type_name(),
                self.type_name(),
            ))),
        }
    }
}

impl Array<f64> {
    pub(crate) fn matrix_mul(&self, other: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (a, b) = (self, other);
        let a_row_shape = a.shape.row();
        let b_row_shape = b.shape.row();
        if !shape_prefixes_match(&a_row_shape, &b_row_shape) {
            return Err(env.error(format!(
                "Cannot multiply arrays of shape {} and {}",
                a.shape, b.shape
            )));
        }
        let prod_shape = if a_row_shape.len() >= b_row_shape.len() {
            &a_row_shape
        } else {
            &b_row_shape
        };
        let prod_row_shape = prod_shape.row();
        let prod_elems = prod_row_shape.elements();
        let mut result_data = eco_vec![0.0; self.row_count() * other.row_count() * prod_elems];
        let result_slice = result_data.make_mut();
        let mut result_shape = Shape::from([a.row_count(), b.row_count()]);
        result_shape.extend(prod_row_shape.iter().copied());
        let inner = |a_row: &[f64], res_row: &mut [f64]| {
            if a_row.is_empty() {
                return;
            }
            let mut prod_row = vec![0.0; prod_shape.elements()];
            let mut i = 0;
            for b_row in b.row_slices() {
                _ = bin_pervade_recursive(
                    (a_row, &a_row_shape),
                    (b_row, &b_row_shape),
                    &mut prod_row,
                    None,
                    None,
                    InfalliblePervasiveFn::new(pervade::mul::num_num),
                    env,
                );
                let (sum, rest) = prod_row.split_at_mut(prod_elems);
                for chunk in rest.chunks_exact(prod_elems) {
                    for (a, b) in sum.iter_mut().zip(chunk.iter()) {
                        *a += *b;
                    }
                }
                res_row[i..i + prod_elems].copy_from_slice(sum);
                i += prod_elems;
            }
        };
        let result_chunk_size = b.row_count() * prod_elems;
        if result_chunk_size > 0 {
            let iter = (a.row_slices()).zip(result_slice.chunks_exact_mut(result_chunk_size));
            par_if!(
                a.row_count() > 100 || b.row_count() > 100,
                (iter.par_bridge()).for_each(|(a_row, res_row)| inner(a_row, res_row)),
                iter.for_each(|(a_row, res_row)| inner(a_row, res_row))
            )
        }
        Ok(Array::new(result_shape, result_data))
    }
    pub(crate) fn matrix_div(&self, other: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (a, b) = (other, self);
        if a.rank() != 2 || b.rank() != 2 {
            return Err(env.error(format!(
                "Matrix division requires arrays of rank 2, \
                but their shapes are {} and {}",
                a.shape, b.shape
            )));
        }
        if [a.shape[0], a.shape[1]] != [b.shape[1], b.shape[0]] {
            return Err(env.error(format!(
                "Matrix division requires arrays of compatible shapes, \
                but their shapes are {} and {}",
                a.shape, b.shape
            )));
        }
        // let mut result_data = eco_vec![0.0; a.element_count().max(b.element_count())];
        Err(env.error("Matrix division is not yet implemented"))
    }
}

fn derive_undices(mut indices: Vec<isize>, rank: usize, env: &Uiua) -> UiuaResult<Vec<usize>> {
    for i in &mut indices {
        let u = (*i).unsigned_abs();
        if *i >= 0 && u >= rank || *i < 0 && u > rank {
            return Err(env.error(format!("Cannot orient axis {i} in array of rank {rank}")));
        }
        *i = if *i >= 0 {
            u as isize
        } else {
            (rank - u) as isize
        };
    }
    Ok(cast_vec(indices))
}

impl Value {
    /// `orient` a value by this value
    pub fn orient(&self, target: &mut Self, env: &Uiua) -> UiuaResult {
        let indices = self.as_ints(env, "Orient indices must be integers")?;
        let undices = derive_undices(indices, target.rank(), env)?;
        target.match_fill(env);
        val_as_arr!(target, |a| a.orient(undices, env))
    }
    pub(crate) fn anti_orient(&self, target: Self, env: &Uiua) -> UiuaResult<Self> {
        let indices = self.as_ints(env, "Unorient indices must be integers")?;
        let undices = derive_undices(indices, target.rank(), env)?;
        val_as_arr!(target, |a| a.anti_orient(undices, env).map(Into::into))
    }
    pub(crate) fn undo_anti_orient(
        self,
        indices: Self,
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let indices = indices.as_ints(env, "Orient indices must be integers")?;
        let undices = derive_undices(indices, self.rank(), env)?;
        self.generic_bin_into(
            into,
            |a, b| a.undo_anti_orient(undices.clone(), b, env).map(Into::into),
            |a, b| a.undo_anti_orient(undices.clone(), b, env).map(Into::into),
            |a, b| a.undo_anti_orient(undices.clone(), b, env).map(Into::into),
            |a, b| a.undo_anti_orient(undices.clone(), b, env).map(Into::into),
            |a, b| a.undo_anti_orient(undices.clone(), b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot undo orient of {} array into {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

fn derive_orient_rotations(rank: usize, undices: &[usize]) -> Vec<(usize, i32)> {
    let mut orientation: Vec<usize> = (0..rank).collect();
    let mut depth_rotations: Vec<(usize, i32)> = Vec::new();
    for (i, &u) in undices.iter().enumerate() {
        let j = orientation.iter().position(|&o| o == u).unwrap();
        if i == j {
            continue;
        }
        if j != orientation.len() - 1 {
            orientation[j..].rotate_left(1);
            depth_rotations.push((j, 1));
        }
        orientation[i..].rotate_right(1);
        depth_rotations.push((i, -1));
    }
    depth_rotations
}

fn derive_orient_data(undices: &mut Vec<usize>, shape: &[usize], env: &Uiua) -> UiuaResult<Shape> {
    let rank = shape.len();
    // Validate indices
    for &i in &*undices {
        if i >= rank {
            return Err(env.error(format!("Cannot orient axis {i} in array of rank {rank}")));
        }
    }
    // Derive new shape
    let duplicate_count = undices
        .iter()
        .enumerate()
        .filter(|&(i, a)| undices[..i].contains(a))
        .count();
    let new_rank = rank + duplicate_count;
    let mut new_shape = Shape::with_capacity(new_rank);
    for &i in &*undices {
        new_shape.push(shape[i]);
    }
    for i in 0..rank {
        if !undices.contains(&i) {
            new_shape.push(shape[i]);
            undices.push(i);
        }
    }
    Ok(new_shape)
}

impl<T: ArrayValue> Array<T> {
    fn orient(&mut self, undices: Vec<usize>, env: &Uiua) -> UiuaResult {
        if undices.len() > self.rank() {
            return match env.scalar_fill() {
                Ok(fill) => self.filled_orient(undices, fill.value, env),
                Err(e) => Err(env
                    .error(format!(
                        "Cannot orient array of rank {} with {} indices{e}",
                        self.rank(),
                        undices.len()
                    ))
                    .fill()),
            };
        }
        if undices
            .iter()
            .enumerate()
            .any(|(i, a)| undices[..i].contains(a))
        {
            return match env.scalar_fill() {
                Ok(fill) => self.filled_orient(undices, fill.value, env),
                Err(e) => Err(env
                    .error(format!("Orient indices must be unique{e}"))
                    .fill()),
            };
        }
        for (depth, amnt) in derive_orient_rotations(self.rank(), &undices) {
            self.transpose_depth(depth, amnt);
        }
        Ok(())
    }
    fn filled_orient(&mut self, mut undices: Vec<usize>, fill: T, env: &Uiua) -> UiuaResult {
        let new_shape = derive_orient_data(&mut undices, &self.shape, env)?;
        let new_data = eco_vec![fill; new_shape.elements()];
        let mut new_arr = Array::new(new_shape, new_data);
        swap(self, &mut new_arr);
        new_arr.orient_into(self, &undices);
        Ok(())
    }
    fn orient_into(self, into: &mut Self, undices: &[usize]) {
        let mut into_index = vec![0usize; into.rank()];
        let mut orig_index = vec![0usize; self.rank()];
        let slice = into.data.as_mut_slice();
        for (i, elem) in self.data.into_iter().enumerate() {
            self.shape.flat_to_dims(i, &mut orig_index);
            for (j, ii) in into_index.iter_mut().enumerate() {
                *ii = orig_index[undices[j]];
            }
            if let Some(j) = into.shape.dims_to_flat(&into_index) {
                slice[j] = elem;
            }
        }
        into.meta.take_sorted_flags();
        into.validate();
    }
    fn anti_orient(mut self, mut undices: Vec<usize>, env: &Uiua) -> UiuaResult<Self> {
        fn derive_anti_orient_data(
            undices: &mut Vec<usize>,
            shape: &[usize],
            env: &Uiua,
        ) -> UiuaResult<(Shape, usize, usize)> {
            let rank = shape.len();

            // Add missing axes
            let duplicate_count = undices
                .iter()
                .enumerate()
                .filter(|&(i, a)| undices[..i].contains(a))
                .count();
            let max_undex = undices.iter().max().copied().unwrap_or(0);
            let min_allowed_rank = max_undex + duplicate_count + 1;
            if rank < min_allowed_rank {
                return Err(env.error(format!(
                    "Orient indices imply a rank of at least {min_allowed_rank}, \
                    but the array is rank {rank}"
                )));
            }
            let new_rank = rank - duplicate_count;
            for i in 0..new_rank {
                if !undices.contains(&i) {
                    undices.push(i);
                }
            }

            // New shape
            let mut new_shape = Shape::with_capacity(new_rank);
            for i in 0..new_rank {
                new_shape.push(
                    (undices.iter().enumerate())
                        .filter(|&(_, &j)| j == i)
                        .map(|(j, _)| shape[j])
                        .min()
                        .unwrap(),
                );
            }

            // Trailing dimensions
            let trailing_dims = undices
                .iter()
                .enumerate()
                .rev()
                .take_while(|&(i, a)| !undices[..i].contains(a))
                .zip((0..new_rank).rev())
                .take_while(|&((_, &a), b)| a == b)
                .count();

            Ok((new_shape, duplicate_count, trailing_dims))
        }

        let (new_shape, duplicates, trailing_dims) =
            derive_anti_orient_data(&mut undices, &self.shape, env)?;

        if new_shape.elements() == 0 {
            return Ok(Array::new(new_shape, CowSlice::new()));
        } else if trailing_dims == self.rank() {
            return Ok(self.clone());
        } else if duplicates == 0 {
            let mut inverted: Vec<usize> = (0..undices.len()).collect();
            inverted.sort_by_key(|&i| undices[i]);
            for (depth, amnt) in derive_orient_rotations(self.rank(), &inverted) {
                self.transpose_depth(depth, amnt);
            }
            return Ok(self);
        }

        let mut data = self.data.clone();
        data.truncate(new_shape.elements());
        let considered_orig_shape = Shape::from(&self.shape[..self.rank() - trailing_dims]);
        let considered_new_shape = Shape::from(&new_shape[..new_shape.len() - trailing_dims]);
        let trailing_row_len: usize = self.shape[considered_orig_shape.len()..].iter().product();
        let mut orig_index = vec![0; considered_orig_shape.len()];
        let mut new_index = vec![0; considered_new_shape.len()];
        for (i, row) in data
            .as_mut_slice()
            .chunks_exact_mut(trailing_row_len)
            .enumerate()
        {
            considered_new_shape.flat_to_dims(i, &mut new_index);
            for (j, oi) in orig_index.iter_mut().enumerate() {
                *oi = new_index[undices[j]];
            }
            let j = considered_orig_shape.dims_to_flat(&orig_index).unwrap();
            row.clone_from_slice(&self.data[j * trailing_row_len..][..trailing_row_len]);
        }

        Ok(Array::new(new_shape, data))
    }
    fn undo_anti_orient(
        self,
        mut undices: Vec<usize>,
        mut into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let new_shape = derive_orient_data(&mut undices, &self.shape, env)?;
        match new_shape.len().cmp(&into.rank()) {
            Ordering::Equal => {}
            Ordering::Less => {
                return Err(env.error(format!(
                    "Indices of length {} cannot be used to reorient \
                    shape {} array into shape {} array",
                    undices.len(),
                    self.shape,
                    into.shape
                )))
            }
            Ordering::Greater => {
                return Err(env.error(format!(
                    "Cannot reorient because the rank of the array changed from {} to {}",
                    new_shape.len() - into.rank(),
                    self.rank(),
                )))
            }
        }
        self.orient_into(&mut into, &undices);
        Ok(into)
    }
}

impl Value {
    /// Get the `base` of a value
    pub fn base(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        let base = self.as_nums(env, "Base must be a number or list of numbers")?;
        Ok(if self.rank() == 0 {
            match of {
                Value::Num(n) => n.base_scalar(base[0], env)?.into(),
                Value::Byte(b) => b.base_scalar(base[0], env)?.into(),
                val => {
                    return Err(env.error(format!(
                        "Cannot get base digits of a {} array",
                        val.type_name()
                    )))
                }
            }
        } else {
            match of {
                Value::Num(n) => n.base_list(&base, env)?.into(),
                Value::Byte(b) => b.base_list(&base, env)?.into(),
                val => {
                    return Err(env.error(format!(
                        "Cannot get base digits of a {} array",
                        val.type_name()
                    )))
                }
            }
        })
    }
    pub(crate) fn antibase(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        let base = self.as_nums(env, "Base must be a number or list of numbers")?;
        Ok(if self.rank() == 0 {
            match of {
                Value::Num(n) => n.antibase_scalar(base[0], env)?.into(),
                Value::Byte(b) => b.antibase_scalar(base[0], env)?.into(),
                val => {
                    return Err(env.error(format!(
                        "Cannot get undo base of a {} array",
                        val.type_name()
                    )))
                }
            }
        } else {
            match of {
                Value::Num(n) => n.antibase_list(&base, env)?.into(),
                Value::Byte(b) => b.antibase_list(&base, env)?.into(),
                val => {
                    return Err(env.error(format!(
                        "Cannot get base digits of a {} array",
                        val.type_name()
                    )))
                }
            }
        })
    }
}

fn digits_needed_for_base(n: f64, base: f64) -> usize {
    if n == 0.0 {
        0
    } else {
        let mut log = n.abs().log(base);
        if (log.fract() - 1.0).abs() <= 4.0 * f64::EPSILON {
            log = log.ceil();
        }
        log.floor() as usize + 1
    }
}

fn validate_base(base: f64, env: &Uiua) -> UiuaResult {
    if base == 0.0 {
        Err(env.error("Base cannot be 0"))
    } else if base == 1.0 {
        Err(env.error("Base cannot be 1"))
    } else if base == -1.0 {
        Err(env.error("Base cannot be ¯1"))
    } else if base.is_nan() {
        Err(env.error("Base cannot be NaN"))
    } else if (-1.0..0.0).contains(&base) {
        Err(env.error("Base cannot be between ¯1 and 0"))
    } else {
        Ok(())
    }
}

impl<T: RealArrayValue + GridFmt> Array<T> {
    fn base_scalar(&self, base: f64, env: &Uiua) -> UiuaResult<Array<f64>> {
        validate_base(base, env)?;

        // Validation
        if base.is_infinite() {
            return Err(env.error("Base cannot be infinite"));
        }
        if let Some(n) = self.data.iter().find(|n| n.to_f64().is_infinite()) {
            return Err(env.error(format!("Cannot take base of {}", n.grid_string(false))));
        }

        Ok(if base >= 0.0 {
            let max_row_len = (self.data.iter())
                .map(|&n| digits_needed_for_base(n.to_f64(), base))
                .max()
                .unwrap_or(0);
            let mut new_shape = self.shape.clone();
            new_shape.push(max_row_len);
            let elem_count = validate_size::<f64>(new_shape.iter().copied(), env)?;
            let mut new_data = eco_vec![0.0; elem_count];
            let slice = new_data.make_mut();
            for (i, n) in self.data.iter().enumerate() {
                let n = n.to_f64();
                let mut abs_n = n.abs();
                let sign = if n < 0.0 { -1.0 } else { 1.0 };
                for j in 0..max_row_len {
                    slice[i * max_row_len + j] = abs_n.rem_euclid(base) * sign;
                    abs_n = abs_n.div_euclid(base);
                }
            }
            Array::new(new_shape, new_data)
        } else {
            let mut rows = Vec::with_capacity(self.row_count());
            for n in &self.data {
                let mut row = Vec::new();
                let mut n = n.to_f64();
                while n.abs() > f64::EPSILON {
                    row.push(n.rem_euclid(base));
                    n = n.div_euclid(base);
                }
                rows.push(row);
            }
            let max_len = rows.iter().map(|row| row.len()).max().unwrap_or(0);
            let mut new_shape = self.shape.clone();
            new_shape.push(max_len);
            let elem_count = validate_size::<f64>(new_shape.iter().copied(), env)?;
            let mut new_data = eco_vec![0.0; elem_count];
            let slice = new_data.make_mut();
            for (i, row) in rows.into_iter().enumerate() {
                for (j, n) in row.into_iter().enumerate() {
                    slice[i * max_len + j] = n;
                }
            }
            Array::new(new_shape, new_data)
        })
    }
    fn base_list(&self, bases: &[f64], env: &Uiua) -> UiuaResult<Array<f64>> {
        let fill = env.scalar_fill::<f64>().ok().map(|fv| fv.value);
        // Validation
        for base in bases.iter().copied().chain(fill) {
            if base.is_infinite() && base.is_sign_negative() {
                return Err(env.error("Base cannot contain negative infinities"));
            }
            validate_base(base, env)?;
        }
        if let Some(n) = self.data.iter().find(|n| n.to_f64().is_infinite()) {
            return Err(env.error(format!("Cannot take base of {}", n.grid_string(false))));
        }

        let fill_digits = if let Some(fill) = fill {
            let product: f64 = bases.iter().product();
            self.data
                .iter()
                .map(|&n| digits_needed_for_base(n.to_f64() / product, fill))
                .max()
                .unwrap_or(0)
        } else {
            0
        };
        let num_digits = bases.len() + fill_digits;
        let mut new_shape = self.shape.clone();
        new_shape.push(num_digits);
        let elem_count = validate_size::<f64>(new_shape.iter().copied(), env)?;
        let mut new_data = eco_vec![0.0; elem_count];
        let slice = new_data.make_mut();
        for (i, n) in self.data.iter().enumerate() {
            let mut n = n.to_f64();
            for (j, base) in bases
                .iter()
                .copied()
                .chain(
                    fill.map(|fill| repeat_n(fill, fill_digits))
                        .into_iter()
                        .flatten(),
                )
                .enumerate()
            {
                if n == f64::INFINITY {
                    slice[i * num_digits + j] = n;
                    break;
                } else {
                    slice[i * num_digits + j] = n.rem_euclid(base);
                    n = n.div_euclid(base);
                }
            }
        }
        Ok(Array::new(new_shape, new_data))
    }
    fn antibase_scalar(&self, base: f64, env: &Uiua) -> UiuaResult<Array<f64>> {
        let mut shape = self.shape.clone();
        let row_len = shape.pop().unwrap_or(1);
        let elem_count = validate_size::<f64>(shape.iter().copied(), env)?;
        let mut data = eco_vec![0f64; elem_count];
        if row_len > 0 {
            let slice = data.make_mut();
            for (i, chunk) in self.data.chunks_exact(row_len).enumerate() {
                for n in chunk.iter().rev() {
                    slice[i] = slice[i].mul_add(base, n.to_f64());
                }
            }
        }
        Ok(Array::new(shape, data))
    }
    fn antibase_list(&self, bases: &[f64], env: &Uiua) -> UiuaResult<Array<f64>> {
        let fill = env.scalar_unfill::<f64>().ok();
        let mut shape = self.shape.clone();
        let row_len = shape.pop().unwrap_or(1);
        let elem_count = validate_size::<f64>(shape.iter().copied(), env)?;
        let mut data = eco_vec![0f64; elem_count];
        if row_len > 0 {
            let slice = data.make_mut();
            let mut bases = bases.to_vec();
            let count = row_len.saturating_sub(bases.len());
            bases.extend(repeat_n(
                fill.as_ref().map(|fv| fv.value).unwrap_or(1.0),
                count,
            ));
            if fill.is_some_and(|fv| fv.is_left()) {
                bases.rotate_right(count);
            }
            let scan: Vec<f64> = bases
                .iter()
                .scan(1.0, |acc, b| {
                    *acc *= if b.is_infinite() { 1.0 } else { *b };
                    Some(*acc)
                })
                .collect();
            for (i, chunk) in self.data.chunks_exact(row_len).enumerate() {
                for (j, (n, &(mut b))) in chunk.iter().zip(&bases).rev().enumerate() {
                    if b.is_infinite() {
                        b = scan[j];
                    }
                    slice[i] = slice[i].mul_add(b, n.to_f64());
                }
            }
        }
        Ok(Array::new(shape, data))
    }
}

impl Value {
    /// Test if a value is an integer in a range
    pub fn memberof_range(&self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        fn fallback(a: &Value, b: &Value, env: &Uiua) -> UiuaResult<Value> {
            let range = a.range(env)?;
            range.memberof(b, env)
        }

        let Ok(range_bound) = self.as_num(env, None) else {
            return fallback(self, &from, env);
        };

        if range_bound.fract() != 0.0 || range_bound.is_infinite() || range_bound.is_nan() {
            return fallback(self, &from, env);
        }

        Ok(match from {
            Value::Num(nums) => {
                let data: EcoVec<u8> = if range_bound >= 0.0 {
                    nums.data
                        .iter()
                        .map(|&number| {
                            number.fract() == 0.0 && number >= 0.0 && number < range_bound
                        })
                        .map(Into::into)
                        .collect()
                } else {
                    nums.data
                        .iter()
                        .map(|&number| {
                            number.fract() == 0.0 && number < 0.0 && number >= range_bound
                        })
                        .map(Into::into)
                        .collect()
                };

                Array::new(nums.shape, data).into()
            }
            Value::Byte(mut bytes) => {
                if range_bound > 0.0 {
                    for b in bytes.data.as_mut_slice() {
                        *b = ((*b as f64) < range_bound) as u8;
                    }
                } else {
                    for b in bytes.data.as_mut_slice() {
                        *b = 0
                    }
                }

                bytes.into()
            }
            from => fallback(self, &from, env)?,
        })
    }
    /// Test if rank 1 subarrays are integer coordinates in a range
    pub fn multidim_memberof_range(&self, elems: Self, env: &Uiua) -> UiuaResult<Self> {
        let of = self;

        fn fallback(a: &Value, b: &Value, env: &Uiua) -> UiuaResult<Value> {
            let mut range = a.range(env)?;
            range.rerank(&Value::Num(1.0.into()), env)?;
            range.memberof(b, env)
        }

        if of.rank() != 1 {
            return fallback(of, &elems, env);
        }

        let Some(bound) =
            (of.as_nums(env, None).ok()).filter(|nums| nums.iter().all(|f| f.fract() == 0.0))
        else {
            return fallback(of, &elems, env);
        };

        if !(elems.rank() == 0 || elems.shape.ends_with(&[bound.len()])) {
            let new_shape = &elems.shape[..elems.rank() - 1];
            return Ok(Value::Byte(Array::new(
                new_shape,
                CowSlice::from_elem(0, new_shape.iter().product()),
            )));
        }

        match elems.rank().cmp(&2) {
            Ordering::Equal => Ok(match elems {
                Value::Num(nums) => {
                    let data: EcoVec<u8> = nums
                        .row_slices()
                        .map(|row| {
                            row.iter().zip(bound.iter()).all(|(&r, &b)| {
                                r.fract() == 0.0 && r >= b.min(0.0) && r < b.max(0.0)
                            })
                        })
                        .map(Into::into)
                        .collect();
                    Array::new(&nums.shape[..1], data).into()
                }
                Value::Byte(bytes) => {
                    let data: EcoVec<u8> = bytes
                        .row_slices()
                        .map(|row| row.iter().zip(bound.iter()).all(|(&r, &b)| (r as f64) < b))
                        .map(Into::into)
                        .collect();
                    Array::new(&bytes.shape[..1], data).into()
                }
                from => fallback(of, &from, env)?,
            }),
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for row in elems.rows() {
                    let Some(row) = of
                        .multidim_memberof_range(row, env)?
                        .as_byte_array()
                        .cloned()
                    else {
                        return fallback(of, &elems, env);
                    };
                    rows.push(row);
                }
                Ok(Value::Byte(Array::from_row_arrays(rows, env)?))
            }
            Ordering::Less => fallback(of, &elems, env),
        }
    }
    /// Generate randomly seeded arrays
    pub fn gen(&self, seed: &Self, env: &Uiua) -> UiuaResult<Value> {
        let mut hasher = DefaultHasher::new();
        seed.hash(&mut hasher);
        let seed = hasher.finish();
        let mut rng = SmallRng::seed_from_u64(seed);

        const SHAPE_REQ: &str = "Shape must be an array of natural \
            numbers with at most rank 2";

        let mut gen = |shape: &[usize]| -> UiuaResult<_> {
            let shape = Shape::from(shape);
            let elem_count = validate_size::<f64>(shape.iter().copied(), env)?;
            let mut data = eco_vec![0.0; elem_count];
            for x in data.make_mut() {
                *x = rng.gen();
            }
            Ok(Array::new(shape, data))
        };

        Ok(match self.as_natural_array(env, SHAPE_REQ) {
            Ok(nats) => match nats.rank() {
                0 | 1 => gen(&nats.data)?.into(),
                2 => {
                    let mut data = EcoVec::new();
                    for row in nats.row_slices() {
                        data.push(Boxed(gen(row)?.into()))
                    }
                    data.into()
                }
                n => return Err(env.error(format!("{SHAPE_REQ}, but it is rank {n}"))),
            },
            Err(e) => {
                if let Value::Box(arr) = self {
                    match arr.rank() {
                        0 => {
                            let shape = arr.data[0].0.as_nats(env, SHAPE_REQ)?;
                            gen(&shape)?.into()
                        }
                        1 => {
                            let mut data = EcoVec::new();
                            for Boxed(row) in &arr.data {
                                let shape = row.as_nats(env, SHAPE_REQ)?;
                                data.push(Boxed(gen(&shape)?.into()));
                            }
                            data.into()
                        }
                        _ => return Err(e),
                    }
                } else {
                    return Err(e);
                }
            }
        })
    }
    /// Pick a random row of an array
    pub fn random_row(&self, env: &Uiua) -> UiuaResult<Value> {
        match self.row_count() {
            0 => Err(env.error("Cannot pick random row of an empty array").fill()),
            1 => Ok(self.row(0)),
            len => {
                let i = RNG.with_borrow_mut(|rng| rng.gen_range(0..len));
                Ok(self.row(i))
            }
        }
    }
}

impl Value {
    pub(crate) fn un_add(mut self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        let per_meta = self.meta.take_per_meta();
        let (mut whole, mut frac): (Value, Value) = match self {
            Value::Byte(arr) => {
                let frac_data = eco_vec![0u8; arr.element_count()];
                let frac = Array::new(arr.shape.clone(), frac_data);
                (frac.into(), arr.into())
            }
            Value::Num(mut arr) => {
                let mut frac_data = eco_vec![0.0; arr.element_count()];
                for (f, s) in arr.data.as_mut_slice().iter_mut().zip(frac_data.make_mut()) {
                    *s = f.rem_euclid(1.0);
                    *f = f.floor();
                }
                let frac = Array::new(arr.shape.clone(), frac_data);
                (frac.into(), arr.into())
            }
            Value::Char(_) => return Err(env.error("Cannot un-add characters")),
            Value::Complex(mut arr) => {
                let mut fract_data = eco_vec![Complex::ZERO; arr.element_count()];
                for (c, f) in arr
                    .data
                    .as_mut_slice()
                    .iter_mut()
                    .zip(fract_data.make_mut())
                {
                    *f = Complex::new(c.re.rem_euclid(1.0), c.im.rem_euclid(1.0));
                    *c = c.floor();
                }
                let frac = Array::new(arr.shape.clone(), fract_data);
                let whole = Array::new(arr.shape.clone(), arr.data);
                (frac.into(), whole.into())
            }
            Value::Box(arr) => {
                let mut whole_data = EcoVec::with_capacity(arr.element_count());
                let mut frac_data = EcoVec::with_capacity(arr.element_count());
                for Boxed(val) in arr.data {
                    let (whole, frac) = val.un_add(env)?;
                    whole_data.push(Boxed(whole));
                    frac_data.push(Boxed(frac));
                }
                let whole = Array::new(arr.shape.clone(), whole_data);
                let frac = Array::new(arr.shape.clone(), frac_data);
                (frac.into(), whole.into())
            }
        };
        whole.meta.set_per_meta(per_meta.clone());
        whole.meta.take_sorted_flags();
        whole.validate();
        frac.meta.set_per_meta(per_meta);
        frac.meta.take_sorted_flags();
        frac.validate();
        Ok((whole, frac))
    }
    /// Decompose a value into its sign and magnitude
    pub(crate) fn un_mul(mut self) -> UiuaResult<(Self, Self)> {
        let per_meta = self.meta.take_per_meta();
        let (mut sign, mut mag): (Value, Value) = match self {
            Value::Byte(arr) => {
                let mut sign_data = eco_vec![1u8; arr.element_count()];
                for (i, s) in arr.data.iter().zip(sign_data.make_mut()) {
                    *s = (*i != 0) as u8;
                }
                let sign = Array::new(arr.shape.clone(), sign_data);
                (sign.into(), arr.into())
            }
            Value::Num(mut arr) => {
                let mut sign_data = eco_vec![1.0; arr.element_count()];
                for (f, s) in arr.data.as_mut_slice().iter_mut().zip(sign_data.make_mut()) {
                    *s = pervade::sign::num(*f);
                    *f = pervade::scalar_abs::num(*f);
                }
                let sign = Array::new(arr.shape.clone(), sign_data);
                (sign.into(), arr.into())
            }
            Value::Char(mut arr) => {
                let mut sign_data = eco_vec![0.0; arr.element_count()];
                for (c, s) in arr.data.as_mut_slice().iter_mut().zip(sign_data.make_mut()) {
                    *s = pervade::sign::char(*c);
                    *c = pervade::scalar_abs::char(*c);
                }
                let sign = Array::new(arr.shape.clone(), sign_data);
                (sign.into(), arr.into())
            }
            Value::Complex(mut arr) => {
                let mut abs_data = eco_vec![0.0; arr.element_count()];
                for (c, a) in arr.data.as_mut_slice().iter_mut().zip(abs_data.make_mut()) {
                    *a = c.abs();
                    *c = c.normalize();
                }
                let abs = Array::new(arr.shape.clone(), abs_data);
                (arr.into(), abs.into())
            }
            Value::Box(arr) => {
                let mut sign_data = EcoVec::with_capacity(arr.element_count());
                let mut mag_data = EcoVec::with_capacity(arr.element_count());
                for Boxed(val) in arr.data {
                    let (mag, sign) = val.un_mul()?;
                    mag_data.push(Boxed(mag));
                    sign_data.push(Boxed(sign));
                }
                let sign = Array::new(arr.shape.clone(), sign_data);
                let mag = Array::new(arr.shape.clone(), mag_data);
                (sign.into(), mag.into())
            }
        };
        sign.meta.set_per_meta(per_meta.clone());
        sign.meta.take_sorted_flags();
        sign.validate();
        mag.meta.set_per_meta(per_meta);
        mag.meta.take_sorted_flags();
        mag.validate();
        Ok((sign, mag))
    }
    pub(crate) fn un_div(mut self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        let per_meta = self.meta.take_per_meta();
        let (mut num, mut denom): (Value, Value) = match self {
            Value::Byte(arr) => {
                let denom_data = eco_vec![1u8; arr.element_count()];
                let denom = Array::new(arr.shape.clone(), denom_data);
                (arr.into(), denom.into())
            }
            Value::Num(mut arr) => {
                let mut denom_data = eco_vec![1.0; arr.element_count()];
                for (f, d) in (arr.data.as_mut_slice().iter_mut()).zip(denom_data.make_mut()) {
                    if f.is_finite() {
                        let gcd = pervade::or::num_num(*f, 1.0);
                        let num = *f / gcd;
                        *d = (num / *f).round();
                        *f = num.round();
                    } else if f.is_infinite() {
                        *f = f.signum();
                        *d = 0.0;
                    } else {
                        *f = f64::NAN;
                        *d = f64::NAN;
                    }
                }
                let denom = Array::new(arr.shape.clone(), denom_data);
                (arr.into(), denom.into())
            }
            val => return Err(env.error(format!("Cannot un-divide a {} array", val.type_name()))),
        };
        num.meta.set_per_meta(per_meta.clone());
        num.meta.take_sorted_flags();
        num.validate();
        denom.meta.set_per_meta(per_meta);
        denom.meta.take_sorted_flags();
        denom.validate();
        Ok((num, denom))
    }
}
