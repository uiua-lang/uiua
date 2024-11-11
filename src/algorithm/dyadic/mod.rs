//! Algorithms for dyadic array operations

mod combine;
mod search;
mod structure;

use core::f64;
use std::{
    borrow::Cow,
    cmp::Ordering,
    hash::{DefaultHasher, Hash, Hasher},
    iter::{once, repeat},
    mem::{replace, take},
};

use ecow::{eco_vec, EcoVec};
use rand::prelude::*;
use rayon::prelude::*;

use crate::{
    algorithm::pervade::{self, bin_pervade_recursive, InfalliblePervasiveFn},
    array::*,
    boxed::Boxed,
    cowslice::{cowslice, CowSlice, Repeat},
    val_as_arr,
    value::Value,
    Shape, Uiua, UiuaResult, RNG,
};

use super::{
    shape_prefixes_match, validate_size, validate_size_of, ArrayCmpSlice, FillContext, SizeError,
};

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

impl<T: Clone + std::fmt::Debug + Send + Sync> Array<T> {
    pub(crate) fn depth_slices<U: Clone + std::fmt::Debug + Send + Sync, C: FillContext>(
        &mut self,
        other: &Array<U>,
        mut a_depth: usize,
        mut b_depth: usize,
        ctx: &C,
        mut f: impl FnMut(&[usize], &mut [T], &[usize], &[U], &C) -> Result<(), C::Error>,
    ) -> Result<(), C::Error> {
        let a = self;
        let mut b = other;
        let mut local_b;
        a_depth = a_depth.min(a.rank());
        b_depth = b_depth.min(b.rank());
        let a_prefix = &a.shape[..a_depth];
        let b_prefix = &b.shape[..b_depth];
        if !a_prefix.iter().zip(b_prefix).all(|(a, b)| a == b) {
            while a.shape.starts_with(&[1]) {
                if a_depth == 0 {
                    break;
                }
                a.shape.remove(0);
                a_depth -= 1;
            }
            if b.shape.starts_with(&[1]) {
                local_b = b.clone();
                while local_b.shape.starts_with(&[1]) {
                    if b_depth == 0 {
                        break;
                    }
                    local_b.shape.remove(0);
                    b_depth -= 1;
                }
                b = &local_b;
            }
            let a_prefix = &a.shape[..a_depth];
            let b_prefix = &b.shape[..b_depth];
            if !a_prefix.iter().zip(b_prefix).all(|(a, b)| a == b) {
                return Err(ctx.error(format!(
                    "Cannot combine arrays with shapes {} and {} \
                    because shape prefixes {} and {} are not compatible",
                    a.shape(),
                    b.shape(),
                    FormatShape(a_prefix),
                    FormatShape(b_prefix)
                )));
            }
        }

        match a_depth.cmp(&b_depth) {
            Ordering::Equal => {}
            Ordering::Less => {
                for &b_dim in b.shape[a_depth..b_depth].iter().rev() {
                    let mut new_a_data = EcoVec::with_capacity(a.element_count() * b_dim);
                    for row in a.row_slices() {
                        for _ in 0..b_dim {
                            new_a_data.extend_from_slice(row);
                        }
                    }
                    a.data = new_a_data.into();
                    a.shape.insert(0, b_dim);
                    a_depth += 1;
                }
            }
            Ordering::Greater => {
                for &a_dim in a.shape[b_depth..a_depth].iter().rev() {
                    let mut new_b_data = EcoVec::with_capacity(b.element_count() * a_dim);
                    for row in b.row_slices() {
                        for _ in 0..a_dim {
                            new_b_data.extend_from_slice(row);
                        }
                    }
                    local_b = b.clone();
                    local_b.data = new_b_data.into();
                    local_b.shape.insert(0, a_dim);
                    b = &local_b;
                    b_depth += 1;
                }
            }
        }

        let a_row_shape = &a.shape[a_depth..];
        let b_row_shape = &b.shape[b_depth..];
        let a_row_len: usize = a_row_shape.iter().product();
        let b_row_len: usize = b_row_shape.iter().product();
        if a_row_len == 0 || b_row_len == 0 {
            return Ok(());
        }
        for (a, b) in (a.data.as_mut_slice())
            .chunks_exact_mut(a_row_len)
            .zip(b.data.as_slice().chunks_exact(b_row_len))
        {
            f(a_row_shape, a, b_row_shape, b, ctx)?;
        }
        Ok(())
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
            val_as_arr!(self, |a| a.reshape_scalar(n, env))
        } else {
            self.reshape_impl(&target_shape, env)
        }
    }
    pub(crate) fn reshape_impl(&mut self, dims: &[Result<isize, bool>], env: &Uiua) -> UiuaResult {
        self.match_fill(env);
        val_as_arr!(self, |a| a.reshape(dims, env))
    }
    pub(crate) fn undo_reshape(&mut self, old_shape: &Self, env: &Uiua) -> UiuaResult {
        if old_shape.as_nat(env, "").is_ok() {
            return Err(env.error("Cannot undo scalar reshape"));
        }
        let orig_shape = old_shape.as_nats(env, "Shape should be a list of integers")?;
        if env.fill().value_for(self).is_some()
            || orig_shape.iter().product::<usize>() == self.shape().iter().product::<usize>()
        {
            let orig_shape_spec: Vec<_> = orig_shape.iter().map(|&d| Ok(d as isize)).collect();
            self.reshape_impl(&orig_shape_spec, env)
        } else {
            Err(env.error(format!(
                "Cannot unreshape array because its old shape was {}, \
                but its new shape is {}, which has a different number of elements",
                FormatShape(&orig_shape),
                self.shape()
            )))
        }
    }
    pub(crate) fn reshape_scalar(&mut self, count: Result<isize, bool>, env: &Uiua) -> UiuaResult {
        val_as_arr!(self, |a| a.reshape_scalar(count, env))
    }
}

impl<T: Clone> Array<T> {
    /// `reshape` this array by replicating it as the rows of a new array
    pub fn reshape_scalar(&mut self, count: Result<isize, bool>, env: &Uiua) -> UiuaResult {
        self.take_map_keys();
        match count {
            Ok(count) => {
                if count < 0 {
                    self.reverse();
                }
                self.reshape_scalar_integer(count.unsigned_abs())
                    .map_err(|e| env.error(e))
            }
            Err(rev) => {
                if rev {
                    self.reverse()
                }
                Ok(())
            }
        }
    }
    pub(crate) fn reshape_scalar_integer(&mut self, count: usize) -> Result<(), SizeError> {
        if count == 0 {
            self.data.clear();
            self.shape.insert(0, 0);
            return Ok(());
        }
        let elem_count = validate_size_of::<T>([count - 1, self.data.len()])?;
        self.data.reserve(elem_count);
        let row = self.data.to_vec();
        for _ in 1..count {
            self.data.extend_from_slice(&row);
        }
        self.shape.insert(0, count);
        Ok(())
    }
}

impl<T: ArrayValue> Array<T> {
    /// `reshape` the array
    pub fn reshape(&mut self, dims: &[Result<isize, bool>], env: &Uiua) -> UiuaResult {
        let fill = env.scalar_fill::<T>();
        let axes = derive_shape(&self.shape, dims, fill.is_ok(), env)?;
        if (axes.first()).map_or(true, |&d| d.unsigned_abs() != self.row_count()) {
            self.take_map_keys();
        }
        let reversed_axes: Vec<usize> = (axes.iter().enumerate())
            .filter_map(|(i, &s)| if s < 0 { Some(i) } else { None })
            .collect();
        let shape: Shape = axes.iter().map(|&s| s.unsigned_abs()).collect();
        validate_size::<T>(shape.iter().copied(), env)?;
        let target_len: usize = shape.iter().product();
        if self.data.len() < target_len {
            match env.scalar_fill::<T>() {
                Ok(fill) => {
                    let start = self.data.len();
                    self.data.extend_repeat(&fill, target_len - start);
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
        self.validate_shape();
        for s in reversed_axes {
            self.reverse_depth(s);
        }
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
    /// `rerank` this value with another
    pub fn rerank(&mut self, rank: &Self, env: &Uiua) -> UiuaResult {
        self.take_map_keys();
        let irank = rank.as_int(env, "Rank must be an integer")?;
        let shape = self.shape_mut();
        let rank = irank.unsigned_abs();
        if irank >= 0 {
            // Positive rank
            if rank >= shape.len() {
                for _ in 0..rank - shape.len() + 1 {
                    shape.insert(0, 1);
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
        self.validate_shape();
        Ok(())
    }
    pub(crate) fn undo_rerank(&mut self, rank: &Self, orig_shape: &Self, env: &Uiua) -> UiuaResult {
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
            return self.undo_deshape(orig_shape, env);
        }
        let orig_shape = orig_shape.as_nats(env, "Shape must be a list of natural numbers")?;
        let rank = irank.unsigned_abs();
        let new_shape: Shape = if irank >= 0 {
            // Positive rank
            (orig_shape.iter())
                .take(orig_shape.len().saturating_sub(rank))
                .chain(
                    (self.shape().iter()).skip((rank + 1).saturating_sub(orig_shape.len()).max(1)),
                )
                .copied()
                .collect()
        } else {
            // Negative rank
            (orig_shape.iter().take(rank))
                .chain(self.shape().iter().skip(1))
                .copied()
                .collect()
        };
        if validate_size::<u8>(new_shape.iter().copied(), env)? != self.element_count() {
            return Ok(());
        }
        *self.shape_mut() = new_shape;
        self.validate_shape();
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
}

impl<T: Clone + Send + Sync> Array<T> {
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
            self.validate_shape();
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
                self.validate_shape();
                self
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    /// `keep` this array with a real-valued scalar
    pub fn keep_scalar_real(mut self, count: f64, env: &Uiua) -> UiuaResult<Self> {
        let abs_count = count.abs();
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
        self.validate_shape();
        Ok(self)
    }
    /// `keep` this array with some counts
    pub fn keep_list(mut self, counts: &[f64], env: &Uiua) -> UiuaResult<Self> {
        if counts.iter().any(|&n| n.fract() != 0.0) {
            return Err(env.error("Keep amount must be a list of integers"));
        }
        self.take_map_keys();
        let counts = pad_keep_counts(counts, self.row_count(), env)?;
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
        self.validate_shape();
        Ok(self)
    }
    fn unkeep(mut self, env: &Uiua) -> UiuaResult<(Value, Self)> {
        self.take_map_keys();
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
        self.validate_shape();
        Ok((counts.into(), self))
    }
    fn undo_keep(self, counts: &[f64], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        if into.rank() == 0 {
            return Err(env.error("Cannot undo keep of scalar array"));
        }
        let from = self;
        let counts = pad_keep_counts(counts, into.row_count(), env)?;
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
                if from.row_count() != true_count {
                    return Err(env.error(format!(
                        "Attempted to undo keep, but the length of \
                        the kept array changed from {true_count} to {}",
                        from.row_count()
                    )));
                }
                if !from.shape.iter().skip(1).eq(into.shape.iter().skip(1)) {
                    let mut original_shape = into.shape.row();
                    original_shape.insert(0, from.row_count());
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
                if from.row_count() != true_count {
                    return Err(env.error(format!(
                        "Attempted to undo keep, but the length of \
                        the kept array changed from {true_count} to {}",
                        from.row_count()
                    )));
                }
                if from.rank() - into.rank() > 1 || from.shape[2..] != into.shape[1..] {
                    return Err(env.error(format!(
                        "Cannot undo keep of array with shape {} \
                        into array with shape {}",
                        from.shape, into.shape
                    )));
                }
                let mut rows = Vec::with_capacity(
                    into.row_count() + from.shape[..2].iter().product::<usize>() - true_count,
                );
                let mut from_rows = from.into_rows();
                for (&count, into_row) in counts.iter().zip(into.into_rows()) {
                    if count < 1.0 {
                        rows.push(into_row);
                    } else {
                        let from_row = from_rows.next().expect(
                            "number of true counts was verified \
                            to match from row count",
                        );
                        rows.extend(from_row.into_rows());
                    }
                }
                into = Array::from_row_arrays_infallible(rows);
            }
        }
        Ok(into)
    }
}

pub(super) fn pad_keep_counts<'a>(
    counts: &'a [f64],
    len: usize,
    env: &Uiua,
) -> UiuaResult<Cow<'a, [f64]>> {
    let mut amount = Cow::Borrowed(counts);
    match amount.len().cmp(&len) {
        Ordering::Equal => {}
        Ordering::Less => match env.either_array_fill::<f64>() {
            Ok(fill) => {
                if let Some(n) = fill.data.iter().find(|&&n| n.fract() != 0.0) {
                    return Err(env.error(format!(
                        "Fill value for keep must be an array of \
                        integers, but one of the values is {n}"
                    )));
                }
                match fill.rank() {
                    0 => {
                        let fill = fill.data[0];
                        let amount = amount.to_mut();
                        amount.extend(repeat(fill).take(len - amount.len()));
                    }
                    1 => {
                        let amount = amount.to_mut();
                        amount.extend((fill.data.iter().copied().cycle()).take(len - amount.len()));
                    }
                    _ => {
                        return Err(env.error(format!(
                            "Fill value for keep must be a scalar or a 1D array, \
                            but it has shape {}",
                            fill.shape
                        )));
                    }
                }
            }
            Err(e) if counts.is_empty() => {
                return Err(env.error(format!(
                    "Cannot keep array with shape {} with array of shape {}{e}",
                    len,
                    FormatShape(&[amount.len()])
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
        self.rotate_depth(rotated, 0, 0, env)
    }
    pub(crate) fn rotate_depth(
        &self,
        rotated: &mut Self,
        a_depth: usize,
        b_depth: usize,
        env: &Uiua,
    ) -> UiuaResult {
        if self.row_count() == 0 {
            return Ok(());
        }
        let by_ints = || self.as_integer_array(env, "Rotation amount must be an array of integers");
        rotated.match_fill(env);
        match rotated {
            Value::Num(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Byte(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Complex(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Char(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Box(a) if a.rank() == a_depth => {
                for Boxed(val) in a.data.as_mut_slice() {
                    self.rotate_depth(val, a_depth, b_depth, env)?;
                }
            }
            Value::Box(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
        }
        Ok(())
    }
}

impl<T: ArrayValue> Array<T> {
    /// `rotate` this array by the given amount
    pub fn rotate(&mut self, by: Array<isize>, env: &Uiua) -> UiuaResult {
        self.rotate_depth(by, 0, 0, env)
    }
    pub(crate) fn rotate_depth(
        &mut self,
        by: Array<isize>,
        depth: usize,
        by_depth: usize,
        env: &Uiua,
    ) -> UiuaResult {
        let mut filled = false;
        let fill = env.scalar_fill::<T>();
        self.depth_slices(&by, depth, by_depth, env, |ash, a, bsh, b, env| {
            if bsh.len() > 1 {
                return Err(env.error(format!("Cannot rotate by rank {} array", bsh.len())));
            }
            if b.len() > ash.len() {
                return Err(env.error(format!(
                    "Cannot rotate rank {} array with index of length {}",
                    ash.len(),
                    b.len()
                )));
            }
            rotate(b, ash, a);
            if let Ok(fill) = &fill {
                fill_shift(b, ash, a, fill.clone());
                filled = true;
            }
            Ok(())
        })?;
        if filled {
            self.reset_meta_flags();
        }
        if depth == 0 {
            if let Some(keys) = self.map_keys_mut() {
                let by = by.data[0];
                keys.rotate(by);
            }
        }
        Ok(())
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
    /// Use this array to `windows` another
    pub fn windows(&self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        let size_array = self.as_integer_array(env, "Window size must be an integer array")?;
        from.match_fill(env);
        Ok(match &*size_array.shape {
            [] | [_] => val_as_arr!(from, |a| a.windows(&size_array.data, env)?.into()),
            [1, _] => val_as_arr!(from, |a| a.chunks(&size_array.data, env)?.into()),
            &[2, n] => {
                let size = &size_array.data[..n];
                let mut stride = Vec::with_capacity(n);
                for &s in &size_array.data[n..] {
                    if s <= 0 {
                        return Err(env.error("Window stride must be positive"));
                    }
                    stride.push(s.unsigned_abs());
                }
                val_as_arr!(from, |a| a.strided_windows(size, &stride, env)?.into())
            }
            _ => return Err(env.error(format!("Invalid windows shape {}", size_array.shape))),
        })
    }
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
    /// Get the `windows` of this array
    pub fn windows(&self, isize_spec: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let stride = vec![1; isize_spec.len()];
        self.strided_windows(isize_spec, &stride, env)
    }
    /// Get the `windows` of this array
    pub(crate) fn strided_windows(
        &self,
        isize_spec: &[isize],
        stride: &[usize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if isize_spec.len() != stride.len() {
            return Err(env.error(format!(
                "Window size {} does not match stride size {}",
                isize_spec.len(),
                stride.len()
            )));
        }
        if isize_spec.iter().any(|&s| s == 0) {
            return Err(env.error("Window size cannot be zero"));
        }
        if isize_spec.len() > self.shape.len() {
            return Err(env.error(format!(
                "Window size {} has too many axes for shape {}",
                FormatShape(isize_spec),
                self.shape()
            )));
        }

        // Do filled windows if there is a fill value
        if let Ok(fill) = env.scalar_fill::<T>() {
            if stride.iter().any(|&s| s != 1) {
                return Err(env.error("Filled strided windows is not currently supported"));
            }
            return Ok(self.filled_windows(isize_spec, fill));
        }

        let mut size_spec = Vec::with_capacity(isize_spec.len());
        for (d, s) in self.shape.iter().zip(isize_spec) {
            size_spec.push(if *s >= 0 { *s } else { *d as isize + 1 + *s });
        }
        // Determine the shape of the windows array
        let mut new_shape = Shape::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(
            (self.shape.iter().zip(&size_spec).zip(stride))
                .map(|((d, w), s)| ((*d + *s) as isize - *w).max(0) as usize / *s),
        );
        new_shape.extend(size_spec.iter().map(|&s| s.max(0) as usize));
        new_shape.extend_from_slice(&self.shape[size_spec.len()..]);
        // Early return if empty
        if self.shape.contains(&0) {
            return Ok(Self::new(new_shape, CowSlice::new()));
        }
        // Check if the window size is too large
        for (size, sh) in size_spec.iter().zip(&self.shape) {
            if *size <= 0 || *size > *sh as isize {
                return Ok(Self::new(new_shape, CowSlice::new()));
            }
        }
        // Make a new window shape with the same rank as the windowed array
        let mut true_size: Vec<usize> = Vec::with_capacity(self.shape.len());
        true_size.extend(size_spec.iter().map(|&s| s as usize));
        if true_size.len() < self.shape.len() {
            true_size.extend(&self.shape[true_size.len()..]);
        }

        let mut dst = EcoVec::from_elem(self.data[0].clone(), new_shape.iter().product());
        let dst_slice = dst.make_mut();
        let mut corner = vec![0; self.shape.len()];
        let mut curr = vec![0; self.shape.len()];
        let mut k = 0;
        'windows: loop {
            // Reset curr
            for i in &mut curr {
                *i = 0;
            }
            // Copy the window at the current corner
            let out_of_bounds = (corner.iter())
                .zip(&self.shape)
                .zip(&true_size)
                .any(|((c, d), w)| *c + *w > *d);
            if !out_of_bounds {
                'items: loop {
                    // Copy the current item
                    let mut src_index = 0;
                    let mut stride = 1;
                    for ((c, i), s) in corner.iter().zip(&curr).zip(&self.shape).rev() {
                        src_index += (*c + *i) * stride;
                        stride *= s;
                    }
                    dst_slice[k] = self.data[src_index].clone();
                    k += 1;
                    // Go to the next item
                    for i in (0..curr.len()).rev() {
                        if curr[i] == true_size[i] - 1 {
                            curr[i] = 0;
                        } else {
                            curr[i] += 1;
                            continue 'items;
                        }
                    }
                    break;
                }
            }
            // Go to the next corner
            for i in (0..corner.len()).rev() {
                if corner[i] >= self.shape[i] - true_size[i] {
                    corner[i] = 0;
                } else {
                    corner[i] += stride.get(i).copied().unwrap_or(1);
                    continue 'windows;
                }
            }
            break Ok(Array::new(new_shape, dst));
        }
    }
    fn filled_windows(&self, isize_spec: &[isize], fill: T) -> Self {
        let mut true_win_size = Vec::with_capacity(isize_spec.len().max(self.shape.len()));
        for (d, s) in self.shape.iter().zip(isize_spec) {
            true_win_size.push(if *s >= 0 { *s } else { *d as isize + 1 + *s } as usize);
        }
        let new_shape: Shape = (self.shape.iter().zip(&true_win_size))
            .map(|(&s, &t)| s + t - 1)
            .chain(
                (true_win_size.iter())
                    .chain(self.shape.iter().skip(true_win_size.len()))
                    .copied(),
            )
            .collect();
        // println!("new_shape: {new_shape:?}");
        // println!("true_win_size: {true_win_size:?}");
        let mut dst = EcoVec::from_elem(fill.clone(), new_shape.iter().product());
        let dst_slice = dst.make_mut();
        let mut index = vec![0usize; true_win_size.len()];
        // Tracks the unsigned offset within the current window
        let mut tracking_curr = vec![0usize; true_win_size.len()];
        // Tracks the signed offset from the current index within the current window
        let mut offset_curr = vec![0isize; true_win_size.len()];
        let mut k = 0;
        let item_len: usize = self.shape.iter().skip(true_win_size.len()).product();
        'windows: loop {
            // println!("index: {index:?}");
            // Reset tracking_curr and offset_curr
            for c in &mut tracking_curr {
                *c = 0;
            }
            for ((o, i), t) in offset_curr.iter_mut().zip(&index).zip(&true_win_size) {
                *o = *i as isize - (*t as isize - 1);
            }
            // Copy the window at the current index
            'items: loop {
                // println!("  offset_curr: {offset_curr:?}, tracking_curr: {tracking_curr:?}");
                // Update offset_curr
                let mut out_of_bounds = false;
                for (o, s) in offset_curr.iter_mut().zip(&self.shape) {
                    if *o < 0 || *o >= *s as isize {
                        out_of_bounds = true;
                        break;
                    }
                }
                // Set the element
                if !out_of_bounds {
                    let mut src_index = 0;
                    let mut stride = item_len;
                    for (o, s) in offset_curr.iter().zip(&self.shape).rev() {
                        src_index += *o as usize * stride;
                        stride *= *s;
                    }
                    for i in 0..item_len {
                        let elem = self.data[src_index + i].clone();
                        dst_slice[k + i] = elem;
                    }
                }
                k += item_len;
                // Go to the next item
                for i in (0..tracking_curr.len()).rev() {
                    if tracking_curr[i] == true_win_size[i] - 1 {
                        tracking_curr[i] = 0;
                        offset_curr[i] = index[i] as isize - (true_win_size[i] as isize - 1);
                    } else {
                        tracking_curr[i] += 1;
                        offset_curr[i] += 1;
                        continue 'items;
                    }
                }
                break;
            }
            // Go to the next index
            for i in (0..index.len()).rev() {
                if index[i] == self.shape[i] + true_win_size[i] - 2 {
                    index[i] = 0;
                } else {
                    index[i] += 1;
                    continue 'windows;
                }
            }
            break Array::new(new_shape, dst);
        }
    }
}

impl Value {
    /// Use this value to `chunks` another
    pub fn chunks(&self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        let isize_spec = self.as_ints(env, "Chunk size must be an integer or list of integers")?;
        from.match_fill(env);
        Ok(val_as_arr!(from, |a| a.chunks(&isize_spec, env)?.into()))
    }
    pub(crate) fn undo_chunks(self, size: &Self, env: &Uiua) -> UiuaResult<Self> {
        let isize_spec = size.as_ints(env, "Chunk size must be an integer or list of integers")?;
        Ok(val_as_arr!(self, |a| a
            .undo_chunks(&isize_spec, env)?
            .into()))
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get `chunks` of this array
    pub fn chunks(mut self, isize_spec: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if isize_spec.iter().any(|&s| s == 0) {
            return Err(env.error("Chunk size cannot be zero"));
        }
        if isize_spec.len() > self.shape.len() {
            return Err(env.error(format!(
                "Chunk size {} has too many axes for shape {}",
                FormatShape(isize_spec),
                self.shape()
            )));
        }

        let fill = env.scalar_fill::<T>().ok();
        let mut size_spec = Vec::with_capacity(isize_spec.len());
        let mut virtual_shape = self.shape.clone();
        for (v, &i) in virtual_shape.iter_mut().zip(isize_spec) {
            let u = i.unsigned_abs();
            let s = if i >= 0 {
                u
            } else if fill.is_some() {
                ((*v + u - 1) / u).max(1)
            } else {
                (*v / u).max(1)
            };
            size_spec.push(s);
            if *v % u != 0 {
                *v = if fill.is_some() {
                    (*v / u + 1) * u
                } else {
                    (*v / u) * u
                };
            }
        }
        let row_shape: Shape = self.shape[size_spec.len()..].iter().copied().collect();
        let row_len = row_shape.elements();

        let mut new_shape = Shape::with_capacity(size_spec.len() + virtual_shape.len());
        for (d, s) in virtual_shape.iter().zip(&size_spec) {
            new_shape.push(*d / *s);
        }
        new_shape.extend(size_spec.iter().copied());
        new_shape.extend(virtual_shape.iter().skip(size_spec.len()).copied());

        // println!("size_spec: {size_spec:?}");
        // println!("new_shape: {new_shape:?}");

        if size_spec.as_slice().len() == 1 {
            if let Some(fill) = &fill {
                self.data
                    .extend_repeat(fill, new_shape.elements() - self.data.len());
            } else {
                self.data.truncate(new_shape.elements());
            }
            self.shape = new_shape;
            return Ok(self);
        }

        if new_shape.elements() == 0 {
            return Ok(Array::new(new_shape, CowSlice::new()));
        }

        let mut new_data =
            EcoVec::with_capacity(validate_size::<T>(new_shape.iter().copied(), env)?);
        let mut corner = vec![0; self.shape.len()];
        let mut offset = vec![0; size_spec.len()];
        'chunks: loop {
            // Reset offset
            for i in &mut offset {
                *i = 0;
            }
            // println!("corner: {corner:?}");
            // Copy the chunk at the current corner
            'items: loop {
                // println!("offset: {offset:?}");
                let mut src_index = 0;
                let mut stride = row_len;
                if let Some(fill) = &fill {
                    let mut oob = false;
                    for ((&c, &i), &s) in corner.iter().zip(&offset).zip(&self.shape).rev() {
                        let axis_index = c + i;
                        if axis_index >= s {
                            oob = true;
                            break;
                        }
                        src_index += axis_index * stride;
                        stride *= s;
                    }
                    if oob {
                        unsafe { new_data.extend_from_trusted(Repeat::new(fill, row_len)) };
                    } else {
                        new_data.extend_from_slice(&self.data[src_index..][..row_len]);
                    }
                } else {
                    for ((c, i), s) in corner.iter().zip(&offset).zip(&self.shape).rev() {
                        src_index += (*c + *i) * stride;
                        stride *= s;
                    }
                    new_data.extend_from_slice(&self.data[src_index..][..row_len]);
                }
                // Go to the next item
                for i in (0..offset.len()).rev() {
                    offset[i] += 1;
                    if offset[i] == size_spec[i] {
                        offset[i] = 0;
                    } else {
                        continue 'items;
                    }
                }
                break;
            }
            // Go to the next corner
            for i in (0..size_spec.len()).rev() {
                corner[i] += size_spec[i];
                if corner[i] == virtual_shape[i] {
                    corner[i] = 0;
                } else {
                    continue 'chunks;
                }
            }
            break;
        }
        Ok(Array::new(new_shape, new_data))
    }
    pub(crate) fn undo_chunks(mut self, isize_spec: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if isize_spec.iter().any(|&s| s == 0) {
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
        self.validate_shape();

        Ok(self)
    }
}

impl Array<f64> {
    pub(crate) fn matrix_mul(&self, other: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (a, b) = (self, other);
        let a_row_shape = a.shape().row();
        let b_row_shape = b.shape().row();
        if !shape_prefixes_match(&a_row_shape, &b_row_shape) {
            return Err(env.error(format!(
                "Cannot multiply arrays of shape {} and {}",
                a.shape(),
                b.shape()
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
            if a.row_count() > 100 || b.row_count() > 100 {
                (iter.par_bridge()).for_each(|(a_row, res_row)| inner(a_row, res_row));
            } else {
                iter.for_each(|(a_row, res_row)| inner(a_row, res_row));
            }
        }
        Ok(Array::new(result_shape, result_data))
    }
}

impl Value {
    /// `orient` a value by this value
    pub fn orient(&self, target: &mut Self, env: &Uiua) -> UiuaResult {
        let indices = self.as_ints(env, "Orient indices must be integers")?;
        let mut undices = Vec::with_capacity(indices.len());
        for i in indices {
            let u = i.unsigned_abs();
            if i >= 0 && u >= target.rank() || i < 0 && u > target.rank() {
                return Err(env.error(format!(
                    "Cannot orient axis {i} in array of rank {}",
                    target.rank()
                )));
            }
            if i >= 0 {
                undices.push(u);
            } else {
                undices.push(target.rank() - u);
            }
        }
        if undices.len() > target.rank() {
            return Err(env.error(format!(
                "Cannot orient array of rank {} with {} indices",
                target.rank(),
                undices.len()
            )));
        }
        for (i, u) in undices.iter().enumerate() {
            if undices[i + 1..].iter().any(|u2| u == u2) {
                return Err(env.error("Orient indices must be unique"));
            }
        }
        for (depth, amnt) in derive_orient_rotations(target.rank(), &undices) {
            target.transpose_depth(depth, amnt);
        }
        Ok(())
    }
    pub(crate) fn anti_orient(&self, target: Self, env: &Uiua) -> UiuaResult<Self> {
        let indices = self.as_ints(env, "Unorient indices must be integers")?;
        val_as_arr!(target, |a| a.anti_orient(&indices, env).map(Into::into))
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

impl<T: ArrayValue> Array<T> {
    fn anti_orient(mut self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        fn derive_orient_data(
            indices: &[isize],
            shape: &[usize],
            env: &Uiua,
        ) -> UiuaResult<(Vec<usize>, Shape, usize, usize)> {
            let rank = shape.len();
            // Normalize indices
            let mut undices = Vec::with_capacity(indices.len());
            for &i in indices {
                let u = i.unsigned_abs();
                if i >= 0 && u >= rank || i < 0 && u > rank {
                    return Err(
                        env.error(format!("Cannot orient axis {i} in array of rank {rank}"))
                    );
                }
                if i >= 0 {
                    undices.push(u);
                } else {
                    undices.push(rank - u);
                }
            }

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

            Ok((undices, new_shape, duplicate_count, trailing_dims))
        }

        let (undices, new_shape, duplicates, trailing_dims) =
            derive_orient_data(indices, &self.shape, env)?;

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

impl<T: RealArrayValue> Array<T> {
    fn base_scalar(&self, base: f64, env: &Uiua) -> UiuaResult<Array<f64>> {
        if base == 0.0 {
            return Err(env.error("Base cannot be 0"));
        }
        if base.is_infinite() {
            return Err(env.error("Base cannot be infinite"));
        }
        if base.is_nan() {
            return Err(env.error("Base cannot be NaN"));
        }
        Ok(if base >= 0.0 {
            let max_row_len = self
                .data
                .iter()
                .map(|&n| {
                    let n = n.to_f64();
                    if n == 0.0 {
                        0
                    } else {
                        n.abs().log(base).floor() as usize + 1
                    }
                })
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
        for &base in bases {
            if base == 0.0 {
                return Err(env.error("Base cannot contain 0s"));
            }
            if base.is_infinite() && base.is_sign_negative() {
                return Err(env.error("Base cannot contain negative infinities"));
            }
            if base.is_nan() {
                return Err(env.error("Base cannot contain NaNs"));
            }
        }
        let mut new_shape = self.shape.clone();
        new_shape.push(bases.len());
        let elem_count = validate_size::<f64>(new_shape.iter().copied(), env)?;
        let mut new_data = eco_vec![0.0; elem_count];
        let slice = new_data.make_mut();
        for (i, n) in self.data.iter().enumerate() {
            let mut n = n.to_f64();
            for (j, base) in bases.iter().enumerate() {
                if n == f64::INFINITY {
                    slice[i * bases.len() + j] = n;
                    break;
                } else {
                    slice[i * bases.len() + j] = n.rem_euclid(*base);
                    n = n.div_euclid(*base);
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
        let mut shape = self.shape.clone();
        let row_len = shape.pop().unwrap_or(1);
        let elem_count = validate_size::<f64>(shape.iter().copied(), env)?;
        let mut data = eco_vec![0f64; elem_count];
        if row_len > 0 {
            let slice = data.make_mut();
            let mut bases = bases.to_vec();
            bases.extend(repeat(1.0).take(row_len.saturating_sub(bases.len())));
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
            b.member(&range, env)
        }

        let Ok(range_bound) = self.as_num(env, "") else {
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
    pub fn multidim_memberof_range(&self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        fn fallback(a: &Value, b: &Value, env: &Uiua) -> UiuaResult<Value> {
            let mut range = a.range(env)?;
            range.rerank(&Value::Num(1.0.into()), env)?;
            b.member(&range, env)
        }

        if self.rank() != 1 {
            return fallback(self, &from, env);
        }

        let Ok(range_bound) = self.as_number_list(env, "", |v| v.fract() == 0.0, |v| v) else {
            return fallback(self, &from, env);
        };

        if !from.shape().ends_with(&[range_bound.len()]) {
            let shape = from.shape();
            let new_shape = &shape[..shape.len() - 1];
            return Ok(Value::Byte(Array::new(
                new_shape,
                CowSlice::from_elem(0, new_shape.iter().product()),
            )));
        }

        match from.rank().cmp(&2) {
            Ordering::Equal => Ok(match from {
                Value::Num(nums) => {
                    let data: EcoVec<u8> = nums
                        .row_slices()
                        .map(|row| {
                            row.iter().zip(&range_bound).all(|(&r, &b)| {
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
                        .map(|row| row.iter().zip(&range_bound).all(|(&r, &b)| (r as f64) < b))
                        .map(Into::into)
                        .collect();
                    Array::new(&bytes.shape[..1], data).into()
                }
                from => fallback(self, &from, env)?,
            }),
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(from.row_count());
                for row in from.rows() {
                    let Some(row) = self
                        .multidim_memberof_range(row, env)?
                        .as_byte_array()
                        .cloned()
                    else {
                        return fallback(self, &from, env);
                    };
                    rows.push(row);
                }
                Ok(Value::Byte(
                    Array::from_row_arrays(rows, env).map(Into::into)?,
                ))
            }
            Ordering::Less => fallback(self, &from, env),
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
