//! Algorithms for dyadic array operations

mod combine;
mod structure;

use core::f64;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    iter::{once, repeat},
    mem::take,
    slice,
};

use ecow::{eco_vec, EcoVec};
use rayon::prelude::*;

use crate::{
    algorithm::pervade::{self, bin_pervade_recursive, InfalliblePervasiveFn},
    array::*,
    boxed::Boxed,
    cowslice::{cowslice, CowSlice, Repeat},
    value::Value,
    Shape, Uiua, UiuaResult,
};

use super::{
    pervade::ArrayRef, shape_prefixes_match, validate_size, validate_size_ctx, ArrayCmpSlice,
    ErrorContext, FillContext,
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
                for &b_dim in b.shape[..b_depth - a_depth].iter().rev() {
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
                for &a_dim in a.shape[..a_depth - b_depth].iter().rev() {
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
            match self {
                Value::Num(a) => a.reshape_scalar(n, env),
                Value::Byte(a) => a.reshape_scalar(n, env),
                Value::Complex(a) => a.reshape_scalar(n, env),
                Value::Char(a) => a.reshape_scalar(n, env),
                Value::Box(a) => a.reshape_scalar(n, env),
            }
        } else {
            self.match_scalar_fill(env);
            match self {
                Value::Num(a) => a.reshape(&target_shape, env),
                Value::Byte(a) => a.reshape(&target_shape, env),
                Value::Complex(a) => a.reshape(&target_shape, env),
                Value::Char(a) => a.reshape(&target_shape, env),
                Value::Box(a) => a.reshape(&target_shape, env),
            }
        }
    }
    pub(crate) fn undo_reshape(&mut self, old_shape: &Self, env: &Uiua) -> UiuaResult {
        if old_shape.as_nat(env, "").is_ok() {
            return Err(env.error("Cannot undo scalar reshae"));
        }
        let orig_shape = old_shape.as_nats(env, "Shape should be a list of integers")?;
        if orig_shape.iter().product::<usize>() == self.shape().iter().product::<usize>() {
            *self.shape_mut() = Shape::from(orig_shape.as_slice());
            Ok(())
        } else {
            Err(env.error(format!(
                "Cannot unreshape array because its old shape was {}, \
                but its new shape is {}, which has a different number of elements",
                FormatShape(&orig_shape),
                self.shape()
            )))
        }
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
                self.reshape_scalar_integer(count.unsigned_abs(), env)
            }
            Err(rev) => {
                if rev {
                    self.reverse()
                }
                Ok(())
            }
        }
    }
    pub(crate) fn reshape_scalar_integer<C: ErrorContext>(
        &mut self,
        count: usize,
        ctx: &C,
    ) -> Result<(), C::Error> {
        if count == 0 {
            self.data.clear();
            self.shape.insert(0, 0);
            return Ok(());
        }
        let elem_count = validate_size_ctx::<T, _>([count - 1, self.data.len()], ctx)?;
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
        let orig_shape = orig_shape.as_nats(env, "Shape must be a list of natural numbers")?;
        let rank = irank.unsigned_abs();
        let new_shape: Shape = if irank >= 0 {
            // Positive rank
            orig_shape
                .iter()
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
            match kept {
                Value::Num(a) => a.keep_list(&counts, env)?.into(),
                Value::Byte(a) => a.keep_list(&counts, env)?.into(),
                Value::Complex(a) => a.keep_list(&counts, env)?.into(),
                Value::Char(a) => a.keep_list(&counts, env)?.into(),
                Value::Box(a) => a.keep_list(&counts, env)?.into(),
            }
        })
    }
    pub(crate) fn unkeep(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.generic_into(
            |a| a.unkeep(env).map(|(a, b)| (a, b.into())),
            |a| a.unkeep(env).map(|(a, b)| (a, b.into())),
            |a| a.unkeep(env).map(|(a, b)| (a, b.into())),
            |a| a.unkeep(env).map(|(a, b)| (a, b.into())),
            |a| a.unkeep(env).map(|(a, b)| (a, b.into())),
        )
    }
    pub(crate) fn undo_keep(self, kept: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = self.as_nums(
            env,
            "Keep amount must be a positive real number \
            or list of natural numbers",
        )?;
        if self.rank() == 0 {
            return Err(env.error("Cannot invert scalar keep"));
        }
        kept.generic_bin_into(
            into,
            |a, b| a.undo_keep(&counts, b, env).map(Into::into),
            |a, b| a.undo_keep(&counts, b, env).map(Into::into),
            |a, b| a.undo_keep(&counts, b, env).map(Into::into),
            |a, b| a.undo_keep(&counts, b, env).map(Into::into),
            |a, b| a.undo_keep(&counts, b, env).map(Into::into),
            |a, b| env.error(format!("Cannot unkeep {a} array with {b} array")),
        )
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
        if counts.iter().any(|&n| n < 0.0 || n.fract() != 0.0) {
            return Err(env.error("Keep amount must be a list of natural numbers"));
        }
        self.take_map_keys();
        let counts = pad_keep_counts(counts, self.row_count(), env)?;
        if self.rank() == 0 {
            if counts.len() != 1 {
                return Err(env.error("Scalar array can only be kept with a single number"));
            }
            let mut new_data = EcoVec::with_capacity(counts[0] as usize);
            for _ in 0..counts[0] as usize {
                new_data.push(self.data[0].clone());
            }
            self = new_data.into();
        } else {
            let mut all_bools = true;
            let mut true_count = 0;
            let mut sum: f64 = 0.0;
            for &n in counts.iter() {
                sum += n;
                match n as usize {
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
                        let n = *n as usize;
                        new_len += n;
                        for _ in 0..n {
                            new_data.extend_from_slice(r);
                        }
                    }
                } else {
                    new_len = counts.iter().sum::<f64>() as usize;
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
    fn undo_keep(self, counts: &[f64], into: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = pad_keep_counts(counts, into.row_count(), env)?;
        if counts.iter().any(|&n| n > 1.0) {
            return Err(env.error("Cannot invert keep with non-boolean counts"));
        }
        let mut new_rows: Vec<_> = Vec::with_capacity(counts.len());
        let mut transformed = self.into_rows();
        for (count, into_row) in counts.iter().zip(into.into_rows()) {
            if *count == 0.0 {
                new_rows.push(into_row);
            } else {
                let new_row = transformed.next().ok_or_else(|| {
                    env.error(
                        "Kept array has fewer rows than it was created with, \
                        so the keep cannot be inverted",
                    )
                })?;
                if new_row.shape != into_row.shape {
                    return Err(env.error(format!(
                        "Kept array's shape was changed from {} to {}, \
                        so the keep cannot be inverted",
                        into_row.shape(),
                        new_row.shape()
                    )));
                }
                new_rows.push(new_row);
            }
        }
        Self::from_row_arrays(new_rows, env)
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
        Ordering::Less => match env.num_array_fill() {
            Ok(fill) => {
                if let Some(n) = fill.data.iter().find(|&&n| n < 0.0 || n.fract() != 0.0) {
                    return Err(env.error(format!(
                        "Fill value for keep must be an array of \
                        non-negative integers, but one of the \
                        values is {n}"
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
    pub fn rotate(&self, rotated: Self, env: &Uiua) -> UiuaResult<Self> {
        self.rotate_depth(rotated, 0, 0, env)
    }
    pub(crate) fn rotate_depth(
        &self,
        mut rotated: Self,
        a_depth: usize,
        b_depth: usize,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if self.row_count() == 0 {
            return Ok(rotated);
        }
        let by_ints = || self.as_integer_array(env, "Rotation amount must be an array of integers");
        rotated.match_scalar_fill(env);
        match &mut rotated {
            Value::Num(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Byte(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Complex(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Char(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
            Value::Box(a) if a.rank() == a_depth => {
                for Boxed(val) in a.data.as_mut_slice() {
                    *val = self.rotate_depth(take(val), a_depth, b_depth, env)?;
                }
            }
            Value::Box(a) => a.rotate_depth(by_ints()?, b_depth, a_depth, env)?,
        }
        Ok(rotated)
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
    pub fn windows(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let size_spec = self.as_ints(env, "Window size must be an integer or list of integers")?;
        Ok(match from {
            Value::Num(a) => a.windows(&size_spec, env)?.into(),
            Value::Byte(a) if env.number_only_fill() => {
                a.convert_ref::<f64>().windows(&size_spec, env)?.into()
            }
            Value::Byte(a) => a.windows(&size_spec, env)?.into(),
            Value::Complex(a) => a.windows(&size_spec, env)?.into(),
            Value::Char(a) => a.windows(&size_spec, env)?.into(),
            Value::Box(a) => a.windows(&size_spec, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the `windows` of this array
    pub fn windows(&self, isize_spec: &[isize], env: &Uiua) -> UiuaResult<Self> {
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
            return Ok(self.filled_windows(isize_spec, fill));
        }

        let mut size_spec = Vec::with_capacity(isize_spec.len());
        for (d, s) in self.shape.iter().zip(isize_spec) {
            size_spec.push(if *s >= 0 { *s } else { *d as isize + 1 + *s });
        }
        // Determine the shape of the windows array
        let mut new_shape = Shape::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(
            self.shape
                .iter()
                .zip(&size_spec)
                .map(|(a, b)| ((*a as isize + 1) - *b).max(0) as usize),
        );
        new_shape.extend(size_spec.iter().map(|&s| s.max(0) as usize));
        new_shape.extend_from_slice(&self.shape[size_spec.len()..]);
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
            // Go to the next corner
            for i in (0..corner.len()).rev() {
                if corner[i] == self.shape[i] - true_size[i] {
                    corner[i] = 0;
                } else {
                    corner[i] += 1;
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
    pub fn chunks(&self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let isize_spec = self.as_ints(env, "Chunk size must be an integer or list of integers")?;
        Ok(match from {
            Value::Num(a) => a.chunks(&isize_spec, env)?.into(),
            Value::Byte(a) if env.number_only_fill() => {
                a.convert_ref::<f64>().chunks(&isize_spec, env)?.into()
            }
            Value::Byte(a) => a.chunks(&isize_spec, env)?.into(),
            Value::Complex(a) => a.chunks(&isize_spec, env)?.into(),
            Value::Char(a) => a.chunks(&isize_spec, env)?.into(),
            Value::Box(a) => a.chunks(&isize_spec, env)?.into(),
        })
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
                (*v + u - 1) / u
            } else {
                *v / u
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
}

impl Value {
    /// Try to `find` this value in another
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            searched,
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot find {} in {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    /// Try to `mask` this value in another
    pub fn mask(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            searched,
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot mask {} in {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Try to `find` this array in another
    pub fn find(&self, haystack: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let needle = self;
        let mut haystack = haystack;
        let mut local_searched: Self;
        let any_dim_greater = (needle.shape().iter().rev())
            .zip(haystack.shape().iter().rev())
            .any(|(a, b)| a > b);
        if self.rank() > haystack.rank() || any_dim_greater {
            // Fill
            match env.scalar_fill() {
                Ok(fill) => {
                    let mut target_shape = haystack.shape.clone();
                    target_shape[0] = needle.row_count();
                    local_searched = haystack.clone();
                    local_searched.fill_to_shape(&target_shape, fill);
                    haystack = &local_searched;
                }
                Err(_) => {
                    let data = cowslice![0; haystack.element_count()];
                    let mut arr = Array::new(haystack.shape.clone(), data);
                    arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
                    return Ok(arr);
                }
            }
        }

        // Pad the shape of the searched-for array
        let mut searched_for_shape = needle.shape.clone();
        while searched_for_shape.len() < haystack.shape.len() {
            searched_for_shape.insert(0, 1);
        }

        // Calculate the pre-padded output shape
        let temp_output_shape: Shape = haystack
            .shape
            .iter()
            .zip(&searched_for_shape)
            .map(|(s, f)| s + 1 - f)
            .collect();

        let mut data = EcoVec::from_elem(0, temp_output_shape.iter().product());
        let data_slice = data.make_mut();

        if haystack.rank() == 1 {
            // Fast path for rank-1 arrays
            if let Some(first) = needle.data.first() {
                // The jump is the number of elements to skip forward on a failed partial match
                let jump = (needle.data.iter())
                    .skip(1)
                    .position(|d| d.array_eq(first))
                    .map_or(needle.data.len(), |i| i + 1);
                let mut i = 0;
                while i < haystack.data.len() + 1 - needle.data.len() {
                    if haystack.data[i].array_eq(first) {
                        if needle.data[1..]
                            .iter()
                            .zip(&haystack.data[i + 1..])
                            .all(|(a, b)| a.array_eq(b))
                        {
                            // All elements match
                            data_slice[i] = 1;
                            i += jump;
                        } else {
                            // Mismatch
                            i += 1;
                        }
                    } else {
                        // Mismatch
                        i += 1;
                    }
                }
            }
        } else {
            let mut corner = vec![0; haystack.shape.len()];
            let mut curr = vec![0; haystack.shape.len()];
            let mut k = 0;

            if haystack.shape.iter().all(|&d| d > 0) {
                'windows: loop {
                    // Reset curr
                    for i in curr.iter_mut() {
                        *i = 0;
                    }
                    // Search the window whose top-left is the current corner
                    'items: loop {
                        // Get index for the current item in the haystack
                        let mut searched_index = 0;
                        let mut stride = 1;
                        for ((c, i), s) in corner.iter().zip(&curr).zip(&haystack.shape).rev() {
                            searched_index += (*c + *i) * stride;
                            stride *= s;
                        }
                        // Get index for the current item in the searched-for array
                        let mut search_for_index = 0;
                        let mut stride = 1;
                        for (i, s) in curr.iter().zip(&searched_for_shape).rev() {
                            search_for_index += *i * stride;
                            stride *= s;
                        }
                        // Compare the current items in the two arrays
                        let same = if let Some(searched_for) = needle.data.get(search_for_index) {
                            haystack.data[searched_index].array_eq(searched_for)
                        } else {
                            false
                        };
                        if !same {
                            data_slice[k] = 0;
                            k += 1;
                            break;
                        }
                        // Go to the next item
                        for i in (0..curr.len()).rev() {
                            if curr[i] == searched_for_shape[i] - 1 {
                                curr[i] = 0;
                            } else {
                                curr[i] += 1;
                                continue 'items;
                            }
                        }
                        data_slice[k] = 1;
                        k += 1;
                        break;
                    }
                    // Go to the next corner
                    for i in (0..corner.len()).rev() {
                        if corner[i] == haystack.shape[i] - searched_for_shape[i] {
                            corner[i] = 0;
                        } else {
                            corner[i] += 1;
                            continue 'windows;
                        }
                    }
                    break;
                }
            }
        }
        let mut arr = Array::new(temp_output_shape, data);
        arr.fill_to_shape(&haystack.shape[..searched_for_shape.len()], 0);
        arr.validate_shape();
        arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
        Ok(arr)
    }
    /// Try to `mask` this array in another
    pub fn mask(&self, haystack: &Self, env: &Uiua) -> UiuaResult<Value> {
        let needle = self;
        if needle.rank() > haystack.rank() {
            return Err(env.error(format!(
                "Cannot look for rank {} array in rank {} array",
                needle.rank(),
                haystack.rank()
            )));
        }
        if (needle.shape.iter().rev())
            .zip(haystack.shape.iter().rev())
            .any(|(n, h)| n > h)
        {
            return Ok(Array::new(
                haystack.shape.clone(),
                eco_vec![0u8; haystack.element_count()],
            )
            .into());
        }
        let mut result_data = eco_vec![0.0; haystack.element_count()];
        let res = result_data.make_mut();

        if haystack.rank() == 1 {
            // Fast path for rank-1 arrays
            if needle.data.len() > 0 {
                let mut curr = 0;
                let mut i = 0;
                while i < haystack.data.len() + 1 - needle.data.len() {
                    if (needle.data.iter())
                        .zip(&haystack.data[i..])
                        .all(|(a, b)| a.array_eq(b))
                    {
                        curr += 1;
                        for j in i..i + needle.data.len() {
                            res[j] = curr as f64;
                        }
                        i += needle.data.len();
                    } else {
                        i += 1;
                    }
                }
            }
        } else {
            let needle_data = needle.data.as_slice();
            let mut needle_shape = needle.shape.clone();
            while needle_shape.len() < haystack.shape.len() {
                needle_shape.insert(0, 1);
            }
            let needle_elems = needle.element_count();
            let mut curr = Vec::new();
            let mut offset = Vec::new();
            let mut sum = vec![0; needle_shape.len()];
            let mut match_num = 0u64;
            for i in 0..res.len() {
                // Check if the needle matches the haystack at the current index
                haystack.shape.flat_to_dims(i, &mut curr);
                let mut matches = true;
                for j in 0..needle_elems {
                    needle_shape.flat_to_dims(j, &mut offset);
                    for ((c, o), s) in curr.iter().zip(&offset).zip(&mut sum) {
                        *s = *c + *o;
                    }
                    if (haystack.shape.dims_to_flat(&sum)).map_or(true, |k| {
                        res[k] > 0.0 || !needle_data[j].array_eq(&haystack.data[k])
                    }) {
                        matches = false;
                        break;
                    }
                }
                // Fill matches
                if matches {
                    match_num += 1;
                    for j in 0..needle_elems {
                        needle_shape.flat_to_dims(j, &mut offset);
                        for ((c, o), s) in curr.iter().zip(&offset).zip(&mut sum) {
                            *s = *c + *o;
                        }
                        let k = haystack.shape.dims_to_flat(&sum).unwrap();
                        res[k] = match_num as f64;
                    }
                }
            }
        }
        let mut val: Value = Array::new(haystack.shape.clone(), result_data).into();
        val.compress();
        Ok(val)
    }
}

impl Value {
    /// Check which rows of this value are `member`s of another
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            of,
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for members of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Check which rows of this array are `member`s of another
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let elems = self;
        let mut arr = match elems.rank().cmp(&of.rank()) {
            Ordering::Equal => {
                let mut result_data = EcoVec::with_capacity(elems.row_count());
                let mut members = HashSet::with_capacity(of.row_count());
                for of in of.row_slices() {
                    members.insert(ArrayCmpSlice(of));
                }
                for elem in elems.row_slices() {
                    result_data.push(members.contains(&ArrayCmpSlice(elem)) as u8);
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                Array::new(shape, result_data)
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for elem in elems.rows() {
                    rows.push(elem.member(of, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if !of.shape.ends_with(&elems.shape) {
                    return Err(env.error(format!(
                        "Cannot look for array of shape {} in array of shape {}",
                        self.shape, of.shape
                    )));
                }
                if of.rank() - elems.rank() == 1 {
                    of.rows().any(|r| *elems == r).into()
                } else {
                    let mut rows = Vec::with_capacity(of.row_count());
                    for of in of.rows() {
                        rows.push(elems.member(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        };
        arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
        Ok(arr)
    }
}

impl Value {
    /// Get the `index of` the rows of this value in another
    pub fn index_of(&self, haystack: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            haystack,
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for indices of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
    /// Get the `coordinate` of the rows of this value in another
    pub fn coordinate(&self, haystack: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            haystack,
            |a, b| a.coordinate(b, env).map(Into::into),
            |a, b| a.coordinate(b, env).map(Into::into),
            |a, b| a.coordinate(b, env).map(Into::into),
            |a, b| a.coordinate(b, env).map(Into::into),
            |a, b| a.coordinate(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for coordinates of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
    /// Get the `progressive index of` the rows of this value in another
    pub fn progressive_index_of(&self, haystack: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            haystack,
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for indices of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the `index of` the rows of this array in another
    pub fn index_of(&self, haystack: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let needle = self;
        let default = (env.scalar_fill::<f64>()).unwrap_or(haystack.row_count() as f64);
        Ok(match needle.rank().cmp(&haystack.rank()) {
            Ordering::Equal => {
                let mut result_data = EcoVec::with_capacity(needle.row_count());
                let mut members = HashMap::with_capacity(haystack.row_count());
                for (i, of) in haystack.row_slices().enumerate() {
                    members.entry(ArrayCmpSlice(of)).or_insert(i);
                }
                for elem in needle.row_slices() {
                    result_data.push(
                        members
                            .get(&ArrayCmpSlice(elem))
                            .map(|i| *i as f64)
                            .unwrap_or(default),
                    );
                }
                Array::new(
                    needle.shape.iter().take(1).copied().collect::<Shape>(),
                    result_data,
                )
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(needle.row_count());
                for elem in needle.rows() {
                    rows.push(elem.index_of(haystack, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if !haystack.shape.ends_with(&needle.shape) {
                    return Err(env.error(format!(
                        "Cannot get index of array of shape {} in array of shape {}",
                        needle.shape(),
                        haystack.shape()
                    )));
                }
                if haystack.rank() - needle.rank() == 1 {
                    (haystack
                        .row_slices()
                        .position(|r| {
                            r.len() == needle.data.len()
                                && r.iter().zip(&needle.data).all(|(a, b)| a.array_eq(b))
                        })
                        .map(|i| i as f64)
                        .unwrap_or(default))
                    .into()
                } else {
                    let mut rows = Vec::with_capacity(haystack.row_count());
                    for of in haystack.rows() {
                        rows.push(needle.index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
    /// Get the `coordinate` of the rows of this array in another
    pub fn coordinate(&self, haystack: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let needle = self;
        Ok(match needle.rank().cmp(&haystack.rank()) {
            Ordering::Equal => {
                let mut result_data = EcoVec::with_capacity(needle.row_count());
                let mut members = HashMap::with_capacity(haystack.row_count());
                for (i, of) in haystack.row_slices().enumerate() {
                    members.entry(ArrayCmpSlice(of)).or_insert(i);
                }
                for row in needle.row_slices() {
                    result_data.push(
                        members
                            .get(&ArrayCmpSlice(row))
                            .map(|i| *i as f64)
                            .unwrap_or(haystack.row_count() as f64),
                    );
                }
                Array::new([needle.row_count(), 1], result_data)
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(needle.row_count());
                for elem in needle.rows() {
                    rows.push(elem.coordinate(haystack, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if !haystack.shape.ends_with(&needle.shape) {
                    return Err(env.error(format!(
                        "Cannot get coordinate of array of shape {} \
                        in array of shape {}",
                        needle.shape(),
                        haystack.shape()
                    )));
                }
                let haystack_item_len: usize =
                    haystack.shape.iter().rev().take(needle.rank()).product();
                if haystack_item_len == 0 {
                    todo!()
                }
                let outer_hay_shape =
                    Shape::from(&haystack.shape[..haystack.rank() - needle.rank()]);
                let index = if let Some(raw_index) = (haystack.data.chunks_exact(haystack_item_len))
                    .position(|ch| ch.iter().zip(&needle.data).all(|(a, b)| a.array_eq(b)))
                {
                    let mut index = Vec::new();
                    outer_hay_shape.flat_to_dims(raw_index, &mut index);
                    index
                } else {
                    outer_hay_shape.to_vec()
                };
                if index.len() == 1 {
                    (index[0] as f64).into()
                } else {
                    index.into()
                }
            }
        })
    }
    /// Get the `progressive index of` the rows of this array in another
    fn progressive_index_of(&self, haystack: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let needle = self;
        Ok(match needle.rank().cmp(&haystack.rank()) {
            Ordering::Equal => {
                let mut cache = HashMap::new();
                let mut result_data = eco_vec![0.0; needle.row_count()];
                let slice = result_data.make_mut();
                let default = haystack.row_count();
                if needle.rank() == 1 {
                    for (dest, elem) in slice.iter_mut().zip(&needle.data) {
                        let key = ArrayCmpSlice(slice::from_ref(elem));
                        *dest = if let Some(i) = cache.get_mut(&key) {
                            *i += 1;
                            *i = (haystack.data[*i..].iter())
                                .position(|of| of.array_eq(elem))
                                .map(|idx| idx + *i)
                                .unwrap_or(default);
                            *i
                        } else {
                            let i = (haystack.data.iter())
                                .position(|of| of.array_eq(elem))
                                .unwrap_or(default);
                            cache.insert(key, i);
                            i
                        } as f64;
                    }
                } else {
                    let haystack_row_len = haystack.row_len();
                    for (dest, row) in slice.iter_mut().zip(needle.row_slices()) {
                        let key = ArrayCmpSlice(row);
                        *dest = if let Some(i) = cache.get_mut(&key) {
                            *i += 1;
                            *i = haystack.data[*i * haystack_row_len..]
                                .chunks_exact(haystack_row_len)
                                .position(|of| ArrayCmpSlice(of) == key)
                                .map(|idx| idx + *i)
                                .unwrap_or(default);
                            *i
                        } else {
                            let i = haystack
                                .data
                                .chunks_exact(haystack_row_len)
                                .position(|of| ArrayCmpSlice(of) == key)
                                .unwrap_or(default);
                            cache.insert(key, i);
                            i
                        } as f64;
                    }
                }
                Array::new(
                    needle.shape.iter().take(1).copied().collect::<Shape>(),
                    result_data,
                )
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(needle.row_count());
                for elem in needle.rows() {
                    rows.push(elem.progressive_index_of(haystack, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if haystack.rank() - needle.rank() == 1 {
                    if needle.rank() == 0 {
                        let needle = &needle.data[0];
                        Array::from(
                            (haystack.data.iter())
                                .position(|of| needle.array_eq(of))
                                .unwrap_or(haystack.row_count()) as f64,
                        )
                    } else {
                        ((haystack.rows().position(|r| r == *needle))
                            .unwrap_or(haystack.row_count()) as f64)
                            .into()
                    }
                } else {
                    let mut rows = Vec::with_capacity(haystack.row_count());
                    for of in haystack.rows() {
                        rows.push(needle.progressive_index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
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
                    ArrayRef::new(&a_row_shape, a_row),
                    ArrayRef::new(&b_row_shape, b_row),
                    &mut prod_row,
                    env,
                    InfalliblePervasiveFn::new(pervade::mul::num_num),
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
        let iter = (a.row_slices()).zip(result_slice.chunks_exact_mut(b.row_count() * prod_elems));
        if a.row_count() > 100 || b.row_count() > 100 {
            (iter.par_bridge()).for_each(|(a_row, res_row)| inner(a_row, res_row));
        } else {
            iter.for_each(|(a_row, res_row)| inner(a_row, res_row));
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
            if u >= target.rank() {
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
        target.orient_impl(&undices);
        Ok(())
    }
    fn orient_impl(&mut self, undices: &[usize]) {
        let mut orientation: Vec<usize> = (0..self.rank()).collect();
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
        for (depth, amnt) in depth_rotations {
            self.transpose_depth(depth, amnt);
        }
    }
    pub(crate) fn undo_orient(&self, target: &mut Self, env: &Uiua) -> UiuaResult {
        let indices = self.as_ints(env, "Unorient indices must be integers")?;
        let mut undices = Vec::with_capacity(indices.len());
        for i in indices {
            let u = i.unsigned_abs();
            if u >= target.rank() {
                return Err(env.error(format!(
                    "Cannot unorient axis {i} in array of rank {}",
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
                "Cannot unorient array of rank {} with {} indices",
                target.rank(),
                undices.len()
            )));
        }
        for i in 0..target.rank() {
            if !undices.contains(&i) {
                undices.push(i);
            }
        }
        let mut inverted_undices = undices.clone();
        for (i, u) in undices.into_iter().enumerate() {
            inverted_undices[u] = i;
        }
        target.orient_impl(&inverted_undices);
        Ok(())
    }
}

impl Value {
    /// `choose` all combinations of `k` rows from a value
    pub fn choose(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let k = self.as_nat(env, "Choose k must be an integer")?;
        from.generic_ref(
            |a| a.choose(k, env).map(Into::into),
            |a| a.choose(k, env).map(Into::into),
            |a| a.choose(k, env).map(Into::into),
            |a| a.choose(k, env).map(Into::into),
            |a| a.choose(k, env).map(Into::into),
        )
    }
    /// `permute` all combinations of `k` rows from a value
    pub fn permute(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let k = self.as_nat(env, "Permute k must be an integer")?;
        from.generic_ref(
            |a| a.permute(k, env).map(Into::into),
            |a| a.permute(k, env).map(Into::into),
            |a| a.permute(k, env).map(Into::into),
            |a| a.permute(k, env).map(Into::into),
            |a| a.permute(k, env).map(Into::into),
        )
    }
}

fn factorial(n: usize) -> f64 {
    if n <= 1 {
        return 1.0;
    }
    (1..=n).map(|i| i as f64).product()
}

impl<T: ArrayValue> Array<T> {
    /// `choose` all combinations of `k` rows from this array
    pub fn choose(&self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot choose from scalar array"));
        }
        let n = self.row_count();
        if k > n {
            return Err(env.error(format!(
                "Cannot choose combinations of {k} rows \
                from array of shape {}",
                self.shape()
            )));
        }
        let mut shape = self.shape.clone();
        let combinations = factorial(n) / (factorial(k) * factorial(n - k));
        if combinations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if combinations > usize::MAX as f64 {
            return Err(env.error(format!("{combinations} combinations would be too many")));
        }
        shape[0] = combinations as usize;
        shape.insert(1, k);
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let mut data = EcoVec::with_capacity(elem_count);
        let row_len = self.row_len();
        let mut indices = vec![0; k];
        'outer: loop {
            env.respect_execution_limit()?;
            if (indices.iter())
                .zip(indices.iter().skip(1))
                .all(|(a, b)| a > b)
            {
                for &i in indices.iter().rev() {
                    data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                }
            }
            // Increment indices
            for i in 0..k {
                indices[i] += 1;
                if indices[i] == n {
                    indices[i] = 0;
                } else {
                    continue 'outer;
                }
            }
            break;
        }
        Ok(Array::new(shape, data))
    }
    /// `permute` all combinations of `k` rows from this array
    pub fn permute(&self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot permute scalar array"));
        }
        let n = self.row_count();
        if k > n {
            return Err(env.error(format!(
                "Cannot get permutations of {k} rows \
                from array of shape {}",
                self.shape()
            )));
        }
        let mut shape = self.shape.clone();
        let permutations = factorial(n) / factorial(n - k);
        if permutations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if permutations > usize::MAX as f64 {
            return Err(env.error(format!("{permutations} permutations would be too many")));
        }
        shape[0] = permutations as usize;
        shape.insert(1, k);
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let mut data = EcoVec::with_capacity(elem_count);
        let row_len = self.row_len();
        let mut indices = vec![0; k];
        let mut set = HashSet::with_capacity(k);
        'outer: loop {
            env.respect_execution_limit()?;
            set.clear();
            if indices.iter().all(|&i| set.insert(i)) {
                for &i in indices.iter().rev() {
                    data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                }
            }
            // Increment indices
            for i in 0..k {
                indices[i] += 1;
                if indices[i] == n {
                    indices[i] = 0;
                } else {
                    continue 'outer;
                }
            }
            break;
        }
        Ok(Array::new(shape, data))
    }
}
