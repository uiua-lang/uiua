//! Code for couple, join, and general array creation

use std::{cmp::Ordering, iter::once};

use ecow::EcoVec;

use crate::{
    algorithm::{
        max_shape, validate_size_impl, validate_size_of, ArrayCmpSlice, FillContext, Indexable,
    },
    cowslice::cowslice,
    fill::FillValue,
    val_as_arr, Array, ArrayValue, Boxed, Complex, FormatShape, Primitive, Shape, Uiua, UiuaResult,
    Value,
};

fn data_index_to_shape_index(mut index: usize, shape: &[usize], out: &mut [usize]) -> bool {
    debug_assert_eq!(shape.len(), out.len());
    if index >= shape.iter().product::<usize>() {
        return false;
    }
    for (&s, o) in shape.iter().zip(out).rev() {
        *o = index % s;
        index /= s;
    }
    true
}

#[test]
fn data_index_to_shape_index_test() {
    let mut out = [0, 0];
    for (index, shape, expected_out, expected_success) in [
        (2, [2, 3], [0, 2], true),
        (2, [1, 3], [0, 2], true),
        (3, [2, 3], [1, 0], true),
        (3, [1, 3], [1, 0], false),
    ] {
        let success = data_index_to_shape_index(index, &shape, &mut out);
        assert_eq!(out, expected_out);
        assert_eq!(success, expected_success);
    }
}

fn shape_index_to_data_index(index: &[usize], shape: &[usize]) -> Option<usize> {
    debug_assert_eq!(shape.len(), index.len());
    let mut data_index = 0;
    for (&s, &i) in shape.iter().zip(index) {
        if i >= s {
            return None;
        }
        data_index = data_index * s + i;
    }
    Some(data_index)
}

#[test]
fn shape_index_to_data_index_test() {
    for (index, shape, expected_data_index) in [
        ([0, 2], [2, 3], Some(2)),
        ([0, 2], [1, 3], Some(2)),
        ([1, 0], [2, 3], Some(3)),
        ([1, 0], [1, 3], None),
    ] {
        let data_index = shape_index_to_data_index(&index, &shape);
        assert_eq!(data_index, expected_data_index);
    }
}

impl<T: ArrayValue> Array<T> {
    /// Fill the array with the given value so it matches the given shape
    pub fn fill_to_shape(&mut self, shape: &[usize], fill: FillValue<T>) {
        while self.rank() < shape.len() {
            self.shape.prepend(1);
        }
        if self.shape == shape {
            return;
        }
        let target_size = shape.iter().product();
        let is_left = fill.is_left();
        let mut new_data = cowslice![fill.value; target_size];
        let new_slice = new_data.as_mut_slice();
        let mut curr = vec![0; shape.len()];
        for new_data_index in 0..target_size {
            data_index_to_shape_index(new_data_index, shape, &mut curr);
            if let Some(data_index) = shape_index_to_data_index(&curr, &self.shape) {
                new_slice[new_data_index] = self.data[data_index].clone();
            }
        }
        if is_left && self.rank() == shape.len() {
            let offset = shape
                .first()
                .copied()
                .unwrap_or(1)
                .saturating_sub(self.row_count());
            let row_len: usize = shape.iter().skip(1).product();
            new_slice.rotate_right(offset * row_len);
        }
        self.data = new_data;
        self.shape = shape.into();
        self.meta.take_sorted_flags();
        self.validate();
    }
}

impl Value {
    /// `join` the array with another
    ///
    /// `allow_ext` allows extending one of the arrays if they have different shapes
    pub fn join(self, other: Self, allow_ext: bool, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, allow_ext, env)
    }
    /// `join` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    #[must_use]
    pub fn join_infallible(self, other: Self, allow_ext: bool) -> Self {
        self.join_impl(other, allow_ext, &()).unwrap()
    }
    fn join_impl<C: FillContext>(
        mut self,
        mut other: Self,
        ext: bool,
        ctx: &C,
    ) -> Result<Self, C::Error> {
        self.match_fill(ctx);
        other.match_fill(ctx);
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.join_impl(b, ext, ctx)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.join_impl(b, ext, ctx)?.into(),
            (Value::Complex(a), Value::Complex(b)) => a.join_impl(b, ext, ctx)?.into(),
            (Value::Char(a), Value::Char(b)) => a.join_impl(b, ext, ctx)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().join_impl(b, ext, ctx)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.join_impl(b.convert(), ext, ctx)?.into(),
            (Value::Complex(a), Value::Num(b)) => a.join_impl(b.convert(), ext, ctx)?.into(),
            (Value::Num(a), Value::Complex(b)) => a.convert().join_impl(b, ext, ctx)?.into(),
            (Value::Complex(a), Value::Byte(b)) => a.join_impl(b.convert(), ext, ctx)?.into(),
            (Value::Byte(a), Value::Complex(b)) => a.convert().join_impl(b, ext, ctx)?.into(),
            (a, b) => a.bin_coerce_to_boxes(
                b,
                ctx,
                |a, b, env| Ok(a.join_impl(b, ext, env)?.into()),
                |a, b| format!("Cannot join {a} array and {b} array"),
            )?,
        })
    }
    pub(crate) fn append<C: FillContext>(
        &mut self,
        mut other: Self,
        ext: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        self.match_fill(ctx);
        other.match_fill(ctx);
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.append(b, ext, ctx)?,
            (Value::Byte(a), Value::Byte(b)) => a.append(b, ext, ctx)?,
            (Value::Complex(a), Value::Complex(b)) => a.append(b, ext, ctx)?,
            (Value::Char(a), Value::Char(b)) => a.append(b, ext, ctx)?,
            (Value::Byte(a), Value::Num(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ext, ctx)?;
                *self = a.into();
            }
            (Value::Num(a), Value::Byte(b)) => a.append(b.convert(), ext, ctx)?,
            (Value::Complex(a), Value::Num(b)) => a.append(b.convert(), ext, ctx)?,
            (Value::Num(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ext, ctx)?;
                *self = a.into();
            }
            (Value::Complex(a), Value::Byte(b)) => a.append(b.convert(), ext, ctx)?,
            (Value::Byte(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ext, ctx)?;
                *self = a.into();
            }
            (a, b) => a.bin_coerce_to_boxes_mut(
                b,
                ctx,
                |a, b, env| a.append(b, ext, env),
                |a, b| format!("Cannot add {b} row to {a} array"),
            )?,
        }
        Ok(())
    }
    pub(crate) fn undo_join(
        self,
        a_shape: Self,
        b_shape: Self,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        let a_shape = a_shape.as_nats(env, "Shape must be a list of natural numbers")?;
        let b_shape = b_shape.as_nats(env, "Shape must be a list of natural numbers")?;
        val_as_arr!(self, |a| a
            .undo_join(&a_shape, &b_shape, env)
            .map(|(a, b)| (a.into(), b.into())))
    }
    pub(crate) fn unjoin(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.unjoin_depth(0, false, env)
    }
    pub(crate) fn unjoin_end(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.unjoin_depth(0, true, env)
    }
    pub(crate) fn unjoin_depth(
        self,
        depth: usize,
        end: bool,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        val_as_arr!(self, |arr| arr
            .unjoin_depth(depth, end, env)
            .map(|(a, b)| (a.into(), b.into())))
    }
    pub(crate) fn unjoin_shape(
        self,
        a_shape: &[usize],
        b_shape: Option<&[usize]>,
        end: bool,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        val_as_arr!(self, |arr| arr
            .unjoin_shape(a_shape, b_shape, end, env)
            .map(|(a, b)| (a.into(), b.into())))
    }
}

impl<T: ArrayValue> Array<T> {
    /// `join` the array with another
    pub fn join(self, other: Self, allow_ext: bool, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, allow_ext, env)
    }
    /// `join` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    #[must_use]
    pub fn join_infallible(self, other: Self, allow_ext: bool) -> Self {
        self.join_impl(other, allow_ext, &()).unwrap()
    }
    fn join_impl<C: FillContext>(
        mut self,
        mut other: Self,
        allow_ext: bool,
        ctx: &C,
    ) -> Result<Self, C::Error> {
        crate::profile_function!();
        let mut sorted_up = false;
        let mut sorted_down = false;
        let mut res = match self.rank().cmp(&other.rank()) {
            Ordering::Less => {
                if self.shape == [0] {
                    return Ok(other);
                }
                let map_keys = (self.rank() == 0)
                    .then(|| self.meta.take_map_keys().zip(other.meta.take_map_keys()))
                    .flatten();
                if other.shape.row_count() == 0 {
                    validate_size_of::<T>(once(1).chain(other.shape[1..].iter().copied()))
                        .map_err(|e| ctx.error(e))?;
                }
                other.meta.combine(&self.meta);
                let target_shape = match ctx.scalar_fill::<T>() {
                    Ok(fill) => {
                        let target_shape = max_shape(&self.shape, &other.shape);
                        let row_shape = &target_shape[1..];
                        self.fill_to_shape(row_shape, fill.clone());
                        other.fill_to_shape(&target_shape, fill);
                        target_shape
                    }
                    Err(e) => {
                        if allow_ext && other.shape.ends_with(&self.shape) {
                            for &b_dim in other.shape[1..other.rank() - self.rank()].iter().rev() {
                                self.reshape_scalar_integer(b_dim, None)
                                    .map_err(|e| ctx.error(e))?;
                            }
                        } else {
                            if other.rank() - self.rank() > 1 {
                                return Err(C::fill_error(ctx.error(format!(
                                    "Cannot join rank {} array with rank {} array{e}",
                                    self.rank(),
                                    other.rank()
                                ))));
                            }
                            if self.shape != other.shape[1..] {
                                return Err(C::fill_error(ctx.error(format!(
                                    "Cannot join arrays of shapes {} and {}{e}",
                                    self.shape, other.shape
                                ))));
                            }
                            match other.row_count() {
                                0 => {
                                    sorted_up = true;
                                    sorted_down = true;
                                }
                                n => {
                                    if other.meta.is_sorted_up() || other.meta.is_sorted_down() {
                                        let row_len = other.row_len();
                                        let b_slice = &other.data[(n - 1) * row_len..];
                                        let ordering =
                                            ArrayCmpSlice(&self.data).cmp(&ArrayCmpSlice(b_slice));
                                        sorted_up = other.meta.is_sorted_up()
                                            && ordering != Ordering::Greater;
                                        sorted_down = other.meta.is_sorted_down()
                                            && ordering != Ordering::Less;
                                    }
                                }
                            }
                        }
                        other.shape
                    }
                };
                let rot_len = self.data.len();
                other.data.extend_from_cowslice(self.data);
                other.data.as_mut_slice().rotate_right(rot_len);
                other.shape = target_shape;
                other.shape[0] += 1;
                // Combine map keys
                if let Some((mut a, b)) = map_keys {
                    let mut to_remove = a.join(b, ctx)?;
                    to_remove.sort_unstable();
                    for i in to_remove.into_iter().rev() {
                        other.remove_row(i);
                    }
                    other.meta.map_keys = Some(a);
                }
                other
            }
            Ordering::Greater => {
                if other.shape == 0 {
                    return Ok(self);
                }
                self.append(other, allow_ext, ctx)?;
                return Ok(self);
            }
            Ordering::Equal => {
                let map_keys = self.meta.take_map_keys().zip(other.meta.take_map_keys());
                let mut res = if self.rank() == 0 {
                    debug_assert_eq!(other.rank(), 0);
                    let label = match (self.meta.take_label(), other.meta.take_label()) {
                        (Some(label), None) | (None, Some(label)) => Some(label),
                        (Some(a), Some(b)) if a == b => Some(a),
                        _ => None,
                    };
                    let ordering = self.data[0].array_cmp(&other.data[0]);
                    sorted_up = ordering != Ordering::Greater;
                    sorted_down = ordering != Ordering::Less;
                    self.meta.set_label(label);
                    self.data.extend(other.data.into_iter().next());
                    self.shape = 2.into();
                    self
                } else {
                    if self.data.len().saturating_add(other.data.len()) >= u32::MAX as usize {
                        Err(ctx.error(super::SizeError {
                            elements: self.data.len() as f64 + other.data.len() as f64,
                            elem_size: size_of::<T>(),
                        }))?
                    }

                    if self.shape[1..] == other.shape[1..] {
                        // Set sorted flags
                        match (self.row_count(), other.row_count()) {
                            (0, 0) => {
                                sorted_up = true;
                                sorted_down = true;
                            }
                            (_, 0) => {
                                sorted_up = self.meta.is_sorted_up();
                                sorted_down = self.meta.is_sorted_down();
                            }
                            (0, _) => {
                                sorted_up = other.meta.is_sorted_up();
                                sorted_down = other.meta.is_sorted_down();
                            }
                            (a, _b) => {
                                let both_sorted_up =
                                    self.meta.is_sorted_up() && other.meta.is_sorted_up();
                                let both_sorted_down =
                                    self.meta.is_sorted_down() && other.meta.is_sorted_down();
                                if both_sorted_up || both_sorted_down {
                                    let row_len = self.row_len();
                                    let a_slice = &self.data[(a - 1) * row_len..];
                                    let b_slice = &other.data[0..row_len];
                                    let ordering =
                                        ArrayCmpSlice(a_slice).cmp(&ArrayCmpSlice(b_slice));
                                    sorted_up = both_sorted_up && ordering != Ordering::Greater;
                                    sorted_down = both_sorted_down && ordering != Ordering::Less;
                                }
                            }
                        }
                    } else {
                        // Fill
                        match ctx.scalar_fill::<T>() {
                            Ok(fill) => {
                                if map_keys.is_some() {
                                    return Err(ctx.error(format!(
                                        "Cannot {} {} map arrays",
                                        Primitive::Fill,
                                        Primitive::Join
                                    )));
                                }
                                let new_row_shape = max_shape(&self.shape[1..], &other.shape[1..]);
                                for (array, fill) in [(&mut self, fill.clone()), (&mut other, fill)]
                                {
                                    let mut new_shape = new_row_shape.clone();
                                    new_shape.prepend(array.shape[0]);
                                    array.fill_to_shape(&new_shape, fill);
                                }
                            }
                            Err(e) => {
                                return Err(C::fill_error(ctx.error(format!(
                                    "Cannot join arrays of shapes {} and {}. {e}",
                                    self.shape, other.shape
                                ))));
                            }
                        }
                    }

                    if self.data.len() >= other.data.len() {
                        if self.meta.label.is_none() {
                            if let Some(label) = other.meta.take_label() {
                                self.meta.label = Some(label);
                            }
                        }
                        self.data.extend_from_cowslice(other.data);
                        self.shape[0] += other.shape[0];
                    } else {
                        if other.meta.label.is_none() {
                            if let Some(label) = self.meta.take_label() {
                                other.meta.label = Some(label);
                            }
                        }
                        let rot_len = self.data.len();
                        other.data.extend_from_cowslice(self.data);
                        other.data.as_mut_slice().rotate_right(rot_len);
                        other.shape[0] += self.shape[0];
                        self = other;
                    }
                    self
                };
                // Combine map keys
                if let Some((mut a, b)) = map_keys {
                    let mut to_remove = a.join(b, ctx)?;
                    to_remove.sort_unstable();
                    for i in to_remove.into_iter().rev() {
                        res.remove_row(i);
                    }
                    res.meta.map_keys = Some(a);
                }
                res
            }
        };
        res.meta.mark_sorted_up(sorted_up);
        res.meta.mark_sorted_down(sorted_down);
        res.validate();
        Ok(res)
    }
    fn append<C: FillContext>(
        &mut self,
        mut other: Self,
        allow_ext: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        let mut sorted_up = false;
        let mut sorted_down = false;
        if self.shape.row_count() == 0 {
            validate_size_of::<T>(once(1).chain(self.shape[1..].iter().copied()))
                .map_err(|e| ctx.error(e))?;
        }
        let map_keys = (other.rank() == 0)
            .then(|| self.meta.take_map_keys().zip(other.meta.take_map_keys()))
            .flatten();
        self.meta.combine(&other.meta);
        if self.shape[1..] == other.shape {
            match self.row_count() {
                0 => {
                    sorted_up = true;
                    sorted_down = true;
                }
                n => {
                    if self.meta.is_sorted_up() || self.meta.is_sorted_down() {
                        let row_len = self.row_len();
                        let a_slice = &self.data[(n - 1) * row_len..];
                        let ordering = ArrayCmpSlice(a_slice).cmp(&ArrayCmpSlice(&other.data));
                        sorted_up = self.meta.is_sorted_up() && ordering != Ordering::Greater;
                        sorted_down = self.meta.is_sorted_down() && ordering != Ordering::Less;
                    }
                }
            }
            self.data.extend_from_cowslice(other.data);
        } else if allow_ext && self.shape[1..].ends_with(&other.shape) {
            if !other.shape.contains(&0) {
                let reps = self.row_len() / other.element_count();
                self.data.extend_repeat_slice(&other.data, reps);
            }
        } else {
            match ctx.scalar_fill::<T>() {
                Ok(fill) => {
                    while self.rank() <= other.rank() {
                        self.shape.push(1);
                    }
                    let target_shape = max_shape(&self.shape, &other.shape);
                    let row_shape = &target_shape[1..];
                    self.fill_to_shape(&target_shape, fill.clone());
                    other.fill_to_shape(row_shape, fill);
                    self.data.extend_from_cowslice(other.data);
                    self.shape = target_shape;
                }
                Err(e) => {
                    if self.rank() <= other.rank() || self.rank() - other.rank() > 1 {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot join rank {} array with rank {} array{e}",
                            self.rank(),
                            other.rank()
                        ))));
                    }
                    if self.shape[1..] != other.shape {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot join arrays of shapes {} and {}{e}",
                            self.shape, other.shape
                        ))));
                    }
                }
            }
        }
        self.shape[0] += 1;
        // Combine map keys
        if let Some((mut a, b)) = map_keys {
            let mut to_remove = a.join(b, ctx)?;
            to_remove.sort_unstable();
            for i in to_remove.into_iter().rev() {
                self.remove_row(i);
            }
            self.meta.map_keys = Some(a);
        }
        self.meta.mark_sorted_up(sorted_up);
        self.meta.mark_sorted_down(sorted_down);
        self.validate();
        Ok(())
    }
    pub(crate) fn undo_join(
        mut self,
        ash: &[usize],
        bsh: &[usize],
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        if self.rank() == 0 {
            return Err(env.error("Cannot unjoin scalar"));
        }
        if ash.is_empty() && bsh.is_empty() {
            if self.row_count() != 2 {
                return Err(env.error(format!(
                    "Attempted to undo join, but the \
                    array's row count changed from 2 to {}",
                    self.row_count()
                )));
            }
            return self.uncouple(env);
        }
        if self.meta.map_keys.is_some() {
            return Err(env.error("Cannot undo join of map arrays"));
        }
        match ash.len().cmp(&bsh.len()) {
            Ordering::Equal => {
                if self.row_count() != ash[0] + bsh[0] {
                    return Err(env.error(format!(
                        "Attempted to undo join, but the \
                        array's row count changed from {} to {}",
                        ash[0] + bsh[0],
                        self.row_count()
                    )));
                }
                let mut b_shape = self.shape.clone();
                b_shape[0] = bsh[0];
                let b_data = self.data.slice((ash[0] * self.row_len())..);
                self.shape[0] = ash[0];
                self.data = self.data.slice(..(ash[0] * self.row_len()));
                self.validate();
                let b = Array::new(b_shape, b_data);
                Ok((self, b))
            }
            Ordering::Less => {
                if self.row_count() == 0 {
                    return Ok((self.clone(), self));
                }
                let left_shape = &self.shape[1..];
                let left_data = self.data.slice(..self.row_len());
                let left = Array::new(left_shape, left_data);
                let right = self.drop(&[Ok(1)], env)?;
                Ok((left, right))
            }
            Ordering::Greater => {
                if self.row_count() == 0 {
                    return Ok((self.clone(), self));
                }
                let right_shape = &self.shape[1..];
                let right_data = self.data.slice((self.row_count() - 1) * self.row_len()..);
                let right = Array::new(right_shape, right_data);
                let left = self.drop(&[Ok(-1)], env)?;
                Ok((left, right))
            }
        }
    }
    pub(crate) fn unjoin_depth(
        mut self,
        mut depth: usize,
        is_end: bool,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        depth = depth.min(self.rank());
        if depth == self.rank() {
            return Err(env.error("Cannot unjoin scalar"));
        }
        let rows_at_depth = self.shape[depth];
        if rows_at_depth == 0 {
            return Err(env.error("Cannot unjoin an empty array"));
        }
        Ok(if depth == 0 {
            let first;
            if is_end {
                let endex = self.row_count() - 1;
                first = self.row(endex);
                self.data = self.data.slice(..endex * self.row_len());
            } else {
                first = self.row(0);
                self.data = self.data.slice(self.row_len()..);
            }
            self.shape[0] -= 1;
            self.validate();
            (first, self)
        } else {
            let n: usize = self.shape[..depth].iter().product();
            let row_len: usize = self.shape[depth..].iter().product();
            let stride: usize = self.shape[depth..].iter().skip(1).product();
            let mut remaining_shape = self.shape.clone();
            let mut removed_shape = self.shape.clone();
            remaining_shape[depth] -= 1;
            removed_shape.remove(depth);
            let removed_data: EcoVec<T> = if is_end {
                (0..n)
                    .flat_map(|i| {
                        let end = (i + 1) * row_len;
                        let start = end - stride;
                        self.data.slice(start..end)
                    })
                    .collect()
            } else {
                (0..n)
                    .flat_map(|i| {
                        let start = i * row_len;
                        let end = start + stride;
                        self.data.slice(start..end)
                    })
                    .collect()
            };
            let remaining_data = if is_end {
                (0..n)
                    .flat_map(|i| {
                        let start = i * row_len;
                        let end = start + row_len - stride;
                        self.data.slice(start..end)
                    })
                    .collect()
            } else {
                (0..n)
                    .flat_map(|i| {
                        let start = i * row_len + stride;
                        let end = start + row_len - stride;
                        self.data.slice(start..end)
                    })
                    .collect()
            };
            let removed = Array::new(removed_shape, removed_data);
            self.data = remaining_data;
            self.shape = remaining_shape;
            self.validate();
            (removed, self)
        })
    }
    pub(crate) fn unjoin_shape(
        mut self,
        a_shape: &[usize],
        b_shape: Option<&[usize]>,
        is_end: bool,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        // dbg!(
        //     &self,
        //     FormatShape(a_shape),
        //     b_shape.map(FormatShape),
        //     is_end
        // );
        if self.rank() == 0 {
            return Err(env.error("Cannot unjoin a scalar"));
        }
        match a_shape {
            [] => {
                if self.row_count() == 0 {
                    return Err(env.error("Cannot unjoin an empty array"));
                }
            }
            [len, rest @ ..] => {
                if self.row_count() < *len {
                    return Err(env.error(format!(
                        "Cannot unjoin {len} rows from an array with {} rows",
                        self.row_count()
                    )));
                }
                if !self.shape[1..].iter().zip(rest).all(|(&a, &b)| a == b) {
                    return Err(env.error(format!(
                        "Cannot unjoin array with shape {} from array with shape {}",
                        FormatShape(a_shape),
                        self.shape
                    )));
                }
            }
        }

        let row_len = self.row_len();

        let was_row_join = if let Some(b_shape) = b_shape {
            b_shape.len() > a_shape.len()
        } else {
            self.rank() > a_shape.len()
        };
        let unjoin_count = if was_row_join {
            1
        } else {
            a_shape.first().copied().unwrap_or(1)
        };
        let unjoined_slice;
        if is_end {
            let split_pos = self.element_count() - unjoin_count * row_len;
            unjoined_slice = self.data.slice(split_pos..);
            self.data = self.data.slice(..split_pos);
        } else {
            let split_pos = unjoin_count * row_len;
            unjoined_slice = self.data.slice(..split_pos);
            self.data = self.data.slice(split_pos..);
        }
        let mut unjoined_shape = self.shape.clone();
        if was_row_join {
            unjoined_shape.make_row();
        } else {
            unjoined_shape[0] = unjoin_count;
        }
        self.shape[0] -= unjoin_count;
        self.validate();
        let mut unjoined = Array::new(unjoined_shape, unjoined_slice);
        if let Some(keys) = self.meta.map_keys_mut() {
            if !a_shape.is_empty() {
                let mut unjoined_keys = keys.clone();
                unjoined_keys.take(unjoin_count);
                unjoined.meta.map_keys = Some(unjoined_keys);
            }
            keys.drop(unjoin_count);
        }
        Ok((unjoined, self))
    }
}

impl Value {
    /// `couple` the value with another
    pub fn couple(mut self, other: Self, allow_ext: bool, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, allow_ext, env)?;
        Ok(self)
    }
    /// `couple` the value with another
    ///
    /// # Panics
    /// Panics if the values have incompatible shapes
    #[must_use]
    pub fn couple_infallible(mut self, other: Self, allow_ext: bool) -> Self {
        self.couple_impl(other, allow_ext, &()).unwrap();
        self
    }
    pub(crate) fn couple_impl<C: FillContext>(
        &mut self,
        mut other: Self,
        allow_ext: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        self.match_fill(ctx);
        other.match_fill(ctx);
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.couple_impl(b, allow_ext, ctx)?,
            (Value::Byte(a), Value::Byte(b)) => a.couple_impl(b, allow_ext, ctx)?,
            (Value::Complex(a), Value::Complex(b)) => a.couple_impl(b, allow_ext, ctx)?,
            (Value::Char(a), Value::Char(b)) => a.couple_impl(b, allow_ext, ctx)?,
            (Value::Box(a), Value::Box(b)) => a.couple_impl(b, allow_ext, ctx)?,
            (Value::Num(a), Value::Byte(b)) => a.couple_impl(b.convert(), allow_ext, ctx)?,
            (Value::Byte(a), Value::Num(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, allow_ext, ctx)?;
                *self = a.into();
            }
            (Value::Complex(a), Value::Num(b)) => a.couple_impl(b.convert(), allow_ext, ctx)?,
            (Value::Num(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, allow_ext, ctx)?;
                *self = a.into();
            }
            (Value::Complex(a), Value::Byte(b)) => a.couple_impl(b.convert(), allow_ext, ctx)?,
            (Value::Byte(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, allow_ext, ctx)?;
                *self = a.into();
            }
            (a, b) => a.bin_coerce_to_boxes_mut(
                b,
                ctx,
                |a, b, ctx| a.couple_impl(b, allow_ext, ctx),
                |a, b| format!("Cannot couple {a} array with {b} array"),
            )?,
        }
        Ok(())
    }
    /// Uncouple the value into two values
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.uncouple_depth(0, env)
    }
    pub(crate) fn uncouple_depth(self, depth: usize, env: &Uiua) -> UiuaResult<(Self, Self)> {
        val_as_arr!(self, |a| a
            .uncouple_depth(depth, env)
            .map(|(a, b)| (a.into(), b.into())))
    }
}

impl<T: ArrayValue> Array<T> {
    /// `couple` the array with another
    pub fn couple(mut self, other: Self, allow_ext: bool, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, allow_ext, env)?;
        Ok(self)
    }
    /// `couple` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    #[must_use]
    pub fn couple_infallible(mut self, other: Self, allow_ext: bool) -> Self {
        self.couple_impl(other, allow_ext, &()).unwrap();
        self
    }
    fn couple_impl<C: FillContext>(
        &mut self,
        mut other: Self,
        allow_ext: bool,
        ctx: &C,
    ) -> Result<(), C::Error> {
        crate::profile_function!();
        self.meta.combine(&other.meta);
        match (self.meta.take_label(), other.meta.take_label()) {
            (Some(a), Some(b)) => {
                self.meta.label = Some(if self.rank() >= other.rank() { a } else { b })
            }
            (Some(a), None) => self.meta.label = Some(a),
            (None, Some(b)) => self.meta.label = Some(b),
            (None, None) => {}
        }
        if self.shape != other.shape {
            match ctx.scalar_fill::<T>() {
                Ok(fill) => {
                    let new_shape = max_shape(&self.shape, &other.shape);
                    self.fill_to_shape(&new_shape, fill.clone());
                    other.fill_to_shape(&new_shape, fill);
                }
                Err(e) => {
                    let err = || {
                        Err(C::fill_error(ctx.error(format!(
                            "Cannot couple arrays with shapes {} and {}{e}",
                            self.shape, other.shape
                        ))))
                    };
                    if allow_ext {
                        if self.shape.ends_with(&other.shape) {
                            for &a_dim in self.shape[0..self.rank() - other.rank()].iter().rev() {
                                other
                                    .reshape_scalar_integer(a_dim, None)
                                    .map_err(|e| ctx.error(e))?;
                            }
                        } else if other.shape.ends_with(&self.shape) {
                            for &b_dim in other.shape[0..other.rank() - self.rank()].iter().rev() {
                                self.reshape_scalar_integer(b_dim, None)
                                    .map_err(|e| ctx.error(e))?;
                            }
                        } else {
                            return err();
                        }
                    } else {
                        return err();
                    }
                }
            }
            self.meta.take_sorted_flags();
            other.meta.take_sorted_flags();
        }
        let (sorted_up, sorted_down) = match (*self).cmp(&other) {
            Ordering::Equal => (false, false),
            Ordering::Less => (true, false),
            Ordering::Greater => (false, true),
        };
        self.data.extend_from_cowslice(other.data);
        self.shape.prepend(2);
        self.meta.mark_sorted_up(sorted_up);
        self.meta.mark_sorted_down(sorted_down);
        self.validate();
        Ok(())
    }
    /// Uncouple the array into two arrays
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.uncouple_depth(0, env)
    }
    pub(crate) fn uncouple_depth(
        mut self,
        mut depth: usize,
        env: &Uiua,
    ) -> UiuaResult<(Self, Self)> {
        depth = depth.min(self.rank());
        if depth == self.rank() {
            return Err(env.error("Cannot uncouple scalar"));
        }
        let rows_at_depth = self.shape[depth];
        if rows_at_depth != 2 {
            return Err(env.error(format!(
                "Cannot uncouple array with {} row{}",
                rows_at_depth,
                if rows_at_depth == 1 { "" } else { "s" }
            )));
        }
        Ok(if depth == 0 {
            let mut rows = self.into_rows();
            let first = rows.next().unwrap();
            let second = rows.next().unwrap();
            (first, second)
        } else {
            let stride: usize = self.shape[depth..].iter().skip(1).product();
            let mut shape = self.shape;
            shape.remove(depth);
            if stride == 0 {
                let arr = Array::new(shape.clone(), EcoVec::new());
                return Ok((arr.clone(), arr));
            }
            let mut new_b_data = EcoVec::with_capacity(shape.elements());
            let slice = self.data.as_mut_slice();
            let row_count = shape.iter().take(depth).product();
            for i in 0..row_count {
                let b_start = (i * 2 + 1) * stride;
                let b_end = b_start + stride;
                new_b_data.extend_from_slice(&slice[b_start..b_end]);
                if i < row_count - 1 {
                    let dest_start = (i + 1) * stride;
                    let src_start = (i + 1) * 2 * stride;
                    for j in 0..stride {
                        slice[dest_start + j] = slice[src_start + j].clone();
                    }
                }
            }
            self.data.truncate(shape.elements());
            let a = Array::new(shape.clone(), self.data);
            let b = Array::new(shape, new_b_data);
            (a, b)
        })
    }
}

impl Value {
    /// Create a value from row values
    ///
    /// # Panics
    /// Panics if the row values have incompatible shapes
    pub fn from_row_values_infallible<V>(values: V) -> Self
    where
        V: Indexable<Item = Value>,
    {
        Self::from_row_values(values, &()).unwrap()
    }
    /// Create a value from row values
    pub fn from_row_values<V, C>(values: V, ctx: &C) -> Result<Self, C::Error>
    where
        V: Indexable<Item = Value>,
        C: FillContext,
    {
        Self::from_row_values_impl(values, ctx, false)
    }
    /// Create a value from row values, allowing shape extension
    pub fn from_row_values_ext<V, C>(values: V, ctx: &C) -> Result<Self, C::Error>
    where
        V: Indexable<Item = Value>,
        C: FillContext,
    {
        Self::from_row_values_impl(values, ctx, true)
    }
    pub(crate) fn from_row_values_impl<V, C>(
        values: V,
        ctx: &C,
        allow_ext: bool,
    ) -> Result<Self, C::Error>
    where
        V: Indexable<Item = Value>,
        C: FillContext,
    {
        fn max_shape(a: Shape, b: &Shape) -> Shape {
            if a.starts_with(b) {
                return a;
            }
            let shape_len = a.len().max(b.len());
            let mut new_shape = Shape::with_capacity(shape_len);
            for _ in 0..shape_len {
                new_shape.push(0);
            }
            for i in 0..new_shape.len() {
                let j = new_shape.len() - i - 1;
                if a.len() > i {
                    new_shape[j] = a[a.len() - i - 1];
                }
                if b.len() > i {
                    new_shape[j] = new_shape[j].max(b[b.len() - i - 1]);
                }
            }
            new_shape
        }

        match values.len() {
            0 => return Ok(Value::default()),
            1 => {
                let mut val = values.into_iter().next().unwrap();
                val.fix();
                return Ok(val);
            }
            _ => {}
        }
        let to_reserve = values.len();
        let max_shape = values
            .iter()
            .fold(Shape::SCALAR, |a: Shape, b| max_shape(a, &b.shape));
        let mut row_values;
        let mut value = match &values[0] {
            Value::Num(_) => {
                let mut has_complex = false;
                let mut box_rank = None;
                for b in &values[1..] {
                    match b {
                        Value::Complex(_) => has_complex = true,
                        Value::Box(arr) => box_rank = box_rank.max(Some(arr.rank())),
                        Value::Char(_) => {
                            return Err(ctx.error("Cannot combine number and character arrays"))
                        }
                        _ => {}
                    }
                }
                row_values = values.into_iter();
                let Value::Num(arr) = row_values.next().unwrap() else {
                    unreachable!()
                };
                if let Some(box_rank) = box_rank {
                    Value::Box(arr.box_depth(box_rank))
                } else if has_complex {
                    Value::Complex(arr.convert())
                } else {
                    Value::Num(arr)
                }
            }
            Value::Byte(_) => {
                let mut has_num = false;
                let mut has_complex = false;
                let mut box_rank = None;
                for b in &values[1..] {
                    match b {
                        Value::Num(_) => has_num = true,
                        Value::Complex(_) => has_complex = true,
                        Value::Box(arr) => box_rank = box_rank.max(Some(arr.rank())),
                        Value::Char(_) => {
                            return Err(ctx.error("Cannot combine number and character arrays"))
                        }
                        Value::Byte(_) => {}
                    }
                }
                row_values = values.into_iter();
                let Value::Byte(arr) = row_values.next().unwrap() else {
                    unreachable!()
                };
                if let Some(box_rank) = box_rank {
                    Value::Box(arr.box_depth(box_rank))
                } else if has_complex {
                    Value::Complex(arr.convert())
                } else if has_num {
                    Value::Num(arr.convert())
                } else {
                    Value::Byte(arr)
                }
            }
            Value::Complex(_) => {
                let mut box_rank = None;
                for b in &values[1..] {
                    match b {
                        Value::Box(arr) => box_rank = box_rank.max(Some(arr.rank())),
                        Value::Char(_) => {
                            return Err(ctx.error("Cannot combine complex and character arrays"))
                        }
                        _ => {}
                    }
                }
                row_values = values.into_iter();
                let Value::Complex(arr) = row_values.next().unwrap() else {
                    unreachable!()
                };
                if let Some(box_rank) = box_rank {
                    Value::Box(arr.box_depth(box_rank))
                } else {
                    Value::Complex(arr)
                }
            }
            Value::Char(_) => {
                let mut box_rank = None;
                for b in &values[1..] {
                    match b {
                        Value::Box(arr) => box_rank = box_rank.max(Some(arr.rank())),
                        Value::Num(_) | Value::Byte(_) => {
                            return Err(ctx.error("Cannot combine character and number arrays"))
                        }
                        Value::Complex(_) => {
                            return Err(ctx.error("Cannot combine character and complex arrays"))
                        }
                        Value::Char(_) => {}
                    }
                }
                row_values = values.into_iter();
                let Value::Char(arr) = row_values.next().unwrap() else {
                    unreachable!()
                };
                if let Some(box_rank) = box_rank {
                    Value::Box(arr.box_depth(box_rank))
                } else {
                    Value::Char(arr)
                }
            }
            Value::Box(_) => {
                row_values = values.into_iter();
                row_values.next().unwrap()
            }
        };

        // Fill value
        value.match_fill(ctx);
        if !allow_ext && value.shape != max_shape || allow_ext && !max_shape.ends_with(&value.shape)
        {
            match &mut value {
                Value::Num(arr) => match ctx.scalar_fill::<f64>() {
                    Ok(fill) => arr.fill_to_shape(&max_shape, fill),
                    Err(e) => {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot combine arrays with shapes {} and {max_shape}{e}",
                            arr.shape
                        ))))
                    }
                },
                Value::Byte(arr) => match ctx.scalar_fill::<u8>() {
                    Ok(fill) => arr.fill_to_shape(&max_shape, fill),
                    Err(e) => {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot combine arrays with shapes {} and {max_shape}{e}",
                            arr.shape
                        ))))
                    }
                },
                Value::Complex(arr) => match ctx.scalar_fill::<Complex>() {
                    Ok(fill) => arr.fill_to_shape(&max_shape, fill),
                    Err(e) => {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot combine arrays with shapes {} and {max_shape}{e}",
                            arr.shape
                        ))))
                    }
                },
                Value::Char(arr) => match ctx.scalar_fill::<char>() {
                    Ok(fill) => arr.fill_to_shape(&max_shape, fill),
                    Err(e) => {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot combine arrays with shapes {} and {max_shape}{e}",
                            arr.shape
                        ))))
                    }
                },
                Value::Box(arr) => match ctx.scalar_fill::<Boxed>() {
                    Ok(fill) => arr.fill_to_shape(&max_shape, fill),
                    Err(e) => {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot combine arrays with shapes {} and {max_shape}{e}",
                            arr.shape
                        ))))
                    }
                },
            }
        }

        // Validate size and reserve space
        let total_elements = validate_size_impl(
            value.elem_size(),
            value.shape.iter().copied().chain([to_reserve]),
        )
        .map_err(|e| ctx.error(e))?;
        value.reserve_min(total_elements);

        // Combine the arrays
        if allow_ext {
            for d in max_shape.iter().take(max_shape.len() - value.rank()).rev() {
                value.reshape_scalar(Ok(*d as isize), true, ctx)?;
            }
        }
        value.shape.prepend(1);
        value.meta.take_label();
        Ok(match value {
            Value::Num(mut a) => {
                for val in row_values {
                    match val {
                        Value::Num(b) => a.append(b, allow_ext, ctx)?,
                        Value::Byte(b) => a.append(b.convert(), allow_ext, ctx)?,
                        _ => unreachable!(),
                    }
                }
                a.into()
            }
            Value::Byte(mut a) => {
                for val in row_values {
                    match val {
                        Value::Byte(b) => a.append(b, allow_ext, ctx)?,
                        _ => unreachable!(),
                    }
                }
                a.into()
            }
            Value::Complex(mut a) => {
                for val in row_values {
                    match val {
                        Value::Num(b) => a.append(b.convert(), allow_ext, ctx)?,
                        Value::Byte(b) => a.append(b.convert(), allow_ext, ctx)?,
                        Value::Complex(b) => a.append(b, allow_ext, ctx)?,
                        _ => unreachable!(),
                    }
                }
                a.into()
            }
            Value::Char(mut a) => {
                for val in row_values {
                    match val {
                        Value::Char(b) => a.append(b, allow_ext, ctx)?,
                        _ => unreachable!(),
                    }
                }
                a.into()
            }
            Value::Box(mut a) => {
                for val in row_values {
                    match val {
                        Value::Box(b) => a.append(b, allow_ext, ctx)?,
                        val => a.append(val.box_depth(a.rank()), allow_ext, ctx)?,
                    }
                }
                a.into()
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    #[track_caller]
    /// Create an array from row arrays
    pub fn from_row_arrays<V>(values: V, env: &Uiua) -> UiuaResult<Self>
    where
        V: IntoIterator<Item = Self>,
        V::IntoIter: ExactSizeIterator,
    {
        Self::from_row_arrays_impl(values, env)
    }
    #[track_caller]
    /// Create an array from row arrays
    ///
    /// # Panics
    /// Panics if the row arrays have incompatible shapes
    pub fn from_row_arrays_infallible<V>(values: V) -> Self
    where
        V: IntoIterator<Item = Self>,
        V::IntoIter: ExactSizeIterator,
    {
        Self::from_row_arrays_impl(values, &()).unwrap()
    }
    #[track_caller]
    fn from_row_arrays_impl<V, C>(values: V, ctx: &C) -> Result<Self, C::Error>
    where
        V: IntoIterator<Item = Self>,
        V::IntoIter: ExactSizeIterator,
        C: FillContext,
    {
        let mut row_values = values.into_iter();
        let total_rows = row_values.len();
        let Some(mut arr) = row_values.next() else {
            return Ok(Self::default());
        };
        if let Some(row) = row_values.next() {
            let total_elements = total_rows * arr.shape.iter().product::<usize>();
            arr.data.reserve_min(total_elements);
            arr.couple_impl(row, false, ctx)?;
            for row in row_values {
                arr.append(row, false, ctx)?;
            }
        } else {
            arr.shape.prepend(1);
        }
        Ok(arr)
    }
}
