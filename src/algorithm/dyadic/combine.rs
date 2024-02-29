//! Code for couple, join, and general array creation

use std::{cmp::Ordering, mem::take};

use ecow::EcoVec;

#[cfg(feature = "bytes")]
use crate::algorithm::op2_bytes_retry_fill;
use crate::{
    algorithm::{max_shape, FillContext},
    cowslice::cowslice,
    Array, ArrayValue, FormatShape, Uiua, UiuaResult, Value,
};

fn data_index_to_shape_index(mut index: usize, shape: &[usize], out: &mut [usize]) -> bool {
    debug_assert_eq!(shape.len(), out.len());
    if index >= shape.iter().product() {
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
    pub fn fill_to_shape(&mut self, shape: &[usize], fill_value: T) {
        while self.rank() < shape.len() {
            self.shape.insert(0, 1);
        }
        if self.shape == shape {
            return;
        }
        let target_size = shape.iter().product();
        let mut new_data = cowslice![fill_value; target_size];
        let new_slice = new_data.as_mut_slice();
        let mut curr = vec![0; shape.len()];
        for new_data_index in 0..target_size {
            data_index_to_shape_index(new_data_index, shape, &mut curr);
            if let Some(data_index) = shape_index_to_data_index(&curr, &self.shape) {
                new_slice[new_data_index] = self.data[data_index].clone();
            }
        }
        self.data = new_data;
        self.shape = shape.into();
    }
}

impl Value {
    /// `join` the array with another
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, env)
    }
    /// `join` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    pub fn join_infallible(self, other: Self) -> Self {
        self.join_impl(other, &()).unwrap()
    }
    fn join_impl<C: FillContext>(mut self, mut other: Self, ctx: &C) -> Result<Self, C::Error> {
        if ctx.unpack_boxes() {
            self.unpack();
            other.unpack();
        }
        self.join_impl_impl(other, ctx)
    }
    fn join_impl_impl<C: FillContext>(self, other: Self, ctx: &C) -> Result<Self, C::Error> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.join_impl(b, ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => op2_bytes_retry_fill::<_, C>(
                a,
                b,
                ctx,
                |a, b| Ok(a.join_impl(b, ctx)?.into()),
                |a, b| Ok(a.join_impl(b, ctx)?.into()),
            )?,
            (Value::Complex(a), Value::Complex(b)) => a.join_impl(b, ctx)?.into(),
            (Value::Char(a), Value::Char(b)) => a.join_impl(b, ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Num(b)) => a.convert().join_impl(b, ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Num(a), Value::Byte(b)) => a.join_impl(b.convert(), ctx)?.into(),
            (Value::Complex(a), Value::Num(b)) => a.join_impl(b.convert(), ctx)?.into(),
            (Value::Num(a), Value::Complex(b)) => a.convert().join_impl(b, ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Complex(a), Value::Byte(b)) => a.join_impl(b.convert(), ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Complex(b)) => a.convert().join_impl(b, ctx)?.into(),
            (a, b) => a.bin_coerce_to_boxes(
                b,
                ctx,
                |a, b, env| Ok(a.join_impl(b, env)?.into()),
                |a, b| format!("Cannot join {a} array and {b} array"),
            )?,
        })
    }
    pub(crate) fn append<C: FillContext>(
        &mut self,
        mut other: Self,
        ctx: &C,
    ) -> Result<(), C::Error> {
        if ctx.unpack_boxes() {
            self.unpack();
            other.unpack();
        }
        self.append_impl(other, ctx)
    }
    fn append_impl<C: FillContext>(&mut self, other: Self, ctx: &C) -> Result<(), C::Error> {
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.append(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => {
                *self = op2_bytes_retry_fill::<_, C>(
                    a.clone(),
                    b,
                    ctx,
                    |mut a, b| {
                        a.append(b, ctx)?;
                        Ok(a.into())
                    },
                    |mut a, b| {
                        a.append(b, ctx)?;
                        Ok(a.into())
                    },
                )?;
            }
            (Value::Complex(a), Value::Complex(b)) => a.append(b, ctx)?,
            (Value::Char(a), Value::Char(b)) => a.append(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Num(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ctx)?;
                *self = a.into();
            }
            #[cfg(feature = "bytes")]
            (Value::Num(a), Value::Byte(b)) => a.append(b.convert(), ctx)?,
            (Value::Complex(a), Value::Num(b)) => a.append(b.convert(), ctx)?,
            (Value::Num(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ctx)?;
                *self = a.into();
            }
            #[cfg(feature = "bytes")]
            (Value::Complex(a), Value::Byte(b)) => a.append(b.convert(), ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.append(b, ctx)?;
                *self = a.into();
            }
            (a, b) => a.bin_coerce_to_boxes_mut(
                b,
                ctx,
                |a, b, env| a.append(b, env),
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
        match self {
            Value::Num(a) => a
                .undo_join(&a_shape, &b_shape, env)
                .map(|(a, b)| (a.into(), b.into())),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => a
                .undo_join(&a_shape, &b_shape, env)
                .map(|(a, b)| (a.into(), b.into())),
            Value::Complex(a) => a
                .undo_join(&a_shape, &b_shape, env)
                .map(|(a, b)| (a.into(), b.into())),
            Value::Char(a) => a
                .undo_join(&a_shape, &b_shape, env)
                .map(|(a, b)| (a.into(), b.into())),
            Value::Box(a) => a
                .undo_join(&a_shape, &b_shape, env)
                .map(|(a, b)| (a.into(), b.into())),
        }
    }
    pub(crate) fn unjoin(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        self.generic_into(
            |arr| arr.unjoin(env).map(|(a, b)| (a.into(), b.into())),
            |arr| arr.unjoin(env).map(|(a, b)| (a.into(), b.into())),
            |arr| arr.unjoin(env).map(|(a, b)| (a.into(), b.into())),
            |arr| arr.unjoin(env).map(|(a, b)| (a.into(), b.into())),
            |arr| arr.unjoin(env).map(|(a, b)| (a.into(), b.into())),
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// `join` the array with another
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, env)
    }
    /// `join` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    pub fn join_infallible(self, other: Self) -> Self {
        self.join_impl(other, &()).unwrap()
    }
    fn join_impl<C: FillContext>(mut self, mut other: Self, ctx: &C) -> Result<Self, C::Error> {
        crate::profile_function!();
        self.combine_meta(other.meta());
        let res = match self.rank().cmp(&other.rank()) {
            Ordering::Less => {
                if let Some(label) = other.take_label() {
                    self.meta_mut().label = Some(label);
                }
                if self.shape() == [0] {
                    return Ok(other);
                }
                let target_shape = match ctx.scalar_fill::<T>() {
                    Ok(fill) => {
                        let target_shape = max_shape(&self.shape, &other.shape);
                        let row_shape = &target_shape[1..];
                        self.fill_to_shape(row_shape, fill.clone());
                        other.fill_to_shape(&target_shape, fill);
                        target_shape
                    }
                    Err(e) => {
                        if other.rank() - self.rank() > 1 {
                            return Err(C::fill_error(ctx.error(format!(
                                "Cannot join rank {} array with rank {} array{e}",
                                self.rank(),
                                other.rank()
                            ))));
                        }
                        if self.shape() != other.shape()[1..] {
                            return Err(C::fill_error(ctx.error(format!(
                                "Cannot join arrays of shapes {} and {}{e}",
                                self.shape(),
                                other.shape()
                            ))));
                        }
                        other.shape
                    }
                };
                self.data.extend(other.data);
                self.shape = target_shape;
                self.shape[0] += 1;
                self
            }
            Ordering::Greater => {
                if other.shape() == 0 {
                    return Ok(self);
                }
                self.append(other, ctx)?;
                self
            }
            Ordering::Equal => {
                if self.rank() == 0 {
                    debug_assert_eq!(other.rank(), 0);
                    self.data.extend(other.data.into_iter().next());
                    self.shape = 2.into();
                    self
                } else {
                    match ctx.scalar_fill::<T>() {
                        Ok(fill) => {
                            let new_row_shape = max_shape(&self.shape[1..], &other.shape[1..]);
                            for (array, fill) in [(&mut self, fill.clone()), (&mut other, fill)] {
                                let mut new_shape = new_row_shape.clone();
                                new_shape.insert(0, array.shape[0]);
                                array.fill_to_shape(&new_shape, fill);
                            }
                        }
                        Err(e) if self.shape[1..] != other.shape[1..] => {
                            return Err(C::fill_error(ctx.error(format!(
                                "Cannot join arrays of shapes {} and {}. {e}",
                                self.shape(),
                                other.shape()
                            ))));
                        }
                        _ => (),
                    }
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self.take_label();
                    self
                }
            }
        };
        res.validate_shape();
        Ok(res)
    }
    fn append<C: FillContext>(&mut self, mut other: Self, ctx: &C) -> Result<(), C::Error> {
        self.combine_meta(other.meta());
        let target_shape = match ctx.scalar_fill::<T>() {
            Ok(fill) => {
                while self.rank() <= other.rank() {
                    self.shape.push(1);
                }
                let target_shape = max_shape(&self.shape, &other.shape);
                let row_shape = &target_shape[1..];
                self.fill_to_shape(&target_shape, fill.clone());
                other.fill_to_shape(row_shape, fill);
                target_shape
            }
            Err(e) => {
                if self.rank() <= other.rank() || self.rank() - other.rank() > 1 {
                    return Err(C::fill_error(ctx.error(format!(
                        "Cannot add rank {} row to rank {} array{e}",
                        other.rank(),
                        self.rank()
                    ))));
                }
                if &self.shape()[1..] != other.shape() {
                    return Err(C::fill_error(ctx.error(format!(
                        "Cannot add shape {} row to array with shape {} rows{e}",
                        other.shape(),
                        FormatShape(&self.shape()[1..]),
                    ))));
                }
                take(&mut self.shape)
            }
        };
        self.data.extend(other.data);
        self.shape = target_shape;
        self.shape[0] += 1;
        self.validate_shape();
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
                self.validate_shape();
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
                let right = self.drop(&[1], env)?;
                Ok((left, right))
            }
            Ordering::Greater => {
                if self.row_count() == 0 {
                    return Ok((self.clone(), self));
                }
                let right_shape = &self.shape[1..];
                let right_data = self.data.slice((self.row_count() - 1) * self.row_len()..);
                let right = Array::new(right_shape, right_data);
                let left = self.drop(&[-1], env)?;
                Ok((left, right))
            }
        }
    }
    pub(crate) fn unjoin(mut self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        if self.rank() == 0 {
            return Err(env.error("Cannot unjoin a scalar"));
        }
        if self.row_count() == 0 {
            return Err(env.error("Cannot unjoin an empty array"));
        }
        let row_len = self.row_len();
        let data_slice = self.data.as_mut_slice();
        let first_data = EcoVec::from(&data_slice[..row_len]);
        data_slice.rotate_left(row_len);
        let new_data_len = data_slice.len() - row_len;
        self.data.truncate(new_data_len);
        let first = Array::new(&self.shape[1..], first_data);
        self.shape[0] -= 1;
        self.validate_shape();
        Ok((first, self))
    }
}

impl Value {
    /// `couple` the value with another
    pub fn couple(mut self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, env)?;
        Ok(self)
    }
    /// `couple` the value with another
    ///
    /// # Panics
    /// Panics if the values have incompatible shapes
    pub fn couple_infallible(mut self, other: Self) -> Self {
        self.couple_impl(other, &()).unwrap();
        self
    }
    pub(crate) fn couple_impl<C: FillContext>(
        &mut self,
        mut other: Self,
        ctx: &C,
    ) -> Result<(), C::Error> {
        if ctx.unpack_boxes() {
            self.unpack();
            other.unpack();
        }
        self.couple_impl_impl(other, ctx)
    }
    fn couple_impl_impl<C: FillContext>(&mut self, other: Self, ctx: &C) -> Result<(), C::Error> {
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.couple_impl(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => {
                *self = op2_bytes_retry_fill::<_, C>(
                    a.clone(),
                    b,
                    ctx,
                    |mut a, b| {
                        a.couple_impl(b, ctx)?;
                        Ok(a.into())
                    },
                    |mut a, b| {
                        a.couple_impl(b, ctx)?;
                        Ok(a.into())
                    },
                )?
            }
            (Value::Complex(a), Value::Complex(b)) => a.couple_impl(b, ctx)?,
            (Value::Char(a), Value::Char(b)) => a.couple_impl(b, ctx)?,
            (Value::Box(a), Value::Box(b)) => a.couple_impl(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Num(a), Value::Byte(b)) => a.couple_impl(b.convert(), ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Num(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, ctx)?;
                *self = a.into();
            }
            (Value::Complex(a), Value::Num(b)) => a.couple_impl(b.convert(), ctx)?,
            (Value::Num(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, ctx)?;
                *self = a.into();
            }
            #[cfg(feature = "bytes")]
            (Value::Complex(a), Value::Byte(b)) => a.couple_impl(b.convert(), ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Complex(b)) => {
                let mut a = a.convert_ref();
                a.couple_impl(b, ctx)?;
                *self = a.into();
            }
            (a, b) => a.bin_coerce_to_boxes_mut(
                b,
                ctx,
                |a, b, ctx| a.couple_impl(b, ctx),
                |a, b| format!("Cannot couple {a} array with {b} array"),
            )?,
        }
        Ok(())
    }
    /// Uncouple the value into two values
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        match self {
            Value::Num(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Complex(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Char(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Box(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    /// `couple` the array with another
    pub fn couple(mut self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, env)?;
        Ok(self)
    }
    /// `couple` the array with another
    ///
    /// # Panics
    /// Panics if the arrays have incompatible shapes
    pub fn couple_infallible(mut self, other: Self) -> Self {
        self.couple_impl(other, &()).unwrap();
        self
    }
    fn couple_impl<C: FillContext>(&mut self, mut other: Self, ctx: &C) -> Result<(), C::Error> {
        crate::profile_function!();
        self.combine_meta(other.meta());
        if self.shape != other.shape {
            match ctx.scalar_fill::<T>() {
                Ok(fill) => {
                    let new_shape = max_shape(&self.shape, &other.shape);
                    self.fill_to_shape(&new_shape, fill.clone());
                    other.fill_to_shape(&new_shape, fill);
                }
                Err(e) => {
                    return Err(C::fill_error(ctx.error(format!(
                        "Cannot couple arrays with shapes {} and {}{e}",
                        self.shape(),
                        other.shape()
                    ))));
                }
            }
        }
        self.data.extend(other.data);
        self.shape.insert(0, 2);
        self.validate_shape();
        self.take_label();
        Ok(())
    }
    /// Uncouple the array into two arrays
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        if self.row_count() != 2 {
            return Err(env.error(format!(
                "Cannot uncouple array with {} row{}",
                self.row_count(),
                if self.row_count() == 1 { "" } else { "s" }
            )));
        }
        let mut rows = self.into_rows();
        let first = rows.next().unwrap();
        let second = rows.next().unwrap();
        Ok((first, second))
    }
}

impl Value {
    /// Create a value from row values
    ///
    /// # Panics
    /// Panics if the row values have incompatible shapes
    pub fn from_row_values_infallible<V>(values: V) -> Self
    where
        V: IntoIterator,
        V::Item: Into<Value>,
        V::IntoIter: ExactSizeIterator,
    {
        Self::from_row_values(values.into_iter().map(Into::into), &()).unwrap()
    }
    /// Create a value from row values
    pub fn from_row_values<V, C>(values: V, ctx: &C) -> Result<Self, C::Error>
    where
        V: IntoIterator<Item = Value>,
        C: FillContext,
    {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Ok(Value::default());
        };
        let (min, max) = row_values.size_hint();
        let to_reserve = max.unwrap_or(min);
        if let Some(row) = row_values.next() {
            let total_elements = to_reserve * value.shape().iter().product::<usize>();
            value.reserve_min(total_elements);
            value.couple_impl(row, ctx)?;
            for row in row_values {
                value.append(row, ctx)?;
            }
        } else {
            value.shape_mut().insert(0, 1);
        }
        Ok(value)
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
            let total_elements = total_rows * arr.shape().iter().product::<usize>();
            arr.data.reserve_min(total_elements);
            arr.couple_impl(row, ctx)?;
            for row in row_values {
                arr.append(row, ctx)?;
            }
        } else {
            arr.shape.insert(0, 1);
        }
        Ok(arr)
    }
}
