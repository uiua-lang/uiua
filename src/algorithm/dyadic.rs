//! Algorithms for dyadic array operations

use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{hash_map::DefaultHasher, HashMap, HashSet},
    hash::{Hash, Hasher},
    iter::{once, repeat},
    mem::take,
};

use ecow::EcoVec;
use tinyvec::tiny_vec;

use crate::{
    algorithm::max_shape,
    array::*,
    boxed::Boxed,
    cowslice::{cowslice, CowSlice},
    value::Value,
    Uiua, UiuaResult,
};

#[cfg(feature = "bytes")]
use super::{op2_bytes_retry_fill, op_bytes_ref_retry_fill, op_bytes_retry_fill};
use super::{ArrayCmpSlice, FillContext};

impl Value {
    fn coerce_to_functions<T, C: FillContext, E: ToString>(
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
}

impl<T: Clone + std::fmt::Debug> Array<T> {
    pub(crate) fn depth_slices<U: Clone + std::fmt::Debug, C: FillContext>(
        &mut self,
        other: &Array<U>,
        mut a_depth: usize,
        mut b_depth: usize,
        ctx: &C,
        f: impl Fn(&[usize], &mut [T], &[usize], &[U], &C) -> Result<(), C::Error>,
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
                    a.format_shape(),
                    b.format_shape(),
                    FormatShape(a_prefix),
                    FormatShape(b_prefix)
                )));
            }
        }
        match a_depth.cmp(&b_depth) {
            Ordering::Equal => {}
            Ordering::Less => {
                for b_dim in b.shape[..b_depth - a_depth].iter().rev() {
                    a.reshape_scalar(*b_dim);
                    a_depth += 1;
                }
            }
            Ordering::Greater => {
                for a_dim in a.shape[..a_depth - b_depth].iter().rev() {
                    local_b = b.clone();
                    local_b.reshape_scalar(*a_dim);
                    b = &local_b;
                    b_depth += 1;
                }
            }
        }

        let a_row_shape = &a.shape[a_depth..];
        let b_row_shape = &b.shape[b_depth..];
        for (a, b) in (a.data.as_mut_slice())
            .chunks_exact_mut(a_row_shape.iter().product())
            .zip(b.data.as_slice().chunks_exact(b_row_shape.iter().product()))
        {
            f(a_row_shape, a, b_row_shape, b, ctx)?;
        }
        Ok(())
    }
}

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
        if ctx.pack_boxes() {
            self.unpack();
            other.unpack();
            if let Ok(val) = self.clone().join_impl_impl(other.clone(), ctx) {
                Ok(val)
            } else {
                match self.rank().cmp(&other.rank()) {
                    Ordering::Greater => {
                        self = self.into_rows().map(Boxed).collect::<Array<_>>().into();
                        other.box_if_not();
                    }
                    Ordering::Less => {
                        self.box_if_not();
                        other = other.into_rows().map(Boxed).collect::<Array<_>>().into();
                    }
                    Ordering::Equal => {
                        self.box_if_not();
                        other.box_if_not();
                    }
                }
                self.join_impl_impl(other, ctx)
            }
        } else {
            self.join_impl_impl(other, ctx)
        }
    }
    fn join_impl_impl<C: FillContext>(self, other: Self, ctx: &C) -> Result<Self, C::Error> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.join_impl(b, ctx)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => op2_bytes_retry_fill::<_, C>(
                a,
                b,
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
            (a, b) => a.coerce_to_functions(
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
        if ctx.pack_boxes() {
            self.unpack();
            other.unpack();
            if self.append_impl(other.clone(), ctx).is_err() {
                *self = take(self)
                    .into_rows()
                    .map(|row| row.boxed_if_not())
                    .collect::<Array<_>>()
                    .into();
                other.box_if_not();
                self.append_impl(other, ctx)
            } else {
                Ok(())
            }
        } else {
            self.append_impl(other, ctx)
        }
    }
    fn append_impl<C: FillContext>(&mut self, other: Self, ctx: &C) -> Result<(), C::Error> {
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.append(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => {
                *self = op2_bytes_retry_fill::<_, C>(
                    a.clone(),
                    b,
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
            (a, b) => {
                *self = a.clone().coerce_to_functions(
                    b,
                    ctx,
                    |mut a, b, env| {
                        a.append(b, env)?;
                        Ok(a.into())
                    },
                    |a, b| format!("Cannot append {a} array to {b} array"),
                )?
            }
        }
        Ok(())
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
        let res = match self.rank().cmp(&other.rank()) {
            Ordering::Less => {
                if self.shape() == [0] {
                    return Ok(other);
                }
                let target_shape = if let Some(fill) = ctx.fill::<T>() {
                    let target_shape = max_shape(&self.shape, &other.shape);
                    let row_shape = &target_shape[1..];
                    self.fill_to_shape(row_shape, fill.clone());
                    other.fill_to_shape(&target_shape, fill);
                    target_shape
                } else if self.rank() == 0 {
                    let val = self.data.into_iter().next().unwrap();
                    self.data = cowslice![val; other.shape().iter().skip(1).product()];
                    other.shape
                } else {
                    if other.rank() - self.rank() > 1 {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot join rank {} array with rank {} array",
                            self.rank(),
                            other.rank()
                        ))));
                    }
                    if self.shape() != &other.shape()[1..] {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot join arrays of shapes {} and {}",
                            self.format_shape(),
                            other.format_shape()
                        ))));
                    }
                    other.shape
                };
                self.data.extend(other.data);
                self.shape = target_shape;
                self.shape[0] += 1;
                self
            }
            Ordering::Greater => {
                if other.shape() == [0] {
                    return Ok(self);
                }
                self.append(other, ctx)?;
                self
            }
            Ordering::Equal => {
                if self.rank() == 0 {
                    debug_assert_eq!(other.rank(), 0);
                    self.data.extend(other.data.into_iter().next());
                    self.shape = tiny_vec![2];
                    self
                } else {
                    if let Some(fill) = ctx.fill::<T>() {
                        let new_row_shape = max_shape(&self.shape[1..], &other.shape[1..]);
                        for (array, fill) in [(&mut self, fill.clone()), (&mut other, fill)] {
                            let mut new_shape = new_row_shape.clone();
                            new_shape.insert(0, array.shape[0]);
                            array.fill_to_shape(&new_shape, fill);
                        }
                    } else if self.shape[1..] != other.shape[1..] {
                        return Err(C::fill_error(ctx.error(format!(
                            "Cannot join arrays of shapes {} and {}",
                            self.format_shape(),
                            other.format_shape()
                        ))));
                    }
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self
                }
            }
        };
        res.validate_shape();
        Ok(res)
    }
    fn append<C: FillContext>(&mut self, mut other: Self, ctx: &C) -> Result<(), C::Error> {
        let target_shape = if let Some(fill) = ctx.fill::<T>() {
            while self.rank() <= other.rank() {
                self.shape.push(1);
            }
            let target_shape = max_shape(&self.shape, &other.shape);
            let row_shape = &target_shape[1..];
            self.fill_to_shape(&target_shape, fill.clone());
            other.fill_to_shape(row_shape, fill);
            target_shape
        } else if other.rank() == 0 {
            let val = other.data.into_iter().next().unwrap();
            other.data = cowslice![val; self.shape().iter().skip(1).product()];
            take(&mut self.shape)
        } else {
            if self.rank() <= other.rank() || self.rank() - other.rank() > 1 {
                return Err(C::fill_error(ctx.error(format!(
                    "Cannot append rank {} array with rank {} array",
                    self.rank(),
                    other.rank()
                ))));
            }
            if &self.shape()[1..] != other.shape() {
                return Err(C::fill_error(ctx.error(format!(
                    "Cannot append arrays of shapes {} and {}",
                    self.format_shape(),
                    other.format_shape()
                ))));
            }
            take(&mut self.shape)
        };
        self.data.extend(other.data);
        self.shape = target_shape;
        self.shape[0] += 1;
        Ok(())
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
        if ctx.pack_boxes() {
            self.unpack();
            other.unpack();
            if self.couple_impl_impl(other.clone(), ctx).is_err() {
                self.box_if_not();
                other.box_if_not();
                self.couple_impl_impl(other, ctx)
            } else {
                Ok(())
            }
        } else {
            self.couple_impl_impl(other, ctx)
        }
    }
    fn couple_impl_impl<C: FillContext>(&mut self, other: Self, ctx: &C) -> Result<(), C::Error> {
        match (&mut *self, other) {
            (Value::Num(a), Value::Num(b)) => a.couple_impl(b, ctx)?,
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => {
                *self = op2_bytes_retry_fill::<_, C>(
                    a.clone(),
                    b,
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
            (a, b) => {
                *self = a.clone().coerce_to_functions(
                    b,
                    ctx,
                    |mut a, b, ctx| {
                        a.couple_impl(b, ctx)?;
                        Ok(a.into())
                    },
                    |a, b| format!("Cannot couple {a} array with {b} array"),
                )?
            }
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
        if self.shape != other.shape {
            if let Some(fill) = ctx.fill::<T>() {
                let new_shape = max_shape(&self.shape, &other.shape);
                self.fill_to_shape(&new_shape, fill.clone());
                other.fill_to_shape(&new_shape, fill);
            } else {
                return Err(C::fill_error(ctx.error(format!(
                    "Cannot couple arrays with shapes {} and {}",
                    self.format_shape(),
                    other.format_shape()
                ))));
            }
        }
        self.data.extend(other.data);
        self.shape.insert(0, 2);
        self.validate_shape();
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
    pub fn from_row_values<V>(values: V, env: &Uiua) -> UiuaResult<Self>
    where
        V: IntoIterator,
        V::Item: Into<Value>,
        V::IntoIter: ExactSizeIterator,
    {
        Self::from_row_values_impl(values.into_iter().map(Into::into), env)
    }
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
        Self::from_row_values_impl(values.into_iter().map(Into::into), &()).unwrap()
    }
    fn from_row_values_impl<V, C>(values: V, ctx: &C) -> Result<Self, C::Error>
    where
        V: IntoIterator<Item = Value>,
        V::IntoIter: ExactSizeIterator,
        C: FillContext,
    {
        let mut row_values = values.into_iter();
        let total_rows = row_values.len();
        let Some(mut value) = row_values.next() else {
            return Ok(Value::default());
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            if count == 2 {
                let total_elements = total_rows * value.shape().iter().product::<usize>();
                value.reserve_min(total_elements);
                value.couple_impl(row, ctx)?;
            } else {
                value.append(row, ctx)?;
            }
        }
        if count == 1 {
            value.shape_mut().insert(0, 1);
        }
        Ok(value)
    }
}

impl<T: ArrayValue> Array<T> {
    #[track_caller]
    /// Create an array from row arrays
    pub fn from_row_arrays(values: impl IntoIterator<Item = Self>, env: &Uiua) -> UiuaResult<Self> {
        Self::from_row_arrays_impl(values, env)
    }
    #[track_caller]
    /// Create an array from row arrays
    ///
    /// # Panics
    /// Panics if the row arrays have incompatible shapes
    pub fn from_row_arrays_infallible(values: impl IntoIterator<Item = Self>) -> Self {
        Self::from_row_arrays_impl(values, &()).unwrap()
    }
    #[track_caller]
    fn from_row_arrays_impl<C: FillContext>(
        values: impl IntoIterator<Item = Self>,
        ctx: &C,
    ) -> Result<Self, C::Error> {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Ok(Self::default());
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            if count == 2 {
                value.couple_impl(row, ctx)?;
            } else {
                value.append(row, ctx)?;
            }
        }
        if count == 1 {
            value.shape.insert(0, 1);
        }
        value.validate_shape();
        Ok(value)
    }
}

impl Value {
    /// `reshape` this value with another
    pub fn reshape(&mut self, shape: &Self, env: &Uiua) -> UiuaResult {
        if let Ok(n) = shape.as_nat(env, "") {
            match self {
                Value::Num(a) => a.reshape_scalar(n),
                #[cfg(feature = "bytes")]
                Value::Byte(a) => a.reshape_scalar(n),
                Value::Complex(a) => a.reshape_scalar(n),
                Value::Char(a) => a.reshape_scalar(n),
                Value::Box(a) => a.reshape_scalar(n),
            }
        } else {
            let target_shape = shape.as_ints(
                env,
                "Shape should be a single natural number \
                or a list of integers",
            )?;
            match self {
                Value::Num(a) => a.reshape(&target_shape, env),
                #[cfg(feature = "bytes")]
                Value::Byte(a) => a.reshape(&target_shape, env),
                Value::Complex(a) => a.reshape(&target_shape, env),
                Value::Char(a) => a.reshape(&target_shape, env),
                Value::Box(a) => a.reshape(&target_shape, env),
            }?
        }
        Ok(())
    }
}

impl<T: Clone> Array<T> {
    /// `reshape` this array by replicating it as the rows of a new array
    pub fn reshape_scalar(&mut self, count: usize) {
        self.data.modify(|data| {
            if count == 0 {
                data.clear();
                return;
            }
            data.reserve((count - 1) * data.len());
            let row = data.clone();
            for _ in 1..count {
                data.extend_from_slice(&row);
            }
        });
        self.shape.insert(0, count);
    }
}

impl<T: ArrayValue> Array<T> {
    /// `reshape` the array
    pub fn reshape(&mut self, dims: &[isize], env: &Uiua) -> UiuaResult {
        let mut neg_count = 0;
        for dim in dims {
            if *dim < 0 {
                neg_count += 1;
            }
        }
        let derive_len = |data_len: usize, other_len: usize| {
            (if env.fill::<T>().is_some() {
                f32::ceil
            } else {
                f32::floor
            }(data_len as f32 / other_len as f32) as usize)
                .max(1)
        };
        let shape: Shape = match neg_count {
            0 => dims.iter().map(|&dim| dim as usize).collect(),
            1 => {
                if dims[0] < 0 {
                    if dims[1..].iter().any(|&dim| dim < 0) {
                        return Err(
                            env.error("Cannot reshape array with multiple negative dimensions")
                        );
                    }
                    let shape_non_leading_len = dims[1..].iter().product::<isize>() as usize;
                    if shape_non_leading_len == 0 {
                        return Err(
                            env.error("Cannot reshape array with any 0 non-leading dimensions")
                        );
                    }
                    let leading_len = derive_len(self.data.len(), shape_non_leading_len);
                    let mut shape = vec![leading_len];
                    shape.extend(dims[1..].iter().map(|&dim| dim as usize));
                    Shape::from(&*shape)
                } else if *dims.last().unwrap() < 0 {
                    if dims.iter().rev().skip(1).any(|&dim| dim < 0) {
                        return Err(
                            env.error("Cannot reshape array with multiple negative dimensions")
                        );
                    }
                    let shape_non_trailing_len =
                        dims.iter().rev().skip(1).product::<isize>() as usize;
                    if shape_non_trailing_len == 0 {
                        return Err(
                            env.error("Cannot reshape array with any 0 non-trailing dimensions")
                        );
                    }
                    let trailing_len = derive_len(self.data.len(), shape_non_trailing_len);
                    let mut shape: Vec<usize> = dims.iter().map(|&dim| dim as usize).collect();
                    shape.pop();
                    shape.push(trailing_len);
                    Shape::from(&*shape)
                } else {
                    let neg_index = dims.iter().position(|&dim| dim < 0).unwrap();
                    let (front, back) = dims.split_at(neg_index);
                    let back = &back[1..];
                    let front_len = front.iter().product::<isize>() as usize;
                    let back_len = back.iter().product::<isize>() as usize;
                    if front_len == 0 || back_len == 0 {
                        return Err(env.error("Cannot reshape array with any 0 outer dimensions"));
                    }
                    let middle_len = derive_len(self.data.len(), front_len * back_len);
                    let mut shape: Vec<usize> = front.iter().map(|&dim| dim as usize).collect();
                    shape.push(middle_len);
                    shape.extend(back.iter().map(|&dim| dim as usize));
                    Shape::from(&*shape)
                }
            }
            n => {
                return Err(env.error(format!("Cannot reshape array with {n} negative dimensions")))
            }
        };
        let target_len: usize = shape.iter().product();
        if self.data.len() < target_len {
            if let Some(fill) = env.fill::<T>() {
                let start = self.data.len();
                self.data.modify(|data| {
                    data.extend(repeat(fill).take(target_len - start));
                });
            } else if self.data.is_empty() {
                return Err(env.error("Cannot reshape empty array without a fill value"));
            } else if self.rank() == 0 {
                self.data = cowslice![self.data[0].clone(); target_len];
            } else {
                let start = self.data.len();
                self.data.modify(|data| {
                    data.reserve(target_len - data.len());
                    for i in 0..target_len - start {
                        data.push(data[i % start].clone());
                    }
                });
            }
        } else {
            self.data.truncate(target_len);
        }
        self.shape = shape;
        self.validate_shape();
        Ok(())
    }
}

impl Value {
    /// `rerank` this value with another
    pub fn rerank(&mut self, rank: &Self, env: &Uiua) -> UiuaResult {
        let irank = rank.as_int(env, "Rank must be a natural number")?;
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
            } else {
                let new_first_dim: usize = shape[..rank].iter().product();
                *shape = once(new_first_dim)
                    .chain(shape[rank..].iter().copied())
                    .collect();
            }
        }
        self.validate_shape();
        Ok(())
    }
    pub(crate) fn unrerank(&mut self, rank: &Self, orig_shape: &Self, env: &Uiua) -> UiuaResult {
        if self.rank() == 0 {
            if let Value::Box(arr) = self {
                arr.data.as_mut_slice()[0]
                    .0
                    .unrerank(rank, orig_shape, env)?;
            }
            return Ok(());
        }
        let irank = rank.as_int(env, "Rank must be a natural number")?;
        let orig_shape = orig_shape.as_nats(env, "Shape must be a list of natural numbers")?;
        let rank = irank.unsigned_abs();
        let shape = self.shape_mut();
        if irank >= 0 {
            // Positive rank
            let new_shape: Shape = orig_shape
                .iter()
                .take(orig_shape.len().saturating_sub(rank))
                .chain(
                    shape
                        .iter()
                        .skip((rank + 1).saturating_sub(orig_shape.len()).max(1)),
                )
                .copied()
                .collect();
            *shape = new_shape;
        } else {
            // Negative rank
            let new_shape: Shape = orig_shape
                .iter()
                .take(rank)
                .chain(shape.iter().skip(1))
                .copied()
                .collect();
            *shape = new_shape;
        }
        self.validate_shape();
        Ok(())
    }
}

impl Value {
    /// Use this value as counts to `keep` another
    pub fn keep(&self, kept: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = self.as_nats(
            env,
            "Keep amount must be a natural number \
            or list of natural numbers",
        )?;
        Ok(if self.rank() == 0 {
            match kept {
                Value::Num(a) => a.scalar_keep(counts[0]).into(),
                #[cfg(feature = "bytes")]
                Value::Byte(a) => a.scalar_keep(counts[0]).into(),
                Value::Complex(a) => a.scalar_keep(counts[0]).into(),
                Value::Char(a) => a.scalar_keep(counts[0]).into(),
                Value::Box(a) => a.scalar_keep(counts[0]).into(),
            }
        } else {
            match kept {
                Value::Num(a) => a.list_keep(&counts, env)?.into(),
                #[cfg(feature = "bytes")]
                Value::Byte(a) => a.list_keep(&counts, env)?.into(),
                Value::Complex(a) => a.list_keep(&counts, env)?.into(),
                Value::Char(a) => a.list_keep(&counts, env)?.into(),
                Value::Box(a) => a.list_keep(&counts, env)?.into(),
            }
        })
    }
    pub(crate) fn unkeep(self, kept: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let counts = self.as_nats(
            env,
            "Keep amount must be a natural number \
            or list of natural numbers",
        )?;
        if self.rank() == 0 {
            return Err(env.error("Cannot invert scalar keep"));
        }
        Ok(match (kept, into) {
            (Value::Num(a), Value::Num(b)) => a.unkeep(&counts, b, env)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Byte(b)) => a.unkeep(&counts, b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.unkeep(&counts, b, env)?.into(),
            (Value::Box(a), Value::Box(b)) => a.unkeep(&counts, b, env)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Num(a), Value::Byte(b)) => a.unkeep(&counts, b.convert(), env)?.into(),
            #[cfg(feature = "bytes")]
            (Value::Byte(a), Value::Num(b)) => a.convert().unkeep(&counts, b, env)?.into(),
            (a, b) => a.coerce_to_functions(
                b,
                env,
                |a, b, env| Ok(a.unkeep(&counts, b, env)?.into()),
                |a, b| format!("Cannot unkeep {a} array with {b} array"),
            )?,
        })
    }
}

impl<T: ArrayValue> Array<T> {
    /// `keep` this array by replicating it as the rows of a new array
    pub fn scalar_keep(mut self, count: usize) -> Self {
        // Scalar kept
        if self.rank() == 0 {
            self.shape.push(count);
            self.data.modify(|data| {
                let value = data[0].clone();
                data.clear();
                for _ in 0..count {
                    data.push(value.clone());
                }
            });
            self.validate_shape();
            return self;
        }
        // Keep nothing
        if count == 0 {
            self.data = CowSlice::new();
            self.shape[0] = 0;
            return self;
        }
        // Keep 1 is a no-op
        if count == 1 {
            return self;
        }
        // Keep ≥2 is a repeat
        self.shape[0] *= count;
        let old_data = self.data.clone();
        self.data.modify(|data| {
            data.reserve(data.len() * count);
            for _ in 1..count {
                data.extend_from_slice(&old_data);
            }
        });
        self.validate_shape();
        self
    }
    /// `keep` this array with some counts
    pub fn list_keep(mut self, counts: &[usize], env: &Uiua) -> UiuaResult<Self> {
        let mut amount = Cow::Borrowed(counts);
        match amount.len().cmp(&self.row_count()) {
            Ordering::Equal => {}
            Ordering::Less => {
                if let Some(fill) = env.fill::<f64>() {
                    if fill < 0.0 || fill.fract() != 0.0 {
                        return Err(env.error(format!(
                            "Fill value for keep must be a non-negative\
                            integer, but it is {fill}"
                        )));
                    }
                    let fill = fill as usize;
                    let mut new_amount = amount.to_vec();
                    new_amount.extend(repeat(fill).take(self.row_count() - amount.len()));
                    amount = new_amount.into();
                } else {
                    return Err(env.error(format!(
                        "Cannot keep array with shape {} with array of shape {}",
                        self.format_shape(),
                        FormatShape(&[amount.len()])
                    )));
                }
            }
            Ordering::Greater => {
                return Err(env.error(if env.fill::<f64>().is_some() {
                    format!(
                        "Cannot keep array with shape {} with array of shape {}.\
                        A fill value is available, but keep can only been filled\
                        if there are fewer counts than rows.",
                        self.format_shape(),
                        FormatShape(amount.as_ref())
                    )
                } else {
                    format!(
                        "Cannot keep array with shape {} with array of shape {}",
                        self.format_shape(),
                        FormatShape(amount.as_ref())
                    )
                }))
            }
        }
        if self.rank() == 0 {
            if amount.len() != 1 {
                return Err(env.error("Scalar array can only be kept with a single number"));
            }
            let mut new_data = EcoVec::with_capacity(amount[0]);
            for _ in 0..amount[0] {
                new_data.push(self.data[0].clone());
            }
            self = new_data.into();
        } else {
            let mut all_bools = true;
            let mut true_count = 0;
            for &n in amount.iter() {
                match n {
                    0 => {}
                    1 => true_count += 1,
                    _ => {
                        all_bools = false;
                        break;
                    }
                }
            }
            let row_len = self.row_len();
            if all_bools {
                let new_flat_len = true_count * row_len;
                let mut new_data = CowSlice::with_capacity(new_flat_len);
                if row_len > 0 {
                    for (b, r) in amount.iter().zip(self.data.chunks_exact(row_len)) {
                        if *b == 1 {
                            new_data.extend_from_slice(r);
                        }
                    }
                }
                self.data = new_data;
                self.shape[0] = true_count;
            } else {
                let mut new_data = CowSlice::new();
                let mut new_len = 0;
                if row_len > 0 {
                    for (n, r) in amount.iter().zip(self.data.chunks_exact(row_len)) {
                        new_len += *n;
                        for _ in 0..*n {
                            new_data.extend_from_slice(r);
                        }
                    }
                } else {
                    new_len = amount.iter().sum();
                }
                self.data = new_data;
                self.shape[0] = new_len;
            }
        }
        self.validate_shape();
        Ok(self)
    }
    pub(crate) fn unkeep(self, counts: &[usize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        if counts.iter().any(|&n| n > 1) {
            return Err(env.error("Cannot invert keep with non-boolean counts"));
        }
        let mut new_rows: Vec<_> = Vec::with_capacity(counts.len());
        let mut transformed = self.into_rows();
        for (count, into_row) in counts.iter().zip(into.into_rows()) {
            if *count == 0 {
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
                        into_row.format_shape(),
                        new_row.format_shape()
                    )));
                }
                new_rows.push(new_row);
            }
        }
        Self::from_row_arrays(new_rows, env)
    }
}

impl Value {
    pub(crate) fn as_shaped_indices(&self, env: &Uiua) -> UiuaResult<(&[usize], Vec<isize>)> {
        Ok(match self {
            Value::Num(arr) => {
                let mut index_data = Vec::with_capacity(arr.element_count());
                for &n in &arr.data {
                    if n.fract() != 0.0 {
                        return Err(env.error(format!(
                            "Index must be an array of integers, but {n} is not an integer"
                        )));
                    }
                    index_data.push(n as isize);
                }
                (&arr.shape, index_data)
            }
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => {
                let mut index_data = Vec::with_capacity(arr.element_count());
                for &n in &arr.data {
                    index_data.push(n as isize);
                }
                (&arr.shape, index_data)
            }
            value => {
                return Err(env.error(format!(
                    "Index must be an array of integers, not {}",
                    value.type_name_plural()
                )))
            }
        })
    }
    /// Use this array as an index to pick from another
    pub fn pick(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let (index_shape, index_data) = self.as_shaped_indices(env)?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.pick(index_shape, &index_data, env)?),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => op_bytes_retry_fill(
                a,
                |a| Ok(a.pick(index_shape, &index_data, env)?.into()),
                |a| Ok(a.pick(index_shape, &index_data, env)?.into()),
            )?,
            Value::Complex(a) => Value::Complex(a.pick(index_shape, &index_data, env)?),
            Value::Char(a) => Value::Char(a.pick(index_shape, &index_data, env)?),
            Value::Box(a) => Value::Box(a.pick(index_shape, &index_data, env)?),
        })
    }
    pub(crate) fn unpick(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (index_shape, index_data) = index.as_shaped_indices(env)?;
        if index_shape.len() > 1 {
            let last_axis_len = *index_shape.last().unwrap();
            if last_axis_len == 0 {
                if index_shape[..index_shape.len() - 1].iter().any(|&n| n > 1) {
                    return Err(env.error("Cannot undo pick with duplicate indices"));
                }
            } else {
                let mut sorted_indices = Vec::with_capacity(index_data.len() / last_axis_len);
                for index in index_data.chunks(last_axis_len) {
                    sorted_indices.push(index);
                }
                sorted_indices.sort_unstable();
                if sorted_indices.windows(2).any(|w| w[0] == w[1]) {
                    return Err(env.error("Cannot undo pick with duplicate indices"));
                }
            }
        }
        self.generic_bin_into(
            into,
            |a, b| a.unpick(index_shape, &index_data, b, env).map(Into::into),
            |a, b| a.unpick(index_shape, &index_data, b, env).map(Into::into),
            |a, b| a.unpick(index_shape, &index_data, b, env).map(Into::into),
            |a, b| a.unpick(index_shape, &index_data, b, env).map(Into::into),
            |a, b| a.unpick(index_shape, &index_data, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot unpick {} array from {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    fn pick(&self, index_shape: &[usize], index_data: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index_shape.len() <= 1 {
            self.pick_single(index_data, env)
        } else {
            self.pick_multi(index_shape, index_data, env)
        }
    }
    fn pick_multi(
        &self,
        index_shape: &[usize],
        index_data: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let index_row_len = index_shape[1..].iter().product();
        let mut new_data =
            CowSlice::with_capacity(index_shape[..index_shape.len() - 1].iter().product());
        if index_row_len == 0 {
            let row = self.pick(&index_shape[1..], index_data, env)?;
            for _ in 0..index_shape[0] {
                new_data.extend_from_slice(&row.data);
            }
        } else {
            for index_row in index_data.chunks(index_row_len) {
                let row = self.pick(&index_shape[1..], index_row, env)?;
                new_data.extend_from_slice(&row.data);
            }
        }
        let mut new_shape = Shape::from(&index_shape[0..index_shape.len() - 1]);
        new_shape.extend_from_slice(&self.shape[*index_shape.last().unwrap()..]);
        Ok(Array::new(new_shape, new_data))
    }
    fn pick_single(&self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot pick from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        let mut picked = self.data.clone();
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let s = s as isize;
            if i >= s || i < -s {
                if let Some(fill) = env.fill::<T>() {
                    picked = cowslice![fill; row_len];
                    continue;
                }
                return Err(env
                    .error(format!(
                        "Index {i} is out of bounds of length {s} (dimension {d}) in shape {}",
                        self.format_shape()
                    ))
                    .fill());
            }
            let i = if i >= 0 { i as usize } else { (s + i) as usize };
            let start = i * row_len;
            let end = start + row_len;
            picked = picked.slice(start..end);
        }
        let shape = Shape::from(&self.shape[index.len()..]);
        Ok(Array::new(shape, picked))
    }
    fn unpick(
        self,
        index_shape: &[usize],
        index_data: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if index_shape.len() <= 1 {
            self.unpick_single(index_data, into, env)
        } else {
            self.unpick_multi(index_shape, index_data, into, env)
        }
    }
    fn unpick_multi(
        self,
        index_shape: &[usize],
        index_data: &[isize],
        mut into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let expected_shape: Shape = index_shape[..index_shape.len() - 1]
            .iter()
            .chain(&into.shape[index_shape[index_shape.len() - 1]..])
            .copied()
            .collect();
        if self.shape != expected_shape {
            return Err(env.error(format!(
                "Attempted to undo pick, but the shape of the selected \
                array changed from {} to {}",
                FormatShape(&expected_shape),
                self.format_shape()
            )));
        }
        let index_row_len: usize = index_shape[1..].iter().product();
        if index_row_len == 0 {
            for from in self.into_rows() {
                into = from.unpick(&index_shape[1..], index_data, into, env)?;
            }
        } else {
            for (index_row, from) in index_data.chunks(index_row_len).zip(self.into_rows()) {
                into = from.unpick(&index_shape[1..], index_row, into, env)?;
            }
        }
        Ok(into)
    }
    fn unpick_single(self, index: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        let expected_shape = &into.shape()[index.len()..];
        if self.shape != expected_shape {
            return Err(env.error(format!(
                "Attempted to undo pick, but the shape of the selected \
                array changed from {} to {}",
                FormatShape(expected_shape),
                self.format_shape()
            )));
        }
        let mut start = 0;
        for (i, (&ind, &f)) in index.iter().zip(into.shape()).enumerate() {
            let ind = if ind >= 0 {
                ind as usize
            } else {
                (f as isize + ind) as usize
            };
            start += ind * into.shape[i + 1..].iter().product::<usize>();
        }
        into.data.modify(|data| {
            for (f, i) in data.make_mut().iter_mut().skip(start).zip(self.data) {
                *f = i;
            }
        });
        Ok(into)
    }
}

impl Value {
    /// Use this value to `take` from another
    pub fn take(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        if from.rank() == 0 {
            return Err(env.error("Cannot take from scalar"));
        }
        let index = self.as_ints(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.take(&index, env)?),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => op_bytes_retry_fill(
                a,
                |a| Ok(a.take(&index, env)?.into()),
                |a| Ok(a.take(&index, env)?.into()),
            )?,
            Value::Complex(a) => Value::Complex(a.take(&index, env)?),
            Value::Char(a) => Value::Char(a.take(&index, env)?),
            Value::Box(a) => Value::Box(a.take(&index, env)?),
        })
    }
    /// Use this value to `drop` from another
    pub fn drop(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        if from.rank() == 0 {
            return Err(env.error("Cannot drop from scalar"));
        }
        let index = self.as_ints(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.drop(&index, env)?),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => Value::Byte(a.drop(&index, env)?),
            Value::Complex(a) => Value::Complex(a.drop(&index, env)?),
            Value::Char(a) => Value::Char(a.drop(&index, env)?),
            Value::Box(a) => Value::Box(a.drop(&index, env)?),
        })
    }
    pub(crate) fn untake(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_ints(env, "Index must be a list of integers")?;
        self.generic_bin_into(
            into,
            |a, b| a.untake(&index, b, env).map(Into::into),
            |a, b| a.untake(&index, b, env).map(Into::into),
            |a, b| a.untake(&index, b, env).map(Into::into),
            |a, b| a.untake(&index, b, env).map(Into::into),
            |a, b| a.untake(&index, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    pub(crate) fn undrop(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_ints(env, "Index must be a list of integers")?;
        self.generic_bin_into(
            into,
            |a, b| a.undrop(&index, b, env).map(Into::into),
            |a, b| a.undrop(&index, b, env).map(Into::into),
            |a, b| a.undrop(&index, b, env).map(Into::into),
            |a, b| a.undrop(&index, b, env).map(Into::into),
            |a, b| a.undrop(&index, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot undrop {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// `take` from this array
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(match index {
            [] => self,
            &[taking] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_taking = taking.unsigned_abs();
                let mut filled = false;
                self.data.modify(|data| {
                    if taking >= 0 {
                        if abs_taking > row_count {
                            if let Some(fill) = T::get_fill(env) {
                                filled = true;
                                data.extend(repeat(fill).take((abs_taking - row_count) * row_len));
                            } else {
                                return Err(env
                                    .error(format!(
                                        "Cannot take {} rows from array with {} row{} \
                                        outside a fill context",
                                        abs_taking,
                                        row_count,
                                        if row_count == 1 { "" } else { "s" }
                                    ))
                                    .fill());
                            }
                        } else {
                            data.truncate(abs_taking * row_len);
                        }
                    } else {
                        *data = if abs_taking > row_count {
                            if let Some(fill) = T::get_fill(env) {
                                filled = true;
                                repeat(fill)
                                    .take((abs_taking - row_count) * row_len)
                                    .chain(take(data))
                                    .collect()
                            } else {
                                return Err(env
                                    .error(format!(
                                        "Cannot take {} rows from array with {} row{} \
                                        outside a fill context",
                                        abs_taking,
                                        row_count,
                                        if row_count == 1 { "" } else { "s" }
                                    ))
                                    .fill());
                            }
                        } else {
                            take(data)
                                .into_iter()
                                .skip((row_count - abs_taking) * row_len)
                                .collect()
                        };
                    }
                    Ok(())
                })?;
                if let Some(s) = self.shape.get_mut(0) {
                    *s = if filled {
                        abs_taking
                    } else {
                        (*s).min(abs_taking)
                    };
                } else if filled {
                    self.shape.push(abs_taking);
                }
                self.validate_shape();
                self
            }
            &[taking, ref sub_index @ ..] => {
                if index.len() > self.rank() {
                    return Err(env.error(format!(
                        "Cannot take from rank {} array with index of length {}",
                        self.rank(),
                        index.len()
                    )));
                }
                let abs_taking = taking.unsigned_abs();
                if sub_index
                    .iter()
                    .zip(&self.shape[1..])
                    .all(|(&i, &s)| i.unsigned_abs() == s)
                {
                    return self.take(&[taking], env);
                }
                let mut new_rows = Vec::with_capacity(abs_taking);
                let mut arr = if taking >= 0 {
                    // Take in each row
                    for row in self.rows().take(abs_taking) {
                        new_rows.push(row.take(sub_index, env)?);
                    }
                    let mut arr = Array::from_row_arrays_infallible(new_rows);
                    // Extend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        if let Some(fill) = T::get_fill(env) {
                            let row_len = arr.row_len();
                            arr.data.extend(
                                repeat(fill).take((abs_taking - arr.row_count()) * row_len),
                            );
                        } else {
                            return Err(env
                                .error(format!(
                                    "Cannot take {} rows from array with {} row{} \
                                    outside a fill context",
                                    abs_taking,
                                    arr.row_count(),
                                    if arr.row_count() == 1 { "" } else { "s" }
                                ))
                                .fill());
                        }
                    }
                    arr
                } else {
                    // Take in each row
                    let start = self.row_count().saturating_sub(abs_taking);
                    for row in self.rows().skip(start) {
                        new_rows.push(row.take(sub_index, env)?);
                    }
                    let mut arr = Array::from_row_arrays_infallible(new_rows);
                    // Prepend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        if let Some(fill) = T::get_fill(env) {
                            let row_len = arr.row_len();
                            arr.data = repeat(fill)
                                .take((abs_taking - arr.row_count()) * row_len)
                                .chain(arr.data)
                                .collect();
                        } else {
                            return Err(env
                                .error(format!(
                                    "Cannot take {} rows from array with {} row{} \
                                    outside a fill context",
                                    abs_taking,
                                    arr.row_count(),
                                    if arr.row_count() == 1 { "" } else { "s" }
                                ))
                                .fill());
                        }
                    }
                    arr
                };
                arr.shape[0] = abs_taking;
                arr.validate_shape();
                arr
            }
        })
    }
    /// `drop` from this array
    pub fn drop(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(match index {
            [] => self,
            &[dropping] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_dropping = dropping.unsigned_abs();
                self.data.modify(|data| {
                    *data = if dropping >= 0 {
                        take(data)
                            .into_iter()
                            .skip(abs_dropping * row_len)
                            .collect()
                    } else {
                        take(data)
                            .into_iter()
                            .take((row_count.saturating_sub(abs_dropping)) * row_len)
                            .collect()
                    };
                });
                if self.shape.is_empty() {
                    self.shape.push(1);
                }
                self.shape[0] = self.shape[0].saturating_sub(abs_dropping);
                self.validate_shape();
                self
            }
            &[dropping, ref sub_index @ ..] => {
                if index.len() > self.rank() {
                    return Err(env.error(format!(
                        "Cannot drop from rank {} array with index of length {}",
                        self.rank(),
                        index.len()
                    )));
                }
                let abs_dropping = dropping.unsigned_abs();
                let mut new_rows = Vec::with_capacity(abs_dropping);
                let row_count = self.row_count();
                if dropping >= 0 {
                    for row in self.rows().skip(abs_dropping) {
                        new_rows.push(row.drop(sub_index, env)?);
                    }
                } else {
                    let end = row_count.saturating_sub(abs_dropping);
                    for row in self.rows().take(end) {
                        new_rows.push(row.drop(sub_index, env)?);
                    }
                };
                Array::from_row_arrays(new_rows, env)?
            }
        })
    }
    fn untake(self, index: &[isize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        self.untake_impl("take", "taken", index, into, env)
    }
    fn untake_impl(
        self,
        name: &str,
        past: &str,
        index: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let from = self;
        match from.rank().cmp(&into.rank()) {
            Ordering::Less => {
                if from.shape[..] != into.shape[1..] {
                    return Err(env.error(format!(
                        "Attempted to undo {name}, but the {past} section's rank was \
                        modified to be incompatible",
                    )));
                }
            }
            Ordering::Equal => {}
            Ordering::Greater => {
                return Err(env.error(format!(
                    "Attempted to undo {name}, but the {past} section's rank was modified from {} to {}",
                    into.rank(),
                    from.rank()
                )));
            }
        }
        Ok(match index {
            [] => into,
            &[untaking] => {
                let into = into.drop(&[untaking], env)?;
                if untaking >= 0 {
                    from.join(into, env)
                } else {
                    into.join(from, env)
                }?
            }
            &[untaking, ref sub_index @ ..] => {
                let abs_untaking = untaking.unsigned_abs();
                if abs_untaking != from.row_count() {
                    return Err(env.error(format!(
                        "Attempted to undo {name}, but the {past} section's row \
                        count was modified from {} to {}",
                        abs_untaking,
                        from.row_count()
                    )));
                }
                let into_row_count = into.row_count();
                let mut new_rows = Vec::with_capacity(into_row_count);
                if untaking >= 0 {
                    for (from, into) in from.rows().zip(into.rows()) {
                        new_rows.push(from.untake_impl(name, past, sub_index, into, env)?);
                    }
                    new_rows.extend(into.rows().skip(abs_untaking));
                } else {
                    let start = into_row_count.saturating_sub(abs_untaking);
                    new_rows.extend(into.rows().take(start));
                    for (from, into) in from.rows().zip(into.rows().skip(start)) {
                        new_rows.push(from.untake_impl(name, past, sub_index, into, env)?);
                    }
                }
                Array::from_row_arrays(new_rows, env)?
            }
        })
    }
    fn undrop(self, index: &[isize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index: Vec<isize> = index
            .iter()
            .zip(&into.shape)
            .map(|(&i, &s)| {
                if i >= 0 {
                    i - s as isize
                } else {
                    i + s as isize
                }
            })
            .collect();
        self.untake_impl("drop", "dropped", &index, into, env)
    }
}

impl Value {
    /// Use this value to `rotate` another
    pub fn rotate(&self, mut rotated: Self, env: &Uiua) -> UiuaResult<Self> {
        let by = self.as_ints(env, "Rotation amount must be a list of integers")?;
        #[cfg(feature = "bytes")]
        if env.fill::<f64>().is_some() {
            if let Value::Byte(bytes) = &rotated {
                rotated = bytes.convert_ref::<f64>().into();
            }
        }
        match &mut rotated {
            Value::Num(a) => a.rotate(&by, env)?,
            #[cfg(feature = "bytes")]
            Value::Byte(a) => a.rotate(&by, env)?,
            Value::Complex(a) => a.rotate(&by, env)?,
            Value::Char(a) => a.rotate(&by, env)?,
            Value::Box(a) => a.rotate(&by, env)?,
        }
        Ok(rotated)
    }
    pub(crate) fn rotate_depth(
        &self,
        mut rotated: Self,
        a_depth: usize,
        b_depth: usize,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let by = self.as_integer_array(env, "Rotation amount must be an array of integers")?;
        #[cfg(feature = "bytes")]
        if env.fill::<f64>().is_some() {
            if let Value::Byte(bytes) = &rotated {
                rotated = bytes.convert_ref::<f64>().into();
            }
        }
        match &mut rotated {
            Value::Num(a) => a.rotate_depth(by, b_depth, a_depth, env)?,
            #[cfg(feature = "bytes")]
            Value::Byte(a) => a.rotate_depth(by, b_depth, a_depth, env)?,
            Value::Complex(a) => a.rotate_depth(by, b_depth, a_depth, env)?,
            Value::Char(a) => a.rotate_depth(by, b_depth, a_depth, env)?,
            Value::Box(a) => a.rotate_depth(by, b_depth, a_depth, env)?,
        }
        Ok(rotated)
    }
}

impl<T: ArrayValue> Array<T> {
    /// `rotate` this array by the given amount
    pub fn rotate(&mut self, by: &[isize], env: &Uiua) -> UiuaResult {
        if by.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot rotate rank {} array with index of length {}",
                self.rank(),
                by.len()
            )));
        }
        let data = self.data.as_mut_slice();
        rotate(by, &self.shape, data);
        if let Some(fill) = env.fill::<T>() {
            fill_shift(by, &self.shape, data, fill);
        }
        Ok(())
    }
    pub(crate) fn rotate_depth(
        &mut self,
        by: Array<isize>,
        depth: usize,
        by_depth: usize,
        env: &Uiua,
    ) -> UiuaResult {
        self.depth_slices(&by, depth, by_depth, env, |ash, a, bsh, b, env| {
            if bsh.len() > 1 {
                return Err(env.error(format!("Cannot rotate by rank {} array", bsh.len())));
            }
            rotate(b, ash, a);
            Ok(())
        })
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
            for val in &mut data[..abs_offset] {
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
    /// Use this value to `select` from another
    pub fn select(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (indices_shape, indices_data) = self.as_shaped_indices(env)?;
        Ok(match from {
            Value::Num(a) => a.select_impl(indices_shape, &indices_data, env)?.into(),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => op_bytes_ref_retry_fill(
                a,
                |a| Ok(a.select_impl(indices_shape, &indices_data, env)?.into()),
                |a| Ok(a.select_impl(indices_shape, &indices_data, env)?.into()),
            )?,
            Value::Complex(a) => a.select_impl(indices_shape, &indices_data, env)?.into(),
            Value::Char(a) => a.select_impl(indices_shape, &indices_data, env)?.into(),
            Value::Box(a) => a.select_impl(indices_shape, &indices_data, env)?.into(),
        })
    }
    pub(crate) fn unselect(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (ind_shape, ind) = index.as_shaped_indices(env)?;
        let mut sorted_indices = ind.clone();
        sorted_indices.sort();
        if sorted_indices.windows(2).any(|win| win[0] == win[1]) {
            return Err(env.error("Cannot undo selection with duplicate indices"));
        }
        self.generic_bin_into(
            into,
            |a, b| a.unselect_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.unselect_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.unselect_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.unselect_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.unselect_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    fn select_impl(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            let row_count = indices_shape[0];
            let row_len = indices_shape[1..].iter().product();
            if row_len == 0 {
                let shape: Shape = indices_shape
                    .iter()
                    .chain(self.shape.iter().skip(1))
                    .copied()
                    .collect();
                return Ok(Array::new(shape, CowSlice::new()));
            }
            let mut rows = Vec::with_capacity(row_count);
            for indices_row in indices.chunks_exact(row_len) {
                rows.push(self.select_impl(&indices_shape[1..], indices_row, env)?);
            }
            Array::from_row_arrays(rows, env)
        } else {
            let mut res = self.select(indices, env)?;
            if indices_shape.is_empty() {
                res.shape.remove(0);
            }
            Ok(res)
        }
    }
    fn unselect_impl(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            Err(env.error("Cannot undo multi-dimensional selection"))
        } else {
            self.unselect(indices, into, env)
        }
    }
    fn select(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let mut selected = CowSlice::with_capacity(self.row_len() * indices.len());
        let row_len = self.row_len();
        let row_count = self.row_count();
        for &i in indices {
            let i = if i >= 0 {
                let ui = i as usize;
                if ui >= row_count {
                    if let Some(fill) = env.fill::<T>() {
                        selected.extend(repeat(fill).take(row_len));
                        continue;
                    }
                    return Err(env
                        .error(format!(
                            "Index {} is out of bounds of length {}",
                            i, row_count
                        ))
                        .fill());
                }
                ui
            } else {
                let pos_i = (row_count as isize + i) as usize;
                if pos_i >= row_count {
                    if let Some(fill) = env.fill::<T>() {
                        selected.extend(repeat(fill).take(row_len));
                        continue;
                    }
                    return Err(env
                        .error(format!(
                            "Index {} is out of bounds of length {}",
                            i, row_count
                        ))
                        .fill());
                }
                pos_i
            };
            let start = i * row_len;
            let end = start + row_len;
            selected.extend_from_slice(&self.data[start..end]);
        }
        let mut shape = self.shape.clone();
        if let Some(s) = shape.get_mut(0) {
            *s = indices.len();
        } else {
            shape.push(indices.len());
        }
        let arr = Array::new(shape, selected);
        arr.validate_shape();
        Ok(arr)
    }
    fn unselect(&self, indices: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        let shape_is_valid = self.row_count() == indices.len()
            || indices.len() == 1 && &into.shape()[1..] == self.shape();
        if !shape_is_valid {
            return Err(env.error(
                "Attempted to undo selection, but \
                the shape of the selected array changed",
            ));
        }
        let into_row_len = into.row_len();
        let into_row_count = into.row_count();
        let into_data = into.data.as_mut_slice();
        for (&i, row) in indices.iter().zip(self.row_slices()) {
            let i = if i >= 0 {
                let ui = i as usize;
                if ui >= into_row_count {
                    return Err(env
                        .error(format!(
                            "Index {} is out of bounds of length {}",
                            i, into_row_count
                        ))
                        .fill());
                }
                ui
            } else {
                let pos_i = (into_row_count as isize + i) as usize;
                if pos_i >= into_row_count {
                    return Err(env
                        .error(format!(
                            "Index {} is out of bounds of length {}",
                            i, into_row_count
                        ))
                        .fill());
                }
                pos_i
            };
            let start = i * into_row_len;
            let end = start + into_row_len;
            for (i, x) in (start..end).zip(row) {
                into_data[i] = x.clone();
            }
        }
        Ok(into)
    }
}

impl Value {
    /// Use this array to `windows` another
    pub fn windows(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let size_spec = self.as_ints(env, "Window size must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => a.windows(&size_spec, env)?.into(),
            #[cfg(feature = "bytes")]
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
                "Window size {isize_spec:?} has too many axes for shape {}",
                self.format_shape()
            )));
        }
        let mut size_spec = Vec::with_capacity(isize_spec.len());
        for (d, s) in self.shape.iter().zip(isize_spec) {
            if s.unsigned_abs() > *d {
                return Err(env.error(format!(
                    "Window size {s} is too large for axis of length {d}",
                )));
            }
            size_spec.push(if *s >= 0 {
                *s as usize
            } else {
                (*d as isize + 1 + *s).max(0) as usize
            });
        }
        // Determine the shape of the windows array
        let mut new_shape = Shape::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(self.shape.iter().zip(&size_spec).map(|(a, b)| a + 1 - *b));
        new_shape.extend_from_slice(&size_spec);
        new_shape.extend_from_slice(&self.shape[size_spec.len()..]);
        // Check if the window size is too large
        for (size, sh) in size_spec.iter().zip(&self.shape) {
            if *size > *sh {
                return Ok(Self::new(new_shape, CowSlice::new()));
            }
        }
        // Make a new window shape with the same rank as the windowed array
        let mut true_size: Vec<usize> = Vec::with_capacity(self.shape.len());
        true_size.extend(size_spec);
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
            for i in curr.iter_mut() {
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
}

impl<T: ArrayValue> Array<T> {
    /// Try to `find` this array in another
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let mut searched = searched;
        let mut local_searched: Self;
        let any_dim_greater = (self.shape().iter().rev())
            .zip(searched.shape().iter().rev())
            .any(|(a, b)| a > b);
        if self.rank() > searched.rank() || any_dim_greater {
            let mut filled = false;
            if any_dim_greater {
                // Fill
                if let Some(fill) = env.fill() {
                    let mut target_shape = searched.shape.clone();
                    target_shape[0] = self.row_count();
                    local_searched = searched.clone();
                    local_searched.fill_to_shape(&target_shape, fill);
                    searched = &local_searched;
                    filled = true;
                }
            }
            if !filled {
                return Err(env.error(format!(
                    "Cannot search for array of shape {} in array of shape {}",
                    self.format_shape(),
                    searched.format_shape()
                )));
            }
        }

        // Pad the shape of the searched-for array
        let mut searched_for_shape = self.shape.clone();
        while searched_for_shape.len() < searched.shape.len() {
            searched_for_shape.insert(0, 1);
        }

        // Calculate the pre-padded output shape
        let temp_output_shape: Shape = searched
            .shape
            .iter()
            .zip(&searched_for_shape)
            .map(|(s, f)| s + 1 - f)
            .collect();

        let mut data = EcoVec::from_elem(0, temp_output_shape.iter().product());
        let data_slice = data.make_mut();
        let mut corner = vec![0; searched.shape.len()];
        let mut curr = vec![0; searched.shape.len()];
        let mut k = 0;

        'windows: loop {
            // Reset curr
            for i in curr.iter_mut() {
                *i = 0;
            }
            // Search the window whose top-left is the current corner
            'items: loop {
                // Get index for the current item in the searched array
                let mut searched_index = 0;
                let mut stride = 1;
                for ((c, i), s) in corner.iter().zip(&curr).zip(&searched.shape).rev() {
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
                let same = if let Some(searched_for) = self.data.get(search_for_index) {
                    searched.data[searched_index].array_eq(searched_for)
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
                if corner[i] == searched.shape[i] - searched_for_shape[i] {
                    corner[i] = 0;
                } else {
                    corner[i] += 1;
                    continue 'windows;
                }
            }
            let mut arr = Array::new(temp_output_shape, data);
            arr.fill_to_shape(&searched.shape[..searched_for_shape.len()], 0);
            arr.validate_shape();
            break Ok(arr);
        }
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
        Ok(match elems.rank().cmp(&of.rank()) {
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
                let res = Array::new(shape, result_data);
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for elem in elems.rows() {
                    rows.push(elem.member(of, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if of.rank() - elems.rank() == 1 {
                    if elems.rank() == 0 {
                        let elem = &elems.data[0];
                        Array::from(of.data.iter().any(|of| elem.array_eq(of)) as u8)
                    } else {
                        of.rows().any(|r| *elems == r).into()
                    }
                } else {
                    let mut rows = Vec::with_capacity(of.row_count());
                    for of in of.rows() {
                        rows.push(elems.member(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
}

impl Value {
    /// Get the `index of` the rows of this value in another
    pub fn index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            searched_in,
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
    /// Get the `progressive index of` the rows of this value in another
    pub fn progressive_index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            searched_in,
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
    pub fn index_of(&self, searched_in: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let searched_for = self;
        Ok(match searched_for.rank().cmp(&searched_in.rank()) {
            Ordering::Equal => {
                let mut result_data = EcoVec::with_capacity(searched_for.row_count());
                let mut members = HashMap::with_capacity(searched_in.row_count());
                for (i, of) in searched_in.row_slices().enumerate() {
                    members.entry(ArrayCmpSlice(of)).or_insert(i);
                }
                for elem in searched_for.row_slices() {
                    result_data.push(
                        members
                            .get(&ArrayCmpSlice(elem))
                            .map(|i| *i as f64)
                            .unwrap_or(searched_in.row_count() as f64),
                    );
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data);
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(searched_for.row_count());
                for elem in searched_for.rows() {
                    rows.push(elem.index_of(searched_in, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if searched_in.rank() - searched_for.rank() == 1 {
                    if searched_for.rank() == 0 {
                        let searched_for = &searched_for.data[0];
                        Array::from(
                            searched_in
                                .data
                                .iter()
                                .position(|of| searched_for.array_eq(of))
                                .unwrap_or(searched_in.row_count())
                                as f64,
                        )
                    } else {
                        (searched_in
                            .row_slices()
                            .position(|r| {
                                r.len() == searched_for.data.len()
                                    && r.iter().zip(&searched_for.data).all(|(a, b)| a.array_eq(b))
                            })
                            .unwrap_or(searched_in.row_count()) as f64)
                            .into()
                    }
                } else {
                    let mut rows = Vec::with_capacity(searched_in.row_count());
                    for of in searched_in.rows() {
                        rows.push(searched_for.index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
    /// Get the `progressive index of` the rows of this array in another
    fn progressive_index_of(&self, searched_in: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let searched_for = self;
        Ok(match searched_for.rank().cmp(&searched_in.rank()) {
            Ordering::Equal => {
                let mut used = HashSet::new();
                let mut result_data = EcoVec::with_capacity(searched_for.row_count());
                if searched_for.rank() == 1 {
                    for elem in &searched_for.data {
                        let mut hasher = DefaultHasher::new();
                        elem.array_hash(&mut hasher);
                        let hash = hasher.finish();
                        result_data.push(
                            searched_in
                                .data
                                .iter()
                                .enumerate()
                                .find(|&(i, of)| elem.array_eq(of) && used.insert((hash, i)))
                                .map(|(i, _)| i)
                                .unwrap_or(searched_in.row_count())
                                as f64,
                        );
                    }
                    return Ok(Array::from(result_data));
                }
                'elem: for elem in searched_for.rows() {
                    for (i, of) in searched_in.rows().enumerate() {
                        let mut hasher = DefaultHasher::new();
                        elem.hash(&mut hasher);
                        let hash = hasher.finish();
                        if elem == of && used.insert((hash, i)) {
                            result_data.push(i as f64);
                            continue 'elem;
                        }
                    }
                    result_data.push(searched_in.row_count() as f64);
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data);
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(searched_for.row_count());
                for elem in searched_for.rows() {
                    rows.push(elem.progressive_index_of(searched_in, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if searched_in.rank() - searched_for.rank() == 1 {
                    if searched_for.rank() == 0 {
                        let searched_for = &searched_for.data[0];
                        Array::from(
                            searched_in
                                .data
                                .iter()
                                .position(|of| searched_for.array_eq(of))
                                .unwrap_or(searched_in.row_count())
                                as f64,
                        )
                    } else {
                        (searched_in
                            .rows()
                            .position(|r| r == *searched_for)
                            .unwrap_or(searched_in.row_count()) as f64)
                            .into()
                    }
                } else {
                    let mut rows = Vec::with_capacity(searched_in.row_count());
                    for of in searched_in.rows() {
                        rows.push(searched_for.progressive_index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
}
