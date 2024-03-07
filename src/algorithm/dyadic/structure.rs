//! Code for pick, select, take, and drop

use std::{
    cmp::Ordering,
    iter::{once, repeat},
    mem::replace,
};

use ecow::EcoVec;

#[cfg(feature = "bytes")]
use crate::algorithm::{op_bytes_ref_retry_fill, op_bytes_retry_fill};
use crate::{
    algorithm::FillContext,
    cowslice::{cowslice, CowSlice},
    Array, ArrayValue, FormatShape, Primitive, Shape, Uiua, UiuaResult, Value,
};

impl<T: Clone> Array<T> {
    pub(crate) fn remove_row(&mut self, index: usize) {
        let row_len = self.row_len();
        let data = self.data.as_mut_slice();
        let start = index * row_len;
        for i in start..data.len() - row_len {
            data[i] = data[i + row_len].clone();
        }
        let new_len = data.len() - row_len;
        self.data.truncate(new_len);
        self.shape[0] -= 1;
    }
    pub(crate) fn set_row(&mut self, index: usize, row: Self) {
        let row_len = row.row_len();
        let start = index * row_len;
        for (a, b) in self.data.as_mut_slice()[start..]
            .iter_mut()
            .zip(row.data.into_iter())
        {
            *a = b;
        }
    }
    pub(crate) fn insert_row(&mut self, index: usize, row: Self) {
        let row_len = row.row_len();
        self.data.reserve(row_len);
        self.data.extend_from_slice(&row.data);
        let start = index * row_len;
        self.data.as_mut_slice()[start..].rotate_right(row_len);
        self.shape[0] += 1;
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
    pub(crate) fn undo_pick(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (idx_shape, index_data) = index.as_shaped_indices(env)?;
        if idx_shape.len() > 1 {
            let last_axis_len = *idx_shape.last().unwrap();
            if last_axis_len == 0 {
                if idx_shape[..idx_shape.len() - 1].iter().any(|&n| n > 1) {
                    return Err(env.error("Cannot undo pick with duplicate indices"));
                }
            } else {
                let mut sorted_indices = Vec::with_capacity(index_data.len() / last_axis_len);
                for (i, index) in index_data.chunks(last_axis_len).enumerate() {
                    sorted_indices.push((i, index));
                }
                sorted_indices.sort_unstable_by_key(|(_, index)| *index);
                if sorted_indices.windows(2).any(|w| {
                    let (ai, a) = w[0];
                    let (bi, b) = w[1];
                    let same = a.iter().zip(b).enumerate().all(|(i, (&a, &b))| {
                        let a = if a >= 0 {
                            a as usize
                        } else {
                            into.shape()[i] - a.unsigned_abs()
                        };
                        let b = if b >= 0 {
                            b as usize
                        } else {
                            into.shape()[i] - b.unsigned_abs()
                        };
                        a == b
                    });
                    same && self.row(ai) != self.row(bi)
                }) {
                    return Err(env.error(
                        "Cannot undo pick with duplicate \
                        indices but different values",
                    ));
                }
            }
        }
        self.generic_bin_into(
            into,
            |a, b| a.undo_pick(idx_shape, &index_data, b, env).map(Into::into),
            |a, b| a.undo_pick(idx_shape, &index_data, b, env).map(Into::into),
            |a, b| a.undo_pick(idx_shape, &index_data, b, env).map(Into::into),
            |a, b| a.undo_pick(idx_shape, &index_data, b, env).map(Into::into),
            |a, b| a.undo_pick(idx_shape, &index_data, b, env).map(Into::into),
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
        let index_row_elems = index_shape[1..].iter().product();
        let index_last_axis_len = *index_shape.last().unwrap();
        let mut new_data =
            CowSlice::with_capacity(index_shape[..index_shape.len() - 1].iter().product());
        if index_row_elems == 0 {
            let row = self.pick(&index_shape[1..], index_data, env)?;
            for _ in 0..index_shape[0] {
                new_data.extend_from_slice(&row.data);
            }
        } else if index_shape[0] == 0 {
            if index_last_axis_len > self.shape.len() {
                return Err(env.error(format!(
                    "Cannot pick from rank {} array with index of length {}",
                    self.rank(),
                    index_last_axis_len
                )));
            }
        } else {
            for index_row in index_data.chunks(index_row_elems) {
                let row = self.pick(&index_shape[1..], index_row, env)?;
                new_data.extend_from_slice(&row.data);
            }
        }
        let mut new_shape = Shape::from(&index_shape[..index_shape.len() - 1]);
        new_shape.extend_from_slice(&self.shape[index_last_axis_len..]);
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
                match env.scalar_fill::<T>() {
                    Ok(fill) => {
                        picked = cowslice![fill; row_len];
                        continue;
                    }
                    Err(e) => {
                        return Err(env
                            .error(format!(
                                "Index {i} is out of bounds of length {s} (dimension {d}) in shape {}{e}",
                                self.shape()
                            ))
                            .fill());
                    }
                }
            }
            let i = if i >= 0 { i as usize } else { (s + i) as usize };
            let start = i * row_len;
            let end = start + row_len;
            picked = picked.slice(start..end);
        }
        let shape = Shape::from(&self.shape[index.len()..]);
        Ok(Array::new(shape, picked))
    }
    fn undo_pick(
        self,
        index_shape: &[usize],
        index_data: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if index_shape.len() <= 1 {
            self.unpick_single(index_data, into, env)
        } else {
            self.undo_pick_multi(index_shape, index_data, into, env)
        }
    }
    fn undo_pick_multi(
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
                self.shape()
            )));
        }
        let index_row_len: usize = index_shape[1..].iter().product();
        if index_row_len == 0 {
            for from in self.into_rows() {
                into = from.undo_pick(&index_shape[1..], index_data, into, env)?;
            }
        } else {
            for (index_row, from) in index_data.chunks(index_row_len).zip(self.into_rows()) {
                into = from.undo_pick(&index_shape[1..], index_row, into, env)?;
            }
        }
        Ok(into)
    }
    fn unpick_single(self, index: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        if into.rank() < index.len() {
            return Err(env.error(format!(
                "Cannot unpick into rank {} array with index of length {}",
                into.rank(),
                index.len()
            )));
        }
        let expected_shape = &into.shape()[index.len()..];
        if self.shape != expected_shape {
            return Err(env.error(format!(
                "Attempted to undo pick, but the shape of the selected \
                array changed from {} to {}",
                FormatShape(expected_shape),
                self.shape()
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
        for (f, i) in (into.data.as_mut_slice().iter_mut())
            .skip(start)
            .zip(self.data)
        {
            *f = i;
        }
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
    pub(crate) fn undo_take(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_ints(env, "Index must be a list of integers")?;
        self.generic_bin_into(
            into,
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    pub(crate) fn undo_drop(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_ints(env, "Index must be a list of integers")?;
        self.generic_bin_into(
            into,
            |a, b| a.undo_drop(&index, b, env).map(Into::into),
            |a, b| a.undo_drop(&index, b, env).map(Into::into),
            |a, b| a.undo_drop(&index, b, env).map(Into::into),
            |a, b| a.undo_drop(&index, b, env).map(Into::into),
            |a, b| a.undo_drop(&index, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot undo drop {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    pub(crate) fn undrop(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_ints(env, "Index must be a list of integers")?;
        from.generic_into(
            |a| a.undrop(&index, env).map(Into::into),
            |a| a.undrop(&index, env).map(Into::into),
            |a| a.undrop(&index, env).map(Into::into),
            |a| a.undrop(&index, env).map(Into::into),
            |a| a.undrop(&index, env).map(Into::into),
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// `take` from this array
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let map_keys = self.take_map_keys();
        let row_count = self.row_count();
        let mut arr = match index {
            [] => self,
            &[taking] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_taking = taking.unsigned_abs();
                let mut filled = false;
                if taking >= 0 {
                    if abs_taking > row_count {
                        match T::get_fill(env) {
                            Ok(fill) => {
                                filled = true;
                                self.data.extend_from_slice(&vec![
                                    fill;
                                    (abs_taking - row_count)
                                        * row_len
                                ]);
                            }
                            Err(e) => {
                                return Err(env
                                    .error(format!(
                                        "Cannot take {} rows from array with {} row{} \
                                        outside a fill context{e}",
                                        taking,
                                        row_count,
                                        if row_count == 1 { "" } else { "s" }
                                    ))
                                    .fill());
                            }
                        }
                    } else {
                        self.data.truncate(abs_taking * row_len);
                    }
                } else if abs_taking > row_count {
                    match T::get_fill(env) {
                        Ok(fill) => {
                            filled = true;
                            let new_data =
                                EcoVec::from_elem(fill, (abs_taking - row_count) * row_len);
                            let old_data = replace(&mut self.data, new_data.into());
                            self.data.extend_from_slice(&old_data);
                        }
                        Err(e) => {
                            return Err(env
                                .error(format!(
                                    "Cannot take {} rows from array with {} row{} \
                                    outside a fill context{e}",
                                    taking,
                                    row_count,
                                    if row_count == 1 { "" } else { "s" }
                                ))
                                .fill());
                        }
                    }
                } else {
                    self.data.as_mut_slice().rotate_right(abs_taking * row_len);
                    self.data.truncate(abs_taking * row_len);
                }
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
                        match T::get_fill(env) {
                            Ok(fill) => {
                                let row_len: usize =
                                    sub_index.iter().map(|&i| i.unsigned_abs()).product();
                                arr.data.extend(
                                    repeat(fill).take((abs_taking - arr.row_count()) * row_len),
                                );
                            }
                            Err(e) => {
                                return Err(env
                                    .error(format!(
                                        "Cannot take {} rows from array with {} row{} \
                                        outside a fill context{e}",
                                        abs_taking,
                                        arr.row_count(),
                                        if arr.row_count() == 1 { "" } else { "s" }
                                    ))
                                    .fill());
                            }
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
                        match T::get_fill(env) {
                            Ok(fill) => {
                                let row_len: usize =
                                    sub_index.iter().map(|&i| i.unsigned_abs()).product();
                                arr.data = repeat(fill)
                                    .take((abs_taking - arr.row_count()) * row_len)
                                    .chain(arr.data)
                                    .collect();
                            }
                            Err(e) => {
                                return Err(env
                                    .error(format!(
                                        "Cannot take {} rows from array with {} row{} \
                                        outside a fill context{e}",
                                        abs_taking,
                                        arr.row_count(),
                                        if arr.row_count() == 1 { "" } else { "s" }
                                    ))
                                    .fill());
                            }
                        }
                    }
                    arr
                };
                arr.shape = index.iter().map(|&i| i.unsigned_abs()).collect();
                arr.validate_shape();
                arr
            }
        };
        if let Some(mut map_keys) = map_keys {
            if let Some(taking) = index.first().copied() {
                if taking.unsigned_abs() > row_count {
                    return Err(env.error(format!(
                        "Cannot {} {} a map array",
                        Primitive::Fill.format(),
                        Primitive::Take.format()
                    )));
                }
                if taking >= 0 {
                    map_keys.take(taking.unsigned_abs());
                } else {
                    map_keys.drop(row_count - taking.unsigned_abs());
                }
            }
            arr.meta_mut().map_keys = Some(map_keys);
        }
        Ok(arr)
    }
    /// `drop` from this array
    pub fn drop(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot drop from scalar"));
        }
        let map_keys = self.take_map_keys();
        let row_count = self.row_count();
        let mut arr = match index {
            [] => self,
            &[dropping] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_dropping = dropping.unsigned_abs().min(row_count);
                if dropping >= 0 {
                    self.data.as_mut_slice().rotate_left(abs_dropping * row_len);
                    self.data.truncate((row_count - abs_dropping) * row_len);
                }
                self.data.truncate((row_count - abs_dropping) * row_len);
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
                if row_count == abs_dropping {
                    let mut shape = self.shape;
                    for (s, n) in shape
                        .iter_mut()
                        .zip(once(abs_dropping).chain(sub_index.iter().map(|&i| i.unsigned_abs())))
                    {
                        *s = s.saturating_sub(n);
                    }
                    Array::new(shape, CowSlice::new())
                } else {
                    Array::from_row_arrays(new_rows, env)?
                }
            }
        };
        if let Some(mut map_keys) = map_keys {
            if let Some(dropping) = index.first().copied() {
                if dropping >= 0 {
                    map_keys.drop(dropping.unsigned_abs());
                } else {
                    let taken = row_count.saturating_sub(dropping.unsigned_abs());
                    map_keys.take(taken);
                }
            }
            arr.meta_mut().map_keys = Some(map_keys);
        }
        Ok(arr)
    }
    fn undo_take(self, index: &[isize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        self.undo_take_impl("take", "taken", index, into, env)
    }
    fn undo_take_impl(
        self,
        name: &str,
        past: &str,
        index: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if self.map_keys().is_some() {
            return Err(env.error("Cannot undo take from map array"));
        }
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
                        new_rows.push(from.undo_take_impl(name, past, sub_index, into, env)?);
                    }
                    new_rows.extend(into.rows().skip(abs_untaking));
                } else {
                    let start = into_row_count.saturating_sub(abs_untaking);
                    new_rows.extend(into.rows().take(start));
                    for (from, into) in from.rows().zip(into.rows().skip(start)) {
                        new_rows.push(from.undo_take_impl(name, past, sub_index, into, env)?);
                    }
                }
                Array::from_row_arrays(new_rows, env)?
            }
        })
    }
    fn undo_drop(self, index: &[isize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        if self.map_keys().is_some() {
            return Err(env.error("Cannot undo drop from map array"));
        }
        let index: Vec<isize> = index
            .iter()
            .zip(&into.shape)
            .map(|(&i, &s)| {
                if i >= 0 {
                    (i - s as isize).min(0)
                } else {
                    (i + s as isize).max(0)
                }
            })
            .collect();
        self.undo_take_impl("drop", "dropped", &index, into, env)
    }
    fn undrop(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if self.map_keys().is_some() {
            return Err(env.error("Cannot undrop from map array"));
        }
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot undrop from array of rank {} \
                with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        Ok(match index {
            [] => self,
            &[0] => self,
            &[undropping] => {
                let fill = env
                    .scalar_fill::<T>()
                    .map_err(|e| env.error(format!("Cannot undrop without fill{e}")))?;
                let abs_undropping = undropping.unsigned_abs();
                let elem_count = self.shape().row().elements() * abs_undropping;
                self.data.extend(repeat(fill).take(elem_count));
                if undropping > 0 {
                    self.data.as_mut_slice().rotate_right(elem_count);
                }
                self.shape[0] += abs_undropping;
                self
            }
            &[undropping, ref sub_index @ ..] => {
                let mut rows = Vec::with_capacity(self.row_count());
                for row in self.into_rows() {
                    rows.push(row.undrop(sub_index, env)?);
                }
                let arr = Self::from_row_arrays_infallible(rows);
                arr.undrop(&[undropping], env)?
            }
        })
    }
}

impl Value {
    /// Use this value to `select` from another
    pub fn select(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (indices_shape, indices_data) = self.as_shaped_indices(env)?;
        Ok(match from {
            Value::Num(a) => a.select(indices_shape, &indices_data, env)?.into(),
            #[cfg(feature = "bytes")]
            Value::Byte(a) => op_bytes_ref_retry_fill(
                a,
                |a| Ok(a.select(indices_shape, &indices_data, env)?.into()),
                |a| Ok(a.select(indices_shape, &indices_data, env)?.into()),
            )?,
            Value::Complex(a) => a.select(indices_shape, &indices_data, env)?.into(),
            Value::Char(a) => a.select(indices_shape, &indices_data, env)?.into(),
            Value::Box(a) => a.select(indices_shape, &indices_data, env)?.into(),
        })
    }
    pub(crate) fn undo_select(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (ind_shape, ind) = index.as_shaped_indices(env)?;
        let mut sorted_indices: Vec<_> = ind.iter().copied().enumerate().collect();
        sorted_indices.sort_unstable_by_key(|(_, index)| *index);
        if sorted_indices.windows(2).any(|win| {
            let (ai, a) = win[0];
            let (bi, b) = win[1];
            let a = if a >= 0 {
                a as usize
            } else {
                into.row_count() - a.unsigned_abs()
            };
            let b = if b >= 0 {
                b as usize
            } else {
                into.row_count() - b.unsigned_abs()
            };
            a == b && self.row(ai) != self.row(bi)
        }) {
            return Err(env.error(
                "Cannot undo selection with duplicate \
                indices but different values",
            ));
        }
        self.generic_bin_into(
            into,
            |a, b| a.undo_select_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select_impl(ind_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select_impl(ind_shape, &ind, b, env).map(Into::into),
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
    /// `select` from this array
    fn select(&self, indices_shape: &[usize], indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
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
                rows.push(self.select(&indices_shape[1..], indices_row, env)?);
            }
            let mut arr = Array::from_row_arrays(rows, env)?;
            if let Some(label) = &self.meta().label {
                arr.meta_mut().label = Some(label.clone());
            }
            Ok(arr)
        } else {
            let mut res = self.select_impl(indices, env)?;
            if indices_shape.is_empty() {
                res.shape.remove(0);
            }
            Ok(res)
        }
    }
    fn undo_select_impl(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            Err(env.error("Cannot undo multi-dimensional selection"))
        } else {
            self.undo_select(indices_shape, indices, into, env)
        }
    }
    fn select_impl(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let mut selected = CowSlice::with_capacity(self.row_len() * indices.len());
        let row_len = self.row_len();
        let row_count = self.row_count();
        for &i in indices {
            let i = if i >= 0 {
                let ui = i as usize;
                if ui >= row_count {
                    match env.scalar_fill::<T>() {
                        Ok(fill) => {
                            selected.extend(repeat(fill).take(row_len));
                            continue;
                        }
                        Err(e) => {
                            return Err(env
                                .error(format!(
                                    "Index {} is out of bounds of length {}{e}",
                                    i, row_count
                                ))
                                .fill());
                        }
                    }
                }
                ui
            } else {
                let pos_i = (row_count as isize + i) as usize;
                if pos_i >= row_count {
                    match env.scalar_fill::<T>() {
                        Ok(fill) => {
                            selected.extend(repeat(fill).take(row_len));
                            continue;
                        }
                        Err(e) => {
                            return Err(env
                                .error(format!(
                                    "Index {} is out of bounds of length {}{e}",
                                    i, row_count
                                ))
                                .fill());
                        }
                    }
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
        Ok(Array::new(shape, selected))
    }
    fn undo_select(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        mut into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let shape_is_valid = self.row_count() == indices.len() || indices_shape.is_empty();
        if !shape_is_valid {
            return Err(env.error(
                "Attempted to undo selection, but \
                the shape of the selected array changed",
            ));
        }
        if indices_shape.is_empty() {
            undo_select_inner(once(self.data.as_slice()), indices, &mut into, env)?;
        } else {
            undo_select_inner(self.row_slices(), indices, &mut into, env)?;
        }
        Ok(into)
    }
}

fn undo_select_inner<'a, T: ArrayValue>(
    row_slices: impl Iterator<Item = &'a [T]>,
    indices: &[isize],
    into: &mut Array<T>,
    env: &Uiua,
) -> UiuaResult {
    let into_row_len = into.row_len();
    let into_row_count = into.row_count();
    let into_data = into.data.as_mut_slice();
    for (&index, row) in indices.iter().zip(row_slices) {
        let i = if index >= 0 {
            let uns_index = index as usize;
            if uns_index >= into_row_count {
                return Err(env
                    .error(format!(
                        "Index {} is out of bounds of length {}",
                        index, into_row_count
                    ))
                    .fill());
            }
            uns_index
        } else {
            let pos_i = (into_row_count as isize + index) as usize;
            if pos_i >= into_row_count {
                return Err(env
                    .error(format!(
                        "Index {} is out of bounds of length {}",
                        index, into_row_count
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
    Ok(())
}
