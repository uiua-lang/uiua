//! Code for pick, select, take, and drop

use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::HashSet,
    iter::{once, repeat},
};

use ecow::EcoVec;

use crate::{
    algorithm::{validate_size, FillContext},
    cowslice::{cowslice, extend_repeat_slice, CowSlice},
    grid_fmt::GridFmt,
    val_as_arr, Array, ArrayValue, FormatShape, Primitive, Shape, Uiua, UiuaResult, Value,
};

impl<T: Clone> Array<T> {
    #[track_caller]
    pub(crate) fn remove_row(&mut self, index: usize) {
        debug_assert!(
            index < self.row_count(),
            "Index {index} is out of bounds of {} rows",
            self.row_count()
        );
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
        let row_len = self.row_len();
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
    #[track_caller]
    pub(crate) fn remove_row(&mut self, index: usize) {
        val_as_arr!(self, |a| a.remove_row(index))
    }
    pub(crate) fn as_shaped_indices(
        &self,
        filled: bool,
        env: &Uiua,
    ) -> UiuaResult<(&[usize], Vec<isize>)> {
        Ok(match self {
            Value::Num(arr) => {
                let mut index_data = Vec::with_capacity(arr.element_count());
                for &n in &arr.data {
                    index_data.push(if n.fract() != 0.0 {
                        if filled {
                            isize::MAX
                        } else if n.fract().is_nan() {
                            return Err(env.error(format!(
                                "{} cannot be used as an index without a fill",
                                n.grid_string(false)
                            )));
                        } else {
                            return Err(env.error(format!(
                                "Index must be an array of integers, but {n} is not an integer"
                            )));
                        }
                    } else {
                        n as isize
                    });
                }
                (&arr.shape, index_data)
            }
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
    pub fn pick(self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        from.match_fill(env);
        let (index_shape, index_data) = self.as_shaped_indices(env.is_scalar_filled(&from), env)?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.pick(index_shape, &index_data, env)?),
            Value::Byte(a) => Value::Byte(a.pick(index_shape, &index_data, env)?),
            Value::Complex(a) => Value::Complex(a.pick(index_shape, &index_data, env)?),
            Value::Char(a) => Value::Char(a.pick(index_shape, &index_data, env)?),
            Value::Box(a) => Value::Box(a.pick(index_shape, &index_data, env)?),
        })
    }
    pub(crate) fn undo_pick(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (idx_shape, index_data) = index.as_shaped_indices(env.is_scalar_filled(&self), env)?;
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
                let depth = idx_shape.len() - 2;
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
                    same && self.depth_row(depth, ai) != self.depth_row(depth, bi)
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
        let fill = env.scalar_fill::<T>();
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let s = s as isize;
            if i >= s || i < -s {
                match &fill {
                    Ok(fill) => {
                        picked = cowslice![fill.clone(); row_len];
                        continue;
                    }
                    Err(e) => {
                        let message = format!(
                            "Index {i} is out of bounds of length {s} \
                            (dimension {d}) in shape {}{e}",
                            self.shape()
                        );
                        return Err(env.error(message).fill());
                    }
                }
            } else if i < 0 {
                if let Ok(fill) = &fill {
                    picked = cowslice![fill.clone(); row_len];
                    continue;
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
                "Attempted to undo pick, but the shape of the picked \
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
                "Attempted to undo pick, but the shape of the picked \
                array changed from {} to {}",
                FormatShape(expected_shape),
                self.shape()
            )));
        }
        let mut start = 0;
        let has_fill = env.scalar_fill::<T>().is_ok() || env.scalar_unfill::<T>().is_ok();
        for (i, (&ind, &s)) in index.iter().zip(into.shape()).enumerate() {
            let ind = if ind >= 0 {
                ind as usize
            } else {
                if has_fill {
                    return Ok(into);
                }
                (s as isize + ind) as usize
            };
            if ind >= s {
                return Ok(into);
            }
            start += ind * into.shape[i + 1..].iter().product::<usize>();
        }
        for (x, i) in (into.data.as_mut_slice().iter_mut())
            .skip(start)
            .zip(self.data)
        {
            *x = i;
        }
        Ok(into)
    }
}

impl Value {
    /// Use this value to `take` from another
    pub fn take(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index =
            self.as_ints_or_infs(env, "Taken amount must be a list of integers or infinity")?;
        from.take_impl(&index, env)
    }
    pub(crate) fn take_impl(
        mut self,
        index: &[Result<isize, bool>],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        self.match_fill(env);
        val_as_arr!(self, |a| a.take(index, env).map(Into::into))
    }
    /// Use this value to `drop` from another
    pub fn drop(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index =
            self.as_ints_or_infs(env, "Dropped amount must be a list of integers or infinity")?;
        val_as_arr!(from, |a| a.drop(&index, env).map(Into::into))
    }
    pub(crate) fn undo_take(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = match index.as_ints(env, None) {
            Ok(indices) => indices,
            Err(_) => {
                let with_infs = index
                    .as_ints_or_infs(env, "Taken amount must be a list of integers or infinity")?;
                let mut indices = Vec::with_capacity(with_infs.len());
                for (i, d) in with_infs.into_iter().zip(into.shape()) {
                    indices.push(i.unwrap_or(*d as isize));
                }
                indices
            }
        };
        self.generic_bin_into(
            into,
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| a.undo_take(&index, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot undo take {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    pub(crate) fn undo_drop(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = match index.as_ints(env, None) {
            Ok(indices) => indices,
            Err(_) => {
                let with_infs = index.as_ints_or_infs(
                    env,
                    "Dropped amount must be a list of integers or infinity",
                )?;
                let mut indices = Vec::with_capacity(with_infs.len());
                for (i, d) in with_infs.into_iter().zip(into.shape()) {
                    indices.push(i.unwrap_or(*d as isize));
                }
                indices
            }
        };
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
    pub(crate) fn anti_drop(&self, mut target: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_ints(env, "Index must be an integer or list of integers")?;
        target.match_fill(env);
        val_as_arr!(target, |a| a.anti_drop(&index, env).map(Into::into))
    }
    #[track_caller]
    pub(crate) fn drop_n(&mut self, n: usize) {
        val_as_arr!(self, |a| a.drop_front_n(n))
    }
}

impl<T: ArrayValue> Array<T> {
    /// `take` from this array
    pub fn take(mut self, index: &[Result<isize, bool>], env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            self.shape.push(1);
        }

        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot take from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }

        let mut fill = env.array_fill::<T>().map_err(Cow::Borrowed);
        if let Ok(fill_val) = &fill {
            if !self.shape[index.len()..].ends_with(&fill_val.shape) {
                fill = Err(Cow::Owned(format!(
                    "Cannot take {} {} of array with \
                    shape {} with fill of shape {}",
                    index.len(),
                    if index.len() == 1 { "axis" } else { "axes" },
                    self.shape,
                    fill_val.shape
                )));
            }
        }

        let map_keys = self.take_map_keys();
        let row_count = self.row_count();
        let mut arr = match index {
            [] => self,
            &[taking] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let taking = taking.unwrap_or(row_count as isize);
                let abs_taking = taking.unsigned_abs();
                let mut filled = false;
                if taking >= 0 {
                    if abs_taking > row_count {
                        match fill {
                            Ok(fill) => {
                                filled = true;
                                let fill_elems = fill.element_count();
                                if fill_elems > 0 {
                                    let reps = (abs_taking - row_count) * row_len / fill_elems;
                                    self.data.extend_repeat_slice(&fill.data, reps);
                                }
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
                    match fill {
                        Ok(fill) => {
                            filled = true;
                            let fill_elems = fill.element_count();
                            let diff = abs_taking - row_count;
                            if fill_elems > 0 {
                                let reps = diff * row_len / fill_elems;
                                self.data.extend_repeat_slice(&fill.data, reps);
                            }
                            self.data.as_mut_slice().rotate_right(diff * row_len);
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
                let taking = taking.unwrap_or(row_count as isize);
                let abs_taking = taking.unsigned_abs();
                if sub_index
                    .iter()
                    .zip(&self.shape[1..])
                    .all(|(&i, &s)| i.map_or(true, |i| i.unsigned_abs() == s))
                {
                    return self.take(&[Ok(taking)], env);
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
                        match fill {
                            Ok(fill) => {
                                let row_len: usize = (sub_index.iter())
                                    .chain(repeat(&Err(true)))
                                    .zip(&self.shape[1..])
                                    .map(|(&i, &s)| i.map_or(s, isize::unsigned_abs))
                                    .product();
                                let fill_elems = fill.element_count();
                                if fill_elems > 0 {
                                    let reps = (abs_taking - row_count) * row_len / fill_elems;
                                    arr.data.extend_repeat_slice(&fill.data, reps);
                                }
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
                        match fill {
                            Ok(fill) => {
                                let row_len: usize = (sub_index.iter())
                                    .chain(repeat(&Err(true)))
                                    .zip(&self.shape[1..])
                                    .map(|(&i, &s)| i.map_or(s, |i| i.unsigned_abs()))
                                    .product();
                                let fill_elems = fill.element_count();
                                let diff = abs_taking - row_count;
                                if fill_elems > 0 {
                                    let reps = diff * row_len / fill_elems;
                                    arr.data.extend_repeat_slice(&fill.data, reps);
                                }
                                arr.data.as_mut_slice().rotate_right(diff * row_len);
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
                arr.shape = self.shape;
                for (i, s) in index.iter().zip(&mut *arr.shape) {
                    *s = i.map_or(*s, isize::unsigned_abs);
                }
                arr.validate_shape();
                arr
            }
        };
        if let Some(mut map_keys) = map_keys {
            if let Some(taking) = index.first().copied() {
                let taking = taking.unwrap_or(row_count as isize);
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
    pub fn drop(mut self, index: &[Result<isize, bool>], env: &Uiua) -> UiuaResult<Self> {
        let map_keys = self.take_map_keys();
        if self.rank() == 0 {
            self.shape.push(1);
        }
        let row_count = self.row_count();
        let mut arr = match index {
            [] => self,
            &[dropping] => {
                let dropping = dropping.unwrap_or(row_count as isize);
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
                let dropping = dropping.unwrap_or(row_count as isize);
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
                    for (s, n) in shape.iter_mut().zip(once(&Err(true)).chain(sub_index)) {
                        *s = match n {
                            Ok(n) => s.saturating_sub(n.unsigned_abs()),
                            Err(_) => 0,
                        };
                    }
                    Array::new(shape, CowSlice::new())
                } else {
                    Array::from_row_arrays(new_rows, env)?
                }
            }
        };
        if let Some(mut map_keys) = map_keys {
            if let Some(dropping) = index.first().copied() {
                let abs_dropping = dropping.map_or(row_count, isize::unsigned_abs);
                if dropping.map_or(true, |i| i >= 0) {
                    map_keys.drop(abs_dropping);
                } else {
                    let taken = row_count.saturating_sub(abs_dropping);
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
        mut into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if self.map_keys().is_some() {
            return Err(env.error("Cannot undo take from map array"));
        }
        let into_rank = into.rank();
        if into.rank() == 0 {
            into.shape.push(1);
        }
        let from = self;
        match from.rank().cmp(&into.rank()) {
            Ordering::Less => {
                if from.shape[..] != into.shape[1..] {
                    return Err(env.error(format!(
                        "Attempted to undo {name}, but the {past} section's rank \
                        decreased from {into_rank} to {}",
                        from.rank()
                    )));
                }
            }
            Ordering::Equal => {}
            Ordering::Greater => {
                return Err(env.error(format!(
                    "Attempted to undo {name}, but the {past} section's rank \
                    increased from {into_rank} to {}",
                    from.rank()
                )));
            }
        }
        Ok(match index {
            [] => into,
            &[untaking] => {
                let into = into.drop(&[Ok(untaking)], env)?;
                if untaking >= 0 {
                    from.join(into, true, env)
                } else {
                    into.join(from, true, env)
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
    fn undo_drop(self, index: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        if self.map_keys().is_some() {
            return Err(env.error("Cannot undo drop from map array"));
        }
        if into.rank() == 0 {
            into.shape.push(1);
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
    fn anti_drop(mut self, mut index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let fill = env.array_fill::<T>().unwrap_or_else(|_| T::proxy().into());
        if self.shape.len() < index.len() {
            return Err(env.error(format!(
                "Index array specifies {} axes, \
                but the array is rank {}",
                index.len(),
                self.shape.len()
            )));
        }
        // Validate fill shape
        if !self.shape[index.len()..].ends_with(&fill.shape) {
            return Err(env.error(format!(
                "Cannot antidrop {} {} of array with \
                shape {} with fill of shape {}",
                index.len(),
                if index.len() == 1 { "axis" } else { "axes" },
                self.shape,
                fill.shape
            )));
        }

        // Trim trailing zeros
        while index.last() == Some(&0) {
            index = &index[..index.len() - 1];
        }

        if index.iter().all(|&p| p == 0) {
            return Ok(self);
        }

        // Single axis case
        if index.len() == 1 {
            let row_shape = self.shape.row();
            if !row_shape.ends_with(&fill.shape) {
                return Err(env.error(format!(
                    "Cannot antidrop array of shape {} \
                    with fill of shape {}",
                    self.shape, fill.shape
                )));
            }
            let n = index[0];
            let fill_elems = fill.shape.elements();
            let reps = if fill_elems == 0 {
                0
            } else {
                row_shape.elements() / fill_elems
            };
            self.data
                .extend_repeat_slice(&fill.data, n.unsigned_abs() * reps);
            if n > 0 {
                (self.data.as_mut_slice())
                    .rotate_right(n.unsigned_abs() * reps * fill.shape.elements());
            }
            self.shape[0] += n.unsigned_abs();
            self.validate_shape();
            return Ok(self);
        }

        let n = index.len();
        let contig_row_size: usize = self.shape[n - 1..].iter().product();
        let mut new_shape = self.shape.clone();
        for (p, d) in index.iter().zip(&mut *new_shape) {
            *d += p.unsigned_abs();
        }
        let elem_count = validate_size::<T>(new_shape.iter().copied(), env)?;
        let mut new_data = EcoVec::with_capacity(elem_count);
        if self.shape.contains(&0) {
            for _ in 0..elem_count / fill.shape.elements() {
                new_data.extend_from_slice(&fill.data);
            }
            return Ok(Array::new(new_shape, new_data));
        }

        // Calculate padding numbers
        let mut slice_sizes = Vec::with_capacity(n);
        for i in 0..n {
            let mut size = index[i].unsigned_abs();
            for j in i + 1..self.shape.len() {
                size *= index.get(j).copied().unwrap_or(0).unsigned_abs() + self.shape[j];
            }
            slice_sizes.push((size / fill.shape.elements()) as isize * index[i].signum());
        }
        let cap_size = slice_sizes.remove(0);
        let sliver_size = slice_sizes.pop().unwrap_or(0);
        // println!("cap_size: {cap_size}");
        // println!("sliver_size: {sliver_size}");
        // println!("slice_sizes: {slice_sizes:?}");
        let mut curr = vec![0; n - 1];
        let maxes = self.shape[..n - 1].to_vec();
        // println!("contig_row_size: {contig_row_size}");
        // println!("maxes: {maxes:?}");
        // Add leading cap
        if cap_size > 0 {
            extend_repeat_slice(&mut new_data, &fill.data, cap_size.unsigned_abs());
        }
        'outer: for i in 0.. {
            // println!("curr: {curr:?}");
            // Add leading slices
            for i in 0..n - 1 {
                if i != n - 2 && index[i + 1] > 0 && curr[i + 1..].iter().all(|&c| c == 0) {
                    // println!("  extend {i} {:?}", slice_sizes[i]);
                    extend_repeat_slice(&mut new_data, &fill.data, slice_sizes[i].unsigned_abs());
                }
            }
            // Add leading sliver
            if sliver_size > 0 {
                // println!("  sliver: {}", sliver_size);
                extend_repeat_slice(&mut new_data, &fill.data, sliver_size.unsigned_abs());
            }
            // Add array row
            new_data.extend_from_slice(&self.data[contig_row_size * i..][..contig_row_size]);
            // Add trailing sliver
            if sliver_size < 0 {
                // println!("  sliver: {}", sliver_size);
                extend_repeat_slice(&mut new_data, &fill.data, sliver_size.unsigned_abs());
            }
            // Add trailing slices
            for i in 0..n - 1 {
                if i != n - 2
                    && (curr[i + 1..].iter().enumerate()).all(|(j, &c)| c == maxes[i + 1 + j] - 1)
                {
                    // println!("  extend {i} {:?}", slice_sizes[i]);
                    if index[i + 1] < 0 {
                        extend_repeat_slice(
                            &mut new_data,
                            &fill.data,
                            slice_sizes[i].unsigned_abs(),
                        );
                    }
                }
            }
            // Increment curr
            for i in (0..n - 1).rev() {
                curr[i] += 1;
                if curr[i] == maxes[i] {
                    curr[i] = 0;
                } else {
                    continue 'outer;
                }
            }
            break;
        }
        // Add trailing cap
        if cap_size < 0 {
            extend_repeat_slice(&mut new_data, &fill.data, cap_size.unsigned_abs());
        }
        Ok(Array::new(new_shape, new_data))
    }
    #[track_caller]
    pub(crate) fn drop_front_n(&mut self, n: usize) {
        let row_len = self.row_len();
        let start = n * row_len;
        self.data = self.data.slice(start..);
        self.shape[0] -= n;
    }
}

impl Value {
    /// Use this value to `select` from another
    pub fn select(self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        from.match_fill(env);
        let (indices_shape, indices_data) =
            self.as_shaped_indices(env.is_scalar_filled(&from), env)?;
        val_as_arr!(from, |a| {
            let mut a = a;
            a.select(indices_shape, &indices_data, env).map(Into::into)
        })
    }
    pub(crate) fn undo_select(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let mut from = self;
        let (idx_shape, mut ind) = index.as_shaped_indices(env.is_scalar_filled(&from), env)?;
        let into_row_count = into.row_count();
        // Sort indices
        let mut sorted_indices: Vec<_> = ind.iter().copied().enumerate().collect();
        sorted_indices.sort_unstable_by_key(|(_, index)| {
            index_in_bounds(*index, into_row_count).then(|| normalize_index(*index, into_row_count))
        });
        // Remove rows that correspond to indices that are out of bounds
        // This is because those values came from a fill
        let mut idx_shape = Cow::Borrowed(idx_shape);
        let reduced = ind.iter().any(|&i| !index_in_bounds(i, into_row_count));
        if reduced {
            if from.rank() == 0 {
                return Ok(into);
            }
            let from_row_count = from.row_count();
            let mut n = 0;
            sorted_indices.reverse();
            sorted_indices.retain(|&(i, j)| {
                let keep = index_in_bounds(j, into_row_count) || i >= from_row_count;
                if !keep {
                    from.remove_row(i);
                    n += 1;
                }
                keep
            });
            sorted_indices.reverse();
            from.validate_shape();
            if idx_shape.is_empty() {
                idx_shape = Cow::Owned(vec![n]);
            } else {
                idx_shape.to_mut()[0] -= n;
            }
        }
        // Ensure there are no duplicate indices
        let depth = idx_shape.len().saturating_sub(1);
        let from_row_count = from.row_count();
        if sorted_indices.windows(2).any(|win| {
            let (ai, a) = win[0];
            let (bi, b) = win[1];
            if !index_in_bounds(a, into_row_count)
                || !index_in_bounds(b, into_row_count)
                || ai >= from_row_count
                || bi >= from_row_count
            {
                return false;
            }
            let a = normalize_index(a, into_row_count);
            let b = normalize_index(b, into_row_count);
            a == b && from.depth_row(depth, ai) != from.depth_row(depth, bi)
        }) {
            return Err(env.error(
                "Cannot undo selection with duplicate \
                indices but different values",
            ));
        }
        // Update indices
        if reduced {
            sorted_indices.sort_unstable_by_key(|(i, _)| *i);
            ind = sorted_indices.into_iter().map(|(_, i)| i).collect();
        }
        from.generic_bin_into(
            into,
            |a, b| a.undo_select(&idx_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select(&idx_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select(&idx_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select(&idx_shape, &ind, b, env).map(Into::into),
            |a, b| a.undo_select(&idx_shape, &ind, b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot undo selection of {} into {}",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    pub(crate) fn anti_select(self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        from.match_fill(env);
        let (indices_shape, indices_data) = self.as_shaped_indices(false, env)?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.anti_select(indices_shape, &indices_data, env)?),
            Value::Byte(a) => Value::Byte(a.anti_select(indices_shape, &indices_data, env)?),
            Value::Complex(a) => {
                Value::Complex(a.anti_select(indices_shape, &indices_data, env)?)
            }
            Value::Char(a) => Value::Char(a.anti_select(indices_shape, &indices_data, env)?),
            Value::Box(a) => Value::Box(a.anti_select(indices_shape, &indices_data, env)?),
        })
    }
    pub(crate) fn anti_pick(self, mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        from.match_fill(env);
        let (indices_shape, indices_data) = self.as_shaped_indices(false, env)?;
        val_as_arr!(from, |a| a
            .anti_pick(indices_shape, &indices_data, env)
            .map(Into::into))
    }
}

fn index_in_bounds(index: isize, len: usize) -> bool {
    let ui = index.unsigned_abs();
    if index >= 0 {
        ui < len
    } else {
        ui <= len
    }
}

#[track_caller]
fn normalize_index(index: isize, len: usize) -> usize {
    let ui = index.unsigned_abs();
    if index >= 0 {
        ui
    } else {
        len - ui
    }
}

fn indices_are_total(indices: &[isize], row_count: usize) -> bool {
    let mut max_normal = 0;
    let mut set = HashSet::new();
    for &i in indices {
        let normalized = normalize_index(i, row_count);
        if set.insert(normalized) {
            max_normal = max_normal.max(normalized);
        }
    }
    set.len() == row_count && max_normal == row_count.saturating_sub(1)
}

impl<T: ArrayValue> Array<T> {
    /// `select` from this array
    fn select(
        &mut self,
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
                rows.push(self.select(&indices_shape[1..], indices_row, env)?);
            }
            let mut arr = Array::from_row_arrays(rows, env)?;
            if let Some(label) = &self.meta().label {
                arr.meta_mut().label = Some(label.clone());
            }
            Ok(arr)
        } else {
            let mut selected = CowSlice::with_capacity(self.row_len() * indices.len());
            let row_len = self.row_len();
            let row_count = self.row_count();
            let fill = env.scalar_fill::<T>();
            let map_keys = self
                .is_map()
                .then(|| {
                    let indices_are_total = indices_are_total(indices, row_count);
                    if !indices_are_total {
                        return None;
                    }
                    let keys = self.take_map_keys().unwrap().normalized();
                    Some(val_as_arr!(keys, |keys| {
                        let mut keys = keys;
                        keys.select(indices_shape, indices, env).unwrap().into()
                    }))
                })
                .flatten();
            for &i in indices {
                let i = if i >= 0 {
                    let ui = i as usize;
                    if ui >= row_count {
                        match &fill {
                            Ok(fill) => {
                                selected.extend_repeat(fill, row_len);
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
                    match &fill {
                        Ok(fill) => {
                            selected.extend_repeat(fill, row_len);
                            continue;
                        }
                        Err(e) => {
                            let pos_i = (row_count as isize + i) as usize;
                            if pos_i >= row_count {
                                return Err(env
                                    .error(format!(
                                        "Index {} is out of bounds of length {}{e}",
                                        i, row_count
                                    ))
                                    .fill());
                            }
                            pos_i
                        }
                    }
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
            if indices_shape.is_empty() {
                shape.remove(0);
            }
            let mut array = Array::new(shape, selected);
            if let Some(map_keys) = map_keys {
                array.map(map_keys, &()).unwrap();
            }
            Ok(array)
        }
    }
    fn undo_select(
        self,
        indices_shape: &[usize],
        indices: &[isize],
        mut into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            return Err(env.error("Cannot undo multi-dimensional selection"));
        }
        let from = self;
        let indices_row_count = indices_shape.first().copied().unwrap_or(1);
        let into_rank = into.rank();
        let into_row_count = into.row_count();
        match from.rank().cmp(&into.rank()) {
            Ordering::Equal => {
                if !from.shape.iter().skip(1).eq(into.shape.iter().skip(1)) {
                    let mut original_shape = into.shape.row();
                    original_shape.insert(0, indices_row_count);
                    return Err(env.error(format!(
                        "Attempted to undo selection, but \
                        the shape of the selected array changed \
                        from {} to {}",
                        original_shape, from.shape
                    )));
                }
                if from.row_count() != indices_row_count {
                    if indices_shape.is_empty() {
                        // Replacing a row with multiple rows
                        let i = indices[0];
                        let i = normalize_index(i, into_row_count);
                        let into_row_len = into.row_len();
                        let from_row_count = from.row_count();
                        let from_element_count = from.element_count();
                        into.data.as_mut_slice()[i * into_row_len..].rotate_left(into_row_len);
                        into.data.truncate(into.data.len() - into_row_len);
                        into.data.extend_from_cowslice(from.data);
                        into.data.as_mut_slice()[i * into_row_len..]
                            .rotate_right(from_element_count);
                        into.shape[0] += from_row_count;
                        into.shape[0] -= 1;
                        into.validate_shape();
                        return Ok(into);
                    }
                    if from.row_count() < indices_row_count {
                        return Err(env.error(format!(
                            "Attempted to undo selection, but \
                            the length of the selected array changed \
                            from {indices_row_count} to {}",
                            from.row_count()
                        )));
                    }
                }
                // Replacing multiple rows with multiple rows
                let row_len = from.row_len();
                let data = into.data.as_mut_slice();
                for (&i, row) in indices.iter().zip(from.row_slices()) {
                    let i = normalize_index(i, into_row_count);
                    data[i * row_len..][..row_len].clone_from_slice(row);
                }
            }
            Ordering::Less => {
                // Replacing multiple rows with a repeated row
                if !into.shape.ends_with(&from.shape) {
                    return Err(env.error(format!(
                        "Cannot undo select of array with shape {} \
                        into array with shape {}",
                        from.shape, into.shape
                    )));
                }
                let n: usize = into.shape[1..into.rank() - from.rank()].iter().product();
                let row_len = from.element_count();
                let data = into.data.as_mut_slice();
                for &i in indices {
                    let i = normalize_index(i, into_row_count);
                    for j in 0..n {
                        data[i * n * row_len + j * row_len..][..row_len]
                            .clone_from_slice(&from.data);
                    }
                }
            }
            Ordering::Greater => {
                // Replacing each of multiple rows with multiple rows
                if from.row_count() != indices_row_count {
                    return Err(env.error(format!(
                        "Attempted to undo selection, but \
                        the length of the selected array changed \
                        from {indices_row_count} to {}",
                        from.row_count()
                    )));
                }
                if from.rank() - into.rank() > 1
                    || !from.shape.iter().skip(2).eq(into.shape.iter().skip(1))
                {
                    return Err(env.error(format!(
                        "Cannot undo select of array with shape {} \
                        into array with shape {}",
                        from.shape, into.shape
                    )));
                }
                let mut index_row_pairs: Vec<_> = indices
                    .iter()
                    .zip(from.into_rows())
                    .map(|(i, row)| (normalize_index(*i, into_row_count), row))
                    .collect();
                index_row_pairs.sort_unstable_by_key(|(i, _)| *i);
                let mut index_row_pairs = index_row_pairs.into_iter();
                let mut rows = Vec::with_capacity(into_row_count);
                let mut curr = None;
                for (i, into_row) in into.into_rows().enumerate() {
                    curr = curr.or_else(|| index_row_pairs.next());
                    if curr.as_ref().is_some_and(|(j, _)| i == *j) {
                        let (_, from_row) = curr.take().unwrap();
                        rows.extend(from_row.into_rows());
                    } else {
                        rows.push(into_row);
                    }
                }
                into = Array::from_row_arrays_infallible(rows);
                if into_rank == 0 {
                    into.shape = Shape::SCALAR;
                }
            }
        }

        Ok(into)
    }
    fn anti_select(
        self,
        indices_shape: &[usize],
        indices: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        // Validate shape
        if !self.shape.starts_with(indices_shape) {
            return Err(env.error(format!(
                "Cannot invert selection of shape {} array with shape {} indices",
                self.shape,
                FormatShape(indices_shape)
            )));
        }
        let row_shape: Shape = self.shape[indices_shape.len()..].into();
        // Normalize indices
        if let Some(i) = indices
            .iter()
            .find(|&&i| i < 0 && i.unsigned_abs() > indices.len())
        {
            return Err(env.error(format!(
                "Cannot invert selection with negative index {i}, \
                which is greater than the number of indices {}",
                indices.len()
            )));
        }
        let normalized_indices: Vec<usize> = indices
            .iter()
            .map(|&i| normalize_index(i, indices.len()))
            .collect();
        let row_count = normalized_indices
            .iter()
            .max()
            .map(|&max| max + 1)
            .unwrap_or(0);
        // Check indices totality
        let indices_are_total = indices_are_total(indices, row_count);
        // Get fill if not total
        let mut fill = None;
        let mut fill_rep = 0;
        if !indices_are_total {
            let fill_arr = env.array_fill::<T>().map_err(|e| {
                env.error(format!(
                    "Cannot invert selection of non-total indices without a fill{e}"
                ))
                .fill()
            })?;
            if !row_shape.ends_with(&fill_arr.shape) {
                return Err(env.error(format!(
                    "Cannot invert selection of shape {} array with shape {} fill",
                    self.shape, fill_arr.shape
                )));
            }
            fill_rep = row_shape[..row_shape.len() - fill_arr.shape.len()]
                .iter()
                .product();
            fill = Some(fill_arr);
        }
        // Rise indices
        let mut indices_rise: Vec<usize> = (0..normalized_indices.len()).collect();
        indices_rise.sort_unstable_by_key(|&i| normalized_indices[i]);
        let row_elems = row_shape.elements();
        // Init buffer
        let mut data = EcoVec::<T>::with_capacity(row_count * row_shape.elements());
        let mut next = 0;
        // Unselect
        for rise in indices_rise {
            let i = normalized_indices[rise];
            match i.cmp(&next) {
                Ordering::Greater => {
                    for _ in next..i {
                        for _ in 0..fill_rep {
                            data.extend_from_slice(&fill.as_ref().unwrap().data);
                        }
                    }
                }
                Ordering::Less
                    if !(data[data.len() - row_elems..].iter())
                        .zip(&self.data[rise * row_elems..][..row_elems])
                        .all(|(a, b)| a.array_eq(b)) =>
                {
                    return Err(env.error(
                        "Cannot invert selection with duplicate indices but different values",
                    ));
                }
                Ordering::Less => continue,
                Ordering::Equal => {}
            }
            data.extend_from_slice(&self.data[rise * row_elems..][..row_elems]);
            next = i + 1;
        }
        let mut shape = row_shape;
        shape.insert(0, row_count);
        Ok(Array::new(shape, data))
    }
    fn anti_pick(self, indices_shape: &[usize], indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        // Validate shape
        let cell_shape = if let [init @ .., _] = indices_shape {
            // if self.shape.len() < init.len() {
            if !self.shape.starts_with(init) {
                return Err(env.error(format!(
                    "Cannot invert pick of shape {} array with shape {} indices",
                    self.shape,
                    FormatShape(indices_shape)
                )));
            }
            Shape::from(&self.shape[init.len()..])
        } else {
            self.shape.clone()
        };
        let cell_size: usize = cell_shape.iter().product();
        let index_size = indices_shape.last().copied().unwrap_or(1);
        if indices.iter().any(|&i| i < 0) {
            return Err(env.error("Cannot invert pick with negative indices"));
        }
        // Normalize indices
        let normalized_indices: Vec<usize> = indices
            .iter()
            .map(|&i| normalize_index(i, indices.len()))
            .collect();
        let outer_rank = indices_shape.last().copied().unwrap_or(1);
        let mut outer_shape = Shape::from_iter(repeat(0).take(outer_rank));
        if !normalized_indices.is_empty() {
            for index in normalized_indices.chunks_exact(index_size) {
                for (d, &i) in outer_shape.iter_mut().zip(index) {
                    *d = i.max(*d);
                }
            }
            for d in outer_shape.iter_mut() {
                *d += 1;
            }
        }
        let outer_size: usize = outer_shape.iter().product();
        if indices.is_empty() {
            return Ok(if indices_shape == [0] {
                self
            } else {
                let mut shape = outer_shape;
                shape.extend(cell_shape);
                Array::new(shape, [])
            });
        }
        // Check indices totality
        let mut set = HashSet::new();
        let indices_are_total = indices.len() / index_size == outer_size
            && normalized_indices
                .chunks_exact(index_size)
                .all(|i| set.insert(i));
        let mut fill = None;
        let mut fill_rep = 0;
        // Get fill if not total
        if !indices_are_total {
            let fill_arr = env.array_fill::<T>().map_err(|e| {
                env.error(format!(
                    "Cannot invert pick of non-total indices without a fill{e}"
                ))
                .fill()
            })?;
            if !cell_shape.ends_with(&fill_arr.shape) {
                return Err(env.error(format!(
                    "Cannot invert pick of shape {} array with shape {} fill",
                    self.shape, fill_arr.shape
                )));
            }
            fill_rep = cell_shape[..cell_shape.len() - fill_arr.shape.len()]
                .iter()
                .product();
            fill = Some(fill_arr);
        }
        // Rise indices
        let mut indices_rise: Vec<usize> = (0..normalized_indices.len() / index_size).collect();
        indices_rise.sort_unstable_by_key(|&i| &normalized_indices[i * index_size..][..index_size]);
        // Init buffer
        let mut data = EcoVec::<T>::with_capacity(outer_size * cell_size);
        let mut next = 0;
        // Unpick
        for rise in indices_rise {
            let mut i = 0;
            let mut stride = 1;
            let index = &normalized_indices[rise * index_size..];
            for (&d, &j) in outer_shape.iter().zip(index).rev() {
                i += stride * j;
                stride *= d;
            }
            if i > next {
                for _ in next..i {
                    for _ in 0..fill_rep {
                        data.extend_from_slice(&fill.as_ref().unwrap().data);
                    }
                }
            } else if i < next
                && !(data[data.len() - cell_size..].iter())
                    .zip(&self.data[rise * cell_size..][..cell_size])
                    .all(|(a, b)| a.array_eq(b))
            {
                return Err(env.error(
                    "Cannot invert pick with duplicate \
                    indices but different values",
                ));
            }
            data.extend_from_slice(&self.data[rise * cell_size..][..cell_size]);
            next = i + 1;
        }
        if outer_size > next {
            for _ in next..outer_size {
                for _ in 0..fill_rep {
                    data.extend_from_slice(&fill.as_ref().unwrap().data);
                }
            }
        }
        let mut shape = outer_shape;
        shape.extend(cell_shape);
        Ok(Array::new(shape, data))
    }
}
