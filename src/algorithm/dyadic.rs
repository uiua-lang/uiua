use std::{cmp::Ordering, iter::repeat, mem::take, sync::Arc};

use crate::{
    array::*,
    cowslice::CowSlice,
    function::{Function, FunctionId, FunctionKind, Instr},
    value::Value,
    Byte, Uiua, UiuaResult,
};

impl Value {
    fn coerce_to_functions<T, E: ToString>(
        self,
        other: Self,
        env: &Uiua,
        on_success: impl FnOnce(Array<Arc<Function>>, Array<Arc<Function>>) -> UiuaResult<T>,
        on_error: impl FnOnce(&str, &str) -> E,
    ) -> UiuaResult<T> {
        match (self, other) {
            (Value::Func(a), Value::Func(b)) => on_success(a, b),
            (Value::Func(a), mut b) => {
                let shape = take(b.shape_mut());
                let new_data: CowSlice<_> = b
                    .into_flat_values()
                    .map(|b| {
                        Arc::new(Function::new(
                            FunctionId::Constant,
                            vec![Instr::Push(b.into()), Instr::Call(env.span_index())],
                            FunctionKind::Normal,
                        ))
                    })
                    .collect();
                let b = Array::new(shape, new_data);
                on_success(a, b)
            }
            (mut a, Value::Func(b)) => {
                let shape = take(a.shape_mut());
                let new_data: CowSlice<_> = a
                    .into_flat_values()
                    .map(|a| Arc::new(Function::constant(a)))
                    .collect();
                let a = Array::new(shape, new_data);
                on_success(a, b)
            }
            (a, b) => Err(env.error(on_error(a.type_name(), b.type_name()))),
        }
    }
}

impl Value {
    pub fn reshape(&mut self, shape: &Self, env: &Uiua) -> UiuaResult {
        let target_shape = shape.as_naturals(
            env,
            "Shape should be a single natural number \
            or a list of natural numbers",
        )?;
        match self {
            Value::Num(a) => a.reshape(target_shape),
            Value::Byte(a) => a.reshape(target_shape),
            Value::Char(a) => a.reshape(target_shape),
            Value::Func(a) => a.reshape(target_shape),
        }
        Ok(())
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn reshape(&mut self, shape: Vec<usize>) {
        if self.data.is_empty() {
            return;
        }
        let target_len: usize = shape.iter().product();
        self.shape = shape;
        if self.data.len() < target_len {
            let start = self.data.len();
            self.data.modify(|data| {
                data.reserve(target_len - data.len());
                for i in 0..target_len - start {
                    data.push(data[i % start].clone());
                }
            });
        } else {
            self.data.truncate(target_len);
        }
    }
}

impl Value {
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, true, env)
    }
    pub(crate) fn join_impl(self, other: Self, truncate: bool, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.join_impl(b, truncate)),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.join_impl(b, truncate)),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.join_impl(b, truncate)),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().join_impl(b, truncate)),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.join_impl(b.convert(), truncate)),
            (a, b) => a.coerce_to_functions(
                b,
                env,
                |a, b| Ok(Value::Func(a.join_impl(b, truncate))),
                |a, b| format!("Cannot join {a} array and {b} array"),
            )?,
        })
    }
}

fn max_shape(a: &[usize], b: &[usize]) -> Vec<usize> {
    let mut new_shape = vec![0; a.len().max(b.len())];
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

impl<T: ArrayValue> Array<T> {
    pub fn join(self, other: Self) -> Self {
        self.join_impl(other, true)
    }
    pub(crate) fn join_impl(mut self, mut other: Self, truncate: bool) -> Self {
        crate::profile_function!();
        if truncate {
            self.truncate();
            other.truncate();
        }
        let res = match self.rank().cmp(&other.rank()) {
            Ordering::Less => {
                let mut target_shape = max_shape(&self.shape, &other.shape);
                let row_shape = &target_shape[1..];
                self.fill_to_shape(row_shape);
                other.fill_to_shape(&target_shape);
                self.data.extend(other.data);
                target_shape[0] += 1;
                self.shape = target_shape;
                self
            }
            Ordering::Greater => {
                let mut target_shape = max_shape(&self.shape, &other.shape);
                let row_shape = &target_shape[1..];
                self.fill_to_shape(&target_shape);
                other.fill_to_shape(row_shape);
                self.data.extend(other.data);
                target_shape[0] += 1;
                self.shape = target_shape;
                self
            }
            Ordering::Equal => {
                if self.rank() == 0 {
                    debug_assert_eq!(other.rank(), 0);
                    self.data.extend(other.data.into_iter().next());
                    self.shape = vec![2];
                    self
                } else {
                    let new_row_shape = max_shape(&self.shape[1..], &other.shape[1..]);
                    for array in [&mut self, &mut other] {
                        let mut new_shape = new_row_shape.clone();
                        new_shape.insert(0, array.shape[0]);
                        array.fill_to_shape(&new_shape);
                    }
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self
                }
            }
        };
        res.validate_shape();
        res
    }
}

impl Value {
    pub fn replicate(&self, replicated: Self, env: &Uiua) -> UiuaResult<Self> {
        let amount =
            self.as_naturals(env, "Replication amount must be a list of natural numbers")?;
        Ok(match replicated {
            Value::Num(a) => Value::Num(a.replicate(&amount, env)?),
            Value::Byte(a) => Value::Byte(a.replicate(&amount, env)?),
            Value::Char(a) => Value::Char(a.replicate(&amount, env)?),
            Value::Func(a) => Value::Func(a.replicate(&amount, env)?),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn replicate(mut self, amount: &[usize], env: &Uiua) -> UiuaResult<Self> {
        if self.row_count() != amount.len() {
            return Err(env.error(format!(
                "Cannot replicate array with shape {} with array of length {}",
                self.format_shape(),
                amount.len()
            )));
        }
        if self.rank() == 0 {
            if amount.len() != 1 {
                return Err(env.error("Scalar array can only be replicated with a single number"));
            }
            let mut new_data = Vec::with_capacity(amount[0]);
            for _ in 0..amount[0] {
                new_data.push(self.data[0].clone());
            }
            self = new_data.into();
        } else if amount.iter().all(|&n| n <= 1) {
            let row_len = self.row_len();
            let mut new_row_count = self.row_count();
            self.data.modify(|data| {
                for (r, &n) in amount.iter().enumerate().rev() {
                    if n == 0 {
                        let mut iter = take(data).into_iter();
                        *data = iter.by_ref().take(r * row_len).collect();
                        data.extend(iter.skip(row_len));
                        new_row_count -= 1;
                    }
                }
            });
            self.shape[0] = new_row_count;
        } else {
            let mut new_data = Vec::new();
            let mut new_len = 0;
            for (row, &n) in self.rows().zip(amount) {
                new_len += n;
                for _ in 0..n {
                    new_data.extend_from_slice(&row.data);
                }
            }
            self.shape[0] = new_len;
            self.data = new_data.into();
        }
        self.validate_shape();
        Ok(self)
    }
}

impl Value {
    pub(crate) fn into_shaped_indices(self, env: &Uiua) -> UiuaResult<(Vec<usize>, Vec<isize>)> {
        Ok(match self {
            Value::Num(arr) => {
                let mut index_data = Vec::with_capacity(arr.flat_len());
                for n in arr.data {
                    if n.fract() != 0.0 {
                        return Err(env.error(format!(
                            "Index must be an array of integers, but {n} is not an integer"
                        )));
                    }
                    if n.is_fill_value() {
                        return Err(env.error("Index may not contain fill values"));
                    }
                    index_data.push(n as isize);
                }
                (arr.shape, index_data)
            }
            Value::Byte(arr) => {
                let mut index_data = Vec::with_capacity(arr.flat_len());
                for n in arr.data {
                    if n.is_fill_value() {
                        return Err(env.error("Index may not contain fill values"));
                    }
                    index_data.push(n.0 as isize);
                }
                (arr.shape, index_data)
            }
            value => {
                return Err(env.error(format!(
                    "Index must be an array of integers, not {}",
                    value.type_name()
                )))
            }
        })
    }
    pub fn pick(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let (index_shape, index_data) = self.into_shaped_indices(env)?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Byte(a) => Value::Byte(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Char(a) => Value::Char(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Func(a) => Value::Func(a.pick_shaped(&index_shape, &index_data, env)?),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn pick_shaped(
        &self,
        index_shape: &[usize],
        index_data: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if index_shape.len() <= 1 {
            self.pick(index_data, env)
        } else {
            let (shape, data) = self.pick_shaped_impl(index_shape, index_data, env)?;
            Ok(Array::new(shape, data.into()))
        }
    }
    fn pick_shaped_impl(
        &self,
        index_shape: &[usize],
        index_data: &[isize],
        env: &Uiua,
    ) -> UiuaResult<(Vec<usize>, Vec<T>)> {
        let index_row_len = index_shape[1..].iter().product();
        let mut new_data =
            Vec::with_capacity(index_shape[..index_shape.len() - 1].iter().product());
        for index_row in index_data.chunks(index_row_len) {
            let row = self.pick_shaped(&index_shape[1..], index_row, env)?;
            new_data.extend_from_slice(&row.data);
        }
        let mut new_shape = index_shape[0..index_shape.len() - 1].to_vec();
        new_shape.extend_from_slice(&self.shape[*index_shape.last().unwrap()..]);
        Ok((new_shape, new_data))
    }
    pub fn pick(&self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot pick from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let s = s as isize;
            if i >= s || i < -s {
                return Err(env.error(format!(
                    "Index {i} is out of bounds of length {s} (dimension {d}) in shape {}",
                    self.format_shape()
                )));
            }
        }
        let mut picked = self.data.clone();
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let i = if i >= 0 {
                i as usize
            } else {
                (s as isize + i) as usize
            };
            let start = i * row_len;
            let end = start + row_len;
            picked = picked.slice(start..end);
        }
        let shape = self.shape[index.len()..].to_vec();
        Ok((shape, picked).into())
    }
}

impl Value {
    pub fn take(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_indices(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.take(&index, env)?),
            Value::Byte(a) => Value::Byte(a.take(&index, env)?),
            Value::Char(a) => Value::Char(a.take(&index, env)?),
            Value::Func(a) => Value::Func(a.take(&index, env)?),
        })
    }
    pub fn drop(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_indices(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.drop(&index, env)?),
            Value::Byte(a) => Value::Byte(a.drop(&index, env)?),
            Value::Char(a) => Value::Char(a.drop(&index, env)?),
            Value::Func(a) => Value::Func(a.drop(&index, env)?),
        })
    }
    pub(crate) fn untake(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_indices(env, "Index must be a list of integers")?;
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.untake(&index, b, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.untake(&index, b, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.untake(&index, b, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.untake(&index, b, env)?),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.untake(&index, b.convert(), env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().untake(&index, b, env)?),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        })
    }
    pub(crate) fn undrop(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_indices(env, "Index must be a list of integers")?;
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.undrop(&index, b, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.undrop(&index, b, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.undrop(&index, b, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.undrop(&index, b, env)?),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.undrop(&index, b.convert(), env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().undrop(&index, b, env)?),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot undrop {} into {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(match index {
            [] => self,
            &[taking] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_taking = taking.unsigned_abs();
                self.data.modify(|data| {
                    if taking >= 0 {
                        if abs_taking > row_count {
                            data.extend(
                                repeat(T::fill_value()).take((abs_taking - row_count) * row_len),
                            );
                        } else {
                            data.truncate(abs_taking * row_len);
                        }
                    } else {
                        *data = if abs_taking > row_count {
                            repeat(T::fill_value())
                                .take((abs_taking - row_count) * row_len)
                                .chain(take(data))
                                .collect()
                        } else {
                            take(data)
                                .into_iter()
                                .skip((row_count - abs_taking) * row_len)
                                .collect()
                        };
                    }
                });
                if self.shape.is_empty() {
                    self.shape.push(abs_taking);
                } else {
                    self.shape[0] = abs_taking;
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
                    let mut arr = Array::from_row_arrays(new_rows);
                    // Extend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        let row_len = arr.row_len();
                        arr.data.extend(
                            repeat(T::fill_value()).take((abs_taking - arr.row_count()) * row_len),
                        );
                    }
                    arr
                } else {
                    // Take in each row
                    let start = self.row_count().saturating_sub(abs_taking);
                    for row in self.rows().skip(start) {
                        new_rows.push(row.take(sub_index, env)?);
                    }
                    let mut arr = Array::from_row_arrays(new_rows);
                    // Prepend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        let row_len = arr.row_len();
                        arr.data = repeat(T::fill_value())
                            .take((abs_taking - arr.row_count()) * row_len)
                            .chain(arr.data)
                            .collect();
                    }
                    arr
                };
                arr.shape[0] = abs_taking;
                arr.validate_shape();
                arr
            }
        })
    }
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
                let arr = Array::from_row_arrays(new_rows);
                arr.validate_shape();
                arr
            }
        })
    }
    fn untake(self, index: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        let from = self;
        if from.rank() != into.rank() {
            return Err(env.error(format!(
                "Cannot untake rank {} array into rank {} array",
                from.rank(),
                into.rank()
            )));
        }
        Ok(match index {
            [] => into,
            &[untaking] => {
                let abs_untaking = untaking.unsigned_abs();
                if from.row_count() != abs_untaking {
                    return Err(env.error(format!(
                        "Attempted to undo take, but the taken section's \
                        row count was modified from {} to {}",
                        abs_untaking,
                        from.row_count()
                    )));
                }
                let into_row_len = into.row_len();
                let into_row_count = into.row_count();
                into.data.modify(|data| {
                    if untaking >= 0 {
                        for (from, into) in from
                            .row_slices()
                            .zip(data.make_mut().chunks_exact_mut(into_row_len))
                        {
                            into.clone_from_slice(from);
                        }
                    } else {
                        let start = into_row_count.saturating_sub(abs_untaking);
                        for (from, into) in from
                            .row_slices()
                            .zip(data.make_mut().chunks_exact_mut(into_row_len).skip(start))
                        {
                            into.clone_from_slice(from);
                        }
                    }
                });
                into
            }
            &[untaking, ref sub_index @ ..] => {
                let abs_untaking = untaking.unsigned_abs();
                if from.row_count() != abs_untaking {
                    return Err(env.error(format!(
                        "Attempted to undo take, but the taken section's \
                        row count was modified from {} to {}",
                        abs_untaking,
                        from.row_count()
                    )));
                }
                let into_row_count = into.row_count();
                let mut new_rows = Vec::with_capacity(into_row_count);
                if untaking >= 0 {
                    for (from, into) in from.rows().zip(into.rows()) {
                        new_rows.push(from.untake(sub_index, into, env)?);
                    }
                    new_rows.extend(into.rows().skip(abs_untaking));
                } else {
                    let start = into_row_count.saturating_sub(abs_untaking);
                    new_rows.extend(into.rows().take(start));
                    for (from, into) in from.rows().zip(into.rows().skip(start)) {
                        new_rows.push(from.untake(sub_index, into, env)?);
                    }
                }
                let arr = Array::from_row_arrays(new_rows);
                arr.validate_shape();
                arr
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
        self.untake(&index, into, env)
    }
}

impl Value {
    pub fn rotate(&self, mut rotated: Self, env: &Uiua) -> UiuaResult<Self> {
        let by = self.as_indices(env, "Rotation amount must be a list of integers")?;
        match &mut rotated {
            Value::Num(a) => a.rotate(&by, env)?,
            Value::Byte(a) => a.rotate(&by, env)?,
            Value::Char(a) => a.rotate(&by, env)?,
            Value::Func(a) => a.rotate(&by, env)?,
        }
        Ok(rotated)
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn rotate(&mut self, by: &[isize], env: &Uiua) -> UiuaResult {
        if by.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot rotate rank {} array with index of length {}",
                self.rank(),
                by.len()
            )));
        }
        rotate(by, &self.shape, &mut self.data);
        Ok(())
    }
}

fn rotate<T>(by: &[isize], shape: &[usize], data: &mut [T]) {
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

impl Value {
    pub fn couple(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.couple(b).into(),
            (Value::Byte(a), Value::Byte(b)) => a.couple(b).into(),
            (Value::Char(a), Value::Char(b)) => a.couple(b).into(),
            (Value::Func(a), Value::Func(b)) => a.couple(b).into(),
            (Value::Num(a), Value::Byte(b)) => a.couple(b.convert()).into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().couple(b).into(),
            (a, b) => a.coerce_to_functions(
                b,
                env,
                |a, b| Ok(Value::Func(a.couple(b))),
                |a, b| format!("Cannot couple {a} array with {b} array"),
            )?,
        })
    }
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        match self {
            Value::Num(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Byte(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Char(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Func(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
        }
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
    pub fn fill_to_shape(&mut self, shape: &[usize]) {
        while self.rank() < shape.len() {
            self.shape.insert(0, 1);
        }
        if self.shape == shape {
            return;
        }
        let target_size = shape.iter().product();
        let mut new_data = vec![T::fill_value(); target_size];
        let mut curr = vec![0; shape.len()];
        for new_data_index in 0..target_size {
            data_index_to_shape_index(new_data_index, shape, &mut curr);
            if let Some(data_index) = shape_index_to_data_index(&curr, &self.shape) {
                new_data[new_data_index] = self.data[data_index].clone();
            }
        }
        self.data = new_data.into();
        self.shape = shape.to_vec();
    }
    pub fn couple(mut self, mut other: Self) -> Self {
        crate::profile_function!();
        if self.shape != other.shape {
            let new_shape = max_shape(&self.shape, &other.shape);
            self.fill_to_shape(&new_shape);
            other.fill_to_shape(&new_shape);
        }
        self.data.extend(other.data);
        self.shape.insert(0, 2);
        self.validate_shape();
        self
    }
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        if self.row_count() != 2 {
            return Err(env.error(format!(
                "Cannot uncouple array with {} rows",
                self.row_count()
            )));
        }
        let mut rows = self.into_rows();
        let first = rows.next().unwrap();
        let second = rows.next().unwrap();
        Ok((first, second))
    }
}

impl Value {
    pub fn fill(&mut self, fill: Self, env: &Uiua) -> UiuaResult {
        match (&mut *self, fill) {
            (Value::Num(a), Value::Num(b)) => a.fill(b, env),
            (Value::Byte(a), Value::Byte(b)) => a.fill(b, env),
            (Value::Char(a), Value::Char(b)) => a.fill(b, env),
            (Value::Func(a), Value::Func(b)) => a.fill(b, env),
            (Value::Num(a), Value::Byte(b)) => a.fill(b.convert(), env),
            (Value::Byte(a), Value::Num(b)) => {
                let mut a = a.clone().convert();
                a.fill(b, env)?;
                *self = a.into();
                Ok(())
            }
            (a, b) => Err(env.error(format!(
                "Cannot couple {} array with {} array",
                a.type_name(),
                b.type_name()
            ))),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn fill(&mut self, fill: Self, env: &Uiua) -> UiuaResult {
        if !self.shape.ends_with(&fill.shape) {
            return Err(env.error(format!(
                "Cannot fill array with shape {} with array with shape {}",
                self.format_shape(),
                fill.format_shape()
            )));
        }
        for (elem, fill) in self.data.iter_mut().zip(fill.data.iter().cycle()) {
            if elem.is_fill_value() {
                *elem = fill.clone();
            }
        }
        Ok(())
    }
}

impl Value {
    pub fn select(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let mut indices = Vec::with_capacity(self.flat_len());
        match self {
            Value::Num(arr) => {
                for &i in arr.data.iter() {
                    if i.is_fill_value() {
                        indices.push(None);
                        continue;
                    }
                    if i.fract() != 0.0 {
                        return Err(
                            env.error(format!("Indices must be integers, but {} is not", i))
                        );
                    }
                    indices.push(Some(i as isize));
                }
            }
            Value::Byte(arr) => {
                for i in arr.data.iter() {
                    indices.push(i.value().map(|i| i as isize));
                }
            }
            v => {
                return Err(env.error(format!(
                    "Indices must be an array of integers, but it is {}s",
                    v.type_name()
                )))
            }
        }
        let indices_shape = self.shape();
        Ok(match from {
            Value::Num(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Byte(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Char(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Func(a) => a.select_impl(indices_shape, &indices, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn select_impl(
        &self,
        indices_shape: &[usize],
        indices: &[Option<isize>],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            let row_count = indices_shape[0];
            let row_len = indices_shape[1..].iter().product();
            let mut rows = Vec::with_capacity(row_count);
            for row in indices.chunks_exact(row_len) {
                rows.push(self.select_impl(&indices_shape[1..], row, env)?);
            }
            Ok(Array::from_row_arrays(rows))
        } else {
            let mut res = self.select(indices, env)?;
            if indices_shape.is_empty() {
                res.shape.clear();
            }
            Ok(res)
        }
    }
    fn select(&self, indices: &[Option<isize>], env: &Uiua) -> UiuaResult<Self> {
        let mut selected = Vec::with_capacity(self.row_len() * indices.len());
        let row_len = self.row_len();
        let row_count = self.row_count();
        for &i in indices {
            if let Some(i) = i {
                let i = if i >= 0 {
                    let ui = i as usize;
                    if ui >= row_count {
                        return Err(env.error(format!(
                            "Index {} is out of bounds of length {}",
                            i, row_count
                        )));
                    }
                    ui
                } else {
                    let pos_i = (row_count as isize + i) as usize;
                    if pos_i >= row_count {
                        return Err(env.error(format!(
                            "Index {} is out of bounds of length {}",
                            i, row_count
                        )));
                    }
                    pos_i
                };
                let start = i * row_len;
                let end = start + row_len;
                selected.extend_from_slice(&self.data[start..end]);
            } else {
                selected.extend(repeat(T::fill_value()).take(row_len));
            }
        }
        let mut shape = self.shape.clone();
        if let Some(s) = shape.get_mut(0) {
            *s = indices.len();
        } else {
            shape.push(indices.len());
        }
        Ok((shape, selected).into())
    }
}

impl Value {
    pub fn windows(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let size_spec = self.as_naturals(env, "Window size must be a list of natural numbers")?;
        Ok(match from {
            Value::Num(a) => a.windows(&size_spec, env)?.into(),
            Value::Byte(a) => a.windows(&size_spec, env)?.into(),
            Value::Char(a) => a.windows(&size_spec, env)?.into(),
            Value::Func(a) => a.windows(&size_spec, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn windows(&self, size_spec: &[usize], env: &Uiua) -> UiuaResult<Self> {
        if size_spec.len() > self.shape.len() {
            return Err(env.error(format!(
                "Window size {size_spec:?} has too many axes for shape {}",
                self.format_shape()
            )));
        }
        for (i, (size, sh)) in size_spec.iter().zip(&self.shape).enumerate() {
            if *size > *sh {
                return Err(env.error(format!(
                    "Cannot take window of size {size} along axis {i} of shape {}",
                    self.format_shape()
                )));
            }
        }
        // Determine the shape of the windows array
        let mut new_shape = Vec::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(self.shape.iter().zip(size_spec).map(|(a, b)| a - b + 1));
        new_shape.extend(size_spec);
        new_shape.extend(&self.shape[size_spec.len()..]);
        // Make a new window shape with the same rank as the windowed array
        let mut true_size: Vec<usize> = Vec::with_capacity(self.shape.len());
        true_size.extend(size_spec);
        if true_size.len() < self.shape.len() {
            true_size.extend(&self.shape[true_size.len()..]);
        }

        let mut dst = Vec::new();
        let mut corner = vec![0; self.shape.len()];
        let mut curr = vec![0; self.shape.len()];
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
                dst.push(self.data[src_index].clone());
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
            break Ok((new_shape, dst).into());
        }
    }
}

impl Value {
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, searched) {
            (Value::Num(a), Value::Num(b)) => a.find(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.find(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.find(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.find(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.find(&b.clone().convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().find(b, env)?.into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot find {} in {} array",
                    a.type_name(),
                    b.type_name(),
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Array<Byte>> {
        if self.rank() > searched.rank() || self.length() > searched.length() {
            return Err(env.error(format!(
                "Cannot search for array of shape {} in array of shape {}",
                self.format_shape(),
                searched.format_shape()
            )));
        }

        // Pad the shape of the searched-for array
        let mut searched_for_shape = self.shape.clone();
        while searched_for_shape.len() < searched.shape.len() {
            searched_for_shape.insert(0, 1);
        }

        // Determine the ouput shape
        let output_shape: Vec<usize> = searched
            .shape
            .iter()
            .zip(&searched_for_shape)
            .map(|(a, b)| a + 1 - b)
            .collect();

        let mut data = Vec::new();
        let mut corner = vec![0; searched.shape.len()];
        let mut curr = vec![0; searched.shape.len()];

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
                    data.push(Byte::from(false));
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
                data.push(Byte::from(true));
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
            let arr = Array::new(output_shape, data.into());
            arr.validate_shape();
            break Ok(arr);
        }
    }
}

impl Value {
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, of) {
            (Value::Num(a), Value::Num(b)) => a.member(b),
            (Value::Byte(a), Value::Byte(b)) => a.member(b),
            (Value::Char(a), Value::Char(b)) => a.member(b),
            (Value::Func(a), Value::Func(b)) => a.member(b),
            (Value::Num(a), Value::Byte(b)) => a.member(&b.clone().convert()),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().member(b),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot look for members of {} array in {} array",
                    b.type_name(),
                    a.type_name()
                )))
            }
        }
        .into())
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn member(&self, of: &Self) -> Array<Byte> {
        let elems = self;
        match elems.rank().cmp(&of.rank()) {
            Ordering::Equal => {
                let mut result_data = Vec::with_capacity(elems.row_count());
                'elem: for elem in elems.rows() {
                    for of in of.rows() {
                        if elem == of {
                            result_data.push(Byte(1));
                            continue 'elem;
                        }
                    }
                    result_data.push(Byte(0));
                }
                let shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data.into());
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for elem in elems.rows() {
                    rows.push(elem.member(of));
                }
                Array::from_row_arrays(rows)
            }
            Ordering::Less => {
                if of.rank() - elems.rank() == 1 {
                    return (of.rows().any(|r| r == *elems)).into();
                } else {
                    let mut rows = Vec::with_capacity(of.row_count());
                    for of in of.rows() {
                        rows.push(elems.member(&of));
                    }
                    Array::from_row_arrays(rows)
                }
            }
        }
    }
}

impl Value {
    pub fn index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        Ok(match (self, searched_in) {
            (Value::Num(a), Value::Num(b)) => a.index_of(b).into(),
            (Value::Byte(a), Value::Byte(b)) => a.index_of(b).into(),
            (Value::Char(a), Value::Char(b)) => a.index_of(b).into(),
            (Value::Func(a), Value::Func(b)) => a.index_of(b).into(),
            (Value::Num(a), Value::Byte(b)) => a.index_of(&b.clone().convert()).into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().index_of(b).into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot look for indices of {} in {}",
                    a.type_name(),
                    b.type_name(),
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn index_of(&self, searched_in: &Array<T>) -> Array<f64> {
        let searched_for = self;
        match searched_for.rank().cmp(&searched_in.rank()) {
            Ordering::Equal => {
                let mut result_data = Vec::with_capacity(searched_for.row_count());
                'elem: for elem in searched_for.rows() {
                    for (i, of) in searched_in.rows().enumerate() {
                        if elem == of {
                            result_data.push(i as f64);
                            continue 'elem;
                        }
                    }
                    result_data.push(searched_in.row_count() as f64);
                }
                let shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data.into());
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(searched_for.row_count());
                for elem in searched_for.rows() {
                    rows.push(elem.index_of(searched_in));
                }
                Array::from_row_arrays(rows)
            }
            Ordering::Less => {
                if searched_in.rank() - searched_for.rank() == 1 {
                    return (searched_in
                        .rows()
                        .position(|r| r == *searched_for)
                        .unwrap_or(searched_in.row_count()) as f64)
                        .into();
                } else {
                    let mut rows = Vec::with_capacity(searched_in.row_count());
                    for of in searched_in.rows() {
                        rows.push(searched_for.index_of(&of));
                    }
                    Array::from_row_arrays(rows)
                }
            }
        }
    }
}

impl Value {
    pub fn group(&self, grouped: &Self, env: &Uiua) -> UiuaResult<Self> {
        let indices = self.as_indices(env, "Group indices must be a list of integers")?;
        Ok(match grouped {
            Value::Num(a) => a.group(&indices)?.into(),
            Value::Byte(a) => a.group(&indices)?.into(),
            Value::Char(a) => a.group(&indices)?.into(),
            Value::Func(a) => a.group(&indices)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn group(&self, indices: &[isize]) -> UiuaResult<Self> {
        let Some(&max_index) = indices.iter().max() else {
            return Ok((self.shape.clone(), Vec::new()).into());
        };
        let mut groups: Vec<Vec<Array<T>>> = vec![Vec::new(); max_index.max(0) as usize + 1];
        for (r, &g) in indices.iter().enumerate() {
            if g >= 0 && r < self.row_count() {
                groups[g as usize].push(self.row(r));
            }
        }
        let mut rows: Vec<Array<T>> = groups.into_iter().map(Self::from_row_arrays).collect();
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Ok(Self::from_row_arrays(rows))
    }
}

impl Value {
    pub fn partition(&self, partitioned: &Self, env: &Uiua) -> UiuaResult<Self> {
        let markers = self.as_indices(env, "Partition markers must be a list of integers")?;
        Ok(match partitioned {
            Value::Num(a) => a.partition(&markers)?.into(),
            Value::Byte(a) => a.partition(&markers)?.into(),
            Value::Char(a) => a.partition(&markers)?.into(),
            Value::Func(a) => a.partition(&markers)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn partition(&self, markers: &[isize]) -> UiuaResult<Self> {
        let mut groups = Vec::new();
        let mut last_marker = isize::MAX;
        for (row, &marker) in self.rows().zip(markers) {
            if marker > 0 {
                if marker != last_marker {
                    groups.push(Vec::new());
                }
                groups.last_mut().unwrap().push(row);
            }
            last_marker = marker;
        }
        let mut rows: Vec<Array<T>> = groups.into_iter().map(Self::from_row_arrays).collect();
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Ok(Self::from_row_arrays(rows))
    }
}
