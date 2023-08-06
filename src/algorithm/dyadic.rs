use std::{
    cmp::Ordering,
    collections::BTreeMap,
    iter::repeat,
    mem::{swap, take},
    rc::Rc,
};

use ecow::EcoVec;

use crate::{
    algorithm::max_shape,
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
        on_success: impl FnOnce(Array<Rc<Function>>, Array<Rc<Function>>) -> UiuaResult<T>,
        on_error: impl FnOnce(&str, &str) -> E,
    ) -> UiuaResult<T> {
        match (self, other) {
            (Value::Func(a), Value::Func(b)) => on_success(a, b),
            (Value::Func(a), mut b) => {
                let shape = take(b.shape_mut());
                let new_data: CowSlice<_> = b
                    .into_flat_values()
                    .map(|b| {
                        Rc::new(Function {
                            id: FunctionId::Constant,
                            instrs: vec![Instr::Push(b.into()), Instr::Call(env.span_index())],
                            kind: FunctionKind::Normal,
                        })
                    })
                    .collect();
                let b = Array::new(shape, new_data);
                on_success(a, b)
            }
            (mut a, Value::Func(b)) => {
                let shape = take(a.shape_mut());
                let new_data: CowSlice<_> = a
                    .into_flat_values()
                    .map(|a| {
                        Rc::new(Function {
                            id: FunctionId::Constant,
                            instrs: vec![Instr::Push(a.into()), Instr::Call(env.span_index())],
                            kind: FunctionKind::Normal,
                        })
                    })
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
        self.shape = shape;
        let target_len: usize = self.shape.iter().product();
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
                "Cannot replicate array with shape {:?} with array of length {}",
                self.shape,
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
    pub fn pick(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_indices(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.pick(&index, env)?),
            Value::Byte(a) => Value::Byte(a.pick(&index, env)?),
            Value::Char(a) => Value::Char(a.pick(&index, env)?),
            Value::Func(a) => Value::Func(a.pick(&index, env)?),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn pick(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
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
                    "Index {i} is out of bounds of length {s} (dimension {d}) in shape {:?}",
                    self.shape
                )));
            }
        }
        self.data.modify(|data| {
            for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
                let row_len: usize = self.shape[d + 1..].iter().product();
                let i = if i >= 0 {
                    i as usize
                } else {
                    (s as isize + i) as usize
                };
                let start = i * row_len;
                let end = start + row_len;
                *data = take(data)
                    .into_iter()
                    .skip(start)
                    .take(end - start)
                    .collect();
            }
        });
        self.shape.drain(..index.len());
        Ok(self)
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
}

impl<T: ArrayValue> Array<T> {
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot take from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        self.data.modify(|data| {
            for (d, (&s, &taking)) in self.shape.iter().zip(index).enumerate() {
                let row_len: usize = self.shape[d + 1..].iter().product();
                if taking >= 0 {
                    let end = taking as usize * row_len;
                    if end > data.len() {
                        data.extend(repeat(T::fill_value()).take(end - data.len()));
                    } else {
                        data.truncate(end);
                    }
                } else {
                    let taking = taking.unsigned_abs();
                    let start_index = s.saturating_sub(taking);
                    let start = start_index * row_len;
                    *data = take(data).into_iter().skip(start).collect();
                    if taking > s {
                        let mut prefix: EcoVec<T> = repeat(T::fill_value())
                            .take((taking - s) * row_len)
                            .collect();
                        swap(data, &mut prefix);
                        data.extend(prefix);
                    }
                };
            }
        });
        for (s, i) in self.shape.iter_mut().zip(index) {
            *s = i.unsigned_abs();
        }
        self.validate_shape();
        Ok(self)
    }
    pub fn drop(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot drop from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        self.data.modify(|data| {
            for (d, (&s, &dropping)) in self.shape.iter().zip(index).enumerate() {
                let row_len: usize = self.shape[d + 1..].iter().product();
                if dropping >= 0 {
                    let start = dropping as usize * row_len;
                    *data = take(data).into_iter().skip(start).collect();
                } else {
                    let dropping = dropping.unsigned_abs();
                    let end_index = s.saturating_sub(dropping);
                    let end = end_index * row_len;
                    data.truncate(end);
                };
            }
        });
        for (s, i) in self.shape.iter_mut().zip(index) {
            *s = s.saturating_sub(i.unsigned_abs());
        }
        self.validate_shape();
        Ok(self)
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
                "Cannot fill array with shape {:?} with array with shape {:?}",
                self.shape, fill.shape
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
        let indices = self.as_indices(env, "Indices must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => a.select(&indices, env)?.into(),
            Value::Byte(a) => a.select(&indices, env)?.into(),
            Value::Char(a) => a.select(&indices, env)?.into(),
            Value::Func(a) => a.select(&indices, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn select(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot select from a rank 0 array"));
        }
        let mut selected = Vec::with_capacity(self.row_len() * indices.len());
        let row_len = self.row_len();
        for &i in indices {
            let i = if i >= 0 {
                i as usize
            } else {
                (self.shape[0] as isize + i) as usize
            };
            if i >= self.shape[0] {
                return Err(env.error(format!(
                    "Index {} is out of bounds of length {}",
                    i, self.shape[0]
                )));
            }
            let start = i * row_len;
            let end = start + row_len;
            selected.extend_from_slice(&self.data[start..end]);
        }
        let mut shape = self.shape.clone();
        shape[0] = indices.len();
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
                "Window size {size_spec:?} has too many axes for shape {:?}",
                self.shape
            )));
        }
        for (i, (size, sh)) in size_spec.iter().zip(&self.shape).enumerate() {
            if *size > *sh {
                return Err(env.error(format!(
                    "Cannot take window of size {size} along axis {i} of shape {:?}",
                    self.shape
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
        if self.rank() > searched.rank() {
            return Err(env.error(format!(
                "Cannot search for array of shape {:?} in array of shape {:?}",
                self.shape, searched.shape
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
                    searched.data[searched_index].eq(searched_for)
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
        let mut result_data = Vec::with_capacity(elems.row_count());
        'elem: for elem in elems.rows() {
            for of in of.rows() {
                if elem == of {
                    result_data.push(Byte::Value(1));
                    continue 'elem;
                }
            }
            result_data.push(Byte::Value(0));
        }
        let shape = self.shape.iter().cloned().take(1).collect();
        let res = Array::new(shape, result_data.into());
        res.validate_shape();
        res
    }
}

impl Value {
    pub fn index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        Ok(match (self, searched_in) {
            (Value::Num(a), Value::Num(b)) => a.index_of(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.index_of(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.index_of(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.index_of(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.index_of(&b.clone().convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().index_of(b, env)?.into(),
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
    fn index_of(&self, searched_in: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        if searched_in.rank() == 0 {
            return Err(env.error("Cannot look for indices of a scalar array"));
        }
        let searched_for = self;
        if searched_for.rank() == 0 {
            if searched_in.rank() != 1 {
                return Err(env.error(format!(
                    "Cannot look for indices of a scalar array in an array of shape {:?}",
                    searched_in.shape
                )));
            }
            return Ok((searched_in
                .data
                .iter()
                .position(|a| a.eq(&searched_for.data[0]))
                .unwrap_or_else(|| searched_in.row_count()) as f64)
                .into());
        } else {
            if searched_for.shape[1..] != searched_in.shape[1..] {
                return Err(env.error(format!(
                    "Cannot get index in array of different shape: {:?} vs {:?}",
                    searched_for.shape, searched_in.shape
                )));
            }
            let mut result = Vec::with_capacity(searched_for.row_count());
            let mut indices = BTreeMap::new();
            for (i, row) in searched_in.rows().enumerate() {
                indices.insert(row, i);
            }
            for row in searched_for.rows() {
                result.push(
                    indices
                        .get(&row)
                        .map(|i| *i as f64)
                        .unwrap_or_else(|| searched_in.row_count() as f64),
                );
            }
            Ok(Array::from(result))
        }
    }
}

impl Value {
    pub fn group(&self, grouped: &Self, env: &Uiua) -> UiuaResult<Self> {
        let indices = self.as_naturals(env, "Group indices must be a list of natural numbers")?;
        Ok(match grouped {
            Value::Num(a) => a.group(&indices)?.into(),
            Value::Byte(a) => a.group(&indices)?.into(),
            Value::Char(a) => a.group(&indices)?.into(),
            Value::Func(a) => a.group(&indices)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn group(&self, indices: &[usize]) -> UiuaResult<Self> {
        let Some(max_index) = indices.iter().max() else {
            return Ok((self.shape.clone(), Vec::new()).into());
        };
        let mut groups: Vec<Vec<Array<T>>> = vec![Vec::new(); max_index + 1];
        for (r, &g) in indices.iter().enumerate() {
            if g < self.row_count() {
                groups[g].push(self.row(r));
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
        let markers =
            self.as_naturals(env, "Partition markers must be a list of natural numbers")?;
        Ok(match partitioned {
            Value::Num(a) => a.partition(&markers)?.into(),
            Value::Byte(a) => a.partition(&markers)?.into(),
            Value::Char(a) => a.partition(&markers)?.into(),
            Value::Func(a) => a.partition(&markers)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn partition(&self, markers: &[usize]) -> UiuaResult<Self> {
        let mut groups = Vec::new();
        let mut last_marker = usize::MAX;
        for (row, &marker) in self.rows().zip(markers) {
            if marker != 0 {
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
