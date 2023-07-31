use std::{
    cmp::{self, Ordering},
    collections::BTreeMap,
    mem::take,
};

use crate::{algorithm::max_shape, array::*, value::Value, Byte, Uiua, UiuaResult};

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
            (Value::Func(a), Value::Func(b)) => Value::Func(a.join_impl(b, truncate)),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().join_impl(b, truncate)),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.join_impl(b.convert(), truncate)),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot join {} array and {} array",
                    a.type_name(),
                    b.type_name()
                )))
            }
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
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let s = s as isize;
            if i > s || i < -s {
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
                let end = i * row_len;
                data.truncate(end);
            }
        });
        for (s, i) in self.shape.iter_mut().zip(index) {
            *s = i.unsigned_abs();
        }
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
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let s = s as isize;
            if i > s || i < -s {
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
                *data = take(data).into_iter().skip(start).collect();
            }
        });
        for (s, i) in self.shape.iter_mut().zip(index) {
            *s -= i.unsigned_abs();
        }
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
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot couple {} array with {} array",
                    a.type_name(),
                    b.type_name()
                )))
            }
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
        if fill.rank() != 0 {
            return Err(env.error("Cannot fill with non-scalar array"));
        }
        let fill = fill.data.into_iter().next().unwrap();
        for elem in &mut *self.data {
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

        let mut dst = Vec::new();
        let mut corner = vec![0; searched.shape.len()];
        let mut curr = vec![0; searched.shape.len()];

        'windows: loop {
            // Reset curr
            for i in curr.iter_mut() {
                *i = 0;
            }
            // Search the window at the current corner
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
                    dst.push(Byte::from(false));
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
                dst.push(Byte::from(true));
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
            break Ok((output_shape, dst).into());
        }
    }
}

impl Value {
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, of) {
            (Value::Num(a), Value::Num(b)) => a.member(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.member(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.member(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.member(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.member(&b.clone().convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().member(b, env)?.into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot look for members of {} in {}",
                    b.type_name(),
                    a.type_name()
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<Byte>> {
        let shape = cmp::max_by_key(self.shape(), of.shape(), |s| s.len());
        let mut result = Vec::with_capacity(shape.iter().product());
        member(self, of, &mut result, env)?;
        Ok((shape.to_vec(), result).into())
    }
}

fn member<A: Arrayish>(
    elems: &A,
    of: &impl Arrayish<Value = A::Value>,
    result: &mut Vec<Byte>,
    env: &Uiua,
) -> UiuaResult {
    match elems.rank().cmp(&of.rank()) {
        Ordering::Equal => {
            if elems.shape()[1..] != of.shape()[1..] {
                return Err(env.error(format!(
                    "Cannot compare arrays of different shapes: {:?} and {:?}",
                    elems.shape(),
                    of.shape()
                )));
            }
            'elem: for elem in elems.rows() {
                for of in of.rows() {
                    if elem.len() == of.len() && elem.iter().zip(of).all(|(a, b)| a.eq(b)) {
                        result.push(Byte::Value(1));
                        continue 'elem;
                    }
                }
                result.push(Byte::Value(0));
            }
        }
        Ordering::Greater => {
            for row in elems.rows() {
                let row = (&elems.shape()[1..], row);
                member(&row, of, result, env)?;
            }
        }
        Ordering::Less => {
            for row in of.rows() {
                let row = (&of.shape()[1..], row);
                member(elems, &row, result, env)?;
            }
        }
    }
    Ok(())
}

impl Value {
    pub fn index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        Ok(match (self, searched_in) {
            (Value::Num(a), Value::Num(b)) => a.index_of(b, env)?.into(),
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
        let mut rows = groups
            .into_iter()
            .map(|row_arrays| Self::from_row_arrays(row_arrays))
            .collect::<UiuaResult<Vec<_>>>()?;
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Self::from_row_arrays(rows)
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
        let mut rows = groups
            .into_iter()
            .map(|row_arrays| Self::from_row_arrays(row_arrays))
            .collect::<UiuaResult<Vec<_>>>()?;
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Self::from_row_arrays(rows)
    }
}
