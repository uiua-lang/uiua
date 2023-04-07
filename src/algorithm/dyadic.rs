use std::{
    cmp::{self, Ordering},
    collections::BTreeMap,
    iter::repeat,
};

use crate::{array::*, value::Value, Byte, Uiua, UiuaResult};

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
            self.data.reserve(target_len - self.data.len());
            let start = self.data.len();
            for i in 0..target_len - start {
                self.data.push(self.data[i % start].clone());
            }
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
            (Value::Num(a), Value::Num(b)) => Value::Num(a.join_impl(b, truncate, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.join_impl(b, truncate, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.join_impl(b, truncate, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.join_impl(b, truncate, env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().join_impl(b, truncate, env)?),
            (Value::Num(a), Value::Byte(b)) => {
                Value::Num(a.join_impl(b.convert(), truncate, env)?)
            }
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
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, true, env)
    }
    pub(crate) fn join_impl(
        mut self,
        mut other: Self,
        truncate: bool,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        let res = match (self.shape.as_slice(), other.shape.as_slice()) {
            ([], []) => {
                self.data.extend(other.data);
                self.shape = vec![self.flat_len()];
                self
            }
            ([], bsh) => {
                if bsh.len() == 1 {
                    if truncate {
                        other.truncate();
                    }
                    other.data.insert(0, self.data.remove(0));
                    other.shape = vec![other.flat_len()];
                    other
                } else {
                    return Err(env.error(format!(
                        "Cannot join a scalar to a rank {} array",
                        bsh.len()
                    )));
                }
            }
            (ash, []) => {
                if ash.len() == 1 {
                    if truncate {
                        self.truncate();
                    }
                    self.data.push(other.data.remove(0));
                    self.shape = vec![self.flat_len()];
                    self
                } else {
                    return Err(env.error(format!(
                        "Cannot join a rank {} array to a scalar",
                        ash.len()
                    )));
                }
            }
            (ash, bsh) => {
                if ash[1..] == bsh[1..] {
                    if truncate {
                        self.truncate();
                        other.truncate();
                    }
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self.reset_fill();
                    self
                } else if ash[1..] == bsh[..] {
                    if truncate {
                        self.truncate();
                    }
                    self.data.extend(other.data);
                    self.shape[0] += 1;
                    self.reset_fill();
                    self
                } else if ash[..] == bsh[1..] {
                    if truncate {
                        other.truncate();
                    }
                    self.data.extend(other.data);
                    self.shape.insert(0, other.shape[0] + 1);
                    self.reset_fill();
                    self
                } else if self.fill || other.fill {
                    if truncate {
                        self.truncate();
                        other.truncate();
                    }
                    let ash = &self.shape;
                    let bsh = &other.shape;
                    let fill = T::fill_value();
                    match ash.len() as i8 - bsh.len() as i8 {
                        0 => {
                            let mut new_row_shape = vec![0; ash.len().max(bsh.len()) - 1];
                            for i in 0..new_row_shape.len() {
                                new_row_shape[i] =
                                    ash.get(i + 1).max(bsh.get(i + 1)).copied().unwrap();
                            }
                            let new_row_len: usize = new_row_shape.iter().product();
                            let mut new_shape = new_row_shape;
                            new_shape.insert(0, self.row_count() + other.row_count());
                            let mut new_data = Vec::with_capacity(new_shape.iter().product());
                            for row in self.rows().chain(other.rows()) {
                                new_data.extend(row.iter().cloned());
                                new_data.extend(repeat(fill.clone()).take(new_row_len - row.len()));
                            }
                            Array::new(new_shape, new_data)
                        }
                        1 => {
                            let mut new_row_shape = ash[1..].to_vec();
                            for (n, r) in new_row_shape.iter_mut().zip(bsh.iter()) {
                                *n = (*n).max(*r);
                            }
                            let new_row_len: usize = new_row_shape.iter().product();
                            let mut new_shape = new_row_shape;
                            new_shape.insert(0, ash[0] + 1);
                            let mut new_data = Vec::with_capacity(new_shape.iter().product());
                            for row in self.rows() {
                                new_data.extend(row.iter().cloned());
                                new_data.extend(repeat(fill.clone()).take(new_row_len - row.len()));
                            }
                            let other_data_len = other.data.len();
                            new_data.extend(other.data);
                            new_data.extend(repeat(fill).take(new_row_len - other_data_len));
                            Array::new(new_shape, new_data)
                        }
                        -1 => {
                            let mut new_row_shape = bsh[1..].to_vec();
                            for (n, r) in new_row_shape.iter_mut().zip(ash.iter()) {
                                *n = (*n).max(*r);
                            }
                            let new_row_len: usize = new_row_shape.iter().product();
                            let mut new_shape = new_row_shape;
                            new_shape.insert(0, bsh[0] + 1);
                            let mut new_data = Vec::with_capacity(new_shape.iter().product());
                            let self_data_len = self.data.len();
                            new_data.extend(self.data);
                            new_data.extend(repeat(fill.clone()).take(new_row_len - self_data_len));
                            for row in other.rows() {
                                new_data.extend(row.iter().cloned());
                                new_data.extend(repeat(fill.clone()).take(new_row_len - row.len()));
                            }
                            Array::new(new_shape, new_data)
                        }
                        diff => {
                            return Err(env.error(format!(
                                "Can only join arrays with a maximum rank difference of 1, \
                                but {ash:?} and {bsh:?} have a difference of {}",
                                diff.abs()
                            )))
                        }
                    }
                } else {
                    return Err(env.error(format!(
                        "Cannot join {} arrays with shapes {ash:?} and {bsh:?}",
                        T::NAME
                    )));
                }
            }
        };
        res.validate_shape();
        Ok(res)
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
            for (r, &n) in amount.iter().enumerate().rev() {
                if n == 0 {
                    self.data.drain(r * row_len..(r + 1) * row_len);
                    new_row_count -= 1;
                }
            }
            self.shape[0] = new_row_count;
        } else {
            let mut new_data = Vec::new();
            let mut new_len = 0;
            for (row, &n) in self.rows().zip(amount) {
                new_len += n;
                for _ in 0..n {
                    new_data.extend_from_slice(&row);
                }
            }
            self.shape[0] = new_len;
            self.data = new_data;
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
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let i = if i >= 0 {
                i as usize
            } else {
                (s as isize + i) as usize
            };
            let start = i * row_len;
            let end = start + row_len;
            self.data.drain(end..);
            self.data.drain(..start);
        }
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
            if i >= s || i < -s {
                return Err(env.error(format!(
                    "Index {i} is out of bounds of length {s} (dimension {d}) in shape {:?}",
                    self.shape
                )));
            }
        }
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let i = if i >= 0 {
                i as usize
            } else {
                (s as isize + i) as usize
            };
            let end = i * row_len;
            self.data.drain(end..);
        }
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
            if i >= s || i < -s {
                return Err(env.error(format!(
                    "Index {i} is out of bounds of length {s} (dimension {d}) in shape {:?}",
                    self.shape
                )));
            }
        }
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let i = if i >= 0 {
                i as usize
            } else {
                (s as isize + i) as usize
            };
            let start = i * row_len;
            self.data.drain(..start);
        }
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
            (Value::Num(a), Value::Num(b)) => a.couple(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.couple(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.couple(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.couple(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.couple(b.convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().couple(b, env)?.into(),
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

impl<T: ArrayValue> Array<T> {
    pub fn couple(mut self, mut other: Self, env: &Uiua) -> UiuaResult<Self> {
        if self.shape != other.shape {
            if self.fill || other.fill {
                let fill = T::fill_value();
                let mut new_shape = vec![0; self.shape.len().max(other.shape.len())];
                for i in 0..new_shape.len() {
                    new_shape[i] = self.shape.get(i).max(other.shape.get(i)).copied().unwrap();
                }
                let target_size = new_shape.iter().product();
                self.data.resize(target_size, fill.clone());
                self.shape = new_shape.clone();
                other.data.resize(target_size, fill);
                other.shape = new_shape;
            } else {
                return Err(env.error(format!(
                    "Cannot couple arrays of different shapes {:?} and {:?}",
                    self.shape, other.shape,
                )));
            }
        }
        self.data.append(&mut other.data);
        self.shape.insert(0, 2);
        self.reset_fill();
        self.validate_shape();
        Ok(self)
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
        Ok(Array::new(shape, selected))
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
            break Ok(Array::new(new_shape, dst));
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
            break Ok(Array::new(output_shape, dst));
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
        Ok(Array::new(shape.to_vec(), result))
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
            for (elem, of) in elems.rows().zip(of.rows()) {
                let is_member = elem.iter().zip(of.iter()).all(|(a, b)| a.eq(b));
                result.push(Byte::Value(is_member as u8));
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
    pub fn index_of(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, of) {
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
    pub fn index_of(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<f64>> {
        if self.shape[1..] != of.shape[1..] {
            return Err(env.error(format!(
                "Cannot compare arrays of different shapes: {:?} and {:?}",
                self.shape, of.shape
            )));
        }
        let mut result = Vec::with_capacity(self.row_count());
        let mut indices = BTreeMap::new();
        for (i, row) in of.rows().enumerate() {
            indices.insert(row, i);
        }
        for row in self.rows() {
            result.push(
                indices
                    .get(&row)
                    .map(|i| *i as f64)
                    .unwrap_or_else(|| (self.row_count() - 1) as f64),
            );
        }
        Ok(Array::new(self.shape.clone(), result))
    }
}

impl Value {
    pub fn group(&self, grouped: &Self, env: &Uiua) -> UiuaResult<Self> {
        let indices = self.as_naturals(env, "Group indices must be a list of natural numbers")?;
        Ok(match grouped {
            Value::Num(a) => a.group(&indices, env)?.into(),
            Value::Byte(a) => a.group(&indices, env)?.into(),
            Value::Char(a) => a.group(&indices, env)?.into(),
            Value::Func(a) => a.group(&indices, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn group(&self, indices: &[usize], env: &Uiua) -> UiuaResult<Self> {
        let Some(max_index) = indices.iter().max() else {
            return Ok(Self::new(self.shape.clone(), Vec::new()));
        };
        let mut groups: Vec<Vec<Array<T>>> = vec![Vec::new(); max_index + 1];
        for &i in indices {
            if i < self.row_count() {
                groups[i].push(Self::new(self.shape[1..].to_vec(), self.row(i).to_vec()));
            }
        }
        let mut rows = groups
            .into_iter()
            .map(|row_arrays| Self::from_row_arrays(row_arrays, true, env))
            .collect::<UiuaResult<Vec<_>>>()?;
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Self::from_row_arrays(rows, true, env)
    }
}

impl Value {
    pub fn partition(&self, partitioned: &Self, env: &Uiua) -> UiuaResult<Self> {
        let markers =
            self.as_naturals(env, "Partition markers must be a list of natural numbers")?;
        Ok(match partitioned {
            Value::Num(a) => a.partition(&markers, env)?.into(),
            Value::Byte(a) => a.partition(&markers, env)?.into(),
            Value::Char(a) => a.partition(&markers, env)?.into(),
            Value::Func(a) => a.partition(&markers, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn partition(&self, markers: &[usize], env: &Uiua) -> UiuaResult<Self> {
        let mut groups = Vec::new();
        let mut last_marker = usize::MAX;
        for (row, &marker) in self.rows().zip(markers) {
            if marker == 0 {
                groups.push(Vec::new());
                continue;
            }
            if marker != last_marker {
                groups.push(Vec::new());
            }
            groups
                .last_mut()
                .unwrap()
                .push(Self::new(self.shape[1..].to_vec(), row.to_vec()));
            last_marker = marker;
        }
        let mut rows = groups
            .into_iter()
            .map(|row_arrays| Self::from_row_arrays(row_arrays, true, env))
            .collect::<UiuaResult<Vec<_>>>()?;
        for row in &mut rows {
            while row.rank() < self.rank() {
                row.shape.insert(0, 1);
            }
        }
        Self::from_row_arrays(rows, true, env)
    }
}
