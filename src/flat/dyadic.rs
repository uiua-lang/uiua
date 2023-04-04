use std::{
    cmp::{self, Ordering},
    collections::BTreeMap,
};

use crate::{Uiua, UiuaResult};

use super::{
    array::{Array, ArrayValue, Arrayish},
    value::Value,
};

impl Value {
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.join(b, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.join(b, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.join(b, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.join(b, env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().join(b, env)?),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.join(b.convert(), env)?),
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

impl<T> Array<T> {
    pub fn join(mut self, mut other: Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self.shape.as_slice(), other.shape.as_slice()) {
            ([], []) => {
                self.data.extend(other.data);
                self.shape = vec![self.flat_len()];
                self
            }
            ([], bsh) => {
                if bsh.len() == 1 {
                    other.data.insert(0, self.data.remove(0));
                    other.shape = vec![other.flat_len()];
                    other
                } else {
                    return Err(env.error(format!(
                        "Cannot join a scalar to a rank-{} array",
                        bsh.len()
                    )));
                }
            }
            (ash, []) => {
                if ash.len() == 1 {
                    self.data.push(other.data.remove(0));
                    self.shape = vec![self.flat_len()];
                    self
                } else {
                    return Err(env.error(format!(
                        "Cannot join a rank-{} array to a scalar",
                        ash.len()
                    )));
                }
            }
            (ash, bsh) => {
                if ash[1..] == bsh[1..] {
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self
                } else {
                    return Err(env.error(format!(
                        "Cannot join arrays with shapes {ash:?} and {bsh:?}",
                    )));
                }
            }
        })
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

impl<T: Clone> Array<T> {
    pub fn replicate(mut self, amount: &[usize], env: &Uiua) -> UiuaResult<Self> {
        if self.len() != amount.len() {
            return Err(env.error(format!(
                "Cannot replicate array with shape {:?} with array of length {}",
                self.shape,
                amount.len()
            )));
        }
        if amount.iter().all(|&n| n <= 1) {
            let row_len = self.row_len();
            for (r, &n) in amount.iter().enumerate().rev() {
                if n == 0 {
                    self.data.drain(r * row_len..(r + 1) * row_len);
                }
            }
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

impl<T> Array<T> {
    pub fn pick(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot pick from rank-{} array with index of length {}",
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

impl<T> Array<T> {
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot take from rank-{} array with index of length {}",
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
                "Cannot drop from rank-{} array with index of length {}",
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
    pub fn rotate(self, mut rotated: Self, env: &Uiua) -> UiuaResult<Self> {
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

impl<T> Array<T> {
    pub fn rotate(&mut self, by: &[isize], env: &Uiua) -> UiuaResult {
        if by.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot rotate rank-{} array with index of length {}",
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

impl<T> Array<T> {
    pub fn couple(mut self, mut other: Self, env: &Uiua) -> UiuaResult<Self> {
        if self.shape != other.shape {
            return Err(env.error(format!(
                "Cannot couple arrays of different shapes: {:?} and {:?}",
                self.shape, other.shape
            )));
        }
        self.data.append(&mut other.data);
        self.shape.insert(0, 2);
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

impl<T: Clone> Array<T> {
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

impl<T: Clone> Array<T> {
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
        let mut new_shape = Vec::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(self.shape.iter().zip(size_spec).map(|(a, b)| a - b + 1));
        new_shape.extend(size_spec);
        new_shape.extend(&self.shape[size_spec.len()..]);
        let mut true_size = Vec::with_capacity(self.shape.len());
        true_size.extend(size_spec);
        if true_size.len() < self.shape.len() {
            true_size.extend(&self.shape[true_size.len()..]);
        }
        Ok(Array::new(
            new_shape,
            copy_windows(true_size, &self.shape, &self.data),
        ))
    }
}

fn copy_windows<T: Clone>(mut size: Vec<usize>, shape: &[usize], src: &[T]) -> Vec<T> {
    let mut dst = Vec::new();
    let mut corner = vec![0; shape.len()];
    let mut curr = vec![0; shape.len()];
    for (i, dim) in shape.iter().enumerate() {
        if size.len() <= i {
            size.push(*dim);
        }
    }
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
            for ((c, i), s) in corner.iter().zip(&*curr).zip(shape).rev() {
                src_index += (*c + *i) * stride;
                stride *= s;
            }
            dst.push(src[src_index].clone());
            // Go to the next item
            for i in (0..curr.len()).rev() {
                if curr[i] == size[i] - 1 {
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
            if corner[i] == shape[i] - size[i] {
                corner[i] = 0;
            } else {
                corner[i] += 1;
                continue 'windows;
            }
        }
        return dst;
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
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let shape = cmp::max_by_key(self.shape(), of.shape(), |s| s.len());
        let mut result = Vec::with_capacity(shape.iter().product());
        member(self, of, &mut result, env)?;
        Ok(Array::new(shape.to_vec(), result))
    }
}

fn member<A: Arrayish>(
    elems: &A,
    of: &impl Arrayish<Value = A::Value>,
    result: &mut Vec<u8>,
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
                result.push(is_member as u8);
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

impl<T: ArrayValue> Array<T> {
    pub fn index_of(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<f64>> {
        if self.shape[1..] != of.shape[1..] {
            return Err(env.error(format!(
                "Cannot compare arrays of different shapes: {:?} and {:?}",
                self.shape, of.shape
            )));
        }
        let mut result = Vec::with_capacity(self.len());
        let mut indices = BTreeMap::new();
        for (i, row) in of.rows().enumerate() {
            indices.insert(row, i);
        }
        for row in self.rows() {
            result.push(
                indices
                    .get(&row)
                    .map(|i| *i as f64)
                    .unwrap_or_else(|| (self.len() - 1) as f64),
            );
        }
        Ok(Array::new(self.shape.clone(), result))
    }
}
