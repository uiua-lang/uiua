use std::{
    cmp::Ordering,
    collections::BTreeSet,
    iter::repeat,
    mem::{swap, take},
};

use crate::{
    array::{Array, ArrayType},
    value::Value,
    Uiua, UiuaResult,
};

impl Value {
    pub fn join(&mut self, other: Value, env: &Uiua) -> UiuaResult {
        match (self.is_array(), other.is_array()) {
            (true, true) => join_arrays(self.array_mut(), other.into_array(), env)?,
            (true, false) => join_arrays(self.array_mut(), Array::from(other), env)?,
            (false, true) => {
                let mut arr = Array::from(take(self));
                join_arrays(&mut arr, other.into_array(), env)?;
                *self = arr.into();
            }
            (false, false) => {
                let mut arr = Array::from(take(self));
                join_arrays(&mut arr, Array::from(other), env)?;
                *self = arr.into();
            }
        }
        self.array_mut().normalize_type();
        Ok(())
    }
}

fn join_arrays(a: &mut Array, mut b: Array, env: &Uiua) -> UiuaResult {
    if a.shape().is_empty() && b.shape().is_empty() {
        // Atom case
        take_values_from(a, b);
        a.set_shape(vec![2]);
    } else {
        let rank_diff = a.rank() as isize - b.rank() as isize;
        if rank_diff.abs() > 1 {
            return Err(env.error(format!(
                "Joined values cannot have a rank difference greater than 1, \
                    but ranks are {} and {}",
                a.rank(),
                b.rank()
            )));
        }
        match a.rank().cmp(&b.rank()) {
            Ordering::Equal => {
                if a.shape()[1..] != b.shape()[1..] {
                    return Err(env.error(format!(
                        "Joined arrays of the same rank must have the same \
                            non-leading shape, but the shapes are {:?} and {:?}",
                        a.shape(),
                        b.shape()
                    )));
                }
                a.shape_mut()[0] += b.shape()[0];
                take_values_from(a, b);
            }
            Ordering::Greater => {
                if &a.shape()[1..] != b.shape() {
                    return Err(env.error(format!(
                        "Appended arrays must have the same non-leading shape, \
                            but the shapes are {:?} and {:?}",
                        a.shape(),
                        b.shape()
                    )));
                }
                a.shape_mut()[0] += 1;
                take_values_from(a, b);
            }
            Ordering::Less => {
                if a.shape() != &b.shape()[1..] {
                    return Err(env.error(format!(
                        "Prepended arrays must have the same non-leading shape, \
                            but the shapes are {:?} and {:?}",
                        a.shape(),
                        b.shape()
                    )));
                }
                a.reverse();
                b.reverse();
                swap(a, &mut b);
                take_values_from(a, b);
                a.shape_mut()[0] += 1;
                a.reverse();
            }
        }
    }
    Ok(())
}

/// Simply take the values from the other array and append them to this one.
/// Does not update the shape.
fn take_values_from(a: &mut Array, b: Array) {
    match (a.ty(), b.ty()) {
        (ArrayType::Num, ArrayType::Num) => {
            a.numbers_mut().extend(b.into_numbers());
        }
        (ArrayType::Char, ArrayType::Char) => {
            a.chars_mut().extend(b.into_chars());
        }
        (ArrayType::Value, ArrayType::Value) => {
            a.values_mut().extend(b.into_flat_values());
        }
        _ => {
            let shape = a.take_shape();
            let mut values = a.take_flat_values();
            values.append(&mut b.into_flat_values());
            *a = Array::from((shape, values)).normalized_type();
        }
    }
}

impl Value {
    pub fn replicate(&mut self, items: Self, env: &Uiua) -> UiuaResult {
        if !items.is_array() {
            return Err(env.error("Cannot replicate non-array"));
        }
        const MASK_ERROR: &str =
            "Can only replicate with natural number or list of natural numbers";
        let filtered = items.array();
        let mut data = Vec::new();
        if let Some(n) = self.as_nat() {
            let n = n as usize;
            filtered.iter_values(|val| data.extend(repeat(val).take(n).cloned()));
        } else if self.is_array() {
            let filter = self.as_naturals(env, MASK_ERROR)?;
            if filter.len() != filtered.len() {
                return Err(env.error(format!(
                    "Cannot replicate with array of different length: \
                    the filter length is {}, but the array length is {}",
                    filter.len(),
                    filtered.len(),
                )));
            }
            let mut filter = filter.into_iter();
            filtered
                .iter_values(|val| data.extend(repeat(val).take(filter.next().unwrap()).cloned()));
        } else {
            return Err(env.error(MASK_ERROR));
        }
        *self = Array::from(data).normalized_type().into();
        Ok(())
    }
}

impl Value {
    pub fn pick(&mut self, from: Self, env: &Uiua) -> UiuaResult {
        if !from.is_array() || from.array().rank() == 0 {
            return Err(env.error("Cannot pick from rank less than 1"));
        }
        let index = self.as_indices(env, "Index must be a list of integers")?;
        let array = from.array();
        *self = pick(&index, array, env)?;
        Ok(())
    }
    pub fn take(&mut self, from: Self, env: &Uiua) -> UiuaResult {
        if !from.is_array() || from.array().rank() == 0 {
            return Err(env.error("Cannot take from rank less than 1"));
        }
        let index = self.as_indices(env, "Index must be a list of integers")?;
        let array = from.into_array();
        if index.len() > array.rank() {
            return Err(env.error(format!(
                "Cannot take with index of greater rank: \
                the index length is {}, but the array rank is {}",
                index.len(),
                array.rank(),
            )));
        }
        let taken = take_array(&index, array, env)?;
        *self = taken.into();
        Ok(())
    }
    pub fn drop(&mut self, from: Self, env: &Uiua) -> UiuaResult {
        if !from.is_array() || from.array().rank() == 0 {
            return Err(env.error("Cannot drop from rank less than 1"));
        }
        let mut index = self.as_indices(env, "Index must be a list of integers")?;
        let array = from.into_array();
        if index.len() > array.rank() {
            return Err(env.error(format!(
                "Cannot drop with index of greater rank: \
                the index length is {}, but the array rank is {}",
                index.len(),
                array.rank(),
            )));
        }
        for (i, s) in index.iter_mut().zip(array.shape()) {
            *i = if *i >= 0 {
                (*i - (*s as isize)).min(0)
            } else {
                ((*s as isize) + *i).max(0)
            };
        }
        let taken = take_array(&index, array, env)?;
        *self = taken.into();
        Ok(())
    }
    pub fn rotate(&mut self, mut target: Self, env: &Uiua) -> UiuaResult {
        swap(self, &mut target);
        let index = target.as_indices(env, "Index must be a list of integers")?;
        if index.is_empty() || index.iter().all(|i| *i == 0) {
            return Ok(());
        }
        if !self.is_array() || self.array().shape() == [0] {
            return Ok(());
        }
        self.array_mut().data_mut(
            |shape, data| rotate(&index, shape, data),
            |shape, data| rotate(&index, shape, data),
            |shape, data| rotate(&index, shape, data),
            |shape, data| rotate(&index, shape, data),
        );
        Ok(())
    }
    pub fn pair(&mut self, other: Self) {
        *self = Array::from((vec![2], vec![take(self), other]))
            .normalized_type()
            .into();
    }
    pub fn couple(&mut self, mut other: Self, env: &Uiua) -> UiuaResult {
        let a = self.coerce_array();
        let b = other.coerce_array();
        if a.shape() != b.shape() {
            return Err(env.error(format!(
                "Cannot couple arrays of different shapes: \
                the first shape is {:?}, but the second shape is {:?}",
                a.shape(),
                b.shape()
            )));
        }
        match (a.ty(), b.ty()) {
            (ArrayType::Num, ArrayType::Num) => a.numbers_mut().append(b.numbers_mut()),
            (ArrayType::Byte, ArrayType::Byte) => a.bytes_mut().append(b.bytes_mut()),
            (ArrayType::Char, ArrayType::Char) => a.chars_mut().append(b.chars_mut()),
            (ArrayType::Value, ArrayType::Value) => a.values_mut().append(b.values_mut()),
            _ => a.make_values().append(b.make_values()),
        }
        a.shape_mut().insert(0, 2);
        Ok(())
    }
    pub fn select(&mut self, mut from: Self, env: &Uiua) -> UiuaResult {
        let indices = self.as_indices(env, "Indices must be a list of integers")?;
        let array = from.coerce_array();
        let mut selected = Vec::with_capacity(indices.len());
        for index in indices {
            selected.push(pick(&[index], array, env)?);
        }
        *self = Array::from((vec![selected.len()], selected))
            .normalized()
            .into();
        Ok(())
    }
    pub fn windows(&mut self, from: Self, env: &Uiua) -> UiuaResult {
        if !from.is_array() {
            return Err(env.error("Cannot take windows from a non-array"));
        }
        let array = from.array();
        let size_spec = self.as_naturals(env, "Window size must be a list of positive integers")?;
        if size_spec.is_empty() {
            return Ok(());
        }
        let new_array = windows(&size_spec, array, env)?;
        *self = new_array.into();
        Ok(())
    }
    pub fn member(&mut self, of: Self) {
        let members = self.coerce_array();
        let set: BTreeSet<Value> = of.coerce_into_array().into_values().into_iter().collect();
        *self = Array::from(
            take(members)
                .into_values()
                .into_iter()
                .map(|val| set.contains(&val) as u8 as f64)
                .collect::<Vec<_>>(),
        )
        .into();
    }
    pub fn group(&mut self, target: Self, env: &Uiua) -> UiuaResult {
        let indices = self.as_indices(env, "Indices must be a list of integers")?;
        let values = target.coerce_into_array().into_values();
        let group_count = values
            .len()
            .min(indices.iter().max().copied().unwrap_or(0).max(0) as usize + 1);
        let mut groups = vec![Vec::new(); group_count];
        let mut last_index = None;
        for (index, value) in indices.into_iter().zip(values) {
            if index < 0 {
                last_index = None;
                continue;
            }
            let index = index as usize;
            if index < group_count {
                let group = &mut groups[index];
                let subgroup = if let Some(last_index) = last_index {
                    if index != last_index {
                        group.push(Vec::new())
                    }
                    group.last_mut().unwrap()
                } else {
                    group.push(Vec::new());
                    group.last_mut().unwrap()
                };
                subgroup.push(value);
                last_index = Some(index);
            } else {
                last_index = None;
            }
        }
        *self = Array::from(
            groups
                .into_iter()
                .map(|group| {
                    group
                        .into_iter()
                        .map(Array::from)
                        .map(Value::from)
                        .collect::<Vec<_>>()
                })
                .map(Array::from)
                .map(Value::from)
                .collect::<Vec<_>>(),
        )
        .into();
        Ok(())
    }
    pub fn index_of(&mut self, searched_in: Self, env: &Uiua) -> UiuaResult {
        if !searched_in.is_array() {
            return Err(env.error("Cannot search in non-array"));
        }
        if searched_in.rank() == 0 {
            return Err(env.error("Cannot search in rank 0 array"));
        }
        let searched_in = searched_in.into_array().into_values();
        let searched_for = take(self).coerce_into_array();
        let result_shape = searched_for.shape()[..1].to_vec();
        let searched_for = searched_for.into_values();
        let mut indices = Vec::with_capacity(searched_for.len());
        for val in searched_for {
            if let Some(index) = searched_in.iter().position(|v| v == &val) {
                indices.push(index as f64);
            } else {
                indices.push(searched_in.len() as f64);
            }
        }
        *self = Array::from((result_shape, indices)).into();
        Ok(())
    }
    pub fn put(&mut self, value: Self, array: Self, env: &Uiua) -> UiuaResult {
        if !array.is_array() {
            return Err(env.error("Cannot put into non-array"));
        }
        let indices = self.as_indices(env, "Indices must be a list of integers")?;
        let mut array = array.into_array();
        if indices.len() + value.rank() != array.rank() {
            return Err(env.error(format!(
                "Cannot put value of rank {} into array of rank {} at indices {:?}",
                value.rank(),
                array.rank(),
                indices
            )));
        }
        put(&indices, value, &mut array, env)?;
        *self = array.into();
        Ok(())
    }
}

fn signed_index(index: isize, len: usize) -> Option<usize> {
    if index < 0 {
        if (-index) as usize > len {
            None
        } else {
            Some((len as isize + index) as usize)
        }
    } else if (index as usize) < len {
        Some(index as usize)
    } else {
        None
    }
}

fn put(indices: &[isize], value: Value, array: &mut Array, env: &Uiua) -> UiuaResult {
    if indices.len() == 1 {
        let index = signed_index(indices[0], array.shape()[0]);
        if let Some(index) = index {
            let value = value.coerce_into_array();
            if value.shape() != &array.shape()[1..] {
                return Err(env.error(format!(
                    "Cannot put value of shape {:?} into array of shape {:?} at index {}",
                    value.shape(),
                    array.shape(),
                    index
                )));
            }
            let (shape, mut cells) = take(array).into_shape_rows();
            cells[index] = value;
            *array = Array::from_cells(shape, cells);
            Ok(())
        } else {
            Err(env.error(format!(
                "Index {} out of bounds for array of length {}",
                indices[0],
                array.shape()[0]
            )))
        }
    } else {
        let index = signed_index(indices[0], array.shape()[0]);
        if let Some(index) = index {
            let (shape, mut cells) = take(array).into_shape_rows();
            let cell = cells.get_mut(index).unwrap();
            put(&indices[1..], value, cell, env)?;
            *array = Array::from_cells(shape, cells);
            Ok(())
        } else {
            Err(env.error(format!(
                "Index {} out of bounds for array of length {}",
                indices[0],
                array.shape()[0]
            )))
        }
    }
}

fn windows(size_spec: &[usize], array: &Array, env: &Uiua) -> UiuaResult<Array> {
    if size_spec.len() > array.shape().len() {
        return Err(env.error(format!(
            "Window size {size_spec:?} has too many axes for array of shape {:?}",
            array.shape()
        )));
    }
    for (i, (size, shape)) in size_spec.iter().zip(array.shape()).enumerate() {
        if *size > *shape {
            return Err(env.error(format!(
                "Cannot take window of size {size} along axis {i} of array of shape {:?}",
                array.shape()
            )));
        }
    }
    let mut new_shape = Vec::with_capacity(array.shape().len() + size_spec.len());
    new_shape.extend(array.shape().iter().zip(size_spec).map(|(a, b)| a - b + 1));
    new_shape.extend(size_spec);
    new_shape.extend(&array.shape()[size_spec.len()..]);
    let mut true_size = Vec::with_capacity(array.shape().len());
    true_size.extend(size_spec);
    if true_size.len() < array.shape().len() {
        true_size.extend(&array.shape()[true_size.len()..]);
    }
    Ok(array.data_with(
        (true_size, new_shape),
        |(size, nshape), shape, nums| (nshape, copy_windows(size, shape, nums)).into(),
        |(size, nshape), shape, bytes| (nshape, copy_windows(size, shape, bytes)).into(),
        |(size, nshape), shape, chars| (nshape, copy_windows(size, shape, chars)).into(),
        |(size, nshape), shape, values| (nshape, copy_windows(size, shape, values)).into(),
    ))
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

fn rotate<T: Clone>(index: &[isize], shape: &[usize], data: &mut [T]) {
    let cell_count = shape[0];
    if cell_count == 0 {
        return;
    }
    let cell_size = data.len() / cell_count;
    let offset = index[0];
    let mid = (cell_count as isize + offset).rem_euclid(cell_count as isize) as usize;
    let (left, right) = data.split_at_mut(mid * cell_size);
    left.reverse();
    right.reverse();
    data.reverse();
    let index = &index[1..];
    let shape = &shape[1..];
    if index.is_empty() || shape.is_empty() {
        return;
    }
    for cell in data.chunks_mut(cell_size) {
        rotate(index, shape, cell);
    }
}

fn take_array(index: &[isize], array: Array, env: &Uiua) -> UiuaResult<Array> {
    let mut shape = array.shape().to_vec();
    let mut cells = array.into_values();
    let take_count = index[0];
    let take_abs = take_count.unsigned_abs();
    if take_count >= 0 {
        cells.truncate(take_abs);
        if cells.len() < take_abs {
            let fill = cells[0].fill_value(env)?;
            cells.extend(repeat(fill).take(take_abs - cells.len()));
        }
    } else {
        if cells.len() > take_abs {
            cells.drain(0..cells.len() - take_abs);
        }
        if cells.len() < take_abs {
            let fill = cells[0].fill_value(env)?;
            cells = repeat(fill)
                .take(take_abs - cells.len())
                .chain(cells)
                .collect();
        }
    }
    let index = &index[1..];
    if !index.is_empty() {
        cells = cells
            .into_iter()
            .map(|cell| take_array(index, cell.into_array(), env).map(Value::from))
            .collect::<UiuaResult<_>>()?;
    }
    shape[0] = take_abs;
    let arr = if shape.len() > 1 {
        Array::from_cells(shape, cells.into_iter().map(Value::into_array).collect())
            .normalized_type()
    } else {
        Array::from((shape, cells))
    };
    Ok(arr)
}

fn pick(index: &[isize], array: &Array, env: &Uiua) -> UiuaResult<Value> {
    if index.len() > array.rank() {
        return Err(env.error(format!(
            "Cannot pick with index of greater rank: \
                the index length is {}, but the array rank is {}",
            index.len(),
            array.rank(),
        )));
    }
    for (&s, &i) in array.shape().iter().zip(index) {
        let s = s as isize;
        if i >= s || s + i < 0 {
            return Err(env.error(format!(
                "Index out of range: \
                    the index is {:?}, but the shape is {:?}",
                index,
                array.shape()
            )));
        }
    }
    Ok(match array.ty() {
        ArrayType::Num => pick_impl(array.shape(), index, array.numbers()),
        ArrayType::Byte => pick_impl(array.shape(), index, array.bytes()),
        ArrayType::Char => pick_impl(array.shape(), index, array.chars()),
        ArrayType::Value => pick_impl(array.shape(), index, array.values()),
    })
}

fn pick_impl<T>(shape: &[usize], index: &[isize], mut data: &[T]) -> Value
where
    T: Clone + Into<Value>,
    Array: From<(Vec<usize>, Vec<T>)>,
{
    let mut shape_index = 0;
    for &i in index {
        let cell_count = shape[shape_index];
        let cell_size = data.len() / cell_count;
        let start = if i >= 0 {
            i as usize * cell_size
        } else {
            (data.len() as isize + i * cell_size as isize) as usize
        };
        data = &data[start..start + cell_size];
        shape_index += 1;
    }
    if shape_index < shape.len() {
        let shape = shape[shape_index..].to_vec();
        Array::from((shape, data.to_vec())).into()
    } else {
        data[0].clone().into()
    }
}
