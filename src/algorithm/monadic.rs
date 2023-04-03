use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    mem::{swap, take},
    ptr,
};

use crate::{
    array::{Array, ArrayType},
    value::{Type, Value},
    Uiua, UiuaResult,
};

impl Value {
    pub fn deshape(&mut self) {
        if self.is_array() {
            self.array_mut().deshape();
        } else {
            *self = Array::from(take(self)).into();
        }
    }
    pub fn reshape(&mut self, mut other: Value, env: &Uiua) -> UiuaResult {
        swap(self, &mut other);
        let shape = other.as_naturals(env, "Shape must be a list of natural numbers")?;
        let arr = self.coerce_array();
        arr.set_shape(shape);
        let new_len: usize = arr.shape().iter().product();
        match arr.ty() {
            ArrayType::Num => force_length(arr.numbers_mut(), new_len),
            ArrayType::Byte => force_length(arr.bytes_mut(), new_len),
            ArrayType::Char => force_length(arr.chars_mut(), new_len),
            ArrayType::Value => force_length(arr.values_mut(), new_len),
        }
        Ok(())
    }
}

fn force_length<T: Clone>(data: &mut Vec<T>, len: usize) {
    match data.len().cmp(&len) {
        Ordering::Less => {
            let mut i = 0;
            while data.len() < len {
                data.push(data[i].clone());
                i += 1;
            }
        }
        Ordering::Greater => data.truncate(len),
        Ordering::Equal => {}
    }
}

impl Value {
    pub fn range(&mut self, env: &Uiua) -> UiuaResult {
        let shape = self.as_naturals(
            env,
            "Range only accepts a single natural number \
            or a list of natural numbers",
        )?;
        let data = range(&shape);
        *self = Array::from((shape, data)).into();
        Ok(())
    }
}

fn range(shape: &[usize]) -> Vec<Value> {
    let len = shape.iter().product::<usize>();
    let mut data = Vec::with_capacity(len);
    let products: Vec<usize> = (0..shape.len())
        .map(|i| shape[i..].iter().product::<usize>())
        .collect();
    let moduli: Vec<usize> = (0..shape.len())
        .map(|i| shape[i + 1..].iter().product::<usize>())
        .collect();
    for i in 0..len {
        if shape.len() <= 1 {
            data.push((i as f64).into());
        } else {
            let mut cell: Vec<f64> = Vec::with_capacity(shape.len());
            for j in 0..shape.len() {
                cell.push((i % products[j] / moduli[j]) as f64);
            }
            data.push(Array::from(cell).into());
        }
    }
    data
}

impl Value {
    pub fn first(&mut self, env: &Uiua) -> UiuaResult {
        if !self.is_array() {
            return Ok(());
        }
        *self = self
            .array()
            .first()
            .ok_or_else(|| env.error("Empty array has no first"))?;
        Ok(())
    }
    pub fn last(&mut self, env: &Uiua) -> UiuaResult {
        if !self.is_array() {
            return Ok(());
        }
        *self = self
            .array()
            .last()
            .ok_or_else(|| env.error("Empty array has no last"))?;
        Ok(())
    }
    pub fn reverse(&mut self) {
        if self.is_array() {
            self.array_mut().reverse();
        }
    }
}

impl Array {
    pub fn reverse(&mut self) {
        self.data_mut(reverse, reverse, reverse, reverse);
    }
}

fn reverse<T>(shape: &mut [usize], data: &mut [T]) {
    if shape.is_empty() {
        return;
    }
    let cells = shape[0];
    let cell_size: usize = shape.iter().skip(1).product();
    for i in 0..cells / 2 {
        let left = i * cell_size;
        let right = (cells - i - 1) * cell_size;
        let left = &mut data[left] as *mut T;
        let right = &mut data[right] as *mut T;
        unsafe {
            ptr::swap_nonoverlapping(left, right, cell_size);
        }
    }
}

impl Value {
    pub fn enclose(&mut self) {
        *self = Array::from((Vec::new(), vec![take(self)]))
            .normalized_type()
            .into();
    }
    pub fn transpose(&mut self) {
        let arr = self.coerce_array();
        arr.data_mut(transpose, transpose, transpose, transpose);
    }
}

fn transpose<T: Clone>(shape: &mut [usize], data: &mut [T]) {
    if shape.len() < 2 || shape[0] == 0 {
        return;
    }
    let mut temp = Vec::with_capacity(data.len());
    let run_length = data.len() / shape[0];
    for j in 0..run_length {
        for i in 0..shape[0] {
            temp.push(data[i * run_length + j].clone());
        }
    }
    data.clone_from_slice(&temp);
    shape.rotate_left(1);
}

impl Value {
    pub fn grade(&mut self, env: &Uiua) -> UiuaResult {
        let arr = self.coerce_array();
        if arr.rank() < 1 {
            return Err(env.error("Cannot grade rank less than 1"));
        }
        let mut indices: Vec<usize> = (0..arr.shape()[0]).collect();
        let cells = take(arr).into_values();
        indices.sort_by(|&a, &b| cells[a].cmp(&cells[b]));
        let nums: Vec<f64> = indices.iter().map(|&i| i as f64).collect();
        *arr = Array::from((vec![indices.len()], nums));
        Ok(())
    }
    pub fn classify(&mut self, env: &Uiua) -> UiuaResult {
        if self.rank() < 1 {
            return Err(env.error("Cannot classify rank less than 1"));
        }
        let array = take(self).into_array();
        let mut classes = BTreeMap::new();
        let mut classified = Vec::with_capacity(array.shape()[0]);
        for val in array.into_values() {
            let new_class = classes.len();
            let class = *classes.entry(val).or_insert(new_class);
            classified.push(class as f64);
        }
        *self = Array::from(classified).into();
        Ok(())
    }
    pub fn deduplicate(&mut self, env: &Uiua) -> UiuaResult {
        if !self.is_array() {
            return Err(env.error("Cannot deduplicate non-array"));
        }
        let array = take(self).into_array();
        if array.rank() == 0 {
            return Err(env.error("Cannot deduplicate rank 0 array"));
        }
        let mut deduped = Vec::with_capacity(array.shape()[0]);
        let mut seen = BTreeSet::new();
        for val in array.into_values() {
            if seen.insert(val.clone()) {
                deduped.push(val);
            }
        }
        *self = Array::from(deduped).normalized().into();
        Ok(())
    }
    pub fn parse_num(&mut self, env: &Uiua) -> UiuaResult {
        match self.ty() {
            Type::Num => {}
            Type::Byte => {}
            Type::Char => {
                *self = self
                    .char()
                    .to_string()
                    .parse::<f64>()
                    .map(Value::from)
                    .map_err(|e| env.error(e.to_string()))?
            }
            Type::Function => return Err(env.error("Cannot parse function as number")),
            Type::Array => {
                let arr = self.array();
                if !arr.is_chars() {
                    return Err(env.error("Cannot parse non-character as number"));
                }
                if arr.rank() > 1 {
                    return Err(env.error("Cannot parse array of rank > 1 as number"));
                }
                let string = arr.chars().iter().collect::<String>();
                *self = string
                    .parse::<f64>()
                    .map(Value::from)
                    .map_err(|e| env.error(e.to_string()))?
            }
        }
        Ok(())
    }
    pub fn normalize(&mut self, env: &Uiua) -> UiuaResult {
        if self.is_array() {
            if let Some((a, b)) = self.array_mut().normalize() {
                return Err(env.error(format!(
                    "Cannot normalize array with values of difference shapes {a:?} and {b:?}"
                )));
            }
        }
        Ok(())
    }
    pub fn indices(&mut self, env: &Uiua) -> UiuaResult {
        if !self.is_array() {
            return Err(env.error("Cannot get indices of non-array"));
        }
        let array = self.array();
        let mask = self.as_naturals(env, "Can only get indices of rank 1 number array")?;
        let mut indices = Vec::with_capacity(array.shape()[0]);
        for (i, n) in mask.into_iter().enumerate() {
            for _ in 0..n {
                indices.push(i as f64);
            }
        }
        *self = Array::from(indices).into();
        Ok(())
    }
    pub fn sort(&mut self, env: &Uiua) -> UiuaResult {
        if !self.is_array() {
            return Err(env.error("Cannot sort non-array"));
        }
        if self.rank() == 0 {
            return Err(env.error("Cannot sort rank 0 array"));
        }
        self.array_mut().data_mut(
            |shape, numbers| {
                sort_array(shape, numbers, |a, b| {
                    a.partial_cmp(b)
                        .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
                })
            },
            |shape, bytes| sort_array(shape, bytes, Ord::cmp),
            |shape, chars| sort_array(shape, chars, Ord::cmp),
            |shape, values| sort_array(shape, values, Ord::cmp),
        );
        Ok(())
    }
}

type CmpFn<T> = fn(&T, &T) -> Ordering;

pub fn sort_array<T: Clone>(shape: &[usize], data: &mut [T], cmp: CmpFn<T>) {
    if shape.is_empty() {
        return;
    }
    let chunk_size = shape.iter().skip(1).product();
    merge_sort_chunks(chunk_size, data, cmp);
}

fn merge_sort_chunks<T: Clone>(chunk_size: usize, data: &mut [T], cmp: CmpFn<T>) {
    let cells = data.len() / chunk_size;
    assert_ne!(cells, 0);
    if cells == 1 {
        return;
    }
    let mid = cells / 2;
    let mut tmp = Vec::with_capacity(data.len());
    let (left, right) = data.split_at_mut(mid * chunk_size);
    merge_sort_chunks(chunk_size, left, cmp);
    merge_sort_chunks(chunk_size, right, cmp);
    let mut left = left.chunks_exact(chunk_size);
    let mut right = right.chunks_exact(chunk_size);
    let mut left_next = left.next();
    let mut right_next = right.next();
    loop {
        match (left_next, right_next) {
            (Some(l), Some(r)) => {
                let mut ordering = Ordering::Equal;
                for (l, r) in l.iter().zip(r) {
                    ordering = cmp(l, r);
                    if ordering != Ordering::Equal {
                        break;
                    }
                }
                if ordering == Ordering::Less {
                    tmp.extend_from_slice(l);
                    left_next = left.next();
                } else {
                    tmp.extend_from_slice(r);
                    right_next = right.next();
                }
            }
            (Some(l), None) => {
                tmp.extend_from_slice(l);
                left_next = left.next();
            }
            (None, Some(r)) => {
                tmp.extend_from_slice(r);
                right_next = right.next();
            }
            (None, None) => {
                break;
            }
        }
    }
    data.clone_from_slice(&tmp);
}
