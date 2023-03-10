use std::{
    cmp::Ordering,
    iter::repeat,
    mem::{swap, take},
    ptr,
};

use crate::{array::Array, value::Value, vm::Env, RuntimeResult};

type CmpFn<T> = fn(&T, &T) -> Ordering;

impl Value {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        if self.is_array() {
            self.array().len()
        } else {
            1
        }
    }
    pub fn rank(&self) -> usize {
        if self.is_array() {
            self.array().rank()
        } else {
            0
        }
    }
    pub fn shape(&self) -> Vec<usize> {
        if self.is_array() {
            self.array().shape().to_vec()
        } else {
            Vec::new()
        }
    }
    pub fn as_shape(&self, env: &Env) -> RuntimeResult<Vec<usize>> {
        if self.is_array() {
            let arr = self.array();
            let mut shape = Vec::with_capacity(arr.len());
            for f in arr.numbers() {
                let rounded = f.round();
                if (f - rounded).abs() > f64::EPSILON || rounded <= 0.0 {
                    return Err(env.error(
                        "Tried to make a shape from an array with decimal or \
                        nonpositive numbers, but only natural numbers are \
                        allowed",
                    ));
                }
                let rounded = rounded as usize;
                shape.push(rounded);
            }
            Ok(shape)
        } else if self.is_num() {
            let f = self.number();
            let rounded = f.round();
            if (f - rounded).abs() > f64::EPSILON || rounded <= 0.0 {
                return Err(env.error(
                    "Tried to make a shape from decimal or nonpositive \
                    number, but only natural numbers are allowed",
                ));
            }
            Ok(vec![rounded as usize])
        } else {
            Err(env.error("Tried to make a shape from non-number"))
        }
    }
    pub fn range(&self, env: &Env) -> RuntimeResult<Array> {
        let shape = self.as_shape(env)?;
        let data = range(&shape);
        Ok((shape, data).into())
    }
    pub fn reverse(&mut self) {
        if self.is_array() {
            self.array_mut().reverse();
        }
    }
    pub fn join(&mut self, other: Value, env: &Env) -> RuntimeResult {
        match (self.is_array(), other.is_array()) {
            (true, true) => self.array_mut().join(other.into_array(), env),
            (true, false) => self.array_mut().join(Array::from(other), env),
            (false, true) => {
                let mut arr = Array::from(take(self));
                arr.join(other.into_array(), env)?;
                *self = arr.into();
                Ok(())
            }
            (false, false) => {
                let mut arr = Array::from(take(self));
                arr.join(Array::from(other), env)?;
                *self = arr.into();
                Ok(())
            }
        }
    }
    pub fn deshape(&mut self) {
        if self.is_array() {
            self.array_mut().deshape();
        } else {
            *self = Array::from(take(self)).into();
        }
    }
    pub fn reshape(&mut self, other: &mut Value, env: &Env) -> RuntimeResult {
        swap(self, other);
        let shape = other.as_shape(env)?;
        self.coerce_array().reshape(shape);
        Ok(())
    }
    pub fn coerce_array(&mut self) -> &mut Array {
        if !self.is_array() {
            *self = Array::from(take(self)).into();
        }
        self.array_mut()
    }
    pub fn replicate(&mut self, filter: &Self, env: &Env) -> RuntimeResult {
        if !self.is_array() {
            return Err(env.error("Tried to filter non-array"));
        }
        let filtered = self.array_mut();
        let mut data = Vec::new();
        if filter.is_num() {
            if !filter.is_nat() {
                return Err(env.error("Tried to replicate with non-integer"));
            }
            let n = filter.number() as usize;
            for cell in take(filtered).into_values() {
                data.extend(repeat(cell).take(n));
            }
        } else if filter.is_array() {
            let filter = filter.array();
            if filter.len() != filtered.len() {
                return Err(env.error(format!(
                    "Tried to replicate with array of different length: \
                    the filter length is {}, but the array length is {}",
                    filter.len(),
                    filtered.len(),
                )));
            }
            if !filter.is_numbers() {
                return Err(env.error("Tried to replicate with non-number array"));
            }
            if filter.rank() != 1 {
                return Err(env.error("Tried to replicate with non-1D array"));
            }
            for (&n, cell) in filter.numbers().iter().zip(take(filtered).into_values()) {
                if n.trunc() != n || n < 0.0 {
                    return Err(env.error("Tried to replicate with non-natural number"));
                }
                data.extend(repeat(cell).take(n as usize));
            }
        } else {
            return Err(env.error("Tried to replicate with non-number"));
        }
        *self = Array::from(data).normalized(1).into();
        Ok(())
    }
}

pub fn range(shape: &[usize]) -> Vec<Value> {
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

pub fn reverse<T>(shape: &[usize], data: &mut [T]) {
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

pub fn force_length<T: Clone>(data: &mut Vec<T>, len: usize) {
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
