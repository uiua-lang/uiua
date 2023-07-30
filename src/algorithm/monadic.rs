use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    iter::repeat,
    ptr,
};

use crate::{array::*, value::Value, Uiua, UiuaResult};

impl Value {
    pub fn deshape(&mut self) {
        self.generic_mut(
            Array::deshape,
            Array::deshape,
            Array::deshape,
            Array::deshape,
        )
    }
    pub fn parse_num(&self, env: &Uiua) -> UiuaResult<Self> {
        Ok(self
            .as_string(env, "Parsed array must be a string")?
            .parse::<f64>()
            .map_err(|e| env.error(format!("Cannot parse into number: {}", e)))?
            .into())
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn deshape(&mut self) {
        self.shape = vec![self.flat_len()];
    }
}

impl Value {
    pub fn range(&self, env: &Uiua) -> UiuaResult<Self> {
        let mut shape = self.as_naturals(
            env,
            "Range max should be a single natural number \
            or a list of natural numbers",
        )?;
        let data = range(&shape);
        if shape.len() > 1 {
            shape.push(shape.len());
        }
        let array = Array::new(shape, data);
        Ok(array.into())
    }
}

fn range(shape: &[usize]) -> Vec<f64> {
    if shape.contains(&0) {
        return Vec::new();
    }
    let len = shape.len() * shape.iter().product::<usize>();
    let mut data: Vec<f64> = Vec::with_capacity(len);
    let mut curr = vec![0; shape.len()];
    loop {
        for d in &curr {
            data.push(*d as f64);
        }
        let mut i = shape.len() - 1;
        loop {
            curr[i] += 1;
            if curr[i] == shape[i] {
                curr[i] = 0;
                if i == 0 {
                    return data;
                }
                i -= 1;
            } else {
                break;
            }
        }
    }
}

impl Value {
    pub fn first(self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match self {
            Self::Num(array) => array.first(env)?.into(),
            Self::Byte(array) => array.first(env)?.into(),
            Self::Char(array) => array.first(env)?.into(),
            Self::Func(array) => array.first(env)?.into(),
        })
    }
    pub fn last(self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match self {
            Self::Num(array) => array.last(env)?.into(),
            Self::Byte(array) => array.last(env)?.into(),
            Self::Char(array) => array.last(env)?.into(),
            Self::Func(array) => array.last(env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn first(mut self, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot take first of a scalar"));
        }
        if self.shape[0] == 0 {
            return Err(env.error("Cannot take first of an empty array"));
        }
        let row_len = self.row_len();
        self.shape.remove(0);
        self.data.truncate(row_len);
        Ok(self)
    }
    pub fn last(mut self, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot take last of a scalar"));
        }
        let row_len = self.row_len();
        self.shape.remove(0);
        let prefix_len = self.data.len() - row_len;
        self.data.drain(0..prefix_len);
        Ok(self)
    }
}

impl Value {
    pub fn reverse(&mut self) {
        self.generic_mut(
            Array::reverse,
            Array::reverse,
            Array::reverse,
            Array::reverse,
        )
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn reverse(&mut self) {
        if self.shape.is_empty() {
            return;
        }
        let cells = self.shape[0];
        let cell_size: usize = self.shape.iter().skip(1).product();
        for i in 0..cells / 2 {
            let left = i * cell_size;
            let right = (cells - i - 1) * cell_size;
            let left = &mut self.data[left] as *mut T;
            let right = &mut self.data[right] as *mut T;
            unsafe {
                ptr::swap_nonoverlapping(left, right, cell_size);
            }
        }
    }
}

impl Value {
    pub fn transpose(&mut self) {
        self.generic_mut(
            Array::transpose,
            Array::transpose,
            Array::transpose,
            Array::transpose,
        )
    }
    pub fn inv_transpose(&mut self) {
        self.generic_mut(
            Array::inv_transpose,
            Array::inv_transpose,
            Array::inv_transpose,
            Array::inv_transpose,
        )
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn transpose(&mut self) {
        if self.shape.len() < 2 || self.shape[0] == 0 {
            return;
        }
        let mut temp = Vec::with_capacity(self.data.len());
        let row_len = self.row_len();
        let row_count = self.row_count();
        for j in 0..row_len {
            for i in 0..row_count {
                temp.push(self.data[i * row_len + j].clone());
            }
        }
        self.data = temp;
        self.shape.rotate_left(1);
    }
    pub fn inv_transpose(&mut self) {
        if self.shape.len() < 2 || self.shape[0] == 0 {
            return;
        }
        let mut temp = Vec::with_capacity(self.data.len());
        let col_len = *self.shape.last().unwrap();
        let col_count: usize = self.shape.iter().rev().skip(1).product();
        for j in 0..col_len {
            for i in 0..col_count {
                temp.push(self.data[i * col_len + j].clone());
            }
        }
        self.data = temp;
        self.shape.rotate_right(1);
    }
}

impl Value {
    pub fn grade(&self, env: &Uiua) -> UiuaResult<Self> {
        Ok(Self::from_iter(match self {
            Self::Num(array) => array.grade(env)?,
            Self::Byte(array) => array.grade(env)?,
            Self::Char(array) => array.grade(env)?,
            Self::Func(array) => array.grade(env)?,
        }))
    }
    pub fn classify(&self, env: &Uiua) -> UiuaResult<Self> {
        Ok(Self::from_iter(match self {
            Self::Num(array) => array.classify(env)?,
            Self::Byte(array) => array.classify(env)?,
            Self::Char(array) => array.classify(env)?,
            Self::Func(array) => array.classify(env)?,
        }))
    }
    pub fn deduplicate(&mut self) {
        self.generic_mut(
            Array::deduplicate,
            Array::deduplicate,
            Array::deduplicate,
            Array::deduplicate,
        )
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn grade(&self, env: &Uiua) -> UiuaResult<Vec<usize>> {
        if self.rank() == 0 {
            return Err(env.error("Cannot grade a rank-0 array"));
        }
        let mut indices = (0..self.flat_len()).collect::<Vec<_>>();
        indices.sort_by(|&a, &b| {
            self.row(a)
                .iter()
                .zip(self.row(b))
                .map(|(a, b)| a.cmp(b))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        Ok(indices)
    }
    pub fn classify(&self, env: &Uiua) -> UiuaResult<Vec<usize>> {
        if self.rank() == 0 {
            return Err(env.error("Cannot classify a rank-0 array"));
        }
        let mut classes = BTreeMap::new();
        let mut classified = Vec::with_capacity(self.row_count());
        for row in self.rows() {
            let new_class = classes.len();
            let class = *classes.entry(row).or_insert(new_class);
            classified.push(class);
        }
        Ok(classified)
    }
    pub fn deduplicate(&mut self) {
        if self.rank() == 0 {
            return;
        }
        let mut deduped = Vec::new();
        let mut seen = BTreeSet::new();
        let mut new_len = 0;
        for row in self.rows() {
            if seen.insert(row) {
                deduped.extend_from_slice(&row);
                new_len += 1;
            }
        }
        self.data = deduped;
        self.shape[0] = new_len;
    }
}

impl Value {
    pub fn indices(&self, env: &Uiua) -> UiuaResult<Self> {
        let nats =
            self.as_naturals(env, "Argument to indices must be a list of natural numbers")?;
        let mut indices = Vec::new();
        for (i, n) in nats.into_iter().enumerate() {
            indices.extend(repeat(i as f64).take(n));
        }
        Ok(Self::from_iter(indices))
    }
    pub fn sort(&mut self) {
        self.generic_mut(Array::sort, Array::sort, Array::sort, Array::sort)
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn sort(&mut self) {
        if self.rank() == 0 {
            return;
        }
        let row_len = self.row_len();
        merge_sort_rows(row_len, &mut self.data);
    }
}

fn merge_sort_rows<T: ArrayValue>(row_len: usize, data: &mut [T]) {
    let cells = data.len() / row_len;
    assert_ne!(cells, 0);
    if cells == 1 {
        return;
    }
    let mid = cells / 2;
    let mut tmp = Vec::with_capacity(data.len());
    let (left, right) = data.split_at_mut(mid * row_len);
    merge_sort_rows(row_len, left);
    merge_sort_rows(row_len, right);
    let mut left = left.chunks_exact(row_len);
    let mut right = right.chunks_exact(row_len);
    let mut left_next = left.next();
    let mut right_next = right.next();
    loop {
        match (left_next, right_next) {
            (Some(l), Some(r)) => {
                let mut ordering = Ordering::Equal;
                for (l, r) in l.iter().zip(r) {
                    ordering = l.cmp(r);
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

impl Value {
    pub fn invert(&self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match self {
            Self::Func(fs) => {
                let mut invs = Vec::with_capacity(fs.len());
                for f in &fs.data {
                    invs.push(
                        f.inverse()
                            .ok_or_else(|| env.error("No inverse found"))?
                            .into(),
                    );
                }
                Self::Func(Array::new(fs.shape.clone(), invs))
            }
            v => return Err(env.error(format!("Cannot invert {}", v.type_name()))),
        })
    }
}
