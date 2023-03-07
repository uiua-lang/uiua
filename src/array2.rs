use std::{cmp::Ordering, fmt, mem::ManuallyDrop};

use crate::{
    primitive::{force_length, sort_array},
    value2::Value,
};

pub struct Array {
    ty: Type,
    shape: Vec<usize>,
    data: Data,
}

pub union Data {
    numbers: ManuallyDrop<Vec<f64>>,
    chars: ManuallyDrop<Vec<char>>,
    values: ManuallyDrop<Vec<Value>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Type {
    #[default]
    Number,
    Char,
    Value,
}

impl Array {
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub fn len(&self) -> usize {
        self.shape.first().copied().unwrap_or(0)
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    pub fn ty(&self) -> Type {
        self.ty
    }
    pub fn numbers(&self) -> &[f64] {
        assert_eq!(self.ty, Type::Number);
        unsafe { &self.data.numbers }
    }
    pub fn numbers_mut(&mut self) -> &mut Vec<f64> {
        assert_eq!(self.ty, Type::Number);
        unsafe { &mut self.data.numbers }
    }
    pub fn chars(&self) -> &[char] {
        assert_eq!(self.ty, Type::Char);
        unsafe { &self.data.chars }
    }
    pub fn chars_mut(&mut self) -> &mut Vec<char> {
        assert_eq!(self.ty, Type::Char);
        unsafe { &mut self.data.chars }
    }
    pub fn values(&self) -> &[Value] {
        assert_eq!(self.ty, Type::Value);
        unsafe { &self.data.values }
    }
    pub fn values_mut(&mut self) -> &mut Vec<Value> {
        assert_eq!(self.ty, Type::Value);
        unsafe { &mut self.data.values }
    }
    pub fn range(n: usize) -> Self {
        Self::from((0..n).map(|n| n as f64).collect::<Vec<_>>())
    }
    pub fn sort(&mut self) {
        let shape = self.shape.clone();
        match self.ty {
            Type::Number => sort_array(&shape, self.numbers_mut(), |a, b| {
                a.partial_cmp(b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
            }),
            Type::Char => sort_array(&shape, self.chars_mut(), Ord::cmp),
            Type::Value => sort_array(&shape, self.values_mut(), Ord::cmp),
        }
    }
    pub fn shape_prefix_matches(&self, other: &Self) -> bool {
        self.shape.iter().zip(&other.shape).all(|(a, b)| a == b)
    }
    pub fn deshape(&mut self) {
        let data_len: usize = self.shape.iter().product();
        self.shape = vec![data_len];
    }
    pub fn reshape(&mut self, shape: impl IntoIterator<Item = usize>) {
        self.shape = shape.into_iter().collect();
        let new_len: usize = self.shape.iter().product();
        match self.ty {
            Type::Number => force_length(self.numbers_mut(), new_len),
            Type::Char => force_length(self.chars_mut(), new_len),
            Type::Value => force_length(self.values_mut(), new_len),
        }
    }
}

impl Drop for Array {
    fn drop(&mut self) {
        match self.ty {
            Type::Number => unsafe {
                ManuallyDrop::drop(&mut self.data.numbers);
            },
            Type::Char => unsafe {
                ManuallyDrop::drop(&mut self.data.chars);
            },
            Type::Value => unsafe {
                ManuallyDrop::drop(&mut self.data.values);
            },
        }
    }
}

impl Clone for Array {
    fn clone(&self) -> Self {
        match self.ty {
            Type::Number => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    numbers: ManuallyDrop::new(self.numbers().to_vec()),
                },
            },
            Type::Char => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    chars: ManuallyDrop::new(self.chars().to_vec()),
                },
            },
            Type::Value => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    values: ManuallyDrop::new(self.values().to_vec()),
                },
            },
        }
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        if self.ty != other.ty {
            return false;
        }
        if self.shape != other.shape {
            return false;
        }
        match self.ty {
            Type::Number => self.numbers() == other.numbers(),
            Type::Char => self.chars() == other.chars(),
            Type::Value => self.values() == other.values(),
        }
    }
}

impl Eq for Array {}

impl PartialOrd for Array {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Array {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ty
            .cmp(&other.ty)
            .then_with(|| self.shape.cmp(&other.shape))
            .then_with(|| match self.ty {
                Type::Number => {
                    let a = self.numbers();
                    let b = other.numbers();
                    a.len().cmp(&b.len()).then_with(|| {
                        for (a, b) in a.iter().zip(b) {
                            let ordering = match (a.is_nan(), b.is_nan()) {
                                (true, true) => Ordering::Equal,
                                (true, false) => Ordering::Greater,
                                (false, true) => Ordering::Less,
                                (false, false) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
                            };
                            if ordering != Ordering::Equal {
                                return ordering;
                            }
                        }
                        Ordering::Equal
                    })
                }
                Type::Char => self.chars().cmp(other.chars()),
                Type::Value => self.values().cmp(other.values()),
            })
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            Type::Number => {
                let da = DebugArray {
                    shape: &self.shape,
                    data: self.numbers(),
                };
                write!(f, "{da:?}",)
            }
            Type::Char => {
                let da = DebugArray {
                    shape: &self.shape,
                    data: self.chars(),
                };
                write!(f, "{da:?}",)
            }
            Type::Value => {
                let da = DebugArray {
                    shape: &self.shape,
                    data: self.values(),
                };
                write!(f, "{da:?}",)
            }
        }
    }
}

struct DebugArray<'a, T> {
    shape: &'a [usize],
    data: &'a [T],
}

impl<'a, T: fmt::Debug> fmt::Debug for DebugArray<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.shape.is_empty() {
            return write!(f, "{:?}", self.data[0]);
        }
        let cell_size = self.data.len() / self.shape[0];
        let shape = &self.shape[1..];
        f.debug_list()
            .entries(
                self.data
                    .chunks_exact(cell_size)
                    .map(|chunk| DebugArray { shape, data: chunk }),
            )
            .finish()
    }
}

impl From<Vec<f64>> for Array {
    fn from(v: Vec<f64>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: Type::Number,
            data: Data {
                numbers: ManuallyDrop::new(v),
            },
        }
    }
}

impl<'a> From<&'a str> for Array {
    fn from(s: &'a str) -> Self {
        let mut data = Vec::new();
        for c in s.chars() {
            data.push(c);
        }
        Self {
            ty: Type::Char,
            shape: vec![data.len()],
            data: Data {
                chars: ManuallyDrop::new(data),
            },
        }
    }
}

impl From<String> for Array {
    fn from(s: String) -> Self {
        Self::from(s.as_str())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn sort() {
        let mut arr = Array::from(vec![2.0, 1.0, 3.0, 6.0, 0.0, 3.0]);
        println!("{:?}", arr);
        arr.sort();
        println!("{:?}", arr);

        let mut arr = Array::from(vec![2.0, 1.0, 3.0, 6.0, 0.0, 3.0]);
        arr.reshape([4, 2]);
        println!("{:?}", arr);
        arr.sort();
        println!("{:?}", arr);
    }
}
