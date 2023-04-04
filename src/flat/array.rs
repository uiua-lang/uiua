use std::{
    cmp::Ordering,
    slice::{Chunks, ChunksMut},
};

use crate::function::Function;

#[derive(Clone)]
pub struct Array<T> {
    shape: Vec<usize>,
    data: Vec<T>,
}

impl<T: ArrayValue> Default for Array<T> {
    fn default() -> Self {
        Self {
            shape: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn new(shape: Vec<usize>, data: Vec<T>) -> Self {
        Self { shape, data }
    }
    pub fn unit(data: T) -> Self {
        Self {
            shape: Vec::new(),
            data: vec![data],
        }
    }
    pub fn into_pair(self) -> (Vec<usize>, Vec<T>) {
        (self.shape, self.data)
    }
}

impl<T: ArrayValue> PartialEq for Array<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data.len() == other.data.len()
            && self
                .data
                .iter()
                .zip(&other.data)
                .all(|(a, b)| a.cmp(b) == Ordering::Equal)
    }
}

impl<T: ArrayValue> Eq for Array<T> {}

impl<T: ArrayValue> PartialOrd for Array<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: ArrayValue> Ord for Array<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.data
            .iter()
            .zip(&other.data)
            .map(|(a, b)| a.cmp(b))
            .find(|o| o != &Ordering::Equal)
            .unwrap_or_else(|| self.data.len().cmp(&other.data.len()))
    }
}

impl<T> From<T> for Array<T> {
    fn from(data: T) -> Self {
        Self {
            shape: Vec::new(),
            data: vec![data],
        }
    }
}

impl<T> From<(Vec<usize>, Vec<T>)> for Array<T> {
    fn from((shape, data): (Vec<usize>, Vec<T>)) -> Self {
        Self { shape, data }
    }
}

impl<T> From<Vec<T>> for Array<T> {
    fn from(data: Vec<T>) -> Self {
        Self {
            shape: Vec::new(),
            data,
        }
    }
}

impl<T> FromIterator<T> for Array<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self {
            shape: Vec::new(),
            data: iter.into_iter().collect(),
        }
    }
}

pub trait ArrayValue: Clone {
    fn cmp(&self, other: &Self) -> Ordering;
}

impl ArrayValue for f64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .unwrap_or_else(|| self.is_nan().cmp(&other.is_nan()))
    }
}

impl ArrayValue for u8 {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
}

impl ArrayValue for char {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
}

impl ArrayValue for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
}

pub trait Arrayish {
    type Value: ArrayValue;
    fn shape(&self) -> &[usize];
    fn data(&self) -> &[Self::Value];
    fn len(&self) -> usize {
        self.shape().first().copied().unwrap_or(1)
    }
    fn flat_len(&self) -> usize {
        self.data().len()
    }
    fn row_len(&self) -> usize {
        self.shape().iter().skip(1).product()
    }
    fn rows(&self) -> Chunks<Self::Value> {
        self.data().chunks(self.row_len())
    }
    fn shape_prefixes_match(&self, other: &impl Arrayish) -> bool {
        self.shape().iter().zip(other.shape()).all(|(a, b)| a == b)
    }
}

impl<'a, T> Arrayish for &'a T
where
    T: Arrayish,
{
    type Value = T::Value;
    fn shape(&self) -> &[usize] {
        T::shape(self)
    }
    fn data(&self) -> &[Self::Value] {
        T::data(self)
    }
}

pub trait ArrayishMut: Arrayish {
    fn data_mut(&mut self) -> &mut [Self::Value];
    fn rows_mut(&mut self) -> ChunksMut<Self::Value> {
        let row_len = self.row_len();
        self.data_mut().chunks_mut(row_len)
    }
}

impl<T: ArrayValue> Arrayish for Array<T> {
    type Value = T;
    fn shape(&self) -> &[usize] {
        &self.shape
    }
    fn data(&self) -> &[Self::Value] {
        &self.data
    }
}

impl<T: ArrayValue> ArrayishMut for Array<T> {
    fn data_mut(&mut self) -> &mut [Self::Value] {
        &mut self.data
    }
}

impl<T: ArrayValue> Arrayish for (&[usize], &[T]) {
    type Value = T;
    fn shape(&self) -> &[usize] {
        self.0
    }
    fn data(&self) -> &[Self::Value] {
        self.1
    }
}

impl<T: ArrayValue> Arrayish for (&[usize], &mut [T]) {
    type Value = T;
    fn shape(&self) -> &[usize] {
        self.0
    }
    fn data(&self) -> &[Self::Value] {
        self.1
    }
}

impl<T: ArrayValue> ArrayishMut for (&[usize], &mut [T]) {
    fn data_mut(&mut self) -> &mut [Self::Value] {
        self.1
    }
}
