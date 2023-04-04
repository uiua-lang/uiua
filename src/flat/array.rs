use std::{
    cmp::Ordering,
    slice::{self, Chunks, ChunksMut},
};

use crate::{function::Function, Uiua, UiuaResult};

#[derive(Clone)]
pub struct Array<T> {
    shape: Vec<usize>,
    data: Vec<T>,
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

fn unit_arrayish<T: ArrayValue>(value: &T) -> (&[usize], &[T]) {
    (&[], slice::from_ref(value))
}

pub fn bin_pervade<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: Array<A>,
    b: Array<B>,
    env: &Uiua,
    f: impl Fn(A, B) -> C,
) -> UiuaResult<Array<C>> {
    if !a.shape_prefixes_match(&b) {
        return Err(env.error(format!(
            "Shapes {:?} and {:?} do not match",
            a.shape, b.shape
        )));
    }
    let shape = a.shape().max(b.shape()).to_vec();
    let mut data = Vec::with_capacity(a.flat_len().max(b.flat_len()));
    bin_pervade_recursive(a, b, &mut data, f);
    Ok(Array::new(shape, data))
}

pub fn bin_pervade_fallible<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: Array<A>,
    b: Array<B>,
    env: &Uiua,
    f: impl Fn(A, B, &Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult<Array<C>> {
    if !a.shape_prefixes_match(&b) {
        return Err(env.error(format!(
            "Shapes {:?} and {:?} do not match",
            a.shape, b.shape
        )));
    }
    let shape = a.shape().max(b.shape()).to_vec();
    let mut data = Vec::with_capacity(a.flat_len().max(b.flat_len()));
    bin_pervade_recursive_fallible(a, b, &mut data, env, f)?;
    Ok(Array::new(shape, data))
}

fn bin_pervade_recursive<A: Arrayish, B: Arrayish, C: ArrayValue>(
    a: A,
    b: B,
    c: &mut Vec<C>,
    f: impl Fn(A::Value, B::Value) -> C,
) {
    match (a.shape(), b.shape()) {
        ([], []) => c.push(f(a.data()[0].clone(), b.data()[0].clone())),
        (ash, bsh) if ash == bsh => {
            for (a, b) in a.data().iter().zip(b.data().iter()) {
                c.push(f(a.clone(), b.clone()));
            }
        }
        ([], bsh) => {
            for (a, b) in a.data().iter().zip(b.rows()) {
                bin_pervade_recursive(unit_arrayish(a), (&bsh[1..], b), c, &f);
            }
        }
        (ash, []) => {
            for (a, b) in a.rows().zip(b.data().iter()) {
                bin_pervade_recursive((&ash[1..], a), unit_arrayish(b), c, &f);
            }
        }
        (ash, bsh) => {
            for (a, b) in a.rows().zip(b.rows()) {
                bin_pervade_recursive((&ash[1..], a), (&bsh[1..], b), c, &f);
            }
        }
    }
}

fn bin_pervade_recursive_fallible<A: Arrayish, B: Arrayish, C: ArrayValue>(
    a: A,
    b: B,
    c: &mut Vec<C>,
    env: &Uiua,
    f: impl Fn(A::Value, B::Value, &Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult {
    match (a.shape(), b.shape()) {
        ([], []) => c.push(f(a.data()[0].clone(), b.data()[0].clone(), env)?),
        ([], bsh) => {
            for (a, b) in a.data().iter().zip(b.rows()) {
                bin_pervade_recursive_fallible(unit_arrayish(a), (&bsh[1..], b), c, env, f)?;
            }
        }
        (ash, []) => {
            for (a, b) in a.rows().zip(b.data().iter()) {
                bin_pervade_recursive_fallible((&ash[1..], a), unit_arrayish(b), c, env, f)?;
            }
        }
        (ash, bsh) => {
            for (a, b) in a.rows().zip(b.rows()) {
                bin_pervade_recursive_fallible((&ash[1..], a), (&bsh[1..], b), c, env, f)?;
            }
        }
    }
    Ok(())
}
