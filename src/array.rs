use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    ops::Deref,
    rc::Rc,
    slice::{Chunks, ChunksMut},
};

use crate::function::Function;

#[derive(Debug, Clone)]
pub struct Array<T> {
    pub(crate) shape: Vec<usize>,
    pub(crate) data: Vec<T>,
}

impl<T: ArrayValue> Default for Array<T> {
    fn default() -> Self {
        Self {
            shape: Vec::new(),
            data: Vec::new(),
        }
    }
}

impl<T: ArrayValue> fmt::Display for Array<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.rank() {
            0 => write!(f, "{}", self.data[0]),
            1 => {
                let (start, end) = T::format_delims();
                write!(f, "{}", start)?;
                for (i, x) in self.data.iter().enumerate() {
                    if i > 0 {
                        write!(f, "{}", T::format_sep())?;
                    }
                    write!(f, "{}", x)?;
                }
                write!(f, "{}", end)
            }
            _ => {
                write!(f, "[")?;
                for (i, dim) in self.shape().iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{dim}")?;
                }
                write!(f, ",")?;
                for val in &self.data {
                    write!(f, " {}", val)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl<T> Array<T> {
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
    pub fn row_count(&self) -> usize {
        self.shape.first().copied().unwrap_or(1)
    }
    pub fn flat_len(&self) -> usize {
        self.data.len()
    }
    pub fn row_len(&self) -> usize {
        self.shape.iter().skip(1).product()
    }
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub fn rows(&self) -> impl Iterator<Item = Row<T>> {
        (0..self.row_count()).map(|row| Row { array: self, row })
    }
    pub fn rows_mut(&mut self) -> ChunksMut<T> {
        let row_len = self.row_len();
        self.data.chunks_mut(row_len)
    }
    pub fn row(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
    pub fn convert<U>(self) -> Array<U>
    where
        T: Into<U>,
    {
        Array {
            shape: self.shape,
            data: self.data.into_iter().map(Into::into).collect(),
        }
    }
    pub fn into_rows(self) -> impl Iterator<Item = Self> {
        let row_len = self.row_len();
        let mut row_shape = self.shape.clone();
        let row_count = if row_shape.is_empty() {
            1
        } else {
            row_shape.remove(0)
        };
        let mut data = self.data.into_iter();
        (0..row_count)
            .map(move |_| Array::new(row_shape.clone(), data.by_ref().take(row_len).collect()))
    }
    pub fn into_rows_rev(mut self) -> impl Iterator<Item = Self> {
        let row_len = self.row_len();
        let mut row_shape = self.shape.clone();
        let row_count = if row_shape.is_empty() {
            1
        } else {
            row_shape.remove(0)
        };
        (0..row_count).map(move |_| {
            let end = self.data.len() - row_len;
            Array::new(row_shape.clone(), self.data.drain(end..).collect())
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn val_eq<U: Into<T> + Clone>(&self, other: &Array<U>) -> bool {
        self.shape == other.shape
            && self.data.len() == other.data.len()
            && self
                .data
                .iter()
                .zip(&other.data)
                .all(|(a, b)| T::eq(a, &b.clone().into()))
    }
    pub fn val_cmp<U: Into<T> + Clone>(&self, other: &Array<U>) -> Ordering {
        self.data
            .iter()
            .zip(&other.data)
            .map(|(a, b)| a.cmp(&b.clone().into()))
            .find(|o| o != &Ordering::Equal)
            .unwrap_or_else(|| self.data.len().cmp(&other.data.len()))
    }
    pub fn empty_row(&self) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        Array::new(self.shape[1..].to_vec(), Vec::new())
    }
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        if self.rank() == 1 && T::fill_value().is_some() {
            self.data.iter().take_while(|x| !x.is_fill_value()).count()
        } else {
            self.row_count()
        }
    }
}

impl<T: ArrayValue> PartialEq for Array<T> {
    fn eq(&self, other: &Self) -> bool {
        self.shape == other.shape
            && self.data.len() == other.data.len()
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
        Some(self.val_cmp(other))
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
            shape: vec![data.len()],
            data,
        }
    }
}

impl<T> FromIterator<T> for Array<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<Vec<T>>())
    }
}

pub struct Row<'a, T> {
    array: &'a Array<T>,
    row: usize,
}

impl<'a, T> Clone for Row<'a, T> {
    fn clone(&self) -> Self {
        Row {
            array: self.array,
            row: self.row,
        }
    }
}

impl<'a, T> Copy for Row<'a, T> {}

impl<'a, T> AsRef<[T]> for Row<'a, T> {
    fn as_ref(&self) -> &[T] {
        self.array.row(self.row)
    }
}

impl<'a, T> Deref for Row<'a, T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.array.row(self.row)
    }
}

impl<'a, T: ArrayValue> PartialEq for Row<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        self.array.data.len() == other.array.data.len()
            && self
                .array
                .data
                .iter()
                .zip(&other.array.data)
                .all(|(a, b)| a.eq(b))
    }
}

impl<'a, T: ArrayValue> Eq for Row<'a, T> {}

impl<'a, T: ArrayValue> PartialOrd for Row<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T: ArrayValue> Ord for Row<'a, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.array
            .data
            .iter()
            .zip(&other.array.data)
            .map(|(a, b)| a.cmp(b))
            .find(|o| o != &Ordering::Equal)
            .unwrap_or_else(|| self.array.data.len().cmp(&other.array.data.len()))
    }
}

impl<'a, T: ArrayValue> Arrayish for Row<'a, T> {
    type Value = T;
    fn shape(&self) -> &[usize] {
        &self.array.shape[1..]
    }
    fn data(&self) -> &[T] {
        self
    }
}

pub trait ArrayValue: Clone + Debug + Display {
    const NAME: &'static str;
    fn cmp(&self, other: &Self) -> Ordering;
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    fn format_sep() -> &'static str {
        " "
    }
    fn fill_value() -> Option<Self> {
        None
    }
    fn is_fill_value(&self) -> bool {
        false
    }
}

impl ArrayValue for f64 {
    const NAME: &'static str = "number";
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .unwrap_or_else(|| self.is_nan().cmp(&other.is_nan()))
    }
}

impl ArrayValue for u8 {
    const NAME: &'static str = "byte";
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
}

impl ArrayValue for char {
    const NAME: &'static str = "character";
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
    fn format_delims() -> (&'static str, &'static str) {
        ("", "")
    }
    fn format_sep() -> &'static str {
        ""
    }
    fn fill_value() -> Option<Self> {
        Some('\x00')
    }
    fn is_fill_value(&self) -> bool {
        *self == '\x00'
    }
}

impl ArrayValue for Rc<Function> {
    const NAME: &'static str = "function";
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
}

#[allow(clippy::len_without_is_empty)]
pub trait Arrayish {
    type Value: ArrayValue;
    fn shape(&self) -> &[usize];
    fn data(&self) -> &[Self::Value];
    fn rank(&self) -> usize {
        self.shape().len()
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

impl<T: ArrayValue> Arrayish for Array<T> {
    type Value = T;
    fn shape(&self) -> &[usize] {
        &self.shape
    }
    fn data(&self) -> &[Self::Value] {
        &self.data
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
