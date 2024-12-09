use std::{fmt, marker::PhantomData, ops::Range, slice};

use crate::{grid_fmt::GridFmt, Value};

/// A wrapper for an array of indices
pub struct Indices<'a, T> {
    buffer: Buffer<'a, T>,
    /// The shape of the indices
    shape: Result<&'a [usize], usize>,
    pd: PhantomData<T>,
}

enum Buffer<'a, T> {
    Num(&'a [f64]),
    Byte(&'a [u8]),
    Slice(&'a [T]),
}

impl<T> Clone for Buffer<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Buffer<'_, T> {}

impl<T> Clone for Indices<'_, T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Indices<'_, T> {}

#[allow(missing_docs)]
impl<'a, T> Indices<'a, T> {
    pub fn rank(&self) -> usize {
        self.shape().len()
    }
    pub fn shape(&self) -> &[usize] {
        match &self.shape {
            Ok(shape) => shape,
            Err(len) => slice::from_ref(len),
        }
    }
    pub fn row_count(&self) -> usize {
        self.shape().first().copied().unwrap_or(1)
    }
    pub fn row_len(&self) -> usize {
        if self.shape().is_empty() {
            1
        } else {
            self.shape()[1..].iter().product()
        }
    }
    pub fn len(&self) -> usize {
        match self.buffer {
            Buffer::Num(arr) => arr.len(),
            Buffer::Byte(arr) => arr.len(),
            Buffer::Slice(arr) => arr.len(),
        }
    }
    pub fn is_empty(&self) -> bool {
        self.shape().contains(&0)
    }
    pub fn chunks_exact<'b: 'a>(
        &'b self,
        chunk_len: usize,
    ) -> impl ExactSizeIterator<Item = Self> + 'b {
        assert!(chunk_len > 0, "chunk size cannot be 0");
        assert!(self.len() % chunk_len == 0, "chunk size must divide length");
        (0..self.len() / chunk_len).map(move |i| Indices {
            buffer: self.buffer.slice(i * chunk_len, i * chunk_len + chunk_len),
            shape: Ok(&self.shape()[1..]),
            pd: PhantomData,
        })
    }
    pub fn drop_shape_first(&mut self) {
        match self.shape {
            Ok(shape) => self.shape = Ok(&shape[1..]),
            Err(_) => self.shape = Ok(&[]),
        }
    }
    pub fn drop_shape_last(&mut self) {
        match self.shape {
            Ok(shape) => self.shape = Ok(&shape[..shape.len() - 1]),
            Err(_) => self.shape = Ok(&[]),
        }
    }
}

impl<'a, T> From<&'a [T]> for Indices<'a, T> {
    fn from(slice: &'a [T]) -> Self {
        Self {
            buffer: Buffer::Slice(slice),
            shape: Err(slice.len()),
            pd: PhantomData,
        }
    }
}

impl<T> Buffer<'_, T> {
    pub fn slice(&self, start: usize, end: usize) -> Self {
        match self {
            Buffer::Num(arr) => Buffer::Num(&arr[start..end]),
            Buffer::Byte(arr) => Buffer::Byte(&arr[start..end]),
            Buffer::Slice(arr) => Buffer::Slice(&arr[start..end]),
        }
    }
}

impl<T: IndexFromElem> Indices<'_, T> {
    /// Get the index at the given position
    pub fn get(&self, i: usize) -> T {
        match self.buffer {
            Buffer::Num(arr) => {
                let n = arr[i];
                if n.is_finite() {
                    T::from_f64(arr[i])
                } else {
                    T::MAX
                }
            }
            Buffer::Byte(arr) => T::from_u8(arr[i]),
            Buffer::Slice(arr) => arr[i],
        }
    }
    /// Iterate over the indices
    pub fn iter(&self) -> impl ExactSizeIterator<Item = T> + DoubleEndedIterator + '_ {
        (0..self.len()).map(move |i| self.get(i))
    }
    /// Split the indices into the first element and the rest
    #[track_caller]
    pub fn split_first(mut self) -> (T, Self) {
        let Err(n) = self.shape else {
            panic!("Cannot split first rank 2+ array")
        };
        if n == 0 {
            panic!("Cannot split first empty array")
        }
        let first = self.get(0);
        self.shape = Err(n - 1);
        (first, self)
    }
}

pub struct Iter<'a, T> {
    indices: Indices<'a, T>,
    range: Range<usize>,
}
impl<'a, T: IndexFromElem> Iterator for Iter<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(|i| self.indices.get(i))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.range.size_hint()
    }
}
impl<'a, T: IndexFromElem> IntoIterator for Indices<'a, T> {
    type Item = T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            indices: self,
            range: 0..self.len(),
        }
    }
}
impl<'a, T: IndexFromElem> IntoIterator for &Indices<'a, T> {
    type Item = T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            indices: *self,
            range: 0..self.len(),
        }
    }
}

impl Value {
    /// Get a list index wrapper for this value
    pub fn as_index_list<T: IndexFromElem>(
        &self,
        mut expectation: &str,
    ) -> Result<Indices<'_, T>, String> {
        if expectation.is_empty() {
            expectation = T::default_expectation();
        }
        if let Value::Num(_) | Value::Byte(_) = self {
            if self.rank() > 1 {
                return Err(format!("{expectation}, but it is rank {}", self.rank()));
            }
        }
        self.as_index_array(expectation)
    }
    /// Get an indices wrapper for this value
    pub fn as_index_array<T: IndexFromElem>(
        &self,
        mut expectation: &str,
    ) -> Result<Indices<'_, T>, String> {
        if expectation.is_empty() {
            expectation = T::default_expectation();
        }
        Ok(match self {
            Value::Num(arr) => {
                for &n in &arr.data {
                    if let Err(e) = T::f64_valid(n) {
                        return Err(format!(
                            "{expectation}, but it {} is {e}",
                            n.grid_string(false)
                        ));
                    }
                }
                Indices {
                    buffer: Buffer::Num(&arr.data),
                    shape: if let [n] = &*arr.shape {
                        Err(*n)
                    } else {
                        Ok(arr.shape())
                    },
                    pd: PhantomData,
                }
            }
            Value::Byte(arr) => Indices {
                buffer: Buffer::Byte(&arr.data),
                shape: if let [n] = &*arr.shape {
                    Err(*n)
                } else {
                    Ok(arr.shape())
                },
                pd: PhantomData,
            },
            value => {
                return Err(if value.rank() == 0 {
                    format!("{expectation}, but it is a {}", value.type_name())
                } else {
                    format!("{expectation}, but it is {}", value.type_name_plural())
                })
            }
        })
    }
    /// Get an indices wrapper for this value
    pub fn as_maybe_filled_index_array<T: IndexFromElem>(
        &self,
        filled: bool,
        mut expectation: &str,
    ) -> Result<Indices<'_, T>, String> {
        if expectation.is_empty() {
            expectation = T::default_expectation();
        }
        Ok(match self {
            Value::Num(arr) => {
                for &n in &arr.data {
                    if n.is_finite() {
                        if let Err(e) = T::f64_valid(n) {
                            return Err(format!(
                                "{expectation}, but it {} is {e}",
                                n.grid_string(false)
                            ));
                        }
                    } else if !filled {
                        return Err(format!(
                            "{} cannot be used as an index without a fill",
                            n.grid_string(false),
                        ));
                    }
                }
                Indices {
                    buffer: Buffer::Num(&arr.data),
                    shape: if let [n] = &*arr.shape {
                        Err(*n)
                    } else {
                        Ok(arr.shape())
                    },
                    pd: PhantomData,
                }
            }
            Value::Byte(arr) => Indices {
                buffer: Buffer::Byte(&arr.data),
                shape: if let [n] = &*arr.shape {
                    Err(*n)
                } else {
                    Ok(arr.shape())
                },
                pd: PhantomData,
            },
            value => {
                return Err(if value.rank() == 0 {
                    format!("{expectation}, but it is a {}", value.type_name())
                } else {
                    format!("{expectation}, but it is {}", value.type_name_plural())
                })
            }
        })
    }
}

/// An error encountered when converting a value to an index
#[allow(missing_docs)]
pub enum IntoIndexError {
    Negative,
    TooLow,
    TooHigh,
    NonInteger,
}

impl fmt::Display for IntoIndexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Negative => write!(f, "negative"),
            Self::TooLow => write!(f, "too high"),
            Self::TooHigh => write!(f, "too low"),
            Self::NonInteger => write!(f, "not an integer"),
        }
    }
}

/// A trait for types that can be used as array indices
pub trait IndexFromElem: Copy {
    /// The maximum value of this type
    const MAX: Self;
    /// The default expectation message
    fn default_expectation() -> &'static str;
    /// Whether an `f64` can be converted to this type
    fn f64_valid(elem: f64) -> Result<(), IntoIndexError>;
    /// Convert a `u8` to this type
    fn from_u8(elem: u8) -> Self;
    /// Convert a `f64` to this type
    fn from_f64(elem: f64) -> Self;
}

impl IndexFromElem for usize {
    const MAX: Self = usize::MAX;
    fn default_expectation() -> &'static str {
        "Indices must be natural numbers"
    }
    fn f64_valid(elem: f64) -> Result<(), IntoIndexError> {
        if elem < 0.0 {
            return Err(IntoIndexError::Negative);
        }
        if elem > usize::MAX as f64 {
            return Err(IntoIndexError::TooHigh);
        }
        if elem.fract() != 0.0 {
            return Err(IntoIndexError::NonInteger);
        }
        Ok(())
    }
    fn from_u8(elem: u8) -> Self {
        elem as usize
    }
    fn from_f64(elem: f64) -> Self {
        elem as usize
    }
}

impl IndexFromElem for isize {
    const MAX: Self = isize::MAX;
    fn default_expectation() -> &'static str {
        "Indices must be integers"
    }
    fn f64_valid(elem: f64) -> Result<(), IntoIndexError> {
        if elem < isize::MIN as f64 {
            return Err(IntoIndexError::Negative);
        }
        if elem > isize::MAX as f64 {
            return Err(IntoIndexError::TooHigh);
        }
        if elem.fract() != 0.0 {
            return Err(IntoIndexError::NonInteger);
        }
        Ok(())
    }
    fn from_u8(elem: u8) -> Self {
        elem as isize
    }
    fn from_f64(elem: f64) -> Self {
        elem as isize
    }
}
