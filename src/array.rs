use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    hash::{Hash, Hasher},
};

use ecow::EcoVec;
use tinyvec::{tiny_vec, TinyVec};

use crate::{
    boxed::Boxed,
    cowslice::{cowslice, CowSlice},
    grid_fmt::GridFmt,
    value::Value,
    Complex, Uiua,
};

/// Uiua's array type
#[derive(Clone)]
pub struct Array<T> {
    pub(crate) shape: Shape,
    pub(crate) data: CowSlice<T>,
}

/// Uiua's array shape type
pub type Shape = TinyVec<[usize; 3]>;

impl<T: ArrayValue> Default for Array<T> {
    fn default() -> Self {
        Self {
            shape: tiny_vec![0],
            data: CowSlice::new(),
        }
    }
}

impl<T: ArrayValue> fmt::Debug for Array<T>
where
    Array<T>: GridFmt,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.grid_string())
    }
}

impl<T: ArrayValue> fmt::Display for Array<T>
where
    Array<T>: GridFmt,
{
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
                write!(f, "\n{}", self.grid_string())
            }
        }
    }
}

#[track_caller]
#[inline(always)]
fn validate_shape<T>(shape: &[usize], data: &[T]) {
    debug_assert_eq!(
        shape.iter().product::<usize>(),
        data.len(),
        "shape {shape:?} does not match data length {}",
        data.len()
    );
}

impl<T> Array<T> {
    #[track_caller]
    /// Create an array from a shape and data
    ///
    /// # Panics
    /// Panics in debug mode if the shape does not match the data length
    pub fn new(shape: impl Into<Shape>, data: impl Into<CowSlice<T>>) -> Self {
        let shape = shape.into();
        let data = data.into();
        validate_shape(&shape, &data);
        Self { shape, data }
    }
    #[track_caller]
    #[inline(always)]
    /// Debug-only function to validate that the shape matches the data length
    pub(crate) fn validate_shape(&self) {
        validate_shape(&self.shape, &self.data);
    }
    /// Get the number of rows in the array
    pub fn row_count(&self) -> usize {
        self.shape.first().copied().unwrap_or(1)
    }
    /// Get the number of elements in the array
    pub fn element_count(&self) -> usize {
        self.data.len()
    }
    /// Get the number of elements in a row
    pub fn row_len(&self) -> usize {
        self.shape.iter().skip(1).product()
    }
    /// Get the rank of the array
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    /// Get the shape of the array
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    /// Get a formattable shape of the array
    pub fn format_shape(&self) -> FormatShape<'_> {
        FormatShape(self.shape())
    }
    /// Get an iterator over the row slices of the array
    pub fn row_slices(&self) -> impl ExactSizeIterator<Item = &[T]> + DoubleEndedIterator {
        (0..self.row_count()).map(move |row| self.row_slice(row))
    }
    /// Get a slice of a row
    #[track_caller]
    pub fn row_slice(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
}

impl<T: ArrayValue> Array<T> {
    /// Create a scalar array
    pub fn scalar(data: T) -> Self {
        Self::new(Shape::new(), cowslice![data])
    }
    /// Attempt to convert the array into a scalar
    pub fn into_scalar(self) -> Result<T, Self> {
        if self.shape.is_empty() {
            Ok(self.data.into_iter().next().unwrap())
        } else {
            Err(self)
        }
    }
    /// Attempt to get a reference to the scalar value
    pub fn as_scalar(&self) -> Option<&T> {
        if self.shape.is_empty() {
            Some(&self.data[0])
        } else {
            None
        }
    }
    /// Attempt to get a mutable reference to the scalar value
    pub fn as_scalar_mut(&mut self) -> Option<&mut T> {
        if self.shape.is_empty() {
            Some(&mut self.data.as_mut_slice()[0])
        } else {
            None
        }
    }
    /// Get an iterator over the row arrays of the array
    pub fn rows(&self) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator + '_ {
        (0..self.row_count()).map(|row| self.row(row))
    }
    /// Get a row array
    #[track_caller]
    pub fn row(&self, row: usize) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        let row_count = self.row_count();
        if row >= row_count {
            panic!("row index out of bounds: {} >= {}", row, row_count);
        }
        let row_len = self.row_len();
        let start = row * row_len;
        let end = start + row_len;
        Self::new(&self.shape[1..], self.data.slice(start..end))
    }
    /// Convert the elements of the array
    pub fn convert<U>(self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        self.convert_with(Into::into)
    }
    /// Convert the elements of the array with a function
    pub fn convert_with<U: Clone>(self, f: impl FnMut(T) -> U) -> Array<U> {
        Array {
            shape: self.shape,
            data: self.data.into_iter().map(f).collect(),
        }
    }
    /// Convert the elements of the array with a fallible function
    pub fn try_convert_with<U: Clone, E>(
        self,
        f: impl FnMut(T) -> Result<U, E>,
    ) -> Result<Array<U>, E> {
        Ok(Array {
            shape: self.shape,
            data: self.data.into_iter().map(f).collect::<Result<_, _>>()?,
        })
    }
    /// Convert the elements of the array without consuming it
    pub fn convert_ref<U>(&self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        self.convert_ref_with(Into::into)
    }
    /// Convert the elements of the array with a function without consuming it
    pub fn convert_ref_with<U: Clone>(&self, f: impl FnMut(T) -> U) -> Array<U> {
        Array {
            shape: self.shape.clone(),
            data: self.data.iter().cloned().map(f).collect(),
        }
    }
    /// Consume the array and get an iterator over its rows
    pub fn into_rows(self) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator {
        (0..self.row_count()).map(move |i| self.row(i))
    }
    pub(crate) fn first_dim_zero(&self) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        let mut shape = self.shape.clone();
        shape[0] = 0;
        Array::new(shape, CowSlice::new())
    }
    /// Get a pretty-printed string representing the array
    ///
    /// This is what is printed by the `&s` function
    pub fn show(&self) -> String {
        self.grid_string()
    }
}

impl Array<Boxed> {
    /// Attempt to unbox a scalar box array
    pub fn into_unboxed(self) -> Result<Value, Self> {
        match self.into_scalar() {
            Ok(v) => Ok(v.0),
            Err(a) => Err(a),
        }
    }
}

impl<T: ArrayValue + ArrayCmp<U>, U: ArrayValue> PartialEq<Array<U>> for Array<T> {
    fn eq(&self, other: &Array<U>) -> bool {
        if self.shape() != other.shape() {
            return false;
        }
        self.data
            .iter()
            .zip(&other.data)
            .all(|(a, b)| a.array_eq(b))
    }
}

impl<T: ArrayValue> Eq for Array<T> {}

impl<T: ArrayValue + ArrayCmp<U>, U: ArrayValue> PartialOrd<Array<U>> for Array<T> {
    fn partial_cmp(&self, other: &Array<U>) -> Option<Ordering> {
        let rank_cmp = self.rank().cmp(&other.rank());
        if rank_cmp != Ordering::Equal {
            return Some(rank_cmp);
        }
        let cmp = self
            .data
            .iter()
            .zip(&other.data)
            .map(|(a, b)| a.array_cmp(b))
            .find(|o| o != &Ordering::Equal)
            .unwrap_or_else(|| self.data.len().cmp(&other.data.len()));
        Some(cmp)
    }
}

impl<T: ArrayValue> Ord for Array<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<T: ArrayValue> Hash for Array<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.shape.hash(hasher);
        self.data.iter().for_each(|x| x.array_hash(hasher));
    }
}

impl<T: ArrayValue> From<T> for Array<T> {
    fn from(data: T) -> Self {
        Self::scalar(data)
    }
}

impl<T: ArrayValue> From<EcoVec<T>> for Array<T> {
    fn from(data: EcoVec<T>) -> Self {
        Self::new(tiny_vec![data.len()], data)
    }
}

impl<T: ArrayValue> From<CowSlice<T>> for Array<T> {
    fn from(data: CowSlice<T>) -> Self {
        Self::new(tiny_vec![data.len()], data)
    }
}

impl<'a, T: ArrayValue> From<&'a [T]> for Array<T> {
    fn from(data: &'a [T]) -> Self {
        Self::new(tiny_vec![data.len()], data)
    }
}

impl<T: ArrayValue> FromIterator<T> for Array<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<CowSlice<T>>())
    }
}

impl From<String> for Array<char> {
    fn from(s: String) -> Self {
        Self::new(tiny_vec![s.len()], s.chars().collect::<CowSlice<_>>())
    }
}

impl From<Vec<bool>> for Array<u8> {
    fn from(data: Vec<bool>) -> Self {
        Self::new(
            tiny_vec![data.len()],
            data.into_iter().map(u8::from).collect::<CowSlice<_>>(),
        )
    }
}

impl From<bool> for Array<u8> {
    fn from(data: bool) -> Self {
        Self::new(tiny_vec![], cowslice![u8::from(data)])
    }
}

impl From<Vec<usize>> for Array<f64> {
    fn from(data: Vec<usize>) -> Self {
        Self::new(
            tiny_vec![data.len()],
            data.into_iter().map(|u| u as f64).collect::<CowSlice<_>>(),
        )
    }
}

impl FromIterator<String> for Array<Boxed> {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Array::from(
            iter.into_iter()
                .map(Value::from)
                .map(Boxed)
                .collect::<CowSlice<_>>(),
        )
    }
}

/// A trait for types that can be used as array elements
#[allow(unused_variables)]
pub trait ArrayValue: Clone + Debug + Display + GridFmt + ArrayCmp + Send + Sync + 'static {
    /// The type name
    const NAME: &'static str;
    /// Get the fill value from the environment
    fn get_fill(env: &Uiua) -> Option<Self>;
    /// Hash the value
    fn array_hash<H: Hasher>(&self, hasher: &mut H);
    /// Delimiters for formatting
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    /// Separator for formatting
    fn format_sep() -> &'static str {
        " "
    }
}

impl ArrayValue for f64 {
    const NAME: &'static str = "number";
    fn get_fill(env: &Uiua) -> Option<Self> {
        env.num_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        let v = if self.is_nan() {
            f64::NAN
        } else if *self == 0.0 && self.is_sign_negative() {
            0.0
        } else {
            *self
        };
        v.to_bits().hash(hasher)
    }
}

impl ArrayValue for u8 {
    const NAME: &'static str = "number";
    fn get_fill(env: &Uiua) -> Option<Self> {
        env.byte_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash(hasher)
    }
}

impl ArrayValue for char {
    const NAME: &'static str = "character";
    fn get_fill(env: &Uiua) -> Option<Self> {
        env.char_fill()
    }
    fn format_delims() -> (&'static str, &'static str) {
        ("", "")
    }
    fn format_sep() -> &'static str {
        ""
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash(hasher)
    }
}

impl ArrayValue for Boxed {
    const NAME: &'static str = "box";
    fn get_fill(env: &Uiua) -> Option<Self> {
        env.box_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash(hasher)
    }
}

impl ArrayValue for Complex {
    const NAME: &'static str = "complex";
    fn get_fill(env: &Uiua) -> Option<Self> {
        env.complex_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        for n in [self.re, self.im] {
            n.array_hash(hasher);
        }
    }
}

/// Trait for comparing array elements
pub trait ArrayCmp<U = Self> {
    /// Compare two elements
    fn array_cmp(&self, other: &U) -> Ordering;
    /// Check if two elements are equal
    fn array_eq(&self, other: &U) -> bool {
        self.array_cmp(other) == Ordering::Equal
    }
}

impl ArrayCmp for f64 {
    fn array_cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .unwrap_or_else(|| self.is_nan().cmp(&other.is_nan()))
    }
}

impl ArrayCmp for u8 {
    fn array_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl ArrayCmp for Complex {
    fn array_cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other)
            .unwrap_or_else(|| self.re.is_nan().cmp(&other.re.is_nan()))
            .then_with(|| self.im.is_nan().cmp(&other.im.is_nan()))
    }
}

impl ArrayCmp for char {
    fn array_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl ArrayCmp for Boxed {
    fn array_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

impl ArrayCmp<f64> for u8 {
    fn array_cmp(&self, other: &f64) -> Ordering {
        (*self as f64).array_cmp(other)
    }
}

impl ArrayCmp<u8> for f64 {
    fn array_cmp(&self, other: &u8) -> Ordering {
        self.array_cmp(&(*other as f64))
    }
}

/// A formattable shape
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FormatShape<'a>(pub &'a [usize]);

impl<'a> fmt::Debug for FormatShape<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<'a> fmt::Display for FormatShape<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        for (i, dim) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " × ")?;
            }
            write!(f, "{dim}")?;
        }
        write!(f, "]")
    }
}
