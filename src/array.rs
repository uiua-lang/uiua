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
    Uiua,
};

/// Uiua's array type
#[derive(Clone)]
pub struct Array<T> {
    pub(crate) shape: Shape,
    pub(crate) data: CowSlice<T>,
}

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
}

impl<T: ArrayValue> Array<T> {
    pub fn unit(data: T) -> Self {
        Self::new(Shape::new(), cowslice![data])
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
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    pub fn format_shape(&self) -> FormatShape<'_> {
        FormatShape(self.shape())
    }
    pub fn into_scalar(self) -> Result<T, Self> {
        if self.shape.is_empty() {
            Ok(self.data.into_iter().next().unwrap())
        } else {
            Err(self)
        }
    }
    pub fn as_scalar(&self) -> Option<&T> {
        if self.shape.is_empty() {
            Some(&self.data[0])
        } else {
            None
        }
    }
    pub fn as_scalar_mut(&mut self) -> Option<&mut T> {
        if self.shape.is_empty() {
            Some(&mut self.data.as_mut_slice()[0])
        } else {
            None
        }
    }
    pub fn rows(&self) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator + '_ {
        (0..self.row_count()).map(|row| self.row(row))
    }
    pub fn row_slices(&self) -> impl ExactSizeIterator<Item = &[T]> + DoubleEndedIterator {
        (0..self.row_count()).map(move |row| self.row_slice(row))
    }
    #[track_caller]
    pub fn row_slice(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
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
    pub fn convert<U>(self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        self.convert_with(Into::into)
    }
    pub fn convert_with<U: Clone>(self, f: impl FnMut(T) -> U) -> Array<U> {
        Array {
            shape: self.shape,
            data: self.data.into_iter().map(f).collect(),
        }
    }
    pub fn try_convert_with<U: Clone, E>(
        self,
        f: impl FnMut(T) -> Result<U, E>,
    ) -> Result<Array<U>, E> {
        Ok(Array {
            shape: self.shape,
            data: self.data.into_iter().map(f).collect::<Result<_, _>>()?,
        })
    }
    pub fn convert_ref<U>(&self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        self.convert_ref_with(Into::into)
    }
    pub fn convert_ref_with<U: Clone>(&self, f: impl FnMut(T) -> U) -> Array<U> {
        Array {
            shape: self.shape.clone(),
            data: self.data.iter().cloned().map(f).collect(),
        }
    }
    pub fn into_rows(self) -> impl ExactSizeIterator<Item = Self> {
        let row_len = self.row_len();
        let mut row_shape = self.shape.clone();
        let row_count = if row_shape.is_empty() {
            1
        } else {
            row_shape.remove(0)
        };
        let mut data = self.data.into_iter();
        (0..row_count).map(move |_| {
            Array::new(
                row_shape.clone(),
                data.by_ref().take(row_len).collect::<CowSlice<_>>(),
            )
        })
    }
    pub fn into_rows_rev(self) -> impl Iterator<Item = Self> {
        let row_len = self.row_len();
        let mut row_shape = self.shape.clone();
        let row_count = if row_shape.is_empty() {
            1
        } else {
            row_shape.remove(0)
        };
        let mut data = self.data.into_iter().rev();
        (0..row_count).map(move |_| {
            let row: CowSlice<_> = data.by_ref().take(row_len).rev().collect();
            Array::new(row_shape.clone(), row)
        })
    }
    pub(crate) fn first_dim_zero(&self) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        let mut shape = self.shape.clone();
        shape[0] = 0;
        Array::new(shape, CowSlice::new())
    }
    pub fn show(&self) -> String {
        self.grid_string()
    }
}

impl Array<Boxed> {
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
        Self::unit(data)
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

#[allow(unused_variables)]
pub trait ArrayValue: Clone + Debug + Display + GridFmt + ArrayCmp + Send + Sync + 'static {
    const NAME: &'static str;
    fn get_fill(env: &Uiua) -> Option<Self>;
    fn array_hash<H: Hasher>(&self, hasher: &mut H);
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    fn format_sep() -> &'static str {
        " "
    }
    fn subrank(&self) -> usize {
        0
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

#[derive(Debug)]
pub enum ArrayCmpError {
    WrongType(&'static str),
    NotConstant,
}

impl fmt::Display for ArrayCmpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::WrongType(ty) => write!(f, "box has wrong type: {ty}"),
            Self::NotConstant => write!(f, "function is not a box"),
        }
    }
}

pub trait ArrayCmp<U = Self> {
    fn array_cmp(&self, other: &U) -> Ordering;
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
