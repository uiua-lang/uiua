use std::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
    iter::repeat,
    rc::Rc,
    slice::Chunks,
};

use crate::{
    cowslice::{cowslice, CowSlice},
    function::Function,
    primitive::Primitive,
    Byte,
};

#[derive(Clone)]
pub struct Array<T> {
    pub(crate) shape: Vec<usize>,
    pub(crate) data: CowSlice<T>,
}

impl<T: ArrayValue> Default for Array<T> {
    fn default() -> Self {
        Self {
            shape: vec![0],
            data: CowSlice::new(),
        }
    }
}
impl<T: ArrayValue> fmt::Debug for Array<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<T: ArrayValue> fmt::Display for Array<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.rank() {
            0 => write!(f, "{}", self.data[0]),
            1 => {
                let (start, end) = T::format_delims();
                write!(f, "{}", start)?;
                for (i, x) in self.data.iter().filter(|v| !v.is_fill_value()).enumerate() {
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

impl<T: ArrayValue> Array<T> {
    #[track_caller]
    pub fn new(shape: Vec<usize>, data: CowSlice<T>) -> Self {
        validate_shape(&shape, &data);
        Self { shape, data }
    }
    #[track_caller]
    #[inline(always)]
    /// Debug only function to validate that the shape matches the data length
    pub(crate) fn validate_shape(&self) {
        validate_shape(&self.shape, &self.data);
    }
    pub fn unit(data: T) -> Self {
        Self::new(Vec::new(), cowslice![data])
    }
    pub fn row_count(&self) -> usize {
        self.shape.first().copied().unwrap_or(1)
    }
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        if self.rank() == 1 {
            self.data.iter().take_while(|x| !x.is_fill_value()).count()
        } else {
            self.row_count()
        }
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
    pub fn into_scalar(self) -> Result<T, Self> {
        if self.shape.is_empty() {
            Ok(self.data.into_iter().next().unwrap())
        } else {
            Err(self)
        }
    }
    pub fn rows(&self) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator + '_ {
        (0..self.row_count()).map(|row| self.row(row))
    }
    pub fn row_slices(&self) -> impl ExactSizeIterator<Item = &[T]> + DoubleEndedIterator {
        (0..self.row_count()).map(move |row| self.row_slice(row))
    }
    pub fn row_slice(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
    pub fn row(&self, row: usize) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        let row_len = self.row_len();
        let start = row * row_len;
        let end = start + row_len;
        Self::new(self.shape[1..].to_vec(), self.data.slice(start..end))
    }
    pub fn convert<U>(self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        Array {
            shape: self.shape,
            data: self.data.into_iter().map(Into::into).collect(),
        }
    }
    pub fn convert_ref<U>(&self) -> Array<U>
    where
        T: Into<U>,
        U: Clone,
    {
        Array {
            shape: self.shape.clone(),
            data: self.data.iter().cloned().map(Into::into).collect(),
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
            let mut row: CowSlice<_> = data.by_ref().take(row_len).collect();
            row.reverse();
            Array::new(row_shape.clone(), row)
        })
    }
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
    pub(crate) fn first_dim_zero(&self) -> Self {
        if self.rank() == 0 {
            return self.clone();
        }
        let mut shape = self.shape.clone();
        shape[0] = 0;
        Array::new(shape, CowSlice::new())
    }
    /// Remove fill elements from the end of the array
    pub fn truncate(&mut self) {
        if self.rank() == 0 {
            return;
        }
        let row_count = self.row_count();
        let mut new_len = row_count;
        for (i, row) in self.row_slices().enumerate().rev() {
            if row.iter().all(|x| x.is_fill_value()) {
                new_len = i;
            } else {
                break;
            }
        }
        if new_len == row_count {
            return;
        }
        self.data.truncate(new_len * self.row_len());
        self.shape[0] = new_len;
    }
    #[track_caller]
    pub fn from_row_arrays(values: impl IntoIterator<Item = Self>) -> Self {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Self::default();
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            value = if count == 2 {
                value.couple(row)
            } else {
                value.join_impl(row, false)
            };
        }
        if count == 1 {
            value.shape.insert(0, 1);
        }
        value
    }
    fn is_all_fill(&self) -> bool {
        self.data.iter().all(|x| x.is_fill_value())
    }
}

impl<T: ArrayValue> PartialEq for Array<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.rank() != other.rank() {
            return false;
        }
        match self.rank() {
            0 => self.data[0].eq(&other.data[0]),
            1 => {
                let mut a = self
                    .data
                    .iter()
                    .skip_while(|x| x.is_fill_value())
                    .take_while(|x| !x.is_fill_value());
                let mut b = other
                    .data
                    .iter()
                    .skip_while(|x| x.is_fill_value())
                    .take_while(|x| !x.is_fill_value());
                a.by_ref().zip(b.by_ref()).all(|(a, b)| a.eq(b))
                    && a.next().is_none()
                    && b.next().is_none()
            }
            _ => {
                let a = self
                    .rows()
                    .skip_while(|x| x.is_all_fill())
                    .take_while(|x| x.is_all_fill());
                let b = other
                    .rows()
                    .skip_while(|x| !x.is_all_fill())
                    .take_while(|x| !x.is_all_fill());
                a.eq(b)
            }
        }
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
        let rank_cmp = self.rank().cmp(&other.rank());
        if rank_cmp != Ordering::Equal {
            return rank_cmp;
        }
        match self.rank() {
            0 => self.data[0].cmp(&other.data[0]),
            1 => {
                let mut a = self
                    .data
                    .iter()
                    .skip_while(|x| x.is_fill_value())
                    .take_while(|x| !x.is_fill_value());
                let mut b = other
                    .data
                    .iter()
                    .skip_while(|x| x.is_fill_value())
                    .take_while(|x| !x.is_fill_value());
                a.by_ref()
                    .zip(b.by_ref())
                    .map(|(a, b)| a.cmp(b))
                    .find(|o| o != &Ordering::Equal)
                    .unwrap_or_else(|| a.count().cmp(&b.count()))
            }
            _ => {
                let a = self
                    .rows()
                    .skip_while(|x| x.is_all_fill())
                    .take_while(|x| x.is_all_fill());
                let b = other
                    .rows()
                    .skip_while(|x| !x.is_all_fill())
                    .take_while(|x| !x.is_all_fill());
                a.cmp(b)
            }
        }
    }
}

impl<T: ArrayValue> From<T> for Array<T> {
    fn from(data: T) -> Self {
        Self::unit(data)
    }
}

impl<T: ArrayValue> From<(Vec<usize>, CowSlice<T>)> for Array<T> {
    fn from((shape, data): (Vec<usize>, CowSlice<T>)) -> Self {
        Self::new(shape, data)
    }
}

impl<T: ArrayValue> From<(Vec<usize>, Vec<T>)> for Array<T> {
    fn from((shape, data): (Vec<usize>, Vec<T>)) -> Self {
        Self::new(shape, data.into())
    }
}

impl<T: ArrayValue> From<Vec<T>> for Array<T> {
    fn from(data: Vec<T>) -> Self {
        Self::new(vec![data.len()], data.into())
    }
}

impl<T: ArrayValue> From<CowSlice<T>> for Array<T> {
    fn from(data: CowSlice<T>) -> Self {
        Self::new(vec![data.len()], data)
    }
}

impl<T: ArrayValue> FromIterator<T> for Array<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<Vec<T>>())
    }
}

impl From<String> for Array<char> {
    fn from(s: String) -> Self {
        Self::new(vec![s.len()], s.chars().collect())
    }
}

impl From<Vec<bool>> for Array<Byte> {
    fn from(data: Vec<bool>) -> Self {
        Self::new(vec![data.len()], data.into_iter().map(Byte::from).collect())
    }
}

impl FromIterator<String> for Array<char> {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        let mut lines: Vec<String> = iter.into_iter().collect();
        let max_len = lines.iter().map(|s| s.chars().count()).max().unwrap_or(0);
        let mut data = Vec::with_capacity(max_len * lines.len());
        let shape = vec![lines.len(), max_len];
        for line in lines.drain(..) {
            data.extend(line.chars());
            data.extend(repeat('\x00').take(max_len - line.chars().count()));
        }
        (shape, data).into()
    }
}

pub trait ArrayValue: Clone + Debug + Display {
    const NAME: &'static str;
    fn cmp(&self, other: &Self) -> Ordering;
    fn fill_value() -> Self;
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    fn format_sep() -> &'static str {
        " "
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
    fn fill_value() -> Self {
        f64::NAN
    }
    fn is_fill_value(&self) -> bool {
        self.is_nan()
    }
}

impl ArrayValue for Byte {
    const NAME: &'static str = "byte";
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
    fn fill_value() -> Self {
        Byte::Fill
    }
    fn is_fill_value(&self) -> bool {
        *self == Byte::Fill
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
    fn fill_value() -> Self {
        '\0'
    }
    fn is_fill_value(&self) -> bool {
        *self == '\0'
    }
}

impl ArrayValue for Rc<Function> {
    const NAME: &'static str = "function";
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self, other)
    }
    fn fill_value() -> Self {
        Rc::new(Primitive::FillValue.into())
    }
    fn is_fill_value(&self) -> bool {
        self.as_primitive() == Some(Primitive::FillValue)
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
    fn format_shape(&self) -> FormatShape<'_> {
        FormatShape(self.shape())
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

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FormatShape<'a>(&'a [usize]);

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
