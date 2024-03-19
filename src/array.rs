use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

use bitflags::bitflags;
use ecow::{EcoString, EcoVec};
use serde::*;

use crate::{
    algorithm::map::MapKeys,
    cowslice::{cowslice, CowSlice},
    grid_fmt::GridFmt,
    Boxed, Complex, HandleKind, Shape, Uiua, Value,
};

/// Uiua's array type
#[derive(Clone, Serialize, Deserialize)]
#[serde(bound(
    serialize = "T: Clone + Serialize",
    deserialize = "T: Clone + Deserialize<'de>"
))]
#[repr(C)]
pub struct Array<T> {
    #[serde(rename = "s", default, skip_serializing_if = "<[_]>::is_empty")]
    pub(crate) shape: Shape,
    #[serde(rename = "d", default, skip_serializing_if = "<[_]>::is_empty")]
    pub(crate) data: CowSlice<T>,
    #[serde(
        rename = "m",
        flatten,
        default,
        skip_serializing_if = "Option::is_none",
        with = "meta_ser"
    )]
    pub(crate) meta: Option<Arc<ArrayMeta>>,
}

mod meta_ser {
    use super::*;
    pub fn serialize<S: Serializer>(
        meta: &Option<Arc<ArrayMeta>>,
        s: S,
    ) -> Result<S::Ok, S::Error> {
        if let Some(meta) = meta {
            meta.serialize(s)
        } else {
            s.serialize_none()
        }
    }
    pub fn deserialize<'de, D: Deserializer<'de>>(
        d: D,
    ) -> Result<Option<Arc<ArrayMeta>>, D::Error> {
        Ok(Option::<ArrayMeta>::deserialize(d)?.map(Arc::new))
    }
}

/// Non-shape metadata for an array
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct ArrayMeta {
    /// The label
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub label: Option<EcoString>,
    /// Flags for the array
    #[serde(default, skip_serializing_if = "ArrayFlags::is_empty")]
    pub flags: ArrayFlags,
    /// The keys of a map array
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub map_keys: Option<MapKeys>,
    /// The pointer value for FFI
    #[serde(skip)]
    pub pointer: Option<usize>,
    /// The kind of system handle
    #[serde(skip)]
    pub handle_kind: Option<HandleKind>,
}

bitflags! {
    /// Flags for an array
    #[derive(Clone, Copy, Default, Serialize, Deserialize)]
    pub struct ArrayFlags: u8 {
        /// No flags
        const NONE = 0;
        /// The array is boolean
        const BOOLEAN = 1;
    }
}

impl ArrayFlags {
    /// Check if the array is boolean
    pub fn is_boolean(self) -> bool {
        self.contains(Self::BOOLEAN)
    }
    /// Reset all flags
    pub fn reset(&mut self) {
        *self = Self::NONE;
    }
}

/// Default metadata for an array
pub static DEFAULT_META: ArrayMeta = ArrayMeta {
    label: None,
    flags: ArrayFlags::NONE,
    map_keys: None,
    pointer: None,
    handle_kind: None,
};

/// Array metadata that can be persisted across operations
#[derive(Clone, Default)]
pub struct PersistentMeta {
    label: Option<EcoString>,
    map_keys: Option<MapKeys>,
}

impl PersistentMeta {
    /// XOR this metadata with another
    pub fn xor(self, other: Self) -> Self {
        Self {
            label: self.label.xor(other.label),
            map_keys: self.map_keys.xor(other.map_keys),
        }
    }
    /// XOR several metadatas
    pub fn xor_all(metas: impl IntoIterator<Item = Self>) -> Self {
        let mut label = None;
        let mut map_keys = None;
        let mut set_label = false;
        let mut set_map_keys = false;
        for meta in metas {
            if let Some(l) = meta.label {
                if set_label {
                    label = None;
                } else {
                    label = Some(l);
                    set_label = true;
                }
            }
            if let Some(keys) = meta.map_keys {
                if set_map_keys {
                    map_keys = None;
                } else {
                    map_keys = Some(keys);
                    set_map_keys = true;
                }
            }
        }
        Self { label, map_keys }
    }
}

impl<T: ArrayValue> Default for Array<T> {
    fn default() -> Self {
        Self {
            shape: 0.into(),
            data: CowSlice::new(),
            meta: None,
        }
    }
}

impl<T: ArrayValue> fmt::Debug for Array<T>
where
    Array<T>: GridFmt,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.grid_string(true))
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
                write!(f, "\n{}", self.grid_string(false))
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
        Self {
            shape,
            data,
            meta: None,
        }
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
    pub fn shape(&self) -> &Shape {
        &self.shape
    }
    /// Get a mutable reference to the shape of the array
    pub fn shape_mut(&mut self) -> &mut Shape {
        &mut self.shape
    }
    /// Get the metadata of the array
    pub fn meta(&self) -> &ArrayMeta {
        self.meta.as_deref().unwrap_or(&DEFAULT_META)
    }
    pub(crate) fn meta_mut_impl(meta: &mut Option<Arc<ArrayMeta>>) -> &mut ArrayMeta {
        let meta = meta.get_or_insert_with(Default::default);
        Arc::make_mut(meta)
    }
    /// Get a mutable reference to the metadata of the array if it exists
    pub fn get_meta_mut(&mut self) -> Option<&mut ArrayMeta> {
        self.meta.as_mut().map(Arc::make_mut)
    }
    /// Get a mutable reference to the metadata of the array
    pub fn meta_mut(&mut self) -> &mut ArrayMeta {
        Self::meta_mut_impl(&mut self.meta)
    }
    /// Take the label from the metadata
    pub fn take_label(&mut self) -> Option<EcoString> {
        self.get_meta_mut().and_then(|meta| meta.label.take())
    }
    /// Take the map keys from the metadata
    pub fn take_map_keys(&mut self) -> Option<MapKeys> {
        self.get_meta_mut().and_then(|meta| meta.map_keys.take())
    }
    /// The the persistent metadata of the array
    pub fn take_per_meta(&mut self) -> PersistentMeta {
        if let Some(meta) = self.get_meta_mut() {
            let label = meta.label.take();
            let map_keys = meta.map_keys.take();
            PersistentMeta { label, map_keys }
        } else {
            PersistentMeta::default()
        }
    }
    /// Set the map keys in the metadata
    pub fn set_per_meta(&mut self, per_meta: PersistentMeta) {
        if let Some(keys) = per_meta.map_keys {
            self.meta_mut().map_keys = Some(keys);
        } else if let Some(meta) = self.get_meta_mut() {
            meta.map_keys = None;
        }
        if let Some(label) = per_meta.label {
            self.meta_mut().label = Some(label);
        } else if let Some(meta) = self.get_meta_mut() {
            meta.label = None;
        }
    }
    /// Get a reference to the map keys
    pub fn map_keys(&self) -> Option<&MapKeys> {
        self.meta().map_keys.as_ref()
    }
    /// Get a mutable reference to the map keys
    pub fn map_keys_mut(&mut self) -> Option<&mut MapKeys> {
        self.get_meta_mut().and_then(|meta| meta.map_keys.as_mut())
    }
    /// Reset all metadata
    pub fn reset_meta(&mut self) {
        self.meta = None;
    }
    /// Reset all metadata flags
    pub fn reset_meta_flags(&mut self) {
        if self.meta.is_some() {
            self.meta_mut().flags.reset();
        }
    }
    /// Get an iterator over the row slices of the array
    pub fn row_slices(&self) -> impl ExactSizeIterator<Item = &[T]> + DoubleEndedIterator + Clone {
        (0..self.row_count()).map(move |row| self.row_slice(row))
    }
    /// Get a slice of a row
    #[track_caller]
    pub fn row_slice(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
    /// Combine the metadata of two arrays
    pub fn combine_meta(&mut self, other: &ArrayMeta) {
        if let Some(meta) = self.get_meta_mut() {
            meta.flags &= other.flags;
            meta.map_keys = None;
            if meta.handle_kind != other.handle_kind {
                meta.handle_kind = None;
            }
        }
    }
}

impl<T: ArrayValue> Array<T> {
    /// Create a scalar array
    pub fn scalar(data: T) -> Self {
        Self::new(Shape::scalar(), cowslice![data])
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
    pub(crate) fn row_shaped_slice(&self, index: usize, row_shape: Shape) -> Self {
        let row_len: usize = row_shape.iter().product();
        let start = index * row_len;
        let end = start + row_len;
        Self::new(row_shape, self.data.slice(start..end))
    }
    /// Get an iterator over the row arrays of the array that have the given shape
    pub fn row_shaped_slices(
        &self,
        row_shape: Shape,
    ) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator + '_ {
        let row_len: usize = row_shape.iter().product();
        let row_count = self.element_count() / row_len;
        (0..row_count).map(move |i| {
            let start = i * row_len;
            let end = start + row_len;
            Self::new(row_shape.clone(), self.data.slice(start..end))
        })
    }
    /// Get an iterator over the row arrays of the array that have the given shape
    pub fn into_row_shaped_slices(self, row_shape: Shape) -> impl DoubleEndedIterator<Item = Self> {
        let row_len: usize = row_shape.iter().product();
        let zero_count = if row_len == 0 { self.row_count() } else { 0 };
        let row_sh = row_shape.clone();
        let nonzero = self
            .data
            .into_slices(row_len)
            .map(move |data| Self::new(row_sh.clone(), data));
        let zero = (0..zero_count).map(move |_| Self::new(row_shape.clone(), CowSlice::new()));
        nonzero.chain(zero)
    }
    /// Get a row array
    #[track_caller]
    pub fn row(&self, row: usize) -> Self {
        if self.rank() == 0 {
            let mut row = self.clone();
            row.take_map_keys();
            row.take_label();
            return row;
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
    #[track_caller]
    pub(crate) fn depth_row(&self, depth: usize, row: usize) -> Self {
        if self.rank() <= depth {
            let mut row = self.clone();
            row.take_map_keys();
            row.take_label();
            return row;
        }
        let row_count: usize = self.shape[..depth + 1].iter().product();
        if row >= row_count {
            panic!("row index out of bounds: {} >= {}", row, row_count);
        }
        let row_len: usize = self.shape[depth + 1..].iter().product();
        let start = row * row_len;
        let end = start + row_len;
        Self::new(&self.shape[depth + 1..], self.data.slice(start..end))
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
        self.grid_string(true)
    }
    pub(crate) fn pop_row(&mut self) -> Option<Self> {
        if self.row_count() == 0 {
            return None;
        }
        let data = self.data.split_off(self.data.len() - self.row_len());
        self.shape[0] -= 1;
        let shape: Shape = self.shape[1..].into();
        self.validate_shape();
        Some(Self::new(shape, data))
    }
    /// Get a mutable slice of a row
    #[track_caller]
    pub fn row_slice_mut(&mut self, row: usize) -> &mut [T] {
        let row_len = self.row_len();
        &mut self.data.as_mut_slice()[row * row_len..(row + 1) * row_len]
    }
    /// Add a 1-length dimension to the front of the array's shape
    pub fn fix(&mut self) {
        self.fix_depth(0);
    }
    pub(crate) fn fix_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        self.shape.insert(depth, 1);
        if depth == 0 {
            if let Some(keys) = self.map_keys_mut() {
                keys.fix();
            }
        }
    }
    /// Remove a 1-length dimension from the front of the array's shape
    pub fn unfix(&mut self) -> bool {
        if let Some(keys) = self.map_keys_mut() {
            keys.unfix();
        }
        self.shape.unfix()
    }
}

impl<T: Clone> Array<T> {
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
            meta: self.meta,
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
            meta: self.meta,
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
            meta: self.meta.clone(),
        }
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
    /// Attempt to unbox a scalar box array
    pub fn as_unboxed(&self) -> Option<&Value> {
        self.as_scalar().map(|v| &v.0)
    }
    /// Attempt to unbox a scalar box array
    pub fn as_unboxed_mut(&mut self) -> Option<&mut Value> {
        self.as_scalar_mut().map(|v| &mut v.0)
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
            .unwrap_or_else(|| self.shape.cmp(&other.shape));
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
        if let Some(scalar) = self.as_scalar() {
            if let Some(value) = scalar.nested_value() {
                value.hash(hasher);
                return;
            }
        }
        T::TYPE_ID.hash(hasher);
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
        Self::new(data.len(), data)
    }
}

impl<T: ArrayValue> From<CowSlice<T>> for Array<T> {
    fn from(data: CowSlice<T>) -> Self {
        Self::new(data.len(), data)
    }
}

impl<'a, T: ArrayValue> From<&'a [T]> for Array<T> {
    fn from(data: &'a [T]) -> Self {
        Self::new(data.len(), data)
    }
}

impl<T: ArrayValue> FromIterator<T> for Array<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<CowSlice<T>>())
    }
}

impl From<String> for Array<char> {
    fn from(s: String) -> Self {
        Self::new(s.len(), s.chars().collect::<CowSlice<_>>())
    }
}

impl From<Vec<bool>> for Array<u8> {
    fn from(data: Vec<bool>) -> Self {
        Self::new(
            data.len(),
            data.into_iter().map(u8::from).collect::<CowSlice<_>>(),
        )
    }
}

impl From<bool> for Array<u8> {
    fn from(data: bool) -> Self {
        Self::new(Shape::scalar(), cowslice![u8::from(data)])
    }
}

impl From<Vec<usize>> for Array<f64> {
    fn from(data: Vec<usize>) -> Self {
        Self::new(
            data.len(),
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
pub trait ArrayValue:
    Clone + fmt::Debug + fmt::Display + GridFmt + ArrayCmp + Send + Sync + 'static
{
    /// The type name
    const NAME: &'static str;
    /// A glyph indicating the type
    const SYMBOL: char;
    /// An ID for the type
    const TYPE_ID: u8;
    /// Get the fill value from the environment
    fn get_fill(env: &Uiua) -> Result<Self, &'static str>;
    /// Hash the value
    fn array_hash<H: Hasher>(&self, hasher: &mut H);
    /// Get the proxy value
    fn proxy() -> Self;
    /// Delimiters for formatting
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    /// Marker for empty lists in grid formatting
    fn empty_list_inner() -> &'static str {
        ""
    }
    /// Separator for formatting
    fn format_sep() -> &'static str {
        " "
    }
    /// Delimiters for grid formatting
    fn grid_fmt_delims(boxed: bool) -> (char, char) {
        if boxed {
            ('⟦', '⟧')
        } else {
            ('[', ']')
        }
    }
    /// Whether to compress all items of a list when grid formatting
    fn compress_list_grid() -> bool {
        false
    }
    /// Get a nested value
    fn nested_value(&self) -> Option<&Value> {
        None
    }
}

impl ArrayValue for f64 {
    const NAME: &'static str = "number";
    const SYMBOL: char = 'ℝ';
    const TYPE_ID: u8 = 0;
    fn get_fill(env: &Uiua) -> Result<Self, &'static str> {
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
    fn proxy() -> Self {
        0.0
    }
}

impl ArrayValue for u8 {
    const NAME: &'static str = "number";
    const SYMBOL: char = 'ℝ';
    const TYPE_ID: u8 = 0;
    fn get_fill(env: &Uiua) -> Result<Self, &'static str> {
        env.byte_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        (*self as f64).to_bits().hash(hasher)
    }
    fn proxy() -> Self {
        0
    }
}

impl ArrayValue for char {
    const NAME: &'static str = "character";
    const SYMBOL: char = '@';
    const TYPE_ID: u8 = 2;
    fn get_fill(env: &Uiua) -> Result<Self, &'static str> {
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
    fn proxy() -> Self {
        ' '
    }
    fn grid_fmt_delims(boxed: bool) -> (char, char) {
        if boxed {
            ('⌜', '⌟')
        } else {
            ('"', '"')
        }
    }
    fn compress_list_grid() -> bool {
        true
    }
}

impl ArrayValue for Boxed {
    const NAME: &'static str = "box";
    const SYMBOL: char = '□';
    const TYPE_ID: u8 = 3;
    fn get_fill(env: &Uiua) -> Result<Self, &'static str> {
        env.box_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher);
    }
    fn proxy() -> Self {
        Boxed(Array::<f64>::new(0, []).into())
    }
    fn empty_list_inner() -> &'static str {
        "□"
    }
    fn nested_value(&self) -> Option<&Value> {
        Some(&self.0)
    }
}

impl ArrayValue for Complex {
    const NAME: &'static str = "complex";
    const SYMBOL: char = 'ℂ';
    const TYPE_ID: u8 = 1;
    fn get_fill(env: &Uiua) -> Result<Self, &'static str> {
        env.complex_fill()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        for n in [self.re, self.im] {
            n.array_hash(hasher);
        }
    }
    fn proxy() -> Self {
        Complex::new(0.0, 0.0)
    }
    fn empty_list_inner() -> &'static str {
        "ℂ"
    }
}

/// [`ArrayValue`]s that are real numbers
pub trait RealArrayValue: ArrayValue + Copy {
    /// Attempt to convert the value into a natural number
    fn as_nat(self) -> Result<u128, AsNatError>;
}

impl RealArrayValue for f64 {
    fn as_nat(self) -> Result<u128, AsNatError> {
        if self < 0.0 {
            return Err(AsNatError::Negative);
        }
        if self.fract().abs() > f64::EPSILON {
            return Err(AsNatError::NotInteger);
        }
        if self > u128::MAX as f64 {
            return Err(AsNatError::TooLarge);
        }
        Ok(self.round() as u128)
    }
}

impl RealArrayValue for u8 {
    fn as_nat(self) -> Result<u128, AsNatError> {
        Ok(self as u128)
    }
}

/// Errors for [`NumericArrayValue::as_nat`]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum AsNatError {
    Negative,
    NotInteger,
    TooLarge,
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
        self.partial_cmp(other).unwrap_or_else(|| {
            (self.re.is_nan(), self.im.is_nan()).cmp(&(other.re.is_nan(), other.im.is_nan()))
        })
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

/// Convert value into a string that can be understood by the interpreter
/// Prefer to use `Value::representation()`
pub(crate) fn dbg_value(value: &Value, depth: usize, prefix: bool) -> String {
    value.generic_ref(
        |a| dbg_array::<f64>(a, depth, prefix),
        |a| dbg_array::<u8>(a, depth, prefix),
        |a| dbg_array::<Complex>(a, depth, prefix),
        |a| dbg_array::<char>(a, depth, prefix),
        |a: &Array<Boxed>| {
            if a.meta().map_keys.is_none() {
                dbg_array(a, depth, prefix)
            } else {
                use crate::algorithm::map::remove_empty_rows;
                let data = a.data.as_slice();
                let keys = remove_empty_rows(data[0].as_value().rows());
                let values = remove_empty_rows(data[1].as_value().rows());
                let depth = depth + 1;
                format!(
                    "{{{keys}\n{padding}{values}}}",
                    keys = dbg_value(&keys, depth, true),
                    values = dbg_value(&values, depth, true),
                    padding = " ".repeat(depth)
                )
            }
        },
    )
}

/// Convert array into a string that can be understood by the interpreter
/// * `depth` - recursion/indentation depth. 0 by default
/// * `prefix` - whether singular chars and boxes need a prefix. true by default
pub(crate) fn dbg_array<T>(array: &Array<T>, depth: usize, prefix: bool) -> String
where
    T: DebugArrayValue,
{
    let mut buffer = String::with_capacity(array.element_count());
    dbg_array_inner(
        &mut buffer,
        array.data.as_slice(),
        array.shape(),
        depth,
        prefix,
    );
    buffer
}

/// Recursive inner function for representation printing
fn dbg_array_inner<T: DebugArrayValue>(
    buffer: &mut String,
    array: &[T],
    shape: &[usize],
    depth: usize,
    prefix: bool,
) {
    let (delims, separator) = match shape.len() {
        0 => {
            if !array.is_empty() {
                buffer.push_str(&array[0].debug_string(depth, prefix))
            };
            return;
        }
        1 => (T::debug_delims(), T::debug_separator()),
        _ => (("[", "]"), "\n"),
    };
    let padding = if separator.contains('\n') {
        " ".repeat(depth + 1)
    } else {
        "".into()
    };
    let row_size = shape.iter().skip(1).product();
    let row_count = shape[0];
    buffer.push_str(delims.0);
    if row_size == 0 {
        for i in 0..row_count {
            if i != 0 {
                buffer.push_str(&padding);
            }
            dbg_array_inner(buffer, array, &shape[1..], depth + 1, false);
            if i != row_count - 1 {
                buffer.push_str(separator);
            }
        }
    } else {
        for (i, row) in array.chunks(row_size).enumerate() {
            if i != 0 {
                buffer.push_str(&padding);
            }
            dbg_array_inner(buffer, row, &shape[1..], depth + 1, false);
            if i != row_count - 1 {
                buffer.push_str(separator);
            }
        }
    }
    buffer.push_str(delims.1);
}

/// A trait for ArrayValue types that can be debug formatted
pub trait DebugArrayValue: ArrayValue {
    /// Row delimiters that can be read by the interpreter
    fn debug_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    /// String representation that can be read by the interpreter
    fn debug_string(&self, _depth: usize, _prefix: bool) -> String {
        self.to_string()
    }
    /// Separator that can be read by the interpreter
    fn debug_separator() -> &'static str {
        Self::format_sep()
    }
}
impl DebugArrayValue for u8 {}
impl DebugArrayValue for f64 {
    fn debug_string(&self, _depth: usize, _prefix: bool) -> String {
        self.to_string().replace('-', "¯")
    }
}
impl DebugArrayValue for Complex {
    fn debug_string(&self, depth: usize, prefix: bool) -> String {
        format!(
            "ℂ{} {}",
            self.im.debug_string(depth, prefix),
            self.re.debug_string(depth, prefix),
        )
    }
}
impl DebugArrayValue for char {
    fn debug_delims() -> (&'static str, &'static str) {
        ("\"", "\"")
    }
    fn debug_string(&self, _depth: usize, prefix: bool) -> String {
        let mut repr = format!("{self:?}")[1..] // gives repr delimited by single quotes
            .replace(r"\'", "'") // don't escape single quote
            .replace('"', r#"\""#) // escape double quote
            .replace(r"\'", r"\\'"); // escape backslash if it's not escaping anything
        repr.pop();
        if prefix {
            format!("@{}", repr.replace(' ', "\\s"))
        } else {
            repr
        }
    }
}
impl DebugArrayValue for Boxed {
    fn debug_delims() -> (&'static str, &'static str) {
        ("{", "}")
    }
    fn debug_string(&self, depth: usize, prefix: bool) -> String {
        let mut buffer = String::from(if prefix { "□" } else { "" });
        buffer.push_str(&dbg_value(
            &self.0,
            depth + if prefix { 1 } else { 0 },
            true,
        ));
        buffer
    }
    fn debug_separator() -> &'static str {
        "\n"
    }
}
