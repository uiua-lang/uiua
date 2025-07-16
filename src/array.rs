use std::{
    any::TypeId,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    sync::Arc,
};

use bitflags::bitflags;
use ecow::{EcoString, EcoVec};
use rayon::prelude::*;
use serde::{de::DeserializeOwned, *};

use crate::{
    algorithm::{
        map::{MapKeys, EMPTY_NAN, TOMBSTONE_NAN},
        ArrayCmpSlice,
    },
    cowslice::{cowslice, CowSlice},
    fill::{Fill, FillValue},
    grid_fmt::GridFmt,
    Boxed, Complex, ExactDoubleIterator, FfiType, HandleKind, Shape, Value, WILDCARD_CHAR,
    WILDCARD_NAN,
};

/// Uiua's array type
#[expect(
    clippy::unsafe_derive_deserialize,
    reason = "done through ArrayRep which is safe"
)]
#[derive(Clone, Serialize, Deserialize)]
#[serde(
    from = "ArrayRep<T>",
    into = "ArrayRep<T>",
    bound(
        serialize = "T: ArrayValueSer + Serialize",
        deserialize = "T: ArrayValueSer + Deserialize<'de>"
    )
)]
#[repr(C)]
pub struct Array<T> {
    /// The array's shape
    pub shape: Shape,
    pub(crate) data: CowSlice<T>,
    /// The array's metadata
    pub meta: ArrayMeta,
}

/// Non-shape metadata for an array
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[repr(C)]
#[non_exhaustive]
pub struct ArrayMetaInner {
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
    pub pointer: Option<MetaPtr>,
    /// The kind of system handle
    #[serde(skip)]
    pub handle_kind: Option<HandleKind>,
}

/// Default metadata for an array
static DEFAULT_META_INNER: ArrayMetaInner = ArrayMetaInner {
    label: None,
    flags: ArrayFlags::NONE,
    map_keys: None,
    pointer: None,
    handle_kind: None,
};

/// Non-shape metadata for an array
///
/// This wraps an optional pointer to a [`ArrayMetaInner`], whose fields can be accessed via `Deref` and `DerefMut`.
///
/// Mutably accessing the fields via the `DerefMut` implementation will populate the metadata.
/// To avoid this, use [`ArrayMeta::get_mut`].
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
#[repr(C)]
pub struct ArrayMeta(Option<Arc<ArrayMetaInner>>);

impl Deref for ArrayMeta {
    type Target = ArrayMetaInner;
    fn deref(&self) -> &Self::Target {
        self.0.as_deref().unwrap_or(&DEFAULT_META_INNER)
    }
}

impl DerefMut for ArrayMeta {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Arc::make_mut(self.0.get_or_insert_with(Default::default))
    }
}

impl ArrayMeta {
    fn get_inner_mut(&mut self) -> Option<&mut ArrayMetaInner> {
        self.0.as_mut().map(Arc::make_mut)
    }
    /// Get a mutable reference to the metadata, but only if any exists
    pub fn get_mut(&mut self) -> Option<&mut Self> {
        self.0.is_some().then_some(self)
    }
    /// Get a mutable reference to the map keys, if any exist
    pub fn map_keys_mut(&mut self) -> Option<&mut MapKeys> {
        self.get_inner_mut()?.map_keys.as_mut()
    }
    /// Check if the metadata is the default
    pub fn is_default(&self) -> bool {
        self.label.is_none()
            && self.map_keys.is_none()
            && self.handle_kind.is_none()
            && self.pointer.is_none()
            && self.flags.is_empty()
    }
    /// Check if the array is sorted ascending
    pub fn is_sorted_up(&self) -> bool {
        self.flags.contains(ArrayFlags::SORTED_UP)
    }
    /// Check if the array is sorted descending
    pub fn is_sorted_down(&self) -> bool {
        self.flags.contains(ArrayFlags::SORTED_DOWN)
    }
    /// Take the persistent metadata
    pub fn take_per_meta(&mut self) -> PersistentMeta {
        self.get_inner_mut()
            .map(|inner| {
                let label = inner.label.take();
                let map_keys = inner.map_keys.take();
                PersistentMeta { label, map_keys }
            })
            .unwrap_or_default()
    }
    /// Take the label from the metadata
    pub fn take_label(&mut self) -> Option<EcoString> {
        let inner = self.get_inner_mut()?;
        if inner.label.is_some() {
            inner.label.take()
        } else {
            None
        }
    }
    /// Take the map keys from the metadata
    pub fn take_map_keys(&mut self) -> Option<MapKeys> {
        self.get_inner_mut()?.map_keys.take()
    }
    /// Take the sorted flags
    pub fn take_sorted_flags(&mut self) -> ArrayFlags {
        let flags = self.flags & ArrayFlags::SORTEDNESS;
        self.flags &= !flags;
        flags
    }
    /// Take the value flags
    pub fn take_value_flags(&mut self) -> ArrayFlags {
        let flags = self.flags & ArrayFlags::VALUE;
        self.flags &= !flags;
        flags
    }
    /// Set the label for the value
    pub fn set_label(&mut self, label: Option<EcoString>) {
        if label.is_none() && self.label.is_none() {
            return;
        }
        self.label = label;
    }
    /// Set the persistent metadata
    pub fn set_per_meta(&mut self, per_meta: PersistentMeta) {
        if self.map_keys.is_some() != per_meta.map_keys.is_some() {
            self.map_keys = per_meta.map_keys;
        }
        if self.label.is_some() != per_meta.label.is_some() {
            self.label = per_meta.label;
        }
    }
    /// Or the sorted flags
    pub fn or_sorted_flags(&mut self, mut flags: ArrayFlags) {
        flags &= ArrayFlags::SORTEDNESS;
        if flags == ArrayFlags::NONE {
            return;
        }
        self.flags |= flags;
    }
    /// Or with reversed sorted flags
    pub fn or_sorted_flags_rev(&mut self, mut flags: ArrayFlags) {
        flags &= ArrayFlags::SORTEDNESS;
        if flags == ArrayFlags::NONE {
            return;
        }
        flags.reverse_sorted();
        self.flags |= flags;
    }
    /// Mark the array as sorted ascending
    ///
    /// It is a logic error to set this to `true` when it is not the case
    pub(crate) fn mark_sorted_up(&mut self, sorted: bool) {
        if sorted {
            self.flags.insert(ArrayFlags::SORTED_UP);
        } else if let Some(inner) = self.get_inner_mut() {
            inner.flags.remove(ArrayFlags::SORTED_UP);
        }
    }
    /// Mark the array as sorted descending
    ///
    /// It is a logic error to set this to `true` when it is not the case
    pub(crate) fn mark_sorted_down(&mut self, sorted: bool) {
        if sorted {
            self.flags.insert(ArrayFlags::SORTED_DOWN);
        } else if let Some(inner) = self.get_inner_mut() {
            inner.flags.remove(ArrayFlags::SORTED_DOWN);
        }
    }
    /// Combine the metadata with another
    pub fn combine(&mut self, other: &Self) {
        if let Some(other) = other.0.as_deref() {
            self.flags &= other.flags;
            self.map_keys = None;
            if self.handle_kind != other.handle_kind {
                self.handle_kind = None;
            }
        }
    }
    /// Reset the flags
    pub fn reset_flags(&mut self) {
        self.flags.reset();
    }
}

/// Array pointer metadata
#[derive(Debug, Clone)]
pub struct MetaPtr {
    /// The pointer value
    pub ptr: usize,
    /// The type of value stored at the pointer when read
    pub ty: FfiType,
}

impl MetaPtr {
    /// Get a null metadata pointer
    pub const fn null() -> Self {
        Self {
            ptr: 0,
            ty: FfiType::Void,
        }
    }
    /// Create a new metadata pointer
    pub fn new(ptr: usize, ffi_type: FfiType) -> Self {
        Self { ptr, ty: ffi_type }
    }
    /// Get the pointer as a raw pointer
    pub fn get(&self) -> *const u8 {
        self.ptr as *const _
    }
    /// Get the pointer as a raw pointer
    pub fn get_mut(&self) -> *mut u8 {
        self.ptr as *mut _
    }
}

impl PartialEq for MetaPtr {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Eq for MetaPtr {}

bitflags! {
    /// Flags for an array
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
    pub struct ArrayFlags: u8 {
        /// No flags
        const NONE = 0;
        /// The array is boolean
        const BOOLEAN = 1;
        /// The array was *created from* a boolean
        const BOOLEAN_LITERAL = 2;
        /// The array is sorted ascending
        const SORTED_UP = 4;
        /// The array is sorted descending
        const SORTED_DOWN = 8;
        /// Value-related flags that are true about the rows as well
        const VALUE = Self::BOOLEAN.bits() | Self::BOOLEAN_LITERAL.bits();
        /// Sortedness-related flags
        const SORTEDNESS = Self::SORTED_UP.bits() | Self::SORTED_DOWN.bits();
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
    /// Reverse the sorted flags
    pub fn reverse_sorted(&mut self) {
        let sorted_up = self.contains(Self::SORTED_UP);
        let sorted_down = self.contains(Self::SORTED_DOWN);
        self.set(Self::SORTED_UP, sorted_down);
        self.set(Self::SORTED_DOWN, sorted_up);
    }
}

/// Array metadata that can be persisted across operations
#[derive(Clone, Default)]
pub struct PersistentMeta {
    pub(crate) label: Option<EcoString>,
    pub(crate) map_keys: Option<MapKeys>,
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
            meta: ArrayMeta::default(),
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
                write!(f, "{start}")?;
                for (i, x) in self.data.iter().enumerate() {
                    if i > 0 {
                        write!(f, "{}", T::format_sep())?;
                    }
                    write!(f, "{x}")?;
                }
                write!(f, "{end}")
            }
            _ => {
                write!(f, "\n{}", self.grid_string(false))
            }
        }
    }
}

#[track_caller]
#[inline(always)]
pub(crate) fn validate_shape(_shape: &[usize], _len: usize) {
    #[cfg(debug_assertions)]
    {
        let elems = if _shape.contains(&0) {
            0
        } else {
            _shape.iter().product()
        };
        assert_eq!(
            elems, _len,
            "shape {_shape:?} does not match data length {_len}"
        )
    }
}

impl<T> Array<T> {
    /// An empty list
    pub const EMPTY_LIST: Self = Array {
        shape: Shape::EMPTY_LIST,
        data: CowSlice::new(),
        meta: ArrayMeta(None),
    };
    #[track_caller]
    /// Create an array from a shape and data
    ///
    /// # Panics
    /// Panics in debug mode if the shape does not match the data length
    pub fn new(shape: impl Into<Shape>, data: impl Into<CowSlice<T>>) -> Self {
        let shape = shape.into();
        let data = data.into();
        validate_shape(&shape, data.len());
        Self {
            shape,
            data,
            meta: ArrayMeta::default(),
        }
    }
    /// Get the number of rows in the array
    #[inline(always)]
    pub fn row_count(&self) -> usize {
        self.shape.row_count()
    }
    /// Get the number of elements in the array
    pub fn element_count(&self) -> usize {
        self.data.len()
    }
    /// Get the number of elements in a row
    #[inline(always)]
    pub fn row_len(&self) -> usize {
        self.shape.row_len()
    }
    /// Get the rank of the array
    #[inline(always)]
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    /// Iterate over the elements of the array
    pub fn elements(&self) -> impl ExactDoubleIterator<Item = &T> {
        self.data.iter()
    }
    /// Get an iterator over the row slices of the array
    pub fn row_slices(
        &self,
    ) -> impl ExactSizeIterator<Item = &[T]> + DoubleEndedIterator + Clone + Send + Sync
    where
        T: Send + Sync,
    {
        let row_len = self.row_len();
        self.data.chunks_exact(row_len.max(1))
    }
    /// Get a slice of a row
    #[track_caller]
    pub fn row_slice(&self, row: usize) -> &[T] {
        let row_len = self.row_len();
        &self.data[row * row_len..(row + 1) * row_len]
    }
}

impl<T: Clone> Array<T> {
    /// Get an iterator over the mutable row slices of the array
    pub fn row_slices_mut(
        &mut self,
    ) -> impl ExactSizeIterator<Item = &mut [T]> + DoubleEndedIterator + Send + Sync
    where
        T: Send + Sync,
    {
        let row_len = self.row_len();
        self.data.as_mut_slice().chunks_exact_mut(row_len.max(1))
    }
    /// Get a row array
    #[track_caller]
    pub fn row(&self, row: usize) -> Self {
        if self.rank() == 0 {
            let mut row = self.clone();
            row.meta.take_map_keys();
            row.meta.take_label();
            return row;
        }
        let row_count = self.row_count();
        if row >= row_count {
            panic!("row index out of bounds: {row} >= {row_count}");
        }
        let row_len = self.row_len();
        let start = row * row_len;
        let end = start + row_len;
        let mut row = Self::new(&self.shape[1..], self.data.slice(start..end));
        let value_flags = self.meta.flags & ArrayFlags::VALUE;
        if value_flags != ArrayFlags::NONE {
            row.meta.flags = value_flags;
        }
        row
    }
}

impl<T: ArrayValue> Array<T> {
    /// Create a scalar array
    pub fn scalar(data: T) -> Self {
        Self::new(Shape::SCALAR, cowslice![data])
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
    pub fn rows(&self) -> impl ExactDoubleIterator<Item = Self> + '_ {
        let value_flags = self.meta.flags & ArrayFlags::VALUE;
        let set_value_flags = value_flags != ArrayFlags::NONE;
        let row_len = self.row_len();
        (0..self.row_count()).map(move |row| {
            if self.rank() == 0 {
                let mut row = self.clone();
                row.meta.take_map_keys();
                row.meta.take_label();
                return row;
            }
            let start = row * row_len;
            let end = start + row_len;
            let mut row = Array::<T>::new(&self.shape[1..], self.data.slice(start..end));
            if set_value_flags {
                row.meta.flags = value_flags;
            }
            row
        })
    }
    pub(crate) fn row_shaped_slice(&self, index: usize, row_shape: Shape) -> Self {
        let row_len = row_shape.elements();
        let start = index * row_len;
        let end = start + row_len;
        Self::new(row_shape, self.data.slice(start..end))
    }
    /// Get an iterator over the row arrays of the array that have the given shape
    pub fn row_shaped_slices(
        &self,
        row_shape: Shape,
    ) -> impl ExactDoubleIterator<Item = Self> + '_ {
        let row_len = row_shape.elements();
        let row_count = self.element_count() / row_len;
        (0..row_count).map(move |i| {
            let start = i * row_len;
            let end = start + row_len;
            Self::new(row_shape.clone(), self.data.slice(start..end))
        })
    }
    /// Get an iterator over the row arrays of the array that have the given shape
    pub fn into_row_shaped_slices(self, row_shape: Shape) -> impl DoubleEndedIterator<Item = Self> {
        let row_len = row_shape.elements();
        let zero_count = if row_len == 0 { self.row_count() } else { 0 };
        let row_sh = row_shape.clone();
        let nonzero = self
            .data
            .into_slices(row_len)
            .map(move |data| Self::new(row_sh.clone(), data));
        let zero = (0..zero_count).map(move |_| Self::new(row_shape.clone(), CowSlice::new()));
        nonzero.chain(zero)
    }
    #[track_caller]
    pub(crate) fn depth_row(&self, depth: usize, row: usize) -> Self {
        if self.rank() <= depth {
            let mut row = self.clone();
            row.meta.take_map_keys();
            row.meta.take_label();
            return row;
        }
        let row_count: usize = self.shape[..=depth].iter().product();
        if row >= row_count {
            panic!("row index out of bounds: {row} >= {row_count}");
        }
        let row_len: usize = self.shape[depth + 1..].iter().product();
        let start = row * row_len;
        let end = start + row_len;
        Self::new(&self.shape[depth + 1..], self.data.slice(start..end))
    }
    #[track_caller]
    pub(crate) fn depth_rows(&self, depth: usize) -> impl ExactDoubleIterator<Item = Self> + '_ {
        let row_count: usize = self.shape[..depth].iter().product();
        let row_len: usize = self.shape[depth..].iter().product();
        (0..row_count).map(move |row| {
            let start = row * row_len;
            let end = start + row_len;
            Self::new(&self.shape[depth..], self.data.slice(start..end))
        })
    }
    #[track_caller]
    /// Create an array that is a slice of this array's rows
    ///
    /// Generally doesn't allocate
    ///
    /// - `start` must be <= `end`
    /// - `start` must be < `self.row_count()`
    /// - `end` must be <= `self.row_count()`
    pub fn slice_rows(&self, start: usize, end: usize) -> Self {
        assert!(start <= end);
        assert!(start < self.row_count());
        assert!(end <= self.row_count());
        let row_len = self.row_len();
        let mut shape = self.shape.clone();
        shape[0] = end - start;
        let start = start * row_len;
        let end = end * row_len;
        Self::new(shape, self.data.slice(start..end))
    }
    /// Consume the array and get an iterator over its rows
    pub fn into_rows(self) -> impl ExactDoubleIterator<Item = Self> {
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
        self.validate();
        Some(Self::new(shape, data))
    }
    /// Get a mutable slice of a row
    #[track_caller]
    pub fn row_slice_mut(&mut self, row: usize) -> &mut [T] {
        let row_len = self.row_len();
        &mut self.data.as_mut_slice()[row * row_len..(row + 1) * row_len]
    }
    /// Set the sortedness flags according to the array's data
    pub fn derive_sortedness(&mut self) {
        if !self.data.iter().all(|x| x.is_sortable()) {
            return;
        }
        let mut sorted_up = true;
        let mut sorted_down = true;
        let mut rows = self.row_slices().map(ArrayCmpSlice);
        if let Some(mut prev) = rows.next() {
            for row in rows {
                match prev.cmp(&row) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        sorted_down = false;
                        if !sorted_up {
                            break;
                        }
                    }
                    Ordering::Greater => {
                        sorted_up = false;
                        if !sorted_down {
                            break;
                        }
                    }
                }
                prev = row;
            }
        } else {
            drop(rows);
        }
        self.meta.mark_sorted_up(sorted_up);
        self.meta.mark_sorted_down(sorted_down);
    }
    #[track_caller]
    #[inline(always)]
    /// Debug-only to ensure the array's invariants are upheld
    pub(crate) fn validate(&self) {
        #[cfg(debug_assertions)]
        {
            validate_shape(&self.shape, self.data.len());
            let is_sorted_up = self.meta.is_sorted_up();
            let is_sorted_down = self.meta.is_sorted_down();
            if is_sorted_up || is_sorted_down {
                let mut rows = (0..self.row_count()).map(|i| self.row_slice(i));
                if let Some(prev) = rows.next() {
                    let mut prev = ArrayCmpSlice(prev);
                    for row in rows {
                        let row = ArrayCmpSlice(row);
                        assert!(
                            !is_sorted_up || prev <= row,
                            "{} array marked as sorted up is not.",
                            T::NAME
                        );
                        assert!(
                            !is_sorted_down || prev >= row,
                            "{} array marked as sorted down is not.",
                            T::NAME
                        );
                        prev = row;
                    }
                }
            }
        }
    }
}

impl<T: Clone> Array<T> {
    /// Convert the elements of the array
    #[inline(always)]
    pub fn convert<U>(self) -> Array<U>
    where
        T: Into<U> + 'static,
        U: Clone + 'static,
    {
        if TypeId::of::<T>() == TypeId::of::<U>() {
            unsafe { std::mem::transmute::<Array<T>, Array<U>>(self) }
        } else {
            self.convert_with(Into::into)
        }
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
    #[track_caller]
    #[allow(dead_code)]
    pub(crate) fn depth_slices(&self, depth: usize) -> impl ExactDoubleIterator<Item = &[T]> {
        let row_len: usize = self.shape[depth..].iter().product();
        self.data.chunks_exact(row_len)
    }
    #[track_caller]
    #[allow(dead_code)]
    pub(crate) fn depth_slices_mut(
        &mut self,
        depth: usize,
    ) -> impl ExactDoubleIterator<Item = &mut [T]> {
        let row_len: usize = self.shape[depth..].iter().product();
        self.data.as_mut_slice().chunks_exact_mut(row_len)
    }
}

impl Array<u8> {
    pub(crate) fn json_bool(b: bool) -> Self {
        let mut arr = Self::from(b);
        arr.meta.flags |= ArrayFlags::BOOLEAN_LITERAL;
        arr
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
        if self.shape != other.shape {
            return false;
        }
        if self.meta.map_keys != other.meta.map_keys {
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
        if let Some(keys) = &self.meta.map_keys {
            keys.hash(hasher);
        }
        T::TYPE_ID.hash(hasher);
        if let Some(scalar) = self.as_scalar() {
            if let Some(value) = scalar.nested_value() {
                value.hash(hasher);
                return;
            }
        }
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
        s.chars().collect()
    }
}

impl From<&str> for Array<char> {
    fn from(s: &str) -> Self {
        s.chars().collect()
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
        let mut arr = Self::new(Shape::SCALAR, cowslice![u8::from(data)]);
        arr.meta.flags |= ArrayFlags::BOOLEAN;
        arr
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

impl<'a> FromIterator<&'a str> for Array<Boxed> {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
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
    Default + Clone + fmt::Debug + fmt::Display + GridFmt + ArrayCmp + Send + Sync + 'static
{
    /// The type name
    const NAME: &'static str;
    /// A glyph indicating the type
    const SYMBOL: char;
    /// An ID for the type
    const TYPE_ID: u8;
    /// Get the scalar fill value from the environment
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str>;
    /// Get the array fill value from the environment
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str>;
    /// Hash the value
    fn array_hash<H: Hasher>(&self, hasher: &mut H);
    /// Get the proxy value
    fn proxy() -> Self;
    /// Get a nested value
    fn nested_value(&self) -> Option<&Value> {
        None
    }
    /// Check if this element has the wildcard value
    fn has_wildcard(&self) -> bool {
        false
    }
    /// Sort a list of this type
    fn sort_list(list: &mut [Self], up: bool) {
        default_sort_list(list, up)
    }
    /// Whether this value can be sorted
    fn is_sortable(&self) -> bool {
        true
    }
}

fn default_sort_list<T: ArrayCmp + Send>(list: &mut [T], up: bool) {
    if up {
        list.par_sort_unstable_by(T::array_cmp);
    } else {
        list.par_sort_unstable_by(|a, b| b.array_cmp(a));
    }
}

impl ArrayValue for f64 {
    const NAME: &'static str = "number";
    const SYMBOL: char = 'ℝ';
    const TYPE_ID: u8 = 0;
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str> {
        fill.num_scalar()
    }
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str> {
        fill.num_array()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        let v = if self.to_bits() == EMPTY_NAN.to_bits() {
            EMPTY_NAN
        } else if self.to_bits() == TOMBSTONE_NAN.to_bits() {
            TOMBSTONE_NAN
        } else if self.to_bits() == WILDCARD_NAN.to_bits() {
            WILDCARD_NAN
        } else if self.is_nan() {
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
    fn has_wildcard(&self) -> bool {
        self.to_bits() == WILDCARD_NAN.to_bits()
    }
    fn is_sortable(&self) -> bool {
        !self.is_nan()
    }
}

#[cfg(test)]
#[test]
fn f64_summarize() {
    assert_eq!(f64::summarize(&[2.0, 6.0, 1.0]), "1-6 μ3");
}

impl ArrayValue for u8 {
    const NAME: &'static str = "number";
    const SYMBOL: char = 'ℝ';
    const TYPE_ID: u8 = 0;
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str> {
        fill.byte_scalar()
    }
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str> {
        fill.byte_array()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        (*self as f64).to_bits().hash(hasher)
    }
    fn proxy() -> Self {
        0
    }
    fn sort_list(list: &mut [Self], up: bool) {
        let mut counts: [usize; 256] = [0; 256];
        for x in &mut *list {
            counts[*x as usize] += 1;
        }
        let mut i = 0;
        if up {
            for (j, &count) in counts.iter().enumerate() {
                let n = j as u8;
                for _ in 0..count {
                    list[i] = n;
                    i += 1;
                }
            }
        } else {
            let offset = list.len().saturating_sub(1);
            for (j, &count) in counts.iter().enumerate() {
                let n = j as u8;
                for _ in 0..count {
                    list[offset - i] = n;
                    i += 1;
                }
            }
        }
    }
}

impl ArrayValue for char {
    const NAME: &'static str = "character";
    const SYMBOL: char = '@';
    const TYPE_ID: u8 = 1;
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str> {
        fill.char_scalar()
    }
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str> {
        fill.char_array()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.hash(hasher)
    }
    fn proxy() -> Self {
        ' '
    }
    fn has_wildcard(&self) -> bool {
        *self == WILDCARD_CHAR
    }
}

impl ArrayValue for Boxed {
    const NAME: &'static str = "box";
    const SYMBOL: char = '□';
    const TYPE_ID: u8 = 2;
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str> {
        fill.box_scalar()
    }
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str> {
        fill.box_array()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher);
    }
    fn proxy() -> Self {
        Boxed(Array::<f64>::new(0, []).into())
    }
    fn nested_value(&self) -> Option<&Value> {
        Some(&self.0)
    }
    fn has_wildcard(&self) -> bool {
        self.0.has_wildcard()
    }
}

impl ArrayValue for Complex {
    const NAME: &'static str = "complex";
    const SYMBOL: char = 'ℂ';
    const TYPE_ID: u8 = 3;
    fn get_scalar_fill(fill: &Fill) -> Result<FillValue<Self>, &'static str> {
        fill.complex_scalar()
    }
    fn get_array_fill(fill: &Fill) -> Result<FillValue<Array<Self>>, &'static str> {
        fill.complex_array()
    }
    fn array_hash<H: Hasher>(&self, hasher: &mut H) {
        for n in [self.re, self.im] {
            n.array_hash(hasher);
        }
    }
    fn proxy() -> Self {
        Complex::new(0.0, 0.0)
    }
}

/// Trait for [`ArrayValue`]s that are real numbers
pub trait RealArrayValue: ArrayValue + Copy {
    /// Whether the value is an integer
    fn is_int(&self) -> bool;
    /// Convert the value to an `f64`
    fn to_f64(&self) -> f64;
}

impl RealArrayValue for f64 {
    fn is_int(&self) -> bool {
        self.fract().abs() < f64::EPSILON
    }
    fn to_f64(&self) -> f64 {
        *self
    }
}

impl RealArrayValue for u8 {
    fn is_int(&self) -> bool {
        true
    }
    fn to_f64(&self) -> f64 {
        *self as f64
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
        self.partial_cmp(other).unwrap_or_else(|| {
            if self.to_bits() == WILDCARD_NAN.to_bits() || other.to_bits() == WILDCARD_NAN.to_bits()
            {
                Ordering::Equal
            } else {
                self.is_nan().cmp(&other.is_nan())
            }
        })
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
    fn array_eq(&self, other: &Self) -> bool {
        *self == *other || *self == WILDCARD_CHAR || *other == WILDCARD_CHAR
    }
    fn array_cmp(&self, other: &Self) -> Ordering {
        if *self == WILDCARD_CHAR || *other == WILDCARD_CHAR {
            Ordering::Equal
        } else {
            self.cmp(other)
        }
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
pub struct FormatShape<'a, T = usize>(pub &'a [T]);

impl<T: fmt::Display> fmt::Debug for FormatShape<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<T: fmt::Display> fmt::Display for FormatShape<'_, T> {
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
#[serde(bound(
    serialize = "T: ArrayValueSer + Serialize",
    deserialize = "T: ArrayValueSer + Deserialize<'de>"
))]
enum ArrayRep<T: ArrayValueSer> {
    List(T::Collection),
    Scalar(T::Scalar),
    Map(Shape, Value, T::Collection),
    Metaless(Shape, T::Collection),
    Full(Shape, T::Collection, ArrayMeta),
}

impl<T: ArrayValueSer> From<ArrayRep<T>> for Array<T> {
    fn from(rep: ArrayRep<T>) -> Self {
        let mut arr = match rep {
            ArrayRep::Scalar(data) => Self::new([], [data.into()]),
            ArrayRep::List(data) => {
                let data = T::make_data(data);
                Self::new(data.len(), data)
            }
            ArrayRep::Map(shape, keys, data) => {
                let data = T::make_data(data);
                let mut arr = Self::new(shape, data);
                _ = arr.map(keys, &());
                arr
            }
            ArrayRep::Metaless(shape, data) => {
                let data = T::make_data(data);
                Self::new(shape, data)
            }
            ArrayRep::Full(shape, data, meta) => {
                let data = T::make_data(data);
                Self { shape, data, meta }
            }
        };

        // Update sortedness flags
        let mut is_sorted_up = true;
        let mut is_sorted_down = true;
        {
            let mut rows = arr.row_slices();
            if let Some(mut curr) = rows.next() {
                for row in rows {
                    if !is_sorted_up && !is_sorted_down {
                        break;
                    }
                    match ArrayCmpSlice(curr).cmp(&ArrayCmpSlice(row)) {
                        Ordering::Equal => {}
                        Ordering::Less => is_sorted_down = false,
                        Ordering::Greater => is_sorted_up = false,
                    }
                    curr = row;
                }
            }
        }
        arr.meta.mark_sorted_up(is_sorted_up);
        arr.meta.mark_sorted_down(is_sorted_down);
        arr
    }
}

impl<T: ArrayValueSer> From<Array<T>> for ArrayRep<T> {
    fn from(mut arr: Array<T>) -> Self {
        if let Some(inner) = (arr.meta.0.take()).filter(|meta| **meta != DEFAULT_META_INNER) {
            let mut inner = Arc::unwrap_or_clone(inner);
            let map_keys = inner.map_keys.take();
            if inner == DEFAULT_META_INNER {
                if let Some(map_keys) = map_keys {
                    let keys = map_keys.normalized();
                    return ArrayRep::Map(arr.shape, keys, T::make_collection(arr.data));
                }
            } else {
                inner.map_keys = map_keys;
            }
            inner.flags = ArrayFlags::empty();
            if inner != DEFAULT_META_INNER {
                return ArrayRep::Full(
                    arr.shape,
                    T::make_collection(arr.data),
                    ArrayMeta(Some(inner.into())),
                );
            }
        }
        match arr.rank() {
            0 if !T::no_scalar() => ArrayRep::Scalar(arr.data[0].clone().into()),
            1 => ArrayRep::List(T::make_collection(arr.data)),
            _ => ArrayRep::Metaless(arr.shape, T::make_collection(arr.data)),
        }
    }
}

trait ArrayValueSer: ArrayValue + fmt::Debug {
    type Scalar: Serialize + DeserializeOwned + fmt::Debug + From<Self> + Into<Self>;
    type Collection: Serialize + DeserializeOwned + fmt::Debug;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection;
    fn make_data(collection: Self::Collection) -> CowSlice<Self>;
    /// Do not use the [`ArrayRep::Scalar`] variant
    fn no_scalar() -> bool {
        false
    }
}

impl ArrayValueSer for u8 {
    type Scalar = u8;
    type Collection = CowSlice<u8>;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection {
        data
    }
    fn make_data(collection: Self::Collection) -> CowSlice<Self> {
        collection
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum BoxCollection {
    #[serde(rename = "empty_boxes")]
    Empty([Boxed; 0]),
    #[serde(untagged)]
    List(CowSlice<Boxed>),
}

impl ArrayValueSer for Boxed {
    type Scalar = Boxed;
    type Collection = BoxCollection;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection {
        if data.is_empty() {
            BoxCollection::Empty([])
        } else {
            BoxCollection::List(data)
        }
    }
    fn make_data(collection: Self::Collection) -> CowSlice<Self> {
        match collection {
            BoxCollection::Empty(_) => CowSlice::new(),
            BoxCollection::List(data) => data,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ComplexCollection {
    #[serde(rename = "empty_complex")]
    Empty([Complex; 0]),
    #[serde(untagged)]
    List(CowSlice<Complex>),
}

impl ArrayValueSer for Complex {
    type Scalar = Complex;
    type Collection = ComplexCollection;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection {
        if data.is_empty() {
            ComplexCollection::Empty([])
        } else {
            ComplexCollection::List(data)
        }
    }
    fn make_data(collection: Self::Collection) -> CowSlice<Self> {
        match collection {
            ComplexCollection::Empty(_) => CowSlice::new(),
            ComplexCollection::List(data) => data,
        }
    }
    fn no_scalar() -> bool {
        true
    }
}

impl ArrayValueSer for f64 {
    type Scalar = F64Rep;
    type Collection = Vec<F64Rep>;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection {
        data.iter().map(|&n| n.into()).collect()
    }
    fn make_data(collection: Self::Collection) -> CowSlice<Self> {
        collection.into_iter().map(f64::from).collect()
    }
}

impl ArrayValueSer for char {
    type Scalar = char;
    type Collection = String;
    fn make_collection(data: CowSlice<Self>) -> Self::Collection {
        data.iter().collect()
    }
    fn make_data(collection: Self::Collection) -> CowSlice<Self> {
        collection.chars().collect()
    }
    fn no_scalar() -> bool {
        true
    }
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
enum F64Rep {
    #[serde(rename = "NaN")]
    NaN,
    #[serde(rename = "empty")]
    MapEmpty,
    #[serde(rename = "tomb")]
    MapTombstone,
    #[serde(rename = "∞")]
    Infinity,
    #[serde(rename = "-∞")]
    NegInfinity,
    #[serde(untagged)]
    Num(f64),
}

impl From<f64> for F64Rep {
    fn from(n: f64) -> Self {
        if n.is_nan() {
            if n.to_bits() == EMPTY_NAN.to_bits() {
                Self::MapEmpty
            } else if n.to_bits() == TOMBSTONE_NAN.to_bits() {
                Self::MapTombstone
            } else {
                Self::NaN
            }
        } else if n.is_infinite() {
            if n.is_sign_positive() {
                Self::Infinity
            } else {
                Self::NegInfinity
            }
        } else {
            Self::Num(n)
        }
    }
}

impl From<F64Rep> for f64 {
    fn from(rep: F64Rep) -> Self {
        match rep {
            F64Rep::NaN => f64::NAN,
            F64Rep::MapEmpty => EMPTY_NAN,
            F64Rep::MapTombstone => TOMBSTONE_NAN,
            F64Rep::Infinity => f64::INFINITY,
            F64Rep::NegInfinity => f64::NEG_INFINITY,
            F64Rep::Num(n) => n,
        }
    }
}
