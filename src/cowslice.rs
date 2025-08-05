//! The backing buffer for Uiua's arrays' data

use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Bound, Deref, RangeBounds},
    ptr,
};

use serde::*;

macro_rules! cowslice {
    ($($tt:tt)*) => {
        $crate::cowslice::CowSlice::from(::ecow::eco_vec![$($tt)*])
    }
}

pub(crate) use cowslice;
use ecow::EcoVec;

use crate::fill::FillValue;

/// The backing buffer for Uiua's arrays' data
///
/// `CowSlice`s are reference-counted buffers that also have associated start and end indices.
/// This allows them to be split into chunks without copying the data.
pub struct CowSlice<T> {
    data: EcoVec<T>,
    start: u32,
    end: u32,
}

impl<T> CowSlice<T> {
    pub const fn new() -> Self {
        Self {
            data: EcoVec::new(),
            start: 0,
            end: 0,
        }
    }
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: EcoVec::with_capacity(capacity),
            start: 0,
            end: 0,
        }
    }
    #[inline]
    pub fn as_slice(&self) -> &[T] {
        &self.data[self.start as usize..self.end as usize]
    }
    #[inline]
    pub fn len(&self) -> usize {
        self.end as usize - self.start as usize
    }
    #[inline]
    pub fn is_unique(&mut self) -> bool {
        self.data.is_unique()
    }
    pub fn is_copy_of(&self, other: &Self) -> bool {
        ptr::eq(self.data.as_ptr(), other.data.as_ptr())
            && self.start == other.start
            && self.end == other.end
    }
}

impl<T: Clone> CowSlice<T> {
    pub fn from_elem(elem: T, len: usize) -> Self {
        Self {
            data: EcoVec::from_elem(elem, len),
            start: 0,
            end: len as u32,
        }
    }
    pub fn truncate(&mut self, len: usize) {
        if self.is_unique() {
            self.data.truncate(self.start as usize + len);
        }
        self.end = (self.start + len as u32).min(self.end);
    }
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        if !self.data.is_unique() {
            let mut new_data = EcoVec::with_capacity(self.len());
            new_data.extend_from_slice(&*self);
            self.data = new_data;
            self.start = 0;
            self.end = self.data.len() as u32;
        }
        &mut self.data.make_mut()[self.start as usize..self.end as usize]
    }
    pub fn extend_from_slice(&mut self, other: &[T]) {
        self.modify(|vec| vec.extend_from_slice(other))
    }
    #[track_caller]
    pub fn slice<R>(&self, range: R) -> Self
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            Bound::Included(&start) => self.start + start as u32,
            Bound::Excluded(&start) => self.start + start as u32 + 1,
            Bound::Unbounded => self.start,
        };
        let end = match range.end_bound() {
            Bound::Included(&end) => self.start + end as u32 + 1,
            Bound::Excluded(&end) => self.start + end as u32,
            Bound::Unbounded => self.end,
        };
        assert!(start <= end);
        assert!(end <= self.end);
        Self {
            data: self.data.clone(),
            start,
            end,
        }
    }
    /// Get an iterator over slices with the given size
    pub fn into_slices(
        self,
        size: usize,
    ) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator {
        let count = if size == 0 {
            0
        } else {
            assert!(self.len() % size == 0);
            self.len() / size
        };
        (0..count).map(move |i| {
            let start = self.start + (i * size) as u32;
            Self {
                data: self.data.clone(),
                start,
                end: start + size as u32,
            }
        })
    }
    #[track_caller]
    fn modify<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut EcoVec<T>) -> R,
    {
        if self.data.is_unique() && self.start == 0 && self.end == self.data.len() as u32 {
            let res = f(&mut self.data);
            self.end = self.data.len() as u32;
            res
        } else {
            let mut vec = EcoVec::from(&**self);
            let res = f(&mut vec);
            *self = vec.into();
            res
        }
    }
    #[track_caller]
    fn modify_end<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut EcoVec<T>) -> R,
    {
        if self.data.is_unique() && self.end == self.data.len() as u32 {
            let res = f(&mut self.data);
            self.end = self.data.len() as u32;
            res
        } else {
            let mut vec = EcoVec::from(&**self);
            let res = f(&mut vec);
            *self = vec.into();
            res
        }
    }
    /// Clear the buffer
    pub fn clear(&mut self) {
        if self.is_unique() {
            self.modify(|vec| vec.clear());
        } else {
            self.data = EcoVec::new();
        }
        self.start = 0;
        self.end = 0;
    }
    /// Reserve space for at least `additional` more elements
    pub fn reserve(&mut self, additional: usize) {
        self.modify_end(|vec| vec.reserve(additional))
    }
    /// Ensure that the capacity is at least `min`
    pub fn reserve_min(&mut self, min: usize) {
        if self.data.capacity() < min {
            self.modify_end(|vec| vec.reserve(min - vec.len()))
        }
    }
    pub fn split_off(&mut self, at: usize) -> Self {
        assert!(at <= self.len());
        let mut other = Self::with_capacity(self.len() - at);
        other.extend_from_slice(&self[at..]);
        self.truncate(at);
        other
    }
    pub fn remove<R>(&mut self, range: R)
    where
        R: RangeBounds<usize>,
    {
        self.modify(|data| {
            let start = match range.start_bound() {
                Bound::Included(&start) => start,
                Bound::Excluded(&start) => start + 1,
                Bound::Unbounded => 0,
            };
            let end = match range.end_bound() {
                Bound::Included(&end) => end + 1,
                Bound::Excluded(&end) => end,
                Bound::Unbounded => data.len(),
            };
            assert!(start <= end);
            data.make_mut().rotate_left(start);
            data.truncate(data.len() - (end - start));
        })
    }
    #[track_caller]
    pub fn extend_from_array<const N: usize>(&mut self, array: [T; N]) {
        self.modify_end(|data| unsafe { data.extend_from_trusted(array) })
    }
    #[track_caller]
    pub fn extend_from_vec(&mut self, vec: Vec<T>) {
        self.modify_end(|data| unsafe { data.extend_from_trusted(vec) })
    }
    #[track_caller]
    pub fn extend_from_ecovec(&mut self, vec: EcoVec<T>) {
        self.modify_end(|data| unsafe { data.extend_from_trusted(vec) })
    }
    #[track_caller]
    pub fn extend_from_cowslice(&mut self, slice: CowSlice<T>) {
        self.modify_end(|data| unsafe { data.extend_from_trusted(slice) })
    }
    #[track_caller]
    pub fn extend_repeat(&mut self, elem: &T, count: usize) {
        self.modify_end(|data| extend_repeat(data, elem, count))
    }
    #[track_caller]
    pub fn extend_repeat_fill(&mut self, fill: &FillValue<T>, count: usize) {
        self.modify_end(|data| {
            extend_repeat(data, &fill.value, count);
            if fill.is_left() {
                data.make_mut().rotate_right(count);
            }
        });
    }
    #[track_caller]
    pub fn extend_repeat_slice(&mut self, slice: &[T], count: usize) {
        self.modify_end(|data| extend_repeat_slice(data, slice, count))
    }
    #[track_caller]
    pub fn extend_repeat_slice_fill(&mut self, slice: FillValue<&[T]>, count: usize) {
        self.modify_end(|data| {
            extend_repeat_slice(data, slice.value, count);
            if slice.is_left() {
                data.make_mut().rotate_right(count * slice.value.len());
            }
        })
    }
    #[track_caller]
    pub unsafe fn extend_from_trusted<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.modify_end(|data| data.extend_from_trusted(iter))
    }
}

pub(crate) fn extend_repeat<T: Clone>(vec: &mut EcoVec<T>, elem: &T, count: usize) {
    unsafe { vec.extend_from_trusted(Repeat { elem, count }) }
}

pub(crate) fn extend_repeat_slice<T: Clone>(vec: &mut EcoVec<T>, slice: &[T], count: usize) {
    match slice {
        [] => {}
        [elem] => extend_repeat(vec, elem, count),
        _ => {
            for _ in 0..count {
                vec.extend_from_slice(slice);
            }
        }
    }
}

pub(crate) fn ecovec_extend_cowslice<T: Clone>(vec: &mut EcoVec<T>, cowslice: CowSlice<T>) {
    unsafe { vec.extend_from_trusted(cowslice) }
}

/// Exact sized repeating iterator
pub(crate) struct Repeat<'a, T> {
    elem: &'a T,
    count: usize,
}
impl<T: Clone> Iterator for Repeat<'_, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.count == 0 {
            return None;
        }
        self.count -= 1;
        Some(self.elem.clone())
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.count, Some(self.count))
    }
}
impl<T: Clone> ExactSizeIterator for Repeat<'_, T> {}

#[test]
fn cow_slice_modify() {
    let mut slice = CowSlice::from([1, 2, 3]);
    slice.modify(|vec| vec.push(4));
    assert_eq!(slice, [1, 2, 3, 4]);

    let mut sub = slice.slice(1..=2);
    sub.modify(|vec| vec.push(5));
    assert_eq!(slice, [1, 2, 3, 4]);
    assert_eq!(sub, [2, 3, 5]);
}

impl<T> Default for CowSlice<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Clone for CowSlice<T> {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            start: self.start,
            end: self.end,
        }
    }
}

impl<T> Deref for CowSlice<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

#[test]
fn cow_slice_deref_mut() {
    let mut slice = CowSlice::from([1, 2, 3, 4]);
    slice.as_mut_slice()[1] = 7;
    assert_eq!(slice, [1, 7, 3, 4]);

    let mut sub = slice.slice(1..=2);
    sub.as_mut_slice()[1] = 5;
    assert_eq!(slice, [1, 7, 3, 4]);
    assert_eq!(sub, [7, 5]);
}

impl<T: Clone> From<CowSlice<T>> for Vec<T> {
    fn from(mut slice: CowSlice<T>) -> Self {
        if slice.data.is_unique() && slice.start == 0 && slice.end == slice.data.len() as u32 {
            slice.data.into_iter().collect()
        } else {
            slice.to_vec()
        }
    }
}

impl<T: Clone> From<EcoVec<T>> for CowSlice<T> {
    fn from(data: EcoVec<T>) -> Self {
        Self {
            start: 0,
            end: data.len() as u32,
            data,
        }
    }
}

impl<T: Clone> From<CowSlice<T>> for EcoVec<T> {
    fn from(mut slice: CowSlice<T>) -> Self {
        if slice.data.is_unique() && slice.start == 0 && slice.end == slice.data.len() as u32 {
            slice.data
        } else {
            slice.as_slice().into()
        }
    }
}

impl<'a, T: Clone> From<&'a [T]> for CowSlice<T> {
    fn from(slice: &'a [T]) -> Self {
        Self {
            start: 0,
            end: slice.len() as u32,
            data: slice.into(),
        }
    }
}

impl<T: Clone, const N: usize> From<[T; N]> for CowSlice<T> {
    fn from(array: [T; N]) -> Self {
        Self {
            start: 0,
            end: N as u32,
            data: array.into(),
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for CowSlice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T> Borrow<[T]> for CowSlice<T> {
    fn borrow(&self) -> &[T] {
        self
    }
}

impl<T> AsRef<[T]> for CowSlice<T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T: PartialEq> PartialEq for CowSlice<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for CowSlice<T> {}

impl<T: PartialOrd> PartialOrd for CowSlice<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Ord> Ord for CowSlice<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: PartialEq> PartialEq<[T]> for CowSlice<T> {
    fn eq(&self, other: &[T]) -> bool {
        **self == *other
    }
}

impl<T: PartialEq> PartialEq<Vec<T>> for CowSlice<T> {
    fn eq(&self, other: &Vec<T>) -> bool {
        **self == *other
    }
}

impl<T: PartialEq, const N: usize> PartialEq<[T; N]> for CowSlice<T> {
    fn eq(&self, other: &[T; N]) -> bool {
        **self == *other
    }
}

impl<T: Hash> Hash for CowSlice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl<T: Clone> IntoIterator for CowSlice<T> {
    type Item = T;
    type IntoIter = CowSliceIntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        CowSliceIntoIter {
            data: self.data,
            start: self.start as usize,
            end: self.end as usize,
        }
    }
}

/// An iterator over a `CowSlice`
pub struct CowSliceIntoIter<T> {
    data: EcoVec<T>,
    start: usize,
    end: usize,
}

impl<T: Clone> Iterator for CowSliceIntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        if self.start >= self.end {
            None
        } else {
            let item = unsafe { self.data.get_unchecked(self.start) }.clone();
            self.start += 1;
            Some(item)
        }
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.end - self.start;
        (len, Some(len))
    }
}

impl<T: Clone> ExactSizeIterator for CowSliceIntoIter<T> {}

impl<'a, T> IntoIterator for &'a CowSlice<T> {
    type Item = &'a T;
    type IntoIter = <&'a [T] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T: Clone> IntoIterator for &'a mut CowSlice<T> {
    type Item = &'a mut T;
    type IntoIter = <&'a mut [T] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.as_mut_slice().iter_mut()
    }
}

impl<T: Clone> FromIterator<T> for CowSlice<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut data = EcoVec::new();
        data.extend(iter);
        data.into()
    }
}

impl<T: Clone> Extend<T> for CowSlice<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.modify_end(|vec| vec.extend(iter))
    }
}

impl<T: Clone> Serialize for CowSlice<T>
where
    T: Serialize,
{
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (**self).serialize(serializer)
    }
}

impl<'de, T: Clone> Deserialize<'de> for CowSlice<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self::from(EcoVec::<T>::deserialize(deserializer)?))
    }
}
