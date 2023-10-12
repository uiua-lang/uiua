use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    iter::{Skip, Take},
    ops::{Bound, Deref, RangeBounds},
};

macro_rules! cowslice {
    ($($item:expr),* $(,)?) => {
        $crate::cowslice::CowSlice::from([$($item),*])
    };
    ($item:expr; $len:expr) => {{
        let len = $len;
        let mut cs = $crate::cowslice::CowSlice::with_capacity(len);
        cs.extend(std::iter::repeat($item).take(len));
        cs
    }}
}

pub(crate) use cowslice;
use ecow::EcoVec;

pub struct CowSlice<T> {
    data: EcoVec<T>,
    start: u32,
    end: u32,
}

impl<T: Clone> CowSlice<T> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn truncate(&mut self, len: usize) {
        self.end = (self.start + len as u32).min(self.end);
    }
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: EcoVec::with_capacity(capacity),
            start: 0,
            end: 0,
        }
    }
    pub fn extend_from_slice(&mut self, other: &[T]) {
        self.modify(|vec| vec.extend_from_slice(other))
    }
    pub fn try_extend<E>(&mut self, iter: impl IntoIterator<Item = Result<T, E>>) -> Result<(), E> {
        self.modify(|vec| {
            for item in iter {
                vec.push(item?);
            }
            Ok(())
        })
    }
    pub fn as_slice(&self) -> &[T] {
        self
    }
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        if !self.data.is_unique() {
            let mut new_data = EcoVec::with_capacity(self.len());
            new_data.extend_from_slice(&*self);
            self.data = new_data;
            self.start = 0;
            self.end = self.data.len() as u32;
        }
        self.data.make_mut()
    }
}

impl<T: Clone> CowSlice<T> {
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
    pub fn modify<F, R>(&mut self, f: F) -> R
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
    pub fn reserve_min(&mut self, min: usize) {
        self.modify(|vec| vec.reserve(vec.capacity().max(min) - vec.len()))
    }
}

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
        Self {
            data: EcoVec::new(),
            start: 0,
            end: 0,
        }
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
        &self.data[self.start as usize..self.end as usize]
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
    type IntoIter = Take<Skip<<EcoVec<T> as IntoIterator>::IntoIter>>;
    fn into_iter(self) -> Self::IntoIter {
        self.data
            .into_iter()
            .skip(self.start as usize)
            .take((self.end - self.start) as usize)
    }
}

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
        self.modify(|vec| vec.extend(iter))
    }
}
