use std::{
    borrow::Borrow,
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    ops::{Bound, Deref, DerefMut, RangeBounds},
    rc::Rc,
};

macro_rules! cowslice {
    ($($item:expr),* $(,)?) => {
        $crate::cowslice::CowSlice::from([$($item),*])
    };
    ($item:expr; $len:expr) => {
        $crate::cowslice::CowSlice::from([$item; $len])
    }
}

pub(crate) use cowslice;

pub type Ptr<T> = Rc<T>;

pub struct CowSlice<T> {
    data: Ptr<[T]>,
    start: usize,
    end: usize,
}

impl<T> CowSlice<T> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn slice<R>(&self, range: R) -> Self
    where
        R: RangeBounds<usize>,
    {
        let start = match range.start_bound() {
            Bound::Included(&start) => self.start + start,
            Bound::Excluded(&start) => self.start + start + 1,
            Bound::Unbounded => self.start,
        };
        let end = match range.end_bound() {
            Bound::Included(&end) => self.start + end + 1,
            Bound::Excluded(&end) => self.start + end,
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
}

impl<T: Clone> CowSlice<T> {
    pub fn modify<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Vec<T>) -> R,
    {
        let mut vec = self.to_vec();
        let res = f(&mut vec);
        *self = vec.into();
        res
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
            data: Ptr::new([]),
            start: 0,
            end: 0,
        }
    }
}

impl<T> Clone for CowSlice<T> {
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
        &self.data[self.start..self.end]
    }
}

impl<T: Clone> DerefMut for CowSlice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        if Ptr::get_mut(&mut self.data).is_some() {
            let data = unsafe { &mut *Ptr::get_mut(&mut self.data).unwrap_unchecked() };
            &mut data[self.start..self.end]
        } else {
            *self = self.to_vec().into();
            Ptr::get_mut(&mut self.data).unwrap()
        }
    }
}

#[test]
fn cow_slice_deref_mut() {
    let mut slice = CowSlice::from([1, 2, 3, 4]);
    slice[1] = 7;
    assert_eq!(slice, [1, 7, 3, 4]);

    let mut sub = slice.slice(1..=2);
    sub[1] = 5;
    assert_eq!(slice, [1, 7, 3, 4]);
    assert_eq!(sub, [7, 5]);
}

impl<T> From<Vec<T>> for CowSlice<T> {
    fn from(vec: Vec<T>) -> Self {
        Self {
            start: 0,
            end: vec.len(),
            data: Ptr::from(vec),
        }
    }
}

impl<'a, T: Clone> From<&'a [T]> for CowSlice<T> {
    fn from(slice: &'a [T]) -> Self {
        Self {
            start: 0,
            end: slice.len(),
            data: Ptr::from(slice),
        }
    }
}

impl<T, const N: usize> From<[T; N]> for CowSlice<T> {
    fn from(array: [T; N]) -> Self {
        Self {
            start: 0,
            end: N,
            data: Ptr::from(array),
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
    type IntoIter = <Vec<T> as IntoIterator>::IntoIter;
    #[allow(clippy::unnecessary_to_owned)]
    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
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
        self.iter_mut()
    }
}

impl<T> FromIterator<T> for CowSlice<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<Vec<_>>())
    }
}
