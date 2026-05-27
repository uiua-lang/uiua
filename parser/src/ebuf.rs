use std::{
    array, fmt,
    mem::take,
    ops::{Deref, DerefMut},
};

use ecow::{EcoVec, eco_vec};
use serde::*;

#[derive(Serialize, Deserialize)]
#[serde(
    bound = "T: Clone + Serialize + for<'a> Deserialize<'a>, [T; N]: Default",
    from = "EcoVec<T>",
    into = "EcoVec<T>"
)]
pub enum EBuf<T, const N: usize> {
    Stack([T; N], usize),
    Heap(EcoVec<T>),
}

use EBuf::*;

impl<T, const N: usize> Default for EBuf<T, N>
where
    [T; N]: Default,
{
    fn default() -> Self {
        Stack(Default::default(), 0)
    }
}

impl<T, const N: usize> EBuf<T, N> {
    pub fn as_slice(&self) -> &[T] {
        &**self
    }
}
impl<T: Clone, const N: usize> EBuf<T, N> {
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        &mut *self
    }
}
impl<T, const N: usize> EBuf<T, N>
where
    T: Clone + Default,
{
    pub fn new_n(default: T, n: usize) -> Self {
        if n <= N {
            let arr = array::from_fn(|i| if i < n { default.clone() } else { T::default() });
            Stack(arr, n)
        } else {
            Heap(eco_vec![default; n])
        }
    }
    pub fn truncate(&mut self, n: usize) {
        if n >= self.len() {
            return;
        }
        if n <= N {
            match self {
                Stack(_, i) => *i = n,
                Heap(vec) => {
                    let mut iter = take(vec).into_iter().take(n);
                    let arr = array::from_fn(|_| iter.next().unwrap_or_default());
                    *self = Stack(arr, n)
                }
            }
        } else {
            match self {
                Stack(_, _) => unreachable!(),
                Heap(vec) => vec.truncate(n),
            }
        }
    }
}
impl<T, const N: usize> EBuf<T, N>
where
    T: Clone,
    [T; N]: Default,
{
    pub fn insert(&mut self, index: usize, val: T) {
        if index == self.len() {
            self.push(val);
            return;
        }

        match self {
            Stack(arr, i) => {
                if *i < N && index < N {
                    arr[index..].rotate_right(1);
                    arr[index] = val;
                    *i += 1;
                } else {
                    let mut vec = EcoVec::from(take(arr));
                    vec.insert(index, val);
                    *self = Heap(vec);
                }
            }
            Heap(vec) => vec.insert(index, val),
        }
    }
    pub fn push(&mut self, val: T) {
        match self {
            Stack(arr, i) => {
                if *i < N {
                    arr[*i] = val;
                    *i += 1;
                } else {
                    let mut vec = EcoVec::from(take(arr));
                    vec.push(val);
                    *self = Heap(vec);
                }
            }
            Heap(vec) => vec.push(val),
        }
    }
}

impl<T: fmt::Debug, const N: usize> fmt::Debug for EBuf<T, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stack(arr, i) => f.debug_list().entries(arr.iter().take(*i)).finish(),
            Heap(vec) => vec.fmt(f),
        }
    }
}

impl<T: Clone + Default, const N: usize, const M: usize> From<[T; M]> for EBuf<T, N> {
    fn from(arr: [T; M]) -> Self {
        if M <= N {
            let mut iter = arr.into_iter();
            Stack(array::from_fn(|_| iter.next().unwrap_or_default()), M)
        } else {
            Heap(arr.into())
        }
    }
}

impl<T: Clone + Default, const N: usize> From<&[T]> for EBuf<T, N> {
    fn from(slice: &[T]) -> Self {
        if slice.len() <= N {
            let mut iter = slice.iter().cloned();
            let arr = array::from_fn(|_| iter.next().unwrap_or_default());
            Stack(arr, slice.len())
        } else {
            Heap(slice.into())
        }
    }
}

impl<T, const N: usize> From<EcoVec<T>> for EBuf<T, N> {
    fn from(vec: EcoVec<T>) -> Self {
        Heap(vec)
    }
}

impl<T: Clone, const N: usize> From<EBuf<T, N>> for EcoVec<T> {
    fn from(buf: EBuf<T, N>) -> Self {
        match buf {
            Stack(arr, i) => arr.into_iter().take(i).collect(),
            Heap(vec) => vec,
        }
    }
}

impl<T, const N: usize> Clone for EBuf<T, N>
where
    T: Clone,
    [T; N]: Default,
{
    fn clone(&self) -> Self {
        match self {
            Stack(arr, i) => {
                let mut new = <[T; N]>::default();
                for (old, new) in arr.iter().zip(&mut new).take(*i) {
                    *new = old.clone();
                }
                Stack(new, *i)
            }
            Heap(vec) => (*vec).clone().into(),
        }
    }
}

impl<T, const N: usize> Deref for EBuf<T, N> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        match self {
            Stack(arr, i) => &arr[..*i],
            Heap(vec) => vec,
        }
    }
}

impl<T: Clone, const N: usize> DerefMut for EBuf<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Stack(arr, i) => &mut arr[..*i],
            Heap(vec) => vec.make_mut(),
        }
    }
}

impl<T, const N: usize> Extend<T> for EBuf<T, N>
where
    T: Clone,
    [T; N]: Default,
{
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        match self {
            Stack(_, _) => {
                for val in iter {
                    self.push(val)
                }
            }
            Heap(vec) => vec.extend(iter),
        }
    }
}

impl<T: Clone, const N: usize> IntoIterator for EBuf<T, N> {
    type Item = T;
    type IntoIter = <EcoVec<T> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        EcoVec::from(self).into_iter()
    }
}
