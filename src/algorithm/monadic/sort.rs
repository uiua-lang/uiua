use std::{cmp::Ordering, ptr};

use ecow::EcoVec;
use rand::RngExt;
use rayon::prelude::*;

use crate::{Array, ArrayValue, Value, algorithm::ArrayCmpSlice, random_with, val_as_arr};

impl Value {
    /// Get the `rise` of the value
    pub fn rise(&self) -> Array<f64> {
        val_as_arr!(self, Array::rise)
    }
    /// Get the `fall` of the value
    pub fn fall(&self) -> Array<f64> {
        val_as_arr!(self, Array::fall)
    }
    /// Sort the value ascending
    pub fn sort_up(&mut self) {
        self.sort_up_depth(0);
    }
    pub(crate) fn sort_up_depth(&mut self, depth: usize) {
        val_as_arr!(self, |a| a.sort_up_depth(depth))
    }
    /// Sort the value descending
    pub fn sort_down(&mut self) {
        self.sort_down_depth(0);
    }
    pub(crate) fn sort_down_depth(&mut self, depth: usize) {
        val_as_arr!(self, |a| a.sort_down_depth(depth))
    }
    /// Shuffle the value
    pub fn shuffle(&mut self) {
        val_as_arr!(self, Array::shuffle)
    }
    /// Check if a value is sorted up
    pub fn is_sorted_up(&self) -> bool {
        val_as_arr!(self, Array::is_sorted_up)
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the `rise` of the array
    pub fn rise(&self) -> Array<f64> {
        if self.rank() == 0 {
            return Array::scalar(0.0);
        }
        if self.row_count() == 0 {
            return Array::default();
        }
        let mut indices = (0..self.row_count())
            .map(|i| i as f64)
            .collect::<EcoVec<_>>();
        if self.meta.is_sorted_up() {
            return indices.into();
        }
        indices.make_mut().par_sort_by(|&a, &b| {
            self.row_slice(a as usize)
                .iter()
                .zip(self.row_slice(b as usize))
                .map(|(a, b)| a.array_cmp(b))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices.into()
    }
    pub(crate) fn rise_indices(&self) -> Vec<usize> {
        if self.rank() == 0 {
            return vec![0];
        }
        if self.row_count() == 0 {
            return Vec::new();
        }
        let mut indices: Vec<usize> = (0..self.row_count()).collect();
        if self.meta.is_sorted_up() {
            return indices;
        }
        indices.par_sort_by(|&a, &b| {
            self.row_slice(a)
                .iter()
                .zip(self.row_slice(b))
                .map(|(a, b)| a.array_cmp(b))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices
    }
    /// Get the `fall` of the array
    pub fn fall(&self) -> Array<f64> {
        if self.rank() == 0 {
            return Array::scalar(0.0);
        }
        if self.row_count() == 0 {
            return Array::default();
        }
        let mut indices = (0..self.row_count())
            .map(|i| i as f64)
            .collect::<EcoVec<_>>();
        if self.meta.is_sorted_down() {
            return indices.into();
        }
        indices.make_mut().par_sort_by(|&a, &b| {
            self.row_slice(a as usize)
                .iter()
                .zip(self.row_slice(b as usize))
                .map(|(a, b)| b.array_cmp(a))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices.into()
    }
    pub(crate) fn fall_indices(&self) -> Vec<usize> {
        if self.rank() == 0 {
            return vec![0];
        }
        if self.row_count() == 0 {
            return Vec::new();
        }
        let mut indices: Vec<usize> = (0..self.row_count()).collect();
        if self.meta.is_sorted_down() {
            return indices;
        }
        indices.par_sort_by(|&a, &b| {
            self.row_slice(a)
                .iter()
                .zip(self.row_slice(b))
                .map(|(a, b)| b.array_cmp(a))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices
    }
    /// Sort an array ascending
    pub fn sort_up(&mut self) {
        self.sort_up_depth(0);
    }
    /// Sort an array descending
    pub fn sort_down(&mut self) {
        self.sort_down_depth(0);
    }
    pub(crate) fn sort_up_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        if self.rank() == depth
            || self.shape.elements() == 0
            || depth == 0 && self.meta.is_sorted_up()
        {
            return;
        }
        if let Some(Some(keys)) = (depth == 0).then(|| self.meta.take_map_keys()) {
            let keys = keys.normalized();
            let rise = self.rise_indices();
            self.sort_up();
            let new_keys = Value::from_row_values_infallible(
                rise.into_iter().map(|i| keys.row(i)).collect::<Vec<_>>(),
            );
            self.map(new_keys, &()).unwrap();
            return;
        }
        let chunk_len: usize = self.shape[depth..].iter().product();
        let subrow_len: usize = self.shape[depth + 1..].iter().product();
        if chunk_len == 0 || subrow_len == 0 {
            return;
        }
        if self.rank() == depth + 1 {
            // Sort simple list
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                T::sort_list(chunk, true);
            }
        } else {
            // Sort arrays lexicographically
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                sort(chunk, subrow_len, |a, b| {
                    ArrayCmpSlice(a).cmp(&ArrayCmpSlice(b))
                });
            }
        }
        if depth == 0 {
            self.meta.mark_sorted_up(true);
            self.meta.mark_sorted_down(false);
        } else {
            self.meta.take_sorted_flags();
        }
        self.validate();
    }
    pub(crate) fn sort_down_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        if self.rank() == depth
            || self.shape.elements() == 0
            || depth == 0 && self.meta.is_sorted_down()
        {
            return;
        }
        if let Some(Some(keys)) = (depth == 0).then(|| self.meta.take_map_keys()) {
            let keys = keys.normalized();
            let fall = self.fall_indices();
            self.sort_down();
            let new_keys = Value::from_row_values_infallible(
                fall.into_iter().map(|i| keys.row(i)).collect::<Vec<_>>(),
            );
            self.map(new_keys, &()).unwrap();
            return;
        }
        let chunk_len: usize = self.shape[depth..].iter().product();
        let subrow_len: usize = self.shape[depth + 1..].iter().product();
        if chunk_len == 0 || subrow_len == 0 {
            return;
        }
        if self.rank() == depth + 1 {
            // Sort simple list
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                T::sort_list(chunk, false);
            }
        } else {
            // Sort arrays lexicographically
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                sort(chunk, subrow_len, |a, b| {
                    ArrayCmpSlice(b).cmp(&ArrayCmpSlice(a))
                });
            }
        }
        if depth == 0 {
            self.meta.mark_sorted_up(false);
            self.meta.mark_sorted_down(true);
        } else {
            self.meta.take_sorted_flags();
        }
        self.validate();
    }
    /// Shuffle the array
    pub fn shuffle(&mut self) {
        if self.row_count() < 2 {
            return;
        }
        random_with(|rng| {
            let row_count = self.row_count();
            let row_len = self.row_len();
            let slice = self.data.as_mut_slice();
            for i in (1..row_count).rev() {
                let j = rng.random_range(0..=i);
                if i == j {
                    continue;
                }
                // Safety: i and j are in bounds and not equal
                unsafe {
                    ptr::swap_nonoverlapping(
                        slice.as_mut_ptr().add(i * row_len),
                        slice.as_mut_ptr().add(j * row_len),
                        row_len,
                    )
                };
            }
        });
        self.meta.take_sorted_flags();
        self.validate();
    }
    /// Check whether the array is sorted up
    pub fn is_sorted_up(&self) -> bool {
        if self.meta.is_sorted_up() {
            return true;
        }
        let mut rows = self.row_slices().map(ArrayCmpSlice);
        let Some(mut prev) = rows.next() else {
            return true;
        };
        for row in rows {
            if prev > row {
                return false;
            }
            prev = row;
        }
        true
    }
    /// Check whether the array is sorted down
    pub fn is_sorted_down(&self) -> bool {
        if self.meta.is_sorted_down() {
            return true;
        }
        let mut rows = self.row_slices().map(ArrayCmpSlice);
        let Some(mut prev) = rows.next() else {
            return true;
        };
        for row in rows {
            if prev < row {
                return false;
            }
            prev = row;
        }
        true
    }
}

fn sort<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy + Sync,
) {
    let Some(row_count) = sort_count(slice, row_len) else {
        return;
    };
    let max_depth = (row_count as f64).log(2.0).ceil() as usize;
    intro_sort(slice, row_len, f, max_depth);
}

fn intro_sort<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy + Sync,
    max_depth: usize,
) {
    let Some(row_count) = sort_count(slice, row_len) else {
        return;
    };
    const MAX_INSERTION_SORT: usize = 20;
    const MAX_SEQUENTIAL: usize = 2000;
    if row_count <= MAX_INSERTION_SORT {
        insertion_sort(slice, row_len, f);
    } else if max_depth == 0 {
        heap_sort(slice, row_len, f);
    } else {
        let pivot = partition(slice, row_len, f);
        let (left, right) = slice.split_at_mut(pivot);
        let right = &mut right[row_len..];
        if left.len().max(right.len()) / row_len <= MAX_SEQUENTIAL {
            intro_sort(left, row_len, f, max_depth - 1);
            intro_sort(right, row_len, f, max_depth - 1);
        } else {
            rayon::join(
                || intro_sort(left, row_len, f, max_depth - 1),
                || intro_sort(right, row_len, f, max_depth - 1),
            );
        }
    }
}

/// The returned pivot index is already scaled by `row_len`.
fn partition<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy,
) -> usize {
    let Some(row_count) = sort_count(slice, row_len) else {
        return 0;
    };
    let piv_a = row_count * 3 / 4 * row_len;
    let piv_b = row_count / 2 * row_len;
    let piv_c = row_count / 4 * row_len;
    let pivot_index = if cmp(slice, row_len, piv_a, piv_b, f) == Ordering::Less {
        if cmp(slice, row_len, piv_b, piv_c, f) == Ordering::Less {
            piv_b
        } else if cmp(slice, row_len, piv_a, piv_c, f) == Ordering::Less {
            piv_c
        } else {
            piv_a
        }
    } else if cmp(slice, row_len, piv_a, piv_c, f) == Ordering::Less {
        piv_a
    } else if cmp(slice, row_len, piv_b, piv_c, f) == Ordering::Less {
        piv_c
    } else {
        piv_b
    };
    let mut a = 0;
    let mut b = slice.len() - row_len;
    loop {
        while a < slice.len() && cmp(slice, row_len, a, pivot_index, f) != Ordering::Greater {
            a += row_len;
        }
        while b > 0 && cmp(slice, row_len, b, pivot_index, f) != Ordering::Less {
            b -= row_len;
        }
        if a >= b {
            break;
        }
        // Safety: a and b are in bounds
        unsafe {
            ptr::swap_nonoverlapping(
                slice.as_mut_ptr().add(a),
                slice.as_mut_ptr().add(b),
                row_len,
            )
        }
    }
    let res = if a < pivot_index {
        a
    } else if b > pivot_index {
        b
    } else {
        pivot_index
    };
    if res != pivot_index {
        // Safety: res and pivot are in bounds
        unsafe {
            ptr::swap_nonoverlapping(
                slice.as_mut_ptr().add(res),
                slice.as_mut_ptr().add(pivot_index),
                row_len,
            )
        };
    }
    res
}

fn insertion_sort<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy,
) {
    let Some(row_count) = sort_count(slice, row_len) else {
        return;
    };
    for i in 1..row_count {
        for j in (0..i).rev() {
            let a = j * row_len;
            let b = a + row_len;
            if cmp(slice, row_len, a, b, f) == Ordering::Greater {
                // Safety: a and b are in bounds
                unsafe {
                    ptr::swap_nonoverlapping(
                        slice.as_mut_ptr().add(a),
                        slice.as_mut_ptr().add(b),
                        row_len,
                    )
                };
            } else {
                break;
            }
        }
    }
}

fn heap_sort<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy,
) {
    let Some(row_count) = sort_count(slice, row_len) else {
        return;
    };
    heapify(slice, row_len, f);
    for end in (1..row_count).rev() {
        unsafe {
            ptr::swap_nonoverlapping(
                slice.as_mut_ptr(),
                slice.as_mut_ptr().add(end * row_len),
                row_len,
            )
        };
        sift_down(slice, row_len, f, 0, end);
    }
}

fn heapify<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy,
) {
    let Some(row_count) = sort_count(slice, row_len) else {
        return;
    };
    let start = heap_parent(row_count - 1) + 1;
    for root in (0..start).rev() {
        sift_down(slice, row_len, f, root, row_count);
    }
}

/// `end` and `root` are in units of `row_len`.
fn sift_down<T: ArrayValue>(
    slice: &mut [T],
    row_len: usize,
    f: impl Fn(&[T], &[T]) -> Ordering + Copy,
    mut root: usize,
    end: usize,
) {
    while heap_left(root) < end {
        let mut child = heap_left(root);
        let right = heap_right(root);
        if right < end && cmp(slice, row_len, child * row_len, right * row_len, f) == Ordering::Less
        {
            child = right;
        }
        let root_offset = root * row_len;
        let child_offset = child * row_len;
        if cmp(slice, row_len, root_offset, child_offset, f) == Ordering::Less {
            // Safety: root and child are in bounds
            unsafe {
                ptr::swap_nonoverlapping(
                    slice.as_mut_ptr().add(root_offset),
                    slice.as_mut_ptr().add(child_offset),
                    row_len,
                )
            };
            root = child;
        } else {
            break;
        }
    }
}

#[inline(always)]
fn heap_parent(i: usize) -> usize {
    (i - 1) / 2
}
#[inline(always)]
fn heap_left(i: usize) -> usize {
    2 * i + 1
}
#[inline(always)]
fn heap_right(i: usize) -> usize {
    2 * i + 2
}

#[inline(always)]
fn sort_count<T>(slice: &[T], row_len: usize) -> Option<usize> {
    #[cfg(debug_assertions)]
    if row_len == 0 {
        return None;
    }
    debug_assert_eq!(slice.len() % row_len, 0);
    let row_count = slice.len() / row_len;
    #[cfg(debug_assertions)]
    if row_count == 0 {
        return None;
    }
    Some(row_count)
}

#[inline(always)]
fn cmp<T: ArrayValue>(
    v: &[T],
    row_len: usize,
    a: usize,
    b: usize,
    f: impl Fn(&[T], &[T]) -> Ordering,
) -> Ordering {
    #[cfg(debug_assertions)]
    {
        f(&v[a..][..row_len], &v[b..][..row_len])
    }
    #[cfg(not(debug_assertions))]
    unsafe {
        f(
            std::slice::from_raw_parts(v.as_ptr().add(a), row_len),
            std::slice::from_raw_parts(v.as_ptr().add(b), row_len),
        )
    }
}
