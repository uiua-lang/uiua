use std::{cmp::Ordering, ptr};

use ecow::EcoVec;
use rand::Rng;
use rayon::prelude::*;

use crate::{algorithm::ArrayCmpSlice, random_with, val_as_arr, Array, ArrayValue, Value};

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
        if self.meta.is_sorted_down() {
            indices.make_mut().reverse();
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
        if self.meta.is_sorted_down() {
            indices.reverse();
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
        if self.meta.is_sorted_up() {
            indices.make_mut().reverse();
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
        if self.meta.is_sorted_up() {
            indices.reverse();
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
        dbg!();
        let depth = depth.min(self.rank());
        if self.rank() == depth
            || self.shape.elements() == 0
            || depth == 0 && self.meta.is_sorted_up()
        {
            return;
        }
        dbg!();
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
        dbg!();
        let chunk_len: usize = self.shape[depth..].iter().product();
        let subrow_len: usize = self.shape[depth + 1..].iter().product();
        if chunk_len == 0 || subrow_len == 0 {
            return;
        }
        dbg!();
        if self.rank() == depth + 1 {
            // Sort simple list
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                T::sort_list(chunk, true);
            }
        } else {
            // Sort arrays lexicographically
            let mut new_chunk = Vec::with_capacity(chunk_len);
            let mut indices = vec![0; chunk_len / subrow_len];
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                for (i, idx) in indices.iter_mut().enumerate() {
                    *idx = i;
                }
                indices.par_sort_unstable_by(|&a, &b| {
                    ArrayCmpSlice(&chunk[a * subrow_len..(a + 1) * subrow_len])
                        .cmp(&ArrayCmpSlice(&chunk[b * subrow_len..(b + 1) * subrow_len]))
                });
                new_chunk.clear();
                for &i in &indices {
                    new_chunk.extend_from_slice(&chunk[i * subrow_len..(i + 1) * subrow_len]);
                }
                chunk.clone_from_slice(&new_chunk);
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
            let mut new_chunk = Vec::with_capacity(chunk_len);
            let mut indices = vec![0; chunk_len / subrow_len];
            for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
                for (i, idx) in indices.iter_mut().enumerate() {
                    *idx = i;
                }
                indices.par_sort_unstable_by(|&a, &b| {
                    ArrayCmpSlice(&chunk[b * subrow_len..(b + 1) * subrow_len])
                        .cmp(&ArrayCmpSlice(&chunk[a * subrow_len..(a + 1) * subrow_len]))
                });
                new_chunk.clear();
                for &i in &indices {
                    new_chunk.extend_from_slice(&chunk[i * subrow_len..(i + 1) * subrow_len]);
                }
                chunk.clone_from_slice(&new_chunk);
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
                let j = rng.gen_range(0..i);
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
}
