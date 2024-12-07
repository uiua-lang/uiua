//! Code for dyadic search functions

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    slice,
};

use ecow::{eco_vec, EcoVec};

use crate::{
    algorithm::{max_shape, validate_size},
    array::*,
    cowslice::cowslice,
    value::Value,
    Shape, Uiua, UiuaResult,
};

use super::{ArrayCmpSlice, FillContext};

impl Value {
    /// Check which rows of this value are `member`s of another
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            of,
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| a.member(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for members of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Check which rows of this array are `member`s of another
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let elems = self;
        let mut arr = match elems.rank().cmp(&of.rank()) {
            Ordering::Equal => {
                let has_wildcard =
                    elems.data.iter().any(T::has_wildcard) || of.data.iter().any(T::has_wildcard);
                let mut result_data = EcoVec::with_capacity(elems.row_count());
                if has_wildcard {
                    for elem in elems.row_slices() {
                        let is_member = of
                            .row_slices()
                            .any(|row| ArrayCmpSlice(row) == ArrayCmpSlice(elem));
                        result_data.push(is_member as u8);
                    }
                } else {
                    let mut members = HashSet::with_capacity(of.row_count());
                    for of in of.row_slices() {
                        members.insert(ArrayCmpSlice(of));
                    }
                    for elem in elems.row_slices() {
                        let is_member = members.contains(&ArrayCmpSlice(elem));
                        result_data.push(is_member as u8);
                    }
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                Array::new(shape, result_data)
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for elem in elems.rows() {
                    rows.push(elem.member(of, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if !of.shape.ends_with(&elems.shape) {
                    return Err(env.error(format!(
                        "Cannot look for array of shape {} in array of shape {}",
                        self.shape, of.shape
                    )));
                }
                if of.rank() - elems.rank() == 1 {
                    of.rows().any(|r| *elems == r).into()
                } else {
                    let mut rows = Vec::with_capacity(of.row_count());
                    for of in of.rows() {
                        rows.push(elems.member(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        };
        arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
        Ok(arr)
    }
}

impl Value {
    /// Get the `index of` the rows of this value in another
    pub fn index_of(&self, haystack: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            haystack,
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| a.index_of(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for indices of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
    /// Get the `progressive index of` the rows of this value in another
    pub fn progressive_index_of(&self, haystack: &Value, env: &Uiua) -> UiuaResult<Value> {
        self.generic_bin_ref(
            haystack,
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| a.progressive_index_of(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot look for indices of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the `index of` the rows of this array in another
    pub fn index_of(&self, haystack: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let needle = self;
        let default = (env.scalar_fill::<f64>()).unwrap_or(haystack.row_count() as f64);
        Ok(match needle.rank().cmp(&haystack.rank()) {
            Ordering::Equal => {
                let has_wildcard = needle.data.iter().any(T::has_wildcard)
                    || haystack.data.iter().any(T::has_wildcard);
                let mut result_data = EcoVec::with_capacity(needle.row_count());
                if has_wildcard {
                    for elem in needle.row_slices() {
                        let index = (haystack.row_slices())
                            .position(|row| ArrayCmpSlice(row) == ArrayCmpSlice(elem))
                            .map(|i| i as f64)
                            .unwrap_or(default);
                        result_data.push(index);
                    }
                } else {
                    let mut members = HashMap::with_capacity(haystack.row_count());
                    for (i, of) in haystack.row_slices().enumerate() {
                        members.entry(ArrayCmpSlice(of)).or_insert(i);
                    }
                    for elem in needle.row_slices() {
                        result_data.push(
                            members
                                .get(&ArrayCmpSlice(elem))
                                .map(|i| *i as f64)
                                .unwrap_or(default),
                        );
                    }
                }
                let shape: Shape = needle.shape.iter().take(1).copied().collect();
                Array::new(shape, result_data)
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(needle.row_count());
                for elem in needle.rows() {
                    rows.push(elem.index_of(haystack, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if !haystack.shape.ends_with(&needle.shape) {
                    return Err(env.error(format!(
                        "Cannot get index of array of shape {} in array of shape {}",
                        needle.shape(),
                        haystack.shape()
                    )));
                }
                if haystack.rank() - needle.rank() == 1 {
                    (haystack
                        .row_slices()
                        .position(|r| {
                            r.len() == needle.data.len()
                                && r.iter().zip(&needle.data).all(|(a, b)| a.array_eq(b))
                        })
                        .map(|i| i as f64)
                        .unwrap_or(default))
                    .into()
                } else {
                    let mut rows = Vec::with_capacity(haystack.row_count());
                    for of in haystack.rows() {
                        rows.push(needle.index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
    /// Get the `progressive index of` the rows of this array in another
    fn progressive_index_of(&self, haystack: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let needle = self;
        Ok(match needle.rank().cmp(&haystack.rank()) {
            Ordering::Equal => {
                let mut cache = HashMap::new();
                let mut result_data = eco_vec![0.0; needle.row_count()];
                let slice = result_data.make_mut();
                let default = haystack.row_count();
                if needle.rank() == 1 {
                    for (dest, elem) in slice.iter_mut().zip(&needle.data) {
                        let key = ArrayCmpSlice(slice::from_ref(elem));
                        *dest = if let Some(i) = cache.get_mut(&key) {
                            *i += 1;
                            *i = if *i < haystack.data.len() {
                                (haystack.data[*i..].iter())
                                    .position(|of| of.array_eq(elem))
                                    .map(|idx| idx + *i)
                                    .unwrap_or(default)
                            } else {
                                default
                            };
                            *i
                        } else {
                            let i = (haystack.data.iter())
                                .position(|of| of.array_eq(elem))
                                .unwrap_or(default);
                            cache.insert(key, i);
                            i
                        } as f64;
                    }
                } else {
                    let haystack_row_len = haystack.row_len();
                    for (dest, row) in slice.iter_mut().zip(needle.row_slices()) {
                        let key = ArrayCmpSlice(row);
                        *dest = if let Some(i) = cache.get_mut(&key) {
                            *i += 1;
                            *i = haystack.data[*i * haystack_row_len..]
                                .chunks_exact(haystack_row_len)
                                .position(|of| ArrayCmpSlice(of) == key)
                                .map(|idx| idx + *i)
                                .unwrap_or(default);
                            *i
                        } else {
                            let i = haystack
                                .data
                                .chunks_exact(haystack_row_len)
                                .position(|of| ArrayCmpSlice(of) == key)
                                .unwrap_or(default);
                            cache.insert(key, i);
                            i
                        } as f64;
                    }
                }
                Array::new(
                    needle.shape.iter().take(1).copied().collect::<Shape>(),
                    result_data,
                )
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(needle.row_count());
                for elem in needle.rows() {
                    rows.push(elem.progressive_index_of(haystack, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if haystack.rank() - needle.rank() == 1 {
                    if needle.rank() == 0 {
                        let needle = &needle.data[0];
                        Array::from(
                            (haystack.data.iter())
                                .position(|of| needle.array_eq(of))
                                .unwrap_or(haystack.row_count()) as f64,
                        )
                    } else {
                        ((haystack.rows().position(|r| r == *needle))
                            .unwrap_or(haystack.row_count()) as f64)
                            .into()
                    }
                } else {
                    let mut rows = Vec::with_capacity(haystack.row_count());
                    for of in haystack.rows() {
                        rows.push(needle.progressive_index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
}

impl Value {
    /// Try to `find` this value in another
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            searched,
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| a.find(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot find {} in {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
    /// Try to `mask` this value in another
    pub fn mask(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        self.generic_bin_ref(
            searched,
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| a.mask(b, env).map(Into::into),
            |a, b| {
                env.error(format!(
                    "Cannot mask {} in {} array",
                    a.type_name(),
                    b.type_name()
                ))
            },
        )
    }
}

impl<T: ArrayValue> Array<T> {
    /// Try to `find` this array in another
    pub fn find(&self, haystack: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        let needle = self;
        let mut haystack = haystack;

        let mut local_searched: Self;
        let any_dim_greater = (needle.shape().iter().rev())
            .zip(haystack.shape().iter().rev())
            .any(|(a, b)| a > b);
        if needle.rank() > haystack.rank() || any_dim_greater {
            // Fill
            match env.scalar_fill() {
                Ok(fill) => {
                    let target_shape = max_shape(&haystack.shape, &needle.shape);
                    local_searched = haystack.clone();
                    local_searched.fill_to_shape(&target_shape, fill);
                    haystack = &local_searched;
                }
                Err(_) => {
                    let data = cowslice![0; haystack.element_count()];
                    let mut arr = Array::new(haystack.shape.clone(), data);
                    arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
                    return Ok(arr);
                }
            }
        }

        // Pad the shape of the searched-for array
        let mut searched_for_shape = needle.shape.clone();
        while searched_for_shape.len() < haystack.shape.len() {
            searched_for_shape.insert(0, 1);
        }

        // Calculate the pre-padded output shape
        let temp_output_shape: Shape = haystack
            .shape
            .iter()
            .zip(&searched_for_shape)
            .map(|(s, f)| s + 1 - f)
            .collect();

        let elem_count = validate_size::<T>(temp_output_shape.iter().copied(), env)?;
        let mut data = EcoVec::from_elem(0, elem_count);
        let data_slice = data.make_mut();

        if haystack.rank() == 1 {
            // Fast path for rank-1 arrays
            if let Some(first) = needle.data.first() {
                // The jump is the number of elements to skip forward on a failed partial match
                let jump = (needle.data.iter())
                    .skip(1)
                    .position(|d| d.array_eq(first))
                    .map_or(needle.data.len(), |i| i + 1);
                let mut i = 0;
                while i < haystack.data.len() + 1 - needle.data.len() {
                    if haystack.data[i].array_eq(first) {
                        if needle.data[1..]
                            .iter()
                            .zip(&haystack.data[i + 1..])
                            .all(|(a, b)| a.array_eq(b))
                        {
                            // All elements match
                            data_slice[i] = 1;
                            i += jump;
                        } else {
                            // Mismatch
                            i += 1;
                        }
                    } else {
                        // Mismatch
                        i += 1;
                    }
                }
            }
        } else {
            let mut corner = vec![0; haystack.shape.len()];
            let mut curr = vec![0; haystack.shape.len()];
            let mut k = 0;

            if haystack.shape.iter().all(|&d| d > 0) {
                'windows: loop {
                    // Reset curr
                    for i in curr.iter_mut() {
                        *i = 0;
                    }
                    // Search the window whose top-left is the current corner
                    'items: loop {
                        // Get index for the current item in the haystack
                        let mut searched_index = 0;
                        let mut stride = 1;
                        for ((c, i), s) in corner.iter().zip(&curr).zip(&haystack.shape).rev() {
                            searched_index += (*c + *i) * stride;
                            stride *= s;
                        }
                        // Get index for the current item in the searched-for array
                        let mut search_for_index = 0;
                        let mut stride = 1;
                        for (i, s) in curr.iter().zip(&searched_for_shape).rev() {
                            search_for_index += *i * stride;
                            stride *= s;
                        }
                        // Compare the current items in the two arrays
                        let same = if let Some(searched_for) = needle.data.get(search_for_index) {
                            haystack.data[searched_index].array_eq(searched_for)
                        } else {
                            false
                        };
                        if !same {
                            data_slice[k] = 0;
                            k += 1;
                            break;
                        }
                        // Go to the next item
                        for i in (0..curr.len()).rev() {
                            if curr[i] == searched_for_shape[i] - 1 {
                                curr[i] = 0;
                            } else {
                                curr[i] += 1;
                                continue 'items;
                            }
                        }
                        data_slice[k] = 1;
                        k += 1;
                        break;
                    }
                    // Go to the next corner
                    for i in (0..corner.len()).rev() {
                        if corner[i] == haystack.shape[i] - searched_for_shape[i] {
                            corner[i] = 0;
                        } else {
                            corner[i] += 1;
                            continue 'windows;
                        }
                    }
                    break;
                }
            }
        }
        let mut arr = Array::new(temp_output_shape, data);
        arr.fill_to_shape(&haystack.shape[..searched_for_shape.len()], 0);
        arr.validate_shape();
        arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
        Ok(arr)
    }
    /// Try to `mask` this array in another
    pub fn mask(&self, haystack: &Self, env: &Uiua) -> UiuaResult<Value> {
        let needle = self;
        if needle.rank() > haystack.rank() {
            return Err(env.error(format!(
                "Cannot look for rank {} array in rank {} array",
                needle.rank(),
                haystack.rank()
            )));
        }
        if (needle.shape.iter().rev())
            .zip(haystack.shape.iter().rev())
            .any(|(n, h)| n > h)
        {
            return Ok(Array::new(
                haystack.shape.clone(),
                eco_vec![0u8; haystack.element_count()],
            )
            .into());
        }
        let mut result_data = eco_vec![0.0; haystack.element_count()];
        let res = result_data.make_mut();

        if haystack.rank() == 1 {
            // Fast path for rank-1 arrays
            if needle.data.len() > 0 {
                let mut curr = 0;
                let mut i = 0;
                while i < haystack.data.len() + 1 - needle.data.len() {
                    if (needle.data.iter())
                        .zip(&haystack.data[i..])
                        .all(|(a, b)| a.array_eq(b))
                    {
                        curr += 1;
                        for j in i..i + needle.data.len() {
                            res[j] = curr as f64;
                        }
                        i += needle.data.len();
                    } else {
                        i += 1;
                    }
                }
            }
        } else {
            let needle_data = needle.data.as_slice();
            let mut needle_shape = needle.shape.clone();
            while needle_shape.len() < haystack.shape.len() {
                needle_shape.insert(0, 1);
            }
            let needle_elems = needle.element_count();
            let mut curr = Vec::new();
            let mut offset = Vec::new();
            let mut sum = vec![0; needle_shape.len()];
            let mut match_num = 0u64;
            for i in 0..res.len() {
                // Check if the needle matches the haystack at the current index
                haystack.shape.flat_to_dims(i, &mut curr);
                let mut matches = true;
                for j in 0..needle_elems {
                    needle_shape.flat_to_dims(j, &mut offset);
                    for ((c, o), s) in curr.iter().zip(&offset).zip(&mut sum) {
                        *s = *c + *o;
                    }
                    if (haystack.shape.dims_to_flat(&sum)).map_or(true, |k| {
                        res[k] > 0.0 || !needle_data[j].array_eq(&haystack.data[k])
                    }) {
                        matches = false;
                        break;
                    }
                }
                // Fill matches
                if matches {
                    match_num += 1;
                    for j in 0..needle_elems {
                        needle_shape.flat_to_dims(j, &mut offset);
                        for ((c, o), s) in curr.iter().zip(&offset).zip(&mut sum) {
                            *s = *c + *o;
                        }
                        let k = haystack.shape.dims_to_flat(&sum).unwrap();
                        res[k] = match_num as f64;
                    }
                }
            }
        }
        let mut val: Value = Array::new(haystack.shape.clone(), result_data).into();
        val.compress();
        Ok(val)
    }
}
