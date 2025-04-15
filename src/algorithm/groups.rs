use std::{
    collections::{HashMap, HashSet},
    ptr,
};

use ecow::{eco_vec, EcoVec};

use crate::{
    cowslice::CowSlice, get_ops, types::push_empty_rows_value, val_as_arr, Array, ArrayValue,
    Boxed, Node, Ops, Primitive, ScalarNum, Shape, SigNode, Uiua, UiuaResult, Value,
};

use super::multi_output;

pub fn split_by(f: SigNode, by_scalar: bool, keep_empty: bool, env: &mut Uiua) -> UiuaResult {
    let delim = env.pop(1)?;
    let haystack = env.pop(2)?;
    if f.sig.args() != 1
        || haystack.rank() > 1
        || delim.rank() > 1
        || by_scalar && !(delim.rank() == 0 || delim.rank() == 1 && delim.row_count() == 1)
        || matches!(delim, Value::Complex(_))
        || matches!(haystack, Value::Complex(_))
    {
        let mask = if by_scalar {
            delim.is_ne(haystack.clone(), env)?
        } else {
            delim.mask(&haystack, env)?.not(env)?
        };
        env.push(haystack);
        env.push(mask);
        return partition(f, env);
    }
    if let Some(Primitive::Box) = f.node.as_primitive() {
        let val = haystack.generic_bin_ref(
            &delim,
            |a, b| a.split_by(b, keep_empty, |data| Boxed(data.into())),
            |a, b| a.split_by(b, keep_empty, |data| Boxed(data.into())),
            |_, _| unreachable!("split by complex"),
            |a, b| a.split_by(b, keep_empty, |data| Boxed(data.into())),
            |a, b| a.split_by(b, keep_empty, |data| Boxed(data.into())),
            |a, b| {
                env.error(format!(
                    "Cannot split {} by {}",
                    a.type_name_plural(),
                    b.type_name_plural()
                ))
            },
        )?;
        env.push(val);
    } else {
        let parts = haystack.generic_bin_ref(
            &delim,
            |a, b| a.split_by(b, keep_empty, Value::from),
            |a, b| a.split_by(b, keep_empty, Value::from),
            |_, _| unreachable!("split by complex"),
            |a, b| a.split_by(b, keep_empty, Value::from),
            |a, b| a.split_by(b, keep_empty, Value::from),
            |a, b| {
                env.error(format!(
                    "Cannot split {} by {}",
                    a.type_name_plural(),
                    b.type_name_plural()
                ))
            },
        )?;
        if let Some(Primitive::Identity) = f.node.as_primitive() {
            let val = Value::from_row_values(parts, env)?;
            env.push(val);
        } else {
            let mut outputs = multi_output(f.sig.outputs(), Vec::new());
            env.without_fill(|env| -> UiuaResult {
                for part in parts {
                    env.push(part);
                    env.exec(f.clone())?;
                    for i in 0..f.sig.outputs() {
                        outputs[i].push(env.pop("split by output")?);
                    }
                }
                Ok(())
            })?;
            for outputs in outputs.into_iter().rev() {
                let val = Value::from_row_values(outputs, env)?;
                env.push(val);
            }
        }
    }
    Ok(())
}

impl<T: ArrayValue> Array<T>
where
    Value: From<CowSlice<T>>,
{
    fn split_by<R: Clone>(
        &self,
        delim: &Self,
        keep_empty: bool,
        f: impl Fn(CowSlice<T>) -> R,
    ) -> UiuaResult<EcoVec<R>> {
        let haystack = self.data.as_slice();
        let delim_slice = delim.data.as_slice();
        if delim_slice.is_empty() {
            return Ok(eco_vec![f(self.data.clone())]);
        }
        let mut curr = 0;
        let mut data = EcoVec::new();
        if delim.rank() == 0 || delim.row_count() == 1 {
            let delim = &delim_slice[0];
            for slice in haystack.split(|elem| elem.array_eq(delim)) {
                if slice.is_empty() && !keep_empty {
                    curr += 1;
                    continue;
                }
                let start = curr;
                let end = start + slice.len();
                data.push(f(self.data.slice(start..end)));
                curr = end + 1;
            }
        } else {
            while curr < haystack.len() {
                let prev_end = haystack[curr..]
                    .windows(delim_slice.len())
                    .position(|win| win.iter().zip(delim_slice).all(|(a, b)| a.array_eq(b)))
                    .map(|i| curr + i)
                    .unwrap_or(haystack.len());
                let next_start = prev_end + delim_slice.len();
                if curr == prev_end && !keep_empty {
                    curr = next_start;
                    continue;
                }
                data.push(f(self.data.slice(curr..prev_end)));
                curr = next_start;
            }
            if keep_empty && curr == haystack.len() {
                data.push(f(CowSlice::new()));
            }
        }
        Ok(data)
    }
}

pub fn partition(f: SigNode, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups::<_, i64>(
        Primitive::Partition,
        f,
        Value::partition_groups,
        |val, markers, _| Ok(val.partition_firsts(markers)),
        |val, markers, _| Ok(val.partition_lasts(markers)),
        partition_lens,
        "⊜ partition indices array must be a list of integers",
        env,
    )
}

impl Value {
    fn partition_groups(self, markers: &Array<i64>) -> Box<dyn ExactSizeIterator<Item = Self>> {
        val_as_arr!(self, |arr| arr.partition_groups(markers))
    }
    fn partition_firsts(self, markers: &[i64]) -> Self {
        val_as_arr!(self, |arr| arr.partition_firsts(markers).into())
    }
    fn partition_lasts(self, markers: &[i64]) -> Self {
        val_as_arr!(self, |arr| arr.partition_lasts(markers).into())
    }
}

struct PartitionIter<T> {
    len: usize,
    curr: usize,
    markers: CowSlice<i64>,
    source: Array<T>,
}

impl<T> Iterator for PartitionIter<T>
where
    T: Clone,
    Array<T>: Into<Value>,
{
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        let row_len = self.source.row_len();
        while self.curr < self.markers.len() {
            let marker = self.markers[self.curr];
            if marker <= 0 {
                self.curr += 1;
            } else {
                let start = self.curr;
                while self.curr < self.markers.len() && self.markers[self.curr] == marker {
                    self.curr += 1;
                }
                let end = self.curr;
                let data = self.source.data.slice(start * row_len..end * row_len);
                let mut shape = self.source.shape.clone();
                shape[0] = end - start;
                return Some(Array::new(shape, data).into());
            }
        }
        None
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len - self.curr;
        (len, Some(len))
    }
}

impl<T> ExactSizeIterator for PartitionIter<T>
where
    T: Clone,
    Array<T>: Into<Value>,
{
}

impl<T: ArrayValue> Array<T>
where
    Array<T>: Into<Value>,
{
    fn partition_groups(self, markers: &Array<i64>) -> Box<dyn ExactSizeIterator<Item = Value>> {
        let mut groups = Vec::new();
        if markers.rank() == 1 {
            let mut count = 0;
            let mut last_marker = i64::MAX;
            for &marker in &markers.data {
                if marker > 0 && marker != last_marker {
                    count += 1;
                }
                last_marker = marker;
            }
            Box::new(PartitionIter {
                len: count,
                curr: 0,
                markers: markers.data.clone(),
                source: self,
            })
        } else {
            let row_shape: Shape = self.shape[markers.rank()..].into();
            let indices = multi_partition_indices(markers);
            for (_, indices) in indices {
                let mut group = Vec::with_capacity(indices.len());
                for index in indices {
                    group.push(self.row_shaped_slice(index, row_shape.clone()));
                }
                groups.push(group);
            }
            Box::new(
                groups
                    .into_iter()
                    .map(Array::from_row_arrays_infallible)
                    .map(Into::into),
            )
        }
    }
    fn partition_firsts(mut self, markers: &[i64]) -> Self {
        if self.shape.len() == 0 {
            self.shape.insert(0, 0);
        } else {
            self.shape[0] = 0;
        }
        let row_len = self.row_len();
        let data = self.data.as_mut_slice();
        let mut last_marker = i64::MAX;
        for (i, &marker) in markers.iter().enumerate() {
            if marker > 0 && marker != last_marker {
                let dest_start = self.shape[0] * row_len;
                let src_start = i * row_len;
                if dest_start != src_start {
                    unsafe {
                        ptr::swap_nonoverlapping(
                            data.as_mut_ptr().add(dest_start),
                            data.as_mut_ptr().add(src_start),
                            row_len,
                        );
                    }
                }
                self.shape[0] += 1;
            }
            last_marker = marker;
        }
        self.data.truncate(self.shape[0] * row_len);
        self.validate();
        self
    }
    fn partition_lasts(mut self, markers: &[i64]) -> Self {
        let row_count = self.row_count();
        if self.shape.len() == 0 {
            self.shape.insert(0, 0);
        } else {
            self.shape[0] = 0;
        }
        let row_len = self.row_len();
        let data = self.data.as_mut_slice();
        let mut last_marker = i64::MAX;
        for (i, &marker) in markers.iter().enumerate().rev() {
            if marker > 0 && marker != last_marker {
                self.shape[0] += 1;
                let dest_start = (row_count - self.shape[0]) * row_len;
                let src_start = i * row_len;
                if dest_start != src_start {
                    unsafe {
                        ptr::swap_nonoverlapping(
                            data.as_mut_ptr().add(dest_start),
                            data.as_mut_ptr().add(src_start),
                            row_len,
                        );
                    }
                }
            }
            last_marker = marker;
        }
        data.rotate_right(self.shape[0] * row_len);
        self.data.truncate(self.shape[0] * row_len);
        self.validate();
        self
    }
}

fn partition_lens(markers: &[i64]) -> Array<f64> {
    let mut lens = EcoVec::new();
    let mut prev = i64::MAX;
    let mut len = 0;
    for &marker in markers {
        if marker > 0 {
            if marker == prev {
                len += 1;
            } else {
                if len > 0 {
                    lens.push(len as f64);
                }
                len = 1;
            }
        } else {
            if len > 0 {
                lens.push(len as f64);
            }
            len = 0;
        }
        prev = marker;
    }
    if len > 0 {
        lens.push(len as f64);
    }
    lens.into()
}

fn multi_partition_indices(markers: &Array<i64>) -> Vec<(i64, Vec<usize>)> {
    if markers.element_count() == 0 {
        return Vec::new();
    }
    let mut groups: Vec<(i64, Vec<Vec<usize>>)> = Vec::new();
    let mut curr = vec![0; markers.rank()];
    for &marker in &markers.data {
        if marker >= 1 {
            let mut adjacent_groups = Vec::new();
            // Find adjacent groups with the same value
            for (g, (val, group)) in groups.iter().enumerate() {
                if *val != marker {
                    continue;
                }
                if group.iter().any(|idx| {
                    idx.iter()
                        .zip(&curr)
                        .map(|(a, b)| a.abs_diff(*b))
                        .sum::<usize>()
                        == 1
                }) {
                    adjacent_groups.push(g);
                }
            }
            // Add the current index to the adjacent group, possibly merging groups
            match adjacent_groups.len() {
                0 => {
                    groups.push((marker, vec![curr.clone()]));
                }
                1 => groups[adjacent_groups[0]].1.push(curr.clone()),
                _ => {
                    let mut adjacent_groups = adjacent_groups.into_iter();
                    let first = adjacent_groups.next().unwrap();
                    for (i, g) in adjacent_groups.enumerate() {
                        let group = groups.remove(g - i).1;
                        groups[first].1.extend(group);
                    }
                    groups[first].1.push(curr.clone());
                }
            }
        }
        // Increment the current index
        for (i, c) in curr.iter_mut().enumerate().rev() {
            if *c < markers.shape[i] - 1 {
                *c += 1;
                break;
            }
            *c = 0;
        }
    }
    let mut shape_muls: Vec<usize> = markers
        .shape
        .iter()
        .rev()
        .scan(1, |mul, &dim| {
            let prev = *mul;
            *mul *= dim;
            Some(prev)
        })
        .collect();
    shape_muls.reverse();
    groups
        .into_iter()
        .map(|(marker, group)| {
            (
                marker,
                (group.into_iter())
                    .map(|index| index.iter().zip(&shape_muls).map(|(i, m)| i * m).sum())
                    .collect(),
            )
        })
        .collect()
}

pub fn undo_partition_part1(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    let sig = f.sig;
    if sig != (1, 1) {
        return Err(env.error(format!(
            "Cannot undo {} on function with signature {sig}",
            Primitive::Partition.format()
        )));
    }
    let partitioned = env.pop(1)?;
    // Untransform rows
    let mut untransformed = Vec::with_capacity(partitioned.row_count());
    for row in partitioned.into_rows().rev() {
        env.push(row);
        env.exec(f.clone())?;
        untransformed.push(Boxed(env.pop("unpartitioned row")?));
    }
    untransformed.reverse();
    env.push(Array::from_iter(untransformed));
    Ok(())
}

#[allow(clippy::unit_arg)]
pub fn undo_partition_part2(env: &mut Uiua) -> UiuaResult {
    let untransformed = env.pop(1)?;
    let markers = env
        .pop(2)?
        .as_number_array(env, "⊜ partition markers must be an array of integers")?;
    let mut original = env.pop(3)?;
    if markers.rank() == 1 {
        if original.row_count() == 0 {
            env.push(original);
            return Ok(());
        }
        // Count partition markers
        let mut marker_partitions: Vec<(i64, usize)> = Vec::new();
        let mut markers = markers.data.into_iter();
        if let Some(mut prev) = markers.next() {
            marker_partitions.push((prev, 1));
            for marker in markers {
                if marker == prev {
                    marker_partitions.last_mut().unwrap().1 += 1;
                } else {
                    marker_partitions.push((marker, 1));
                }
                prev = marker;
            }
        }
        let positive_partitions = marker_partitions.iter().filter(|(m, _)| *m > 0).count();
        if positive_partitions != untransformed.row_count() {
            return Err(env.error(format!(
                "Cannot undo {} because the partitioned array \
                originally had {} rows, but now it has {}",
                Primitive::Partition.format(),
                positive_partitions,
                untransformed.row_count()
            )));
        }

        // Unpartition
        let mut untransformed_rows = untransformed.into_rows().map(Value::unboxed);
        let mut unpartitioned = Vec::with_capacity(marker_partitions.len() * original.row_len());
        let mut original_offset = 0;
        for (marker, part_len) in marker_partitions {
            if marker > 0 {
                unpartitioned.extend(untransformed_rows.next().unwrap().into_rows());
            } else {
                unpartitioned
                    .extend((original_offset..original_offset + part_len).map(|i| original.row(i)));
            }
            original_offset += part_len;
        }
        env.push(Value::from_row_values(unpartitioned, env)?);
    } else {
        let row_shape: Shape = original.shape[markers.rank()..].into();
        let indices = multi_partition_indices(&markers);
        let row_elem_count: usize = row_shape.iter().product();
        let untransformed_rows = untransformed.into_rows().map(Value::unboxed);
        for ((_, indices), untransformed_row) in indices.into_iter().zip(untransformed_rows) {
            if indices.len() != untransformed_row.row_count() {
                return Err(env.error(format!(
                    "Cannot undo {} because a partitioned array's \
                    row's length changed from {} to {}",
                    Primitive::Partition.format(),
                    indices.len(),
                    untransformed_row.row_count(),
                )));
            }
            for (index, row) in indices.into_iter().zip(untransformed_row.into_rows()) {
                let start = index * row_elem_count;
                original.generic_bin_mut(
                    row,
                    |a, b| Ok(update_array_at(a, start, b.data.as_slice())),
                    |a, b| Ok(update_array_at(a, start, b.data.as_slice())),
                    |a, b| Ok(update_array_at(a, start, b.data.as_slice())),
                    |a, b| Ok(update_array_at(a, start, b.data.as_slice())),
                    |a, b| Ok(update_array_at(a, start, b.data.as_slice())),
                    |a, b| {
                        env.error(format!(
                            "Cannot unpartition {} array into {} array",
                            b.type_name(),
                            a.type_name()
                        ))
                    },
                )?;
            }
        }
        env.push(original);
    }
    Ok(())
}

fn update_array_at<T: Clone>(arr: &mut Array<T>, start: usize, new: &[T]) {
    let end = start + new.len();
    arr.data.as_mut_slice()[start..end].clone_from_slice(new);
}

pub fn group(f: SigNode, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups(
        Primitive::Group,
        f,
        Value::group_groups,
        Value::group_firsts,
        Value::group_lasts,
        |indices| {
            let buckets = (indices.iter().max().copied().unwrap_or(-1) + 1).max(0) as usize;
            let mut len_counts = HashMap::with_capacity(buckets);
            for &index in indices {
                if index >= 0 {
                    *len_counts.entry(index.unsigned_abs()).or_insert(0) += 1;
                }
            }
            let mut lens: EcoVec<f64> = eco_vec![0.0; buckets];
            let slice = lens.make_mut();
            for (index, len) in len_counts {
                slice[index] = len as f64;
            }
            lens.into()
        },
        "⊕ group indices array must be an array of integers",
        env,
    )
}

impl Value {
    fn group_groups(self, indices: &Array<isize>) -> Vec<Self> {
        val_as_arr!(self, |arr| arr
            .group_groups(indices)
            .map(Into::into)
            .collect())
    }
    fn group_firsts(self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(val_as_arr!(self, |arr| arr
            .group_firsts(indices, env)?
            .into()))
    }
    fn group_lasts(self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(val_as_arr!(self, |arr| arr
            .group_lasts(indices, env)?
            .into()))
    }
}

impl<T: ArrayValue> Array<T> {
    fn group_groups(self, indices: &Array<isize>) -> impl Iterator<Item = Self> {
        let Some(&max_index) = indices.data.iter().max() else {
            return Vec::<Vec<Self>>::new()
                .into_iter()
                .map(Array::from_row_arrays_infallible);
        };
        let buckets = (max_index.max(-1) + 1).max(0) as usize;
        let mut groups: Vec<Vec<Self>> = vec![Vec::new(); buckets];
        let row_shape = self.shape[indices.rank()..].into();
        for (&g, r) in (indices.data.iter()).zip(self.into_row_shaped_slices(row_shape)) {
            if g >= 0 && g < buckets as isize {
                groups[g as usize].push(r);
            }
        }
        groups.into_iter().map(Array::from_row_arrays_infallible)
    }
    fn group_firsts(self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let buckets = (indices.iter().copied().max().unwrap_or(-1) + 1).max(0) as usize;
        if self.row_count() < buckets {
            return Err(env.error("Cannot take first because a group was empty"));
        }
        let row_len = self.row_len();
        let mut encountered = HashSet::new();
        let mut data = self.data.clone();
        data.truncate(buckets * row_len);
        let data_slice = data.as_mut_slice();
        let mut shape = self.shape.clone();
        if shape.len() == 0 {
            shape.insert(0, buckets);
        } else {
            shape[0] = buckets;
        }
        for (&index, row) in indices.iter().zip(self.row_slices()) {
            if index >= 0 && encountered.insert(index) {
                let start = index.unsigned_abs() * row_len;
                let end = start + row_len;
                data_slice[start..end].clone_from_slice(row);
            }
        }
        if encountered.len() != buckets {
            return Err(env.error("Cannot take first because a group was empty"));
        }
        Ok(Array::new(shape, data))
    }
    fn group_lasts(self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let buckets = (indices.iter().copied().max().unwrap_or(-1) + 1).max(0) as usize;
        if self.row_count() < buckets {
            return Err(env.error("Cannot take last because a group was empty"));
        }
        let row_len = self.row_len();
        let mut encountered = HashSet::new();
        let mut data = self.data.clone();
        data.truncate(buckets * row_len);
        let data_slice = data.as_mut_slice();
        let mut shape = self.shape.clone();
        if shape.len() == 0 {
            shape.insert(0, buckets);
        } else {
            shape[0] = buckets;
        }
        for (&index, row) in indices.iter().zip(self.row_slices()).rev() {
            if index >= 0 && encountered.insert(index) {
                let start = index.unsigned_abs() * row_len;
                let end = start + row_len;
                data_slice[start..end].clone_from_slice(row);
            }
        }
        if encountered.len() != buckets {
            return Err(env.error("Cannot take last because a group was empty"));
        }
        Ok(Array::new(shape, data))
    }
}

pub fn undo_group_part1(ops: Ops, env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let [f] = get_ops(ops, env)?;
    let sig = f.sig;
    if sig != (1, 1) {
        return Err(env.error(format!(
            "Cannot undo {} on function with signature {sig}",
            Primitive::Group.format()
        )));
    }
    let grouped = env.pop(1)?;

    // Untransform rows
    let mut ungrouped_rows = Vec::with_capacity(grouped.row_count());
    for mut row in grouped.into_rows().rev() {
        env.push(row);
        env.exec(f.clone())?;
        row = env.pop("ungrouped row")?;
        ungrouped_rows.push(Boxed(row));
    }
    ungrouped_rows.reverse();
    env.push(Array::from_iter(ungrouped_rows));
    Ok(())
}

pub fn undo_group_part2(env: &mut Uiua) -> UiuaResult {
    let ungrouped_rows = env.pop(1)?;
    let indices = env
        .pop(2)?
        .as_integer_array(env, "⊕ group indices must be an array of integers")?;
    let original = env.pop(3)?;
    if original.row_count() == 0 {
        env.push(original);
        return Ok(());
    }

    let expected_count = (indices.data.iter().max().copied().unwrap_or(-1) + 1).max(0);
    if ungrouped_rows.row_count() as isize != expected_count {
        return Err(env.error(format!(
            "Cannot undo {} because the grouped array's \
            length changed from {} to {}",
            Primitive::Group.format(),
            expected_count,
            ungrouped_rows.row_count(),
        )));
    }

    // Ungroup
    let mut ungrouped_rows: Vec<_> = ungrouped_rows
        .into_rows()
        .map(|row| row.unboxed().into_rows())
        .collect();
    let mut ungrouped = Vec::with_capacity(indices.element_count() * original.row_len());
    let depth = indices.rank().saturating_sub(1);
    for (i, &index) in indices.data.iter().enumerate() {
        let original_row = original.depth_row(depth, i);
        if index >= 0 {
            ungrouped.push(ungrouped_rows[index as usize].next().ok_or_else(|| {
                env.error("A group's length was modified between grouping and ungrouping")
            })?);
        } else {
            ungrouped.push(original_row);
        }
    }
    if ungrouped_rows.iter_mut().any(|row| row.next().is_some()) {
        return Err(env.error("A group's length was modified between grouping and ungrouping"));
    }
    let mut val = Value::from_row_values(ungrouped, env)?;
    val.shape.remove(0);
    for &dim in indices.shape.iter().rev() {
        val.shape.insert(0, dim);
    }
    val.validate();
    env.push(val);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn collapse_groups<I, T: ScalarNum>(
    prim: Primitive,
    f: SigNode,
    get_groups: impl Fn(Value, &Array<T>) -> I,
    firsts: impl Fn(Value, &[T], &Uiua) -> UiuaResult<Value>,
    lasts: impl Fn(Value, &[T], &Uiua) -> UiuaResult<Value>,
    lens: impl Fn(&[T]) -> Array<f64>,
    indices_error: &'static str,
    env: &mut Uiua,
) -> UiuaResult
where
    I: IntoIterator<Item = Value>,
    I::IntoIter: ExactSizeIterator,
{
    let sig = f.sig;
    let indices = env.pop(1)?.as_number_array(env, indices_error)?;
    let values: Vec<Value> = (0..sig.args().max(1))
        .map(|i| env.pop(i + 2))
        .collect::<UiuaResult<_>>()?;

    if indices.shape == [0]
        && push_empty_rows_value(&f, &values, false, &mut Default::default(), env)
    {
        return Ok(());
    }

    for xs in &values {
        if !xs.shape.starts_with(&indices.shape) {
            return Err(env.error(format!(
                "Cannot {} array of shape {} with indices of shape {}",
                prim.format(),
                xs.shape,
                indices.shape
            )));
        }
    }

    // Optimizations
    if indices.rank() == 1 && values.len() == 1 {
        use Node::*;
        use Primitive::*;
        match &f.node {
            Prim(First, _) => {
                let xs = values.into_iter().next().unwrap();
                let val = firsts(xs, &indices.data, env)?;
                env.push(val);
                return Ok(());
            }
            Prim(Last, _) => {
                let xs = values.into_iter().next().unwrap();
                let val = lasts(xs, &indices.data, env)?;
                env.push(val);
                return Ok(());
            }
            Prim(Len, _) => {
                let xs = values.into_iter().next().unwrap();
                if indices.row_count() != xs.row_count() {
                    return Err(env.error(format!(
                        "Cannot {} array of shape {} with indices of shape {}",
                        prim.format(),
                        xs.shape,
                        indices.shape
                    )));
                }
                env.push(lens(&indices.data));
                return Ok(());
            }
            _ => {}
        }
    }

    let mut is_scalar = false;
    let mut group_count = 0;
    let mut groups: Vec<_> = values
        .into_iter()
        .map(|xs| {
            let mut empty_shape = xs.shape.clone();
            is_scalar |= empty_shape.is_empty();
            *empty_shape.row_count_mut() = 0;
            let groups = get_groups(xs, &indices).into_iter();
            group_count = groups.size_hint().0;
            groups.map(move |mut group| {
                if group.row_count() == 0 {
                    group.shape.clone_from(&empty_shape);
                    group.validate();
                }
                group
            })
        })
        .collect();
    let mut rows = multi_output(sig.outputs(), Vec::with_capacity(groups.len()));
    env.without_fill(|env| -> UiuaResult {
        for _ in 0..group_count {
            for group in groups.iter_mut().rev() {
                env.push(group.next().unwrap());
            }
            env.exec(f.clone())?;
            for i in 0..sig.outputs() {
                let value = env.pop(|| format!("{}'s function result", prim.format()))?;
                rows[i].push(value);
            }
            if sig.args() == 0 {
                env.pop("excess value")?;
            }
        }
        Ok(())
    })?;
    for rows in rows.into_iter().rev() {
        let mut val = Value::from_row_values(rows, env)?;
        if is_scalar {
            val.undo_fix();
        }
        env.push(val);
    }
    Ok(())
}

pub fn un_group(f: SigNode, env: &mut Uiua) -> UiuaResult {
    if f.sig != (1, 1) {
        return Err(env.error(format!(
            "{}{}'s function signature must be |1, but it is {}",
            Primitive::Un.format(),
            Primitive::Group.format(),
            f.sig
        )));
    }
    let x = env.pop(1)?;
    let mut indices = EcoVec::with_capacity(x.row_count());
    let mut ungrouped = Vec::with_capacity(x.row_count());
    for (i, row) in x.into_rows().enumerate() {
        env.push(row);
        env.exec(f.clone())?;
        let val = env.pop("ungrouped value")?;
        let count = val.row_count();
        for _ in 0..count {
            indices.push(i as f64);
        }
        ungrouped.extend(val.into_rows());
    }
    env.push(Value::from_row_values(ungrouped, env)?);
    env.push(Array::from(indices));
    Ok(())
}

pub fn un_partition(f: SigNode, env: &mut Uiua) -> UiuaResult {
    if f.sig != (1, 1) {
        return Err(env.error(format!(
            "{}{}'s function signature must be |1, but it is {}",
            Primitive::Un.format(),
            Primitive::Partition.format(),
            f.sig
        )));
    }
    let x = env.pop(1)?;
    let mut indices = EcoVec::with_capacity(x.row_count());
    let mut unpartitioned = Vec::with_capacity(x.row_count());
    if let Some(fill) = env.value_fill().map(|fv| fv.value.clone()) {
        for (i, row) in x.into_rows().enumerate() {
            env.push(row);
            env.exec(f.clone())?;
            let val = env.pop("unpartitioned value")?;
            let count = val.row_count();
            if i > 0 {
                if fill.rank() == val.rank() {
                    for row in fill.rows() {
                        indices.push(0.0);
                        unpartitioned.push(row.clone());
                    }
                } else {
                    indices.push(0.0);
                    unpartitioned.push(fill.clone());
                }
            }
            for _ in 0..count {
                indices.push(1.0);
            }
            unpartitioned.extend(val.into_rows());
        }
    } else {
        for (i, row) in x.into_rows().enumerate() {
            env.push(row);
            env.exec(f.clone())?;
            let val = env.pop("unpartitioned value")?;
            let count = val.row_count();
            for _ in 0..count {
                indices.push((i + 1) as f64);
            }
            unpartitioned.extend(val.into_rows());
        }
    }
    env.push(Value::from_row_values(unpartitioned, env)?);
    env.push(Array::from(indices));
    Ok(())
}
