//! Algorithms for looping modifiers

use crate::{
    array::{Array, ArrayValue},
    value::Value,
    Boxed, Function, Primitive, Shape, Signature, Uiua, UiuaResult,
};

use super::multi_output;

pub fn flip<A, B, C>(f: impl Fn(A, B) -> C + Copy) -> impl Fn(B, A) -> C + Copy {
    move |b, a| f(a, b)
}

pub fn repeat(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let n = env.pop("repetition count")?;
    let n = match n {
        Value::Num(n) => n,
        #[cfg(feature = "bytes")]
        Value::Byte(n) => n.convert(),
        val => {
            return Err(env.error(format!(
                "Repetitions must be a scalar or list of \
                natural numbers or infinity, \
                but it is {}",
                val.type_name_plural()
            )))
        }
    };
    if n.rank() == 0 {
        // Scalar repeat
        repeat_impl(f, n.data[0], env)
    } else {
        // Non-scalar repeat
        let sig = f.signature();
        // Collect arguments
        let mut args_rows: Vec<_> = Vec::with_capacity(sig.args);
        for i in 0..sig.args {
            let arg = env.pop(i + 1)?;
            if !arg.shape().starts_with(n.shape()) {
                return Err(env.error(format!(
                    "The repetitions' shape {} is not compatible \
                    with the argument {}'s shape {}",
                    n.shape(),
                    i + 1,
                    arg.shape(),
                )));
            }
            let row_shape = Shape::from(&arg.shape()[n.rank()..]);
            args_rows.push(arg.into_row_shaped_slices(row_shape));
        }
        args_rows.reverse();
        let mut outputs = multi_output(sig.outputs, Vec::with_capacity(n.data.len()));
        // Repeate with each repetition count
        for n in n.data {
            for arg in args_rows.iter_mut().rev() {
                env.push(arg.next().unwrap());
            }
            repeat_impl(f.clone(), n, env)?;
            for i in 0..sig.outputs {
                outputs[i].push(env.pop("repeat's output")?);
            }
        }
        // Collect outputs
        for output in outputs.into_iter().rev() {
            let mut new_value = Value::from_row_values(output, env)?;
            let mut new_shape = n.shape.clone();
            new_shape.extend_from_slice(&new_value.shape()[1..]);
            *new_value.shape_mut() = new_shape;
            env.push(new_value);
        }
        Ok(())
    }
}

fn repeat_impl(f: Function, n: f64, env: &mut Uiua) -> UiuaResult {
    if n.is_infinite() {
        // Converging repeat
        let sig = f.signature();
        if sig.args == 0 {
            return Err(env.error(format!(
                "Converging {}'s function must have at least 1 argument",
                Primitive::Repeat.format()
            )));
        }
        if sig.args != sig.outputs {
            return Err(env.error(format!(
                "Converging {}'s function must have a net stack change of 0, \
                but its signature is {sig}",
                Primitive::Repeat.format()
            )));
        }
        let mut prev = env.pop(1)?;
        env.push(prev.clone());
        loop {
            env.call(f.clone())?;
            let next = env.pop("converging function result")?;
            let converged = next == prev;
            if converged {
                env.push(next);
                break;
            } else {
                env.push(next.clone());
                prev = next;
            }
        }
    } else {
        // Normal repeat
        if n < 0.0 || n.fract() != 0.0 {
            return Err(env.error("Repetitions must be a natural number or infinity"));
        }
        let n = n as usize;
        for _ in 0..n {
            env.call(f.clone())?;
        }
    }
    Ok(())
}

pub fn do_(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let g = env.pop_function()?;
    let f_sig = f.signature();
    let g_sig = g.signature();
    if g_sig.outputs < 1 {
        return Err(env.error(format!(
            "Do's condition function must return at least 1 value, \
            but its signature is {g_sig}"
        )));
    }
    let copy_count = g_sig.args.saturating_sub(g_sig.outputs - 1);
    let g_sub_sig = Signature::new(g_sig.args, g_sig.outputs + copy_count - 1);
    let comp_sig = f_sig.compose(g_sub_sig);
    if comp_sig.args != comp_sig.outputs {
        return Err(env.error(format!(
            "Do's functions must have a net stack change of 0, \
            but the composed signature of {f_sig} and {g_sig}, \
            minus the condition, is {comp_sig}"
        )));
    }
    loop {
        if env.stack().len() < copy_count {
            // Pop until it fails
            for i in 0..copy_count {
                env.pop(i + 1)?;
            }
        }
        // Copy necessary condition args
        for _ in 0..copy_count {
            env.push(env.stack()[env.stack().len() - copy_count].clone());
        }
        // Call condition
        env.call(g.clone())?;
        // Break if condition is false
        let cond = env
            .pop("do condition")?
            .as_bool(env, "Do condition must be a boolean")?;
        if !cond {
            break;
        }
        // Call body
        env.call(f.clone())?;
    }
    Ok(())
}

pub fn partition(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups(
        Primitive::Partition,
        Value::partition_groups,
        "⊜ partition indices array must be a list of integers",
        "⊜ partition's function has signature |2.1, so it is the reducing form. \
        Its indices array must be a list of integers",
        env,
    )
}

impl Value {
    fn partition_groups(self, markers: Array<isize>, env: &Uiua) -> UiuaResult<Vec<Self>> {
        Ok(match self {
            Value::Num(arr) => (arr.partition_groups(markers, env)?.map(Into::into)).collect(),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => (arr.partition_groups(markers, env)?.map(Into::into)).collect(),
            Value::Complex(arr) => (arr.partition_groups(markers, env)?.map(Into::into)).collect(),
            Value::Char(arr) => (arr.partition_groups(markers, env)?.map(Into::into)).collect(),
            Value::Box(arr) => (arr.partition_groups(markers, env)?.map(Into::into)).collect(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn partition_groups(
        self,
        markers: Array<isize>,
        env: &Uiua,
    ) -> UiuaResult<impl Iterator<Item = Self>> {
        if !self.shape().starts_with(markers.shape()) {
            return Err(env.error(format!(
                "Cannot partition array of shape {} with markers of shape {}",
                self.shape(),
                markers.shape()
            )));
        }
        let mut groups = Vec::new();
        if markers.rank() == 1 {
            let mut last_marker = isize::MAX;
            for (row, marker) in self.into_rows().zip(markers.data) {
                if marker > 0 {
                    if marker != last_marker {
                        groups.push(Vec::new());
                    }
                    groups.last_mut().unwrap().push(row);
                }
                last_marker = marker;
            }
        } else {
            let row_shape: Shape = self.shape()[markers.rank()..].into();
            let indices = multi_partition_indices(markers);
            for (_, indices) in indices {
                let mut group = Vec::with_capacity(indices.len());
                for index in indices {
                    group.push(self.row_shaped_slice(index, row_shape.clone()));
                }
                groups.push(group);
            }
        }
        Ok(groups.into_iter().map(Array::from_row_arrays_infallible))
    }
}

fn multi_partition_indices(markers: Array<isize>) -> Vec<(isize, Vec<usize>)> {
    if markers.element_count() == 0 {
        return Vec::new();
    }
    let mut groups: Vec<(isize, Vec<Vec<usize>>)> = Vec::new();
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
                    let mut new_group = Vec::new();
                    for g in adjacent_groups.into_iter().rev() {
                        new_group.extend(groups.remove(g).1);
                    }
                    new_group.push(curr.clone());
                    groups.push((marker, new_group));
                }
            }
        }
        // Increment the current index
        for (i, c) in curr.iter_mut().enumerate().rev() {
            if *c < markers.shape()[i] - 1 {
                *c += 1;
                break;
            }
            *c = 0;
        }
    }
    let mut shape_muls: Vec<usize> = markers
        .shape()
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

pub fn undo_partition_part1(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
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
        env.call(f.clone())?;
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
        .as_integer_array(env, "⊜ partition markers must be an array of integers")?;
    let mut original = env.pop(3)?;
    if markers.rank() == 1 {
        // Count partition markers
        let mut marker_partitions: Vec<(isize, usize)> = Vec::new();
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
        let row_shape: Shape = original.shape()[markers.rank()..].into();
        let indices = multi_partition_indices(markers);
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

pub fn group(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups(
        Primitive::Group,
        Value::group_groups,
        "⊕ group indices array must be an array of integers",
        "⊕ group's function has signature |2.1, so it is the reducing form. \
        Its indices array must be a list of integers",
        env,
    )
}

impl Value {
    fn group_groups(self, indices: Array<isize>, env: &Uiua) -> UiuaResult<Vec<Self>> {
        Ok(match self {
            Value::Num(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Complex(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Char(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Box(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn group_groups(
        self,
        mut indices: Array<isize>,
        env: &Uiua,
    ) -> UiuaResult<impl Iterator<Item = Self>> {
        let mut target_groups = None;
        if indices.rank() == 1 && indices.row_count() == self.row_count() + 1 {
            let last = *indices.data.last().unwrap();
            if last < 0 {
                return Err(env.error("Cannot make a negative number of groups"));
            }
            target_groups = Some(last.unsigned_abs());
            indices.data.modify(|data| data.pop());
            indices.shape[0] -= 1;
        }
        if !self.shape().starts_with(indices.shape()) {
            return Err(env.error(format!(
                "Cannot {} array of shape {} with indices of shape {}",
                Primitive::Group.format(),
                self.shape(),
                indices.shape()
            )));
        }
        let target_groups = if let Some(target_groups) = target_groups {
            target_groups
        } else {
            let Some(&max_index) = indices.data.iter().max() else {
                return Ok(Vec::<Vec<Self>>::new()
                    .into_iter()
                    .map(Array::from_row_arrays_infallible));
            };
            max_index.max(0) as usize + 1
        };
        let mut groups: Vec<Vec<Self>> = vec![Vec::new(); target_groups];
        let row_shape = self.shape()[indices.rank()..].into();
        for (g, r) in (indices.data.into_iter()).zip(self.into_row_shaped_slices(row_shape)) {
            if g >= 0 && g < target_groups as isize {
                groups[g as usize].push(r);
            }
        }
        Ok(groups.into_iter().map(Array::from_row_arrays_infallible))
    }
}

pub fn undo_group_part1(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
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
        env.call(f.clone())?;
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

    if (indices.data.iter())
        .any(|&index| index >= 0 && index as usize >= ungrouped_rows.row_count())
    {
        return Err(env.error(format!(
            "Cannot undo {} because the grouped array's \
            length changed from {} to {}",
            Primitive::Group.format(),
            indices.element_count(),
            ungrouped_rows.row_count(),
        )));
    }

    // Ungroup
    let mut ungrouped_rows: Vec<_> = ungrouped_rows
        .into_rows()
        .map(|row| row.unboxed().into_rows())
        .collect();
    let mut ungrouped = Vec::with_capacity(indices.element_count() * original.row_len());
    for (i, &index) in indices.data.iter().enumerate() {
        if index >= 0 {
            ungrouped.push(ungrouped_rows[index as usize].next().ok_or_else(|| {
                env.error("A group's length was modified between grouping and ungrouping")
            })?);
        } else {
            ungrouped.push(original.row(i));
        }
    }
    let mut val = Value::from_row_values(ungrouped, env)?;
    val.shape_mut().remove(0);
    for &dim in indices.shape().iter().rev() {
        val.shape_mut().insert(0, dim);
    }
    env.push(val);
    Ok(())
}

fn collapse_groups(
    prim: Primitive,
    get_groups: impl Fn(Value, Array<isize>, &Uiua) -> UiuaResult<Vec<Value>>,
    agg_indices_error: &'static str,
    red_indices_error: &'static str,
    env: &mut Uiua,
) -> UiuaResult {
    let f = env.pop_function()?;
    let sig = f.signature();
    match (sig.args, sig.outputs) {
        (0 | 1, outputs) => {
            let indices = env.pop(1)?.as_integer_array(env, agg_indices_error)?;
            let values = env.pop(2)?;
            let groups = get_groups(values, indices, env)?;
            let mut rows = multi_output(outputs, Vec::with_capacity(groups.len()));
            env.without_fill(|env| -> UiuaResult {
                for group in groups {
                    env.push(group);
                    env.call(f.clone())?;
                    for i in 0..outputs.max(1) {
                        let value = env.pop(|| format!("{}'s function result", prim.format()))?;
                        if sig.args == 1 {
                            rows[i].push(value);
                        }
                    }
                }
                Ok(())
            })?;
            for rows in rows.into_iter().rev() {
                env.push(Value::from_row_values(rows, env)?);
            }
        }
        (2, 1) => {
            let indices = env.pop(1)?.as_integer_array(env, red_indices_error)?;
            let values = env.pop(2)?;
            let mut groups = get_groups(values, indices, env)?.into_iter();
            let mut acc = match env.value_fill().cloned() {
                Some(acc) => acc,
                None => groups.next().ok_or_else(|| {
                    env.error(format!(
                        "Cannot do aggregating {} with no groups",
                        prim.format()
                    ))
                })?,
            };
            env.without_fill(|env| -> UiuaResult {
                for row in groups {
                    env.push(row);
                    env.push(acc);
                    env.call(f.clone())?;
                    acc = env.pop("reduced function result")?;
                }
                env.push(acc);
                Ok(())
            })?;
        }
        _ => {
            return Err(env.error(format!(
                "Cannot {} with a function with signature {sig}",
                prim.format()
            )))
        }
    }
    Ok(())
}
