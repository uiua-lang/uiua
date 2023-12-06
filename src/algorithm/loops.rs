//! Algorithms for looping modifiers

use crate::{
    array::{Array, ArrayValue},
    value::Value,
    ExactDoubleIterator, Primitive, Signature, Uiua, UiuaResult,
};

use super::multi_output;

pub fn flip<A, B, C>(f: impl Fn(A, B) -> C) -> impl Fn(B, A) -> C {
    move |b, a| f(a, b)
}

pub fn repeat(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let n = env
        .pop(2)?
        .as_num(env, "Repetitions must be a single integer or infinity")?;

    const INVERSE_CONTEXT: &str = "; repeat with a negative number repeats the inverse";
    if n.is_infinite() {
        let f = if n < 0.0 {
            f.invert(INVERSE_CONTEXT, env)?
        } else {
            f
        };
        loop {
            env.call(f.clone())?;
        }
    } else {
        if n.fract().abs() > f64::EPSILON {
            return Err(env.error("Repetitions must be a single integer or infinity"));
        };
        let f = if n < 0.0 {
            f.invert(INVERSE_CONTEXT, env)?
        } else {
            f
        };
        for _ in 0..n.abs() as usize {
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
        for value in env.clone_stack_top(copy_count) {
            env.push(value);
        }
        env.call(g.clone())?;
        let cond = env
            .pop("do condition")?
            .as_bool(env, "Do condition must be a boolean")?;
        if !cond {
            break;
        }
        env.call(f.clone())?;
    }
    Ok(())
}

pub fn partition(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups(
        "partition",
        Value::partition_groups,
        "Partition indices must be a list of integers",
        env,
    )
}

pub fn unpartition(env: &mut Uiua) -> UiuaResult {
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
    let markers = env
        .pop(2)?
        .as_ints(env, "⊜ partition markers must be a list of integers")?;
    let original = env.pop(3)?;
    // Untransform rows
    let mut untransformed = Vec::with_capacity(partitioned.row_count());
    for row in partitioned.into_rows() {
        env.push(row);
        env.call(f.clone())?;
        untransformed.push(env.pop("unpartitioned row")?);
    }

    // Count partition markers
    let mut marker_partitions: Vec<(isize, usize)> = Vec::new();
    let mut markers = markers.into_iter();
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
    if positive_partitions != untransformed.len() {
        return Err(env.error(format!(
            "Cannot undo {} because the partitioned array \
            originally had {} rows, but now it has {}",
            Primitive::Partition.format(),
            positive_partitions,
            untransformed.len()
        )));
    }

    // Unpartition
    let mut untransformed_rows = untransformed.into_iter();
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
    Ok(())
}

pub fn ungroup(env: &mut Uiua) -> UiuaResult {
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
    let indices = env
        .pop(2)?
        .as_ints(env, "⊕ group indices must be a list of integers")?;
    let original = env.pop(3)?;

    // Untransform rows
    let mut ungrouped_rows: Vec<Box<dyn ExactDoubleIterator<Item = Value>>> =
        Vec::with_capacity(grouped.row_count());
    for mut row in grouped.into_rows().rev() {
        env.push(row);
        env.call(f.clone())?;
        row = env.pop("ungrouped row")?;
        ungrouped_rows.push(row.into_rows());
    }
    ungrouped_rows.reverse();

    if indices
        .iter()
        .any(|&index| index >= 0 && index as usize >= ungrouped_rows.len())
    {
        return Err(env.error(format!(
            "Cannot undo {} because the grouped array's \
            length changed from {} to {}",
            Primitive::Group.format(),
            indices.len(),
            ungrouped_rows.len(),
        )));
    }

    // Ungroup
    let mut ungrouped = Vec::with_capacity(indices.len() * original.row_len());
    for (i, index) in indices.into_iter().enumerate() {
        if index >= 0 {
            ungrouped.push(ungrouped_rows[index as usize].next().ok_or_else(|| {
                env.error("A group's length was modified between grouping and ungrouping")
            })?);
        } else {
            ungrouped.push(original.row(i));
        }
    }
    env.push(Value::from_row_values(ungrouped, env)?);
    Ok(())
}

impl Value {
    fn partition_groups(&self, markers: &[isize], env: &Uiua) -> UiuaResult<Vec<Self>> {
        Ok(match self {
            Value::Num(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
            Value::Complex(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
            Value::Char(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
            Value::Box(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn partition_groups(
        &self,
        markers: &[isize],
        env: &Uiua,
    ) -> UiuaResult<impl Iterator<Item = Self>> {
        if markers.len() != self.row_count() {
            return Err(env.error(format!(
                "Cannot partition array of shape {} with markers of length {}",
                self.format_shape(),
                markers.len()
            )));
        }
        let mut groups = Vec::new();
        let mut last_marker = isize::MAX;
        for (row, &marker) in self.rows().zip(markers) {
            if marker > 0 {
                if marker != last_marker {
                    groups.push(Vec::new());
                }
                groups.last_mut().unwrap().push(row);
            }
            last_marker = marker;
        }
        Ok(groups.into_iter().map(Array::from_row_arrays_infallible))
    }
}

pub fn group(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    collapse_groups(
        "group",
        Value::group_groups,
        "Group indices must be a list of integers",
        env,
    )
}

impl Value {
    fn group_groups(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Vec<Self>> {
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
        &self,
        indices: &[isize],
        env: &Uiua,
    ) -> UiuaResult<impl Iterator<Item = Self>> {
        if indices.len() != self.row_count() {
            return Err(env.error(format!(
                "Cannot group array of shape {} with indices of length {}",
                self.format_shape(),
                indices.len()
            )));
        }
        let Some(&max_index) = indices.iter().max() else {
            return Ok(Vec::<Vec<Self>>::new()
                .into_iter()
                .map(Array::from_row_arrays_infallible));
        };
        let mut groups: Vec<Vec<Self>> = vec![Vec::new(); max_index.max(0) as usize + 1];
        for (r, &g) in indices.iter().enumerate() {
            if g >= 0 && r < self.row_count() {
                groups[g as usize].push(self.row(r));
            }
        }
        Ok(groups.into_iter().map(Array::from_row_arrays_infallible))
    }
}

fn collapse_groups(
    name: &str,
    get_groups: impl Fn(&Value, &[isize], &Uiua) -> UiuaResult<Vec<Value>>,
    indices_error: &'static str,
    env: &mut Uiua,
) -> UiuaResult {
    let f = env.pop_function()?;
    let sig = f.signature();
    match (sig.args, sig.outputs) {
        (0 | 1, n) => {
            let indices = env.pop(1)?;
            let indices = indices.as_ints(env, indices_error)?;
            let values = env.pop(2)?;
            let groups = get_groups(&values, &indices, env)?;
            let mut rows = multi_output(n, Vec::with_capacity(groups.len()));
            for group in groups {
                env.push(group);
                env.call(f.clone())?;
                for i in 0..n {
                    rows[i].push(env.pop(|| format!("{name}'s function result"))?);
                }
            }
            for rows in rows.into_iter().rev() {
                env.push(Value::from_row_values(rows, env)?);
            }
        }
        (2, 1) => {
            let mut acc = env.pop(1)?;
            let indices = env.pop(2)?;
            let indices = indices.as_ints(env, indices_error)?;
            let values = env.pop(3)?;
            let groups = get_groups(&values, &indices, env)?;
            for row in groups {
                env.push(row);
                env.push(acc);
                env.call(f.clone())?;
                acc = env.pop("reduced function result")?;
            }
            env.push(acc);
        }
        _ => {
            return Err(env.error(format!(
                "Cannot {name} with a function with signature {sig}"
            )))
        }
    }
    Ok(())
}
