//! Algorithms for looping modifiers

use std::hash::{Hash, Hasher};

use crate::{
    array::{Array, ArrayValue},
    value::Value,
    ExactDoubleIterator, Uiua, UiuaResult,
};

pub(crate) fn rank_to_depth(declared_rank: Option<isize>, array_rank: usize) -> usize {
    let declared_rank = declared_rank.unwrap_or(array_rank as isize);
    array_rank
        - if declared_rank < 0 {
            (array_rank as isize + declared_rank).max(0) as usize
        } else {
            (declared_rank as usize).min(array_rank)
        }
}

pub fn flip<A, B, C>(f: impl Fn(A, B) -> C) -> impl Fn(B, A) -> C {
    move |b, a| f(a, b)
}

pub(crate) fn rank_list(name: &str, env: &mut Uiua) -> UiuaResult<Vec<Option<isize>>> {
    let ns = env.pop_function()?;
    let sig = ns.signature();
    if sig.outputs != 1 {
        return Err(env.error(format!(
            "{name}'s rank list function must return 1 value, \
            but its signature is {sig}"
        )));
    }
    match sig.args {
        0 => {}
        1 => env.push(Array::<f64>::default()),
        _ => {
            return Err(env.error(format!(
                "{name}'s rank list function must take 0 or 1 arguments, \
                but its signature is {sig}"
            )))
        }
    }
    env.call(ns)?;
    env.pop("rank list")?.as_rank_list(env, "")
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
            f.invert(INVERSE_CONTEXT, env)?.into()
        } else {
            f
        };
        loop {
            if env.call_catch_break(f.clone())? {
                break;
            }
        }
    } else {
        if n.fract().abs() > f64::EPSILON {
            return Err(env.error("Repetitions must be a single integer or infinity"));
        };
        let f = if n < 0.0 {
            f.invert(INVERSE_CONTEXT, env)?.into()
        } else {
            f
        };
        for _ in 0..n.abs() as usize {
            if env.call_catch_break(f.clone())? {
                return Ok(());
            }
        }
    }
    Ok(())
}

pub fn fixed(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop_function()?;
    let sig = f.signature();
    if sig.args > sig.outputs {
        return Err(env.error(format!(
            "Fixed's function must return at least as many values as it takes arguments, \
            but its signature is {sig}"
        )));
    }
    match sig.outputs - sig.args {
        0 => {
            let mut args = Vec::with_capacity(sig.args);
            for i in 0..sig.args {
                args.push(env.pop(i + 1)?);
            }
            let mut identity = hash(&args);
            loop {
                for arg in args.drain(..).rev() {
                    env.push(arg);
                }
                env.call_error_on_break(f.clone(), "break is not allowed in fixed")?;
                for _ in 0..sig.args {
                    args.push(env.pop("fixed's function result")?);
                }
                let new_identity = hash(&args);
                if new_identity == identity {
                    break;
                }
                identity = new_identity;
            }
            for arg in args.drain(..).rev() {
                env.push(arg);
            }
        }
        1 => loop {
            env.call_error_on_break(f.clone(), "break is not allowed in fixed")?;
            let should_continue = env
                .pop("fixed's continue condition")?
                .as_bool(env, "Fixed's continue condition must be a boolean")?;
            if !should_continue {
                break;
            }
        },
        _ => {
            return Err(env.error(format!(
                "Fixed's function may return at most 1 more value than it takes arguments, \
                but its signature is {sig}"
            )))
        }
    }
    Ok(())
}

fn hash<T: Hash>(val: T) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut hasher = DefaultHasher::new();
    val.hash(&mut hasher);
    hasher.finish()
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
            "Cannot undo partition with on function with signature {sig}"
        )));
    }
    let partitioned = env.pop(1)?;
    // Untransform rows
    let mut untransformed = Vec::with_capacity(partitioned.row_count());
    for row in partitioned.into_rows().rev() {
        env.push(row);
        env.call_error_on_break(f.clone(), "break is not allowed in unpartition")?;
        untransformed.push(env.pop("unpartitioned row")?);
    }
    let original = env.pop_temp_under()?;
    let markers = env
        .pop_temp_under()?
        .as_ints(env, "Partition markers must be a list of integers")?;

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
            "Cannot undo partition because the paritioned array \
            originally had {} rows, but now it has {}",
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
            "Cannot undo group with on function with signature {sig}"
        )));
    }
    let grouped = env.pop(1)?;

    // Untransform rows
    let mut ungrouped_rows: Vec<Box<dyn ExactDoubleIterator<Item = Value>>> =
        Vec::with_capacity(grouped.row_count());
    for mut row in grouped.into_rows().rev() {
        env.push(row);
        env.call_error_on_break(f.clone(), "break is not allowed in ungroup")?;
        row = env.pop("ungrouped row")?;
        ungrouped_rows.push(row.into_rows());
    }
    ungrouped_rows.reverse();
    let original = env.pop_temp_under()?;
    let indices = env
        .pop_temp_under()?
        .as_ints(env, "Group indices must be a list of integers")?;

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
    match sig.args {
        0 | 1 => {
            let indices = env.pop(1)?;
            let indices = indices.as_ints(env, indices_error)?;
            let values = env.pop(2)?;
            let groups = get_groups(&values, &indices, env)?;
            let mut rows = Vec::with_capacity(groups.len());
            for group in groups {
                env.push(group);
                env.call_error_on_break_with(f.clone(), || {
                    format!("break is not allowed in {name}")
                })?;
                rows.push(env.pop(|| format!("{name}'s function result"))?);
            }
            let res = Value::from_row_values(rows, env)?;
            env.push(res);
        }
        2 => {
            let mut acc = env.pop(1)?;
            let indices = env.pop(2)?;
            let indices = indices.as_ints(env, indices_error)?;
            let values = env.pop(3)?;
            let groups = get_groups(&values, &indices, env)?;
            for row in groups {
                env.push(row);
                env.push(acc);
                if env.call_catch_break(f.clone())? {
                    return Ok(());
                }
                acc = env.pop("reduced function result")?;
            }
            env.push(acc);
        }
        args => {
            return Err(env.error(format!(
                "Cannot {name} with a function that takes {args} arguments"
            )))
        }
    }
    Ok(())
}
