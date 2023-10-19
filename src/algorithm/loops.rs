//! Algorithms for looping modifiers

use crate::{
    array::{Array, ArrayValue},
    run::ArrayArg,
    value::Value,
    Uiua, UiuaResult,
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

    if n.is_infinite() {
        let f = if n < 0.0 { f.invert(env)?.into() } else { f };
        loop {
            if env.call_catch_break(f.clone())? {
                break;
            }
        }
    } else {
        if n.fract().abs() > f64::EPSILON {
            return Err(env.error("Repetitions must be a single integer or infinity"));
        };
        let f = if n < 0.0 { f.invert(env)?.into() } else { f };
        for _ in 0..n.abs() as usize {
            if env.call_catch_break(f.clone())? {
                return Ok(());
            }
        }
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

impl Value {
    pub fn partition_groups(&self, markers: &[isize], env: &Uiua) -> UiuaResult<Vec<Self>> {
        Ok(match self {
            Value::Num(arr) => arr
                .partition_groups(markers, env)?
                .map(Into::into)
                .collect(),
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
    pub fn partition_groups(
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
    pub fn group_groups(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Vec<Self>> {
        Ok(match self {
            Value::Num(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Byte(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Char(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
            Value::Box(arr) => arr.group_groups(indices, env)?.map(Into::into).collect(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn group_groups(
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
            let indices = env.pop(ArrayArg(1))?;
            let indices = indices.as_indices(env, indices_error)?;
            let values = env.pop(ArrayArg(2))?;
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
            let mut acc = env.pop(ArrayArg(1))?;
            let indices = env.pop(ArrayArg(2))?;
            let indices = indices.as_indices(env, indices_error)?;
            let values = env.pop(ArrayArg(3))?;
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
