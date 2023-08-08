use std::ops::{Add, Div, Mul, Sub};

use crate::{
    algorithm::pervade::bin_pervade_generic,
    array::{Array, ArrayValue},
    cowslice::cowslice,
    primitive::Primitive,
    value::Value,
    Byte, Uiua, UiuaResult,
};

pub fn reduce(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;

    match (f.as_primitive(), xs) {
        (Some(prim), Value::Num(nums)) => env.push(match prim {
            Primitive::Add => nums.reduce(0.0, Add::add),
            Primitive::Mul => nums.reduce(1.0, Mul::mul),
            Primitive::Max => nums.reduce(f64::NEG_INFINITY, f64::max),
            Primitive::Min => nums.reduce(f64::INFINITY, f64::min),
            _ => return generic_fold(f, Value::Num(nums), None, env),
        }),
        (Some(prim), Value::Byte(bytes)) => env.push(match prim {
            Primitive::Add => bytes.reduce(0.0, |a, b| a + f64::from(b)),
            Primitive::Mul => bytes.reduce(1.0, |a, b| a * f64::from(b)),
            Primitive::Max => bytes.reduce(f64::NEG_INFINITY, |a, b| a.max(f64::from(b))),
            Primitive::Min => bytes.reduce(f64::INFINITY, |a, b| a.min(f64::from(b))),
            _ => return generic_fold(f, Value::Byte(bytes), None, env),
        }),
        (_, xs) => generic_fold(f, xs, None, env)?,
    }
    Ok(())
}

fn generic_fold(f: Value, xs: Value, init: Option<Value>, env: &mut Uiua) -> UiuaResult {
    let mut rows = xs.into_rows_rev();
    let mut acc = init
        .or_else(|| rows.next())
        .ok_or_else(|| env.error("Cannot reduce empty array"))?;
    for row in rows {
        env.push(acc);
        env.push(row);
        env.push(f.clone());
        if env.call_catch_break()? {
            return Ok(());
        }
        acc = env.pop("reduced function result")?;
    }
    env.push(acc);
    Ok(())
}

pub fn fold(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let acc = env.pop(2)?;
    let xs = env.pop(3)?;
    generic_fold(f, xs, Some(acc), env)
}

pub fn scan(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    if xs.rank() == 0 {
        return Err(env.error("Cannot scan rank 0 array"));
    }
    match (f.as_primitive(), xs) {
        (Some(prim), Value::Num(nums)) => {
            let arr = match prim {
                Primitive::Add => nums.scan(0.0, Add::add),
                Primitive::Mul => nums.scan(1.0, Mul::mul),
                Primitive::Max => nums.scan(f64::NEG_INFINITY, f64::max),
                Primitive::Min => nums.scan(f64::INFINITY, f64::min),
                _ => return generic_scan(f, Value::Num(nums), env),
            };
            env.push(arr);
            Ok(())
        }
        (_, xs) => generic_scan(f, xs, env),
    }
}

fn generic_scan(f: Value, xs: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() == 0 {
        env.push(xs.first_dim_zero());
        return Ok(());
    }
    let row_count = xs.row_count();
    let mut rows = xs.into_rows();
    let mut acc = rows.next().unwrap();
    let mut scanned = Vec::with_capacity(row_count);
    scanned.push(acc.clone());
    for row in rows {
        let start_height = env.stack_size();
        env.push(row);
        env.push(acc.clone());
        env.push(f.clone());
        let should_break = env.call_catch_break()?;
        let new_acc = env.pop("scanned function result")?;
        if should_break {
            env.truncate_stack(start_height);
            break;
        }
        acc = new_acc;
        scanned.push(acc.clone());
    }
    env.push(Value::from_row_values(scanned, env)?);
    Ok(())
}

pub fn each(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let mut new_values = Vec::with_capacity(xs.flat_len());
    let mut new_shape = xs.shape().to_vec();
    let mut values = xs.into_flat_values();
    while let Some(val) = values.next() {
        env.push(val);
        env.push(f.clone());
        let broke = env.call_catch_break()?;
        new_values.push(env.pop("each's function result")?);
        if broke {
            for val in values {
                new_values.push(val);
            }
            break;
        }
    }
    let mut eached = Value::from_row_values(new_values, env)?;
    new_shape.extend_from_slice(&eached.shape()[1..]);
    *eached.shape_mut() = new_shape;
    env.push(eached);
    Ok(())
}

pub fn zip(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let ys = env.pop(3)?;
    let xs_shape = xs.shape().to_vec();
    let ys_shape = ys.shape().to_vec();
    let xs_values: Vec<_> = xs.into_flat_values().collect();
    let ys_values: Vec<_> = ys.into_flat_values().collect();
    const BREAK_ERROR: &str = "break is not allowed in zip";
    let (mut shape, values) = bin_pervade_generic(
        &xs_shape,
        xs_values,
        &ys_shape,
        ys_values,
        env,
        |x, y, env| {
            env.push(y);
            env.push(x);
            env.push(f.clone());
            env.call_error_on_break(BREAK_ERROR)?;
            env.pop("zip's function result")
        },
    )?;
    let mut zipped = Value::from_row_values(values, env)?;
    shape.extend_from_slice(&zipped.shape()[1..]);
    *zipped.shape_mut() = shape;
    env.push(zipped);
    Ok(())
}

pub fn rows(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let mut new_rows = Vec::with_capacity(xs.row_count());
    let mut old_rows = xs.into_rows();
    while let Some(row) = old_rows.next() {
        env.push(row);
        env.push(f.clone());
        let broke = env.call_catch_break()?;
        new_rows.push(env.pop("rows' function result")?);
        if broke {
            for row in old_rows {
                new_rows.push(row);
            }
            break;
        }
    }
    let res = Value::from_row_values(new_rows, env)?;
    env.push(res);
    Ok(())
}

pub fn bridge(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let ys = env.pop(3)?;
    const BREAK_ERROR: &str = "break is not allowed in bridge";
    if xs.row_count() != ys.row_count() {
        return Err(env.error(format!(
            "Cannot bridge arrays with different number of rows {} and {}",
            xs.row_count(),
            ys.row_count()
        )));
    }
    let mut new_rows = Vec::with_capacity(xs.row_count());
    let x_rows = xs.into_rows();
    let y_rows = ys.into_rows();
    for (x, y) in x_rows.into_iter().zip(y_rows) {
        env.push(y);
        env.push(x);
        env.push(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_rows.push(env.pop("bridge's function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

pub fn distribute(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let y = env.pop(3)?;
    const BREAK_ERROR: &str = "break is not allowed in distribute";
    let mut new_elems = Vec::with_capacity(xs.flat_len());
    let mut new_shape = xs.shape().to_vec();
    for x in xs.into_flat_values() {
        env.push(y.clone());
        env.push(x);
        env.push(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_elems.push(env.pop("distribute's function result")?);
    }
    let mut values = Value::from_row_values(new_elems, env)?;
    new_shape.extend_from_slice(&values.shape()[1..]);
    *values.shape_mut() = new_shape;
    env.push(values);
    Ok(())
}

pub fn plow(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let y = env.pop(3)?;
    const BREAK_ERROR: &str = "break is not allowed in plow";
    let mut new_rows = Vec::with_capacity(xs.row_count());
    for x in xs.into_rows() {
        env.push(y.clone());
        env.push(x);
        env.push(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_rows.push(env.pop("plow's function result")?);
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

pub fn table(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let ys = env.pop(3)?;
    match (f.as_primitive(), xs, ys) {
        (Some(prim), Value::Num(xs), Value::Num(ys)) => env.push(match prim {
            Primitive::Add => xs.table(ys, Add::add),
            Primitive::Sub => xs.table(ys, Sub::sub),
            Primitive::Mul => xs.table(ys, Mul::mul),
            Primitive::Div => xs.table(ys, Div::div),
            Primitive::Min => xs.table(ys, f64::min),
            Primitive::Max => xs.table(ys, f64::max),
            Primitive::Join | Primitive::Couple => xs.table_join_or_couple(ys),
            _ => return generic_table(f, Value::Num(xs), Value::Num(ys), env),
        }),
        (Some(prim), Value::Byte(xs), Value::Byte(ys)) => match prim {
            Primitive::Add => env.push(xs.table(ys, |a, b| f64::from(a) + f64::from(b))),
            Primitive::Sub => env.push(xs.table(ys, |a, b| f64::from(a) - f64::from(b))),
            Primitive::Mul => env.push(xs.table(ys, |a, b| f64::from(a) * f64::from(b))),
            Primitive::Div => env.push(xs.table(ys, |a, b| f64::from(a) / f64::from(b))),
            Primitive::Min => env.push(xs.table(ys, |a, b| Byte(i16::min(a.0, b.0)))),
            Primitive::Max => env.push(xs.table(ys, |a, b| a.op(b, i16::min))),
            Primitive::Join | Primitive::Couple => env.push(xs.table_join_or_couple(ys)),
            _ => generic_table(f, Value::Byte(xs), Value::Byte(ys), env)?,
        },
        (_, xs, ys) => generic_table(f, xs, ys, env)?,
    }
    Ok(())
}

fn generic_table(f: Value, xs: Value, ys: Value, env: &mut Uiua) -> UiuaResult {
    const BREAK_ERROR: &str = "break is not allowed in table";
    let mut new_shape = xs.shape().to_vec();
    new_shape.extend_from_slice(ys.shape());
    let mut items = Vec::with_capacity(xs.flat_len() * ys.flat_len());
    let y_values = ys.into_flat_values().collect::<Vec<_>>();
    for x in xs.into_flat_values() {
        for y in y_values.iter().cloned() {
            env.push(y);
            env.push(x.clone());
            env.push(f.clone());
            env.call_error_on_break(BREAK_ERROR)?;
            let item = env.pop("tabled function result")?;
            item.validate_shape();
            items.push(item);
        }
    }
    let mut tabled = Value::from_row_values(items, env)?;
    new_shape.extend_from_slice(&tabled.shape()[1..]);
    *tabled.shape_mut() = new_shape;
    tabled.validate_shape();
    env.push(tabled);
    Ok(())
}

pub fn cross(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let xs = env.pop(2)?;
    let ys = env.pop(3)?;
    const BREAK_ERROR: &str = "break is not allowed in cross";
    let mut new_shape = vec![xs.len(), ys.len()];
    let mut items = Vec::with_capacity(xs.len() * ys.len());
    let y_rows = ys.into_rows().collect::<Vec<_>>();
    for x_row in xs.into_rows() {
        for y_row in y_rows.iter().cloned() {
            env.push(y_row);
            env.push(x_row.clone());
            env.push(f.clone());
            env.call_error_on_break(BREAK_ERROR)?;
            let item = env.pop("crossed function result")?;
            item.validate_shape();
            items.push(item);
        }
    }
    let mut crossed = Value::from_row_values(items, env)?;
    new_shape.extend_from_slice(&crossed.shape()[1..]);
    *crossed.shape_mut() = new_shape;
    crossed.validate_shape();
    env.push(crossed);
    Ok(())
}

pub fn repeat(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let f = env.pop(1)?;
    let n = env.pop(2)?.as_num(
        env,
        "Repetitions must be a single natural number or infinity",
    )?;
    if n == f64::INFINITY {
        loop {
            env.push(f.clone());
            if env.call_catch_break()? {
                break;
            }
        }
    } else {
        if n.fract().abs() > f64::EPSILON {
            return Err(env.error("Repetitions must be a single natural number or infinity"));
        };
        for _ in 0..n as u64 {
            env.push(f.clone());
            if env.call_catch_break()? {
                return Ok(());
            }
        }
    }
    Ok(())
}

impl<T: ArrayValue> Array<T> {
    pub fn reduce<R: ArrayValue>(mut self, identity: R, f: impl Fn(R, T) -> R) -> Array<R> {
        match self.shape.len() {
            0 => (
                vec![],
                vec![f(identity, self.data.into_iter().next().unwrap())],
            )
                .into(),
            1 => self.data.into_iter().rev().fold(identity, f).into(),
            _ => {
                let row_len: usize = self.row_len();
                if self.row_count() == 0 {
                    self.shape.remove(0);
                    let data = cowslice![identity; row_len];
                    return (self.shape, data).into();
                }
                let mut new_data = vec![identity; row_len];
                for i in 0..self.row_count() {
                    let start = i * row_len;
                    for j in 0..row_len {
                        new_data[j] = f(new_data[j].clone(), self.data[start + j].clone());
                    }
                }
                self.shape.remove(0);
                (self.shape, new_data).into()
            }
        }
    }
    fn scan(mut self, identity: T, f: impl Fn(T, T) -> T) -> Self {
        match self.shape.len() {
            0 => unreachable!("scan_nums called on unit array, should have been guarded against"),
            1 => {
                let mut acc = identity;
                for val in self.data.iter_mut() {
                    acc = f(val.clone(), acc);
                    *val = acc.clone();
                }
                self
            }
            _ => {
                let row_len: usize = self.row_len();
                if self.row_count() == 0 {
                    return self;
                }
                let shape = self.shape.clone();
                let mut new_data = Vec::with_capacity(self.data.len());
                let mut rows = self.into_rows();
                new_data.extend(rows.next().unwrap().data);
                for row in rows {
                    let start = new_data.len() - row_len;
                    for (i, r) in row.data.into_iter().enumerate() {
                        new_data.push(f(new_data[start + i].clone(), r));
                    }
                }
                (shape, new_data).into()
            }
        }
    }
    fn table<U: ArrayValue, R: ArrayValue>(
        self,
        other: Array<U>,
        f: impl Fn(U, T) -> R,
    ) -> Array<R> {
        let mut new_data = Vec::with_capacity(self.data.len() * other.data.len());
        for x in self.data {
            for y in other.data.iter().cloned() {
                new_data.push(f(y, x.clone()));
            }
        }
        let mut new_shape = self.shape;
        new_shape.extend_from_slice(&other.shape);
        (new_shape, new_data).into()
    }
    fn table_join_or_couple(self, other: Self) -> Self {
        let mut new_data = Vec::with_capacity(self.data.len() * other.data.len() * 2);
        for x in self.data {
            for y in other.data.iter().cloned() {
                new_data.push(x.clone());
                new_data.push(y);
            }
        }
        let mut new_shape = self.shape;
        new_shape.extend_from_slice(&other.shape);
        new_shape.push(2);
        (new_shape, new_data).into()
    }
}

pub fn level(env: &mut Uiua) -> UiuaResult {
    crate::profile_function!();
    let n = env.pop(1)?.as_int(env, "Rank must be a single integer")?;
    let f = env.pop(2)?;
    let xs = env.pop(3)?;
    if xs.rank() == 0 {
        env.push(xs);
        return Ok(());
    }
    let irank = xs.rank() as isize;
    let n = ((-n % irank + irank) % irank) as usize;
    let res = level_recursive(f, xs, n, env)?;
    env.push(res);
    Ok(())
}

fn level_recursive(f: Value, value: Value, n: usize, env: &mut Uiua) -> UiuaResult<Value> {
    if n == 0 {
        env.push(value);
        env.push(f);
        env.call_error_on_break("break is not allowed in rank")?;
        Ok(env.pop("rank's function result")?)
    } else {
        let mut rows = Vec::with_capacity(value.row_count());
        for row in value.into_rows() {
            rows.push(level_recursive(f.clone(), row, n - 1, env)?);
        }
        Value::from_row_values(rows, env)
    }
}
