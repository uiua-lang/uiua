use std::{
    ops::{Add, Mul},
    rc::Rc,
};

use crate::{
    algorithm::pervade::bin_pervade_generic,
    array::{Array, ArrayValue},
    primitive::Primitive,
    rc_take,
    value::Value,
    Uiua, UiuaResult,
};

pub fn reduce(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    match (f.as_primitive(), xs) {
        (Some(prim), Value::Num(nums)) => {
            let arr = match prim {
                Primitive::Add => nums.reduce(0.0, Add::add),
                Primitive::Mul => nums.reduce(1.0, Mul::mul),
                Primitive::Max => nums.reduce(f64::NEG_INFINITY, f64::max),
                Primitive::Min => nums.reduce(f64::INFINITY, f64::min),
                _ => return generic_fold(f, Value::Num(nums), None, env),
            };
            env.push(arr);
            Ok(())
        }
        (_, xs) => generic_fold(f, xs, None, env),
    }
}

fn generic_fold(f: Rc<Value>, xs: Value, init: Option<Rc<Value>>, env: &mut Uiua) -> UiuaResult {
    let mut rows = xs.into_rows_rev();
    let mut acc = init
        .or_else(|| rows.next().map(Rc::new))
        .ok_or_else(|| env.error("Cannot reduce empty array"))?;
    for row in rows {
        env.push_ref(acc);
        env.push(row);
        env.push_ref(f.clone());
        if env.call_catch_break()? {
            return Ok(());
        }
        acc = env.pop("reduced function result")?;
    }
    env.push_ref(acc);
    Ok(())
}

pub fn fold(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let acc = env.pop(2)?;
    let xs = rc_take(env.pop(3)?);
    generic_fold(f, xs, Some(acc), env)
}

pub fn scan(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
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

fn generic_scan(f: Rc<Value>, xs: Value, env: &mut Uiua) -> UiuaResult {
    if xs.row_count() == 0 {
        env.push(xs.empty_row());
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
        env.push_ref(f.clone());
        let should_break = env.call_catch_break()?;
        let new_acc = rc_take(env.pop("scanned function result")?);
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
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    const BREAK_ERROR: &str = "break is not allowed in each";
    let mut new_values = Vec::with_capacity(xs.flat_len());
    let mut new_shape = xs.shape().to_vec();
    let values = xs.into_flat_values();
    for val in values {
        env.push(val);
        env.push_ref(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_values.push(rc_take(env.pop("each's function result")?));
    }
    let mut eached = Value::from_row_values(new_values, env)?;
    new_shape.extend_from_slice(&eached.shape()[1..]);
    *eached.shape_mut() = new_shape;
    env.push(eached);
    Ok(())
}

pub fn zip(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    let ys = rc_take(env.pop(3)?);
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
            env.push_ref(f.clone());
            env.call_error_on_break(BREAK_ERROR)?;
            env.pop("zip's function result").map(rc_take)
        },
    )?;
    let mut zipped = Value::from_row_values(values, env)?;
    shape.extend_from_slice(&zipped.shape()[1..]);
    *zipped.shape_mut() = shape;
    env.push(zipped);
    Ok(())
}

pub fn rows(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    const BREAK_ERROR: &str = "break is not allowed in rows";
    let mut new_rows = Vec::with_capacity(xs.row_count());
    for row in xs.into_rows() {
        env.push(row);
        env.push_ref(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_rows.push(rc_take(env.pop("rows' function result")?));
    }
    let res = Value::from_row_values(new_rows, env)?;
    env.push(res);
    Ok(())
}

pub fn bridge(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    let ys = rc_take(env.pop(3)?);
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
        env.push_ref(f.clone());
        env.call_error_on_break(BREAK_ERROR)?;
        new_rows.push(rc_take(env.pop("bridge's function result")?));
    }
    env.push(Value::from_row_values(new_rows, env)?);
    Ok(())
}

pub fn table(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let xs = rc_take(env.pop(2)?);
    let ys = rc_take(env.pop(3)?);
    const BREAK_ERROR: &str = "break is not allowed in table";
    let mut new_shape = xs.shape().to_vec();
    new_shape.extend_from_slice(ys.shape());
    let mut items = Vec::with_capacity(xs.flat_len() * ys.flat_len());
    let y_values = ys.into_flat_values().collect::<Vec<_>>();
    for x in xs.into_flat_values() {
        for y in y_values.iter().cloned() {
            env.push(y);
            env.push(x.clone());
            env.push_ref(f.clone());
            env.call_error_on_break(BREAK_ERROR)?;
            let item = rc_take(env.pop("tabled function result")?);
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

pub fn repeat(env: &mut Uiua) -> UiuaResult {
    let f = env.pop(1)?;
    let mut acc = env.pop(2)?;
    let n = env.pop(3)?.as_num(
        env,
        "Repetitions must be a single natural number or infinity",
    )?;
    if n == f64::INFINITY {
        loop {
            env.push_ref(acc);
            env.push_ref(f.clone());
            if env.call_catch_break()? {
                break;
            }
            acc = env.pop("repeated function result")?;
        }
    } else {
        if n.fract().abs() > f64::EPSILON {
            return Err(env.error("Repetitions must be a single natural number or infinity"));
        };
        for _ in 0..n as u64 {
            env.push_ref(acc);
            env.push_ref(f.clone());
            if env.call_catch_break()? {
                return Ok(());
            }
            acc = env.pop("repeated function result")?;
        }
        env.push_ref(acc);
    }
    Ok(())
}

impl<T: ArrayValue> Array<T> {
    pub fn reduce(mut self, identity: T, f: impl Fn(T, T) -> T) -> Self {
        match self.shape.len() {
            0 => self,
            1 => self
                .data
                .iter()
                .cloned()
                .reduce(f)
                .unwrap_or(identity)
                .into(),
            _ => {
                let row_len: usize = self.row_len();
                if self.row_count() == 0 {
                    self.shape.remove(0);
                    self.data = vec![identity; row_len];
                    return self;
                }
                for i in 1..self.row_count() {
                    let start = i * row_len;
                    for j in 0..row_len {
                        self.data[j] = f(self.data[j].clone(), self.data[start + j].clone());
                    }
                }
                self.data.truncate(row_len);
                self.shape.remove(0);
                self
            }
        }
    }
    fn scan(self, identity: T, f: impl Fn(T, T) -> T) -> Self {
        match self.shape.len() {
            0 => unreachable!("scan_nums called on unit array, should have been guarded against"),
            1 => self
                .data
                .iter()
                .scan(identity, |acc, n| {
                    *acc = f(acc.clone(), n.clone());
                    Some(acc.clone())
                })
                .collect(),
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
                Array::new(shape, new_data)
            }
        }
    }
}

pub fn rank(env: &mut Uiua) -> UiuaResult {
    let n = env.pop(1)?.as_int(env, "Rank must be a single integer")?;
    let f = env.pop(2)?;
    let xs = rc_take(env.pop(3)?);
    if xs.rank() == 0 {
        env.push(xs);
        return Ok(());
    }
    let irank = xs.rank() as isize;
    let n = ((-n % irank + irank) % irank) as usize;
    let res = rank_recursive(f, xs, n, env)?;
    env.push(res);
    Ok(())
}

fn rank_recursive(f: Rc<Value>, value: Value, n: usize, env: &mut Uiua) -> UiuaResult<Value> {
    if n == 0 {
        env.push(value);
        env.push_ref(f);
        env.call_error_on_break("break is not allowed in rank")?;
        Ok(rc_take(env.pop("rank's function result")?))
    } else {
        let mut rows = Vec::with_capacity(value.row_count());
        for row in value.into_rows() {
            rows.push(rank_recursive(f.clone(), row, n - 1, env)?);
        }
        Value::from_row_values(rows, env)
    }
}
