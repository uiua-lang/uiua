use std::{
    ops::{Add, Mul},
    rc::Rc,
};

use crate::{
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
    Ok(())
}

pub fn each(env: &mut Uiua) -> UiuaResult {
    // let f = env.pop(1)?;
    // let xs = env.pop(2)?;
    // const BREAK_ERROR: &str = "break is not allowed in each";
    // let (shape, values) = xs.into_shape_flat_values();
    // let mut new_values = Vec::with_capacity(values.len());
    // for val in values {
    //     env.push(val);
    //     env.push(f.clone());
    //     env.call_error_on_break(BREAK_ERROR)?;
    //     new_values.push(env.pop("each's function result")?);
    // }
    // env.push(Array::from((shape, new_values)).normalized_type());
    Ok(())
}

pub fn zip(env: &mut Uiua) -> UiuaResult {
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
    // let f = env.pop(1)?;
    // let xs = env.pop(2)?;
    // let ys = env.pop(3)?;
    // const BREAK_ERROR: &str = "break is not allowed in bridge";
    // match (xs.is_array(), ys.is_array()) {
    //     (false, false) => {
    //         env.push(ys);
    //         env.push(xs);
    //         env.push(f);
    //         env.call_error_on_break(BREAK_ERROR)?;
    //     }
    //     (true, true) => {
    //         let x_rows = xs.into_array().into_values();
    //         let y_rows = ys.into_array().into_values();
    //         if x_rows.len() != y_rows.len() {
    //             return Err(env.error(format!(
    //                 "Cannot bridge arrays with different number of rows {:?} and {:?}",
    //                 x_rows.len(),
    //                 y_rows.len()
    //             )));
    //         }
    //         let mut new_rows = Vec::with_capacity(x_rows.len());
    //         for (x, y) in x_rows.into_iter().zip(y_rows) {
    //             env.push(y);
    //             env.push(x);
    //             env.push(f.clone());
    //             env.call_error_on_break(BREAK_ERROR)?;
    //             new_rows.push(env.pop("bridge's function result")?);
    //         }
    //         let mut array = Array::from(new_rows);
    //         if let Some((a, b)) = array.normalize() {
    //             return Err(env.error(format!(
    //                 "Rows in resulting array have different shapes {a:?} and {b:?}"
    //             )));
    //         }
    //         env.push(array);
    //     }
    //     (true, false) => {
    //         let x_rows = xs.into_array().into_values();
    //         let mut new_rows = Vec::with_capacity(x_rows.len());
    //         for x in x_rows {
    //             env.push(ys.clone());
    //             env.push(x);
    //             env.push(f.clone());
    //             env.call_error_on_break(BREAK_ERROR)?;
    //             new_rows.push(env.pop("bridge's function result")?);
    //         }
    //         let mut array = Array::from(new_rows);
    //         if let Some((a, b)) = array.normalize() {
    //             return Err(env.error(format!(
    //                 "Rows in resulting array have different shapes {a:?} and {b:?}"
    //             )));
    //         }
    //         env.push(array);
    //     }
    //     (false, true) => {
    //         let y_rows = ys.into_array().into_values();
    //         let mut new_rows = Vec::with_capacity(y_rows.len());
    //         for y in y_rows {
    //             env.push(y);
    //             env.push(xs.clone());
    //             env.push(f.clone());
    //             env.call_error_on_break(BREAK_ERROR)?;
    //             new_rows.push(env.pop("bridge's function result")?);
    //         }
    //         let mut array = Array::from(new_rows);
    //         if let Some((a, b)) = array.normalize() {
    //             return Err(env.error(format!(
    //                 "Rows in resulting array have different shapes {a:?} and {b:?}"
    //             )));
    //         }
    //         env.push(array);
    //     }
    // }
    Ok(())
}

pub fn table(env: &mut Uiua) -> UiuaResult {
    // let f = env.pop(1)?;
    // let xs = env.pop(2)?;
    // let ys = env.pop(3)?;
    // const BREAK_ERROR: &str = "break is not allowed in table";
    // if !xs.is_array() && !ys.is_array() {
    //     env.push(ys);
    //     env.push(xs);
    //     env.push(f);
    //     return env.call_error_on_break(BREAK_ERROR);
    // }
    // let a = if xs.is_array() {
    //     xs.into_array()
    // } else {
    //     Array::from(xs)
    // };
    // let b = if ys.is_array() {
    //     ys.into_array()
    // } else {
    //     Array::from(ys)
    // };
    // let mut new_shape = a.shape().to_vec();
    // new_shape.extend_from_slice(b.shape());
    // let mut items = Vec::with_capacity(a.len() * b.len());
    // for a in a.into_flat_values() {
    //     for b in b.clone().into_flat_values() {
    //         env.push(b);
    //         env.push(a.clone());
    //         env.push(f.clone());
    //         env.call_error_on_break(BREAK_ERROR)?;
    //         items.push(env.pop("tabled function result")?);
    //     }
    // }
    // env.push(Array::from((new_shape, items)).normalized_type());
    Ok(())
}

pub fn repeat(env: &mut Uiua) -> UiuaResult {
    // let f = env.pop(1)?;
    // let mut acc = env.pop(2)?;
    // let n = env.pop(3)?;
    // if n.is_number() && n.number() == INFINITY {
    //     loop {
    //         env.push(acc);
    //         env.push(f.clone());
    //         if env.call_catch_break()? {
    //             break;
    //         }
    //         acc = env.pop("repeated function result")?;
    //     }
    // } else {
    //     let Some(n) = n.as_nat() else {
    //                     return Err(env.error("Repetitions must be a natural number or infinity"));
    //                 };
    //     for _ in 0..n {
    //         env.push(acc);
    //         env.push(f.clone());
    //         if env.call_catch_break()? {
    //             return Ok(());
    //         }
    //         acc = env.pop("repeated function result")?;
    //     }
    //     env.push(acc);
    // }
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
}
