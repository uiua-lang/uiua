use std::{cmp::Ordering, iter::repeat, mem::take, sync::Arc};

use tinyvec::tiny_vec;

use crate::{
    algorithm::max_shape,
    array::*,
    cowslice::CowSlice,
    function::{Function, FunctionId, FunctionKind, Instr},
    value::Value,
    Uiua, UiuaResult,
};

use super::FillContext;

impl Value {
    fn coerce_to_functions<T, C: FillContext, E: ToString>(
        self,
        other: Self,
        ctx: C,
        on_success: impl FnOnce(Array<Arc<Function>>, Array<Arc<Function>>, C) -> Result<T, C::Error>,
        on_error: impl FnOnce(&str, &str) -> E,
    ) -> Result<T, C::Error> {
        match (self, other) {
            (Value::Func(a), Value::Func(b)) => on_success(a, b, ctx),
            (Value::Func(a), mut b) => {
                let shape = take(b.shape_mut());
                let new_data: CowSlice<_> = b
                    .into_flat_values()
                    .map(|b| {
                        Arc::new(Function::new(
                            FunctionId::Constant,
                            vec![Instr::Push(b.into()), Instr::Call(0)],
                            FunctionKind::Normal,
                        ))
                    })
                    .collect();
                let b = Array::new(shape, new_data);
                on_success(a, b, ctx)
            }
            (mut a, Value::Func(b)) => {
                let shape = take(a.shape_mut());
                let new_data: CowSlice<_> = a
                    .into_flat_values()
                    .map(|a| Arc::new(Function::constant(a)))
                    .collect();
                let a = Array::new(shape, new_data);
                on_success(a, b, ctx)
            }
            (a, b) => Err(ctx.error(on_error(a.type_name(), b.type_name()))),
        }
    }
}

fn data_index_to_shape_index(mut index: usize, shape: &[usize], out: &mut [usize]) -> bool {
    debug_assert_eq!(shape.len(), out.len());
    if index >= shape.iter().product() {
        return false;
    }
    for (&s, o) in shape.iter().zip(out).rev() {
        *o = index % s;
        index /= s;
    }
    true
}

#[test]
fn data_index_to_shape_index_test() {
    let mut out = [0, 0];
    for (index, shape, expected_out, expected_success) in [
        (2, [2, 3], [0, 2], true),
        (2, [1, 3], [0, 2], true),
        (3, [2, 3], [1, 0], true),
        (3, [1, 3], [1, 0], false),
    ] {
        let success = data_index_to_shape_index(index, &shape, &mut out);
        assert_eq!(out, expected_out);
        assert_eq!(success, expected_success);
    }
}

fn shape_index_to_data_index(index: &[usize], shape: &[usize]) -> Option<usize> {
    debug_assert_eq!(shape.len(), index.len());
    let mut data_index = 0;
    for (&s, &i) in shape.iter().zip(index) {
        if i >= s {
            return None;
        }
        data_index = data_index * s + i;
    }
    Some(data_index)
}

#[test]
fn shape_index_to_data_index_test() {
    for (index, shape, expected_data_index) in [
        ([0, 2], [2, 3], Some(2)),
        ([0, 2], [1, 3], Some(2)),
        ([1, 0], [2, 3], Some(3)),
        ([1, 0], [1, 3], None),
    ] {
        let data_index = shape_index_to_data_index(&index, &shape);
        assert_eq!(data_index, expected_data_index);
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn fill_to_shape(&mut self, shape: &[usize], fill_value: T) {
        while self.rank() < shape.len() {
            self.shape.insert(0, 1);
        }
        if self.shape == shape {
            return;
        }
        let target_size = shape.iter().product();
        let mut new_data = vec![fill_value; target_size];
        let mut curr = vec![0; shape.len()];
        for new_data_index in 0..target_size {
            data_index_to_shape_index(new_data_index, shape, &mut curr);
            if let Some(data_index) = shape_index_to_data_index(&curr, &self.shape) {
                new_data[new_data_index] = self.data[data_index].clone();
            }
        }
        self.data = new_data.into();
        self.shape = shape.into();
    }
}

impl Value {
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, env)
    }
    pub fn join_infallible(self, other: Self) -> Self {
        self.join_impl(other, ()).unwrap()
    }
    // pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {}
    fn join_impl<C: FillContext>(self, other: Self, ctx: C) -> Result<Self, C::Error> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.join_impl(b, ctx)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.join_impl(b, ctx)?.into(),
            (Value::Char(a), Value::Char(b)) => a.join_impl(b, ctx)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().join_impl(b, ctx)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.join_impl(b.convert(), ctx)?.into(),
            (a, b) => a.coerce_to_functions(
                b,
                ctx,
                |a, b, env| Ok(a.join_impl(b, env)?.into()),
                |a, b| format!("Cannot join {a} array and {b} array"),
            )?,
        })
    }
    fn append<C: FillContext>(self, other: Self, ctx: C, action: &str) -> Result<Self, C::Error> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.append(b, ctx, action)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.append(b, ctx, action)?.into(),
            (Value::Char(a), Value::Char(b)) => a.append(b, ctx, action)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().append(b, ctx, action)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.append(b.convert(), ctx, action)?.into(),
            (a, b) => a.coerce_to_functions(
                b,
                ctx,
                |a, b, env| Ok(a.append(b, env, action)?.into()),
                |a, b| format!("Cannot {action} {a} array with {b} array"),
            )?,
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn join(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join_impl(other, env)
    }
    pub fn join_infallible(self, other: Self) -> Self {
        self.join_impl(other, ()).unwrap()
    }
    fn join_impl<C: FillContext>(mut self, mut other: Self, ctx: C) -> Result<Self, C::Error> {
        crate::profile_function!();
        let res = match self.rank().cmp(&other.rank()) {
            Ordering::Less => {
                let target_shape = if let Some(fill) = ctx.fill::<T>() {
                    let target_shape = max_shape(&self.shape, &other.shape);
                    let row_shape = &target_shape[1..];
                    self.fill_to_shape(row_shape, fill.clone());
                    other.fill_to_shape(&target_shape, fill);
                    target_shape
                } else {
                    if other.rank() - self.rank() > 1 {
                        return Err(ctx.error(format!(
                            "Cannot join rank {} array with rank {} array",
                            self.rank(),
                            other.rank()
                        )));
                    }
                    if self.shape() != &other.shape()[1..] {
                        return Err(ctx.error(format!(
                            "Cannot join arrays of shapes {} and {}",
                            self.format_shape(),
                            other.format_shape()
                        )));
                    }
                    other.shape
                };
                if let Some((a, b)) = self.data.last().zip(other.data.first()) {
                    a.group_compatibility(b, ctx)?;
                }
                self.data.extend(other.data);
                self.shape = target_shape;
                self.shape[0] += 1;
                self
            }
            Ordering::Greater => self.append(other, ctx, "join")?,
            Ordering::Equal => {
                if self.rank() == 0 {
                    debug_assert_eq!(other.rank(), 0);
                    self.data.extend(other.data.into_iter().next());
                    self.shape = tiny_vec![2];
                    self
                } else {
                    if let Some(fill) = ctx.fill::<T>() {
                        let new_row_shape = max_shape(&self.shape[1..], &other.shape[1..]);
                        for (array, fill) in [(&mut self, fill.clone()), (&mut other, fill)] {
                            let mut new_shape = new_row_shape.clone();
                            new_shape.insert(0, array.shape[0]);
                            array.fill_to_shape(&new_shape, fill);
                        }
                    } else if self.shape[1..] != other.shape[1..] {
                        return Err(ctx.error(format!(
                            "Cannot join arrays of shapes {} and {}",
                            self.format_shape(),
                            other.format_shape()
                        )));
                    }
                    if let Some((a, b)) = self.data.last().zip(other.data.first()) {
                        a.group_compatibility(b, ctx)?;
                    }
                    self.data.extend(other.data);
                    self.shape[0] += other.shape[0];
                    self
                }
            }
        };
        res.validate_shape();
        Ok(res)
    }
    fn append<C: FillContext>(
        mut self,
        mut other: Self,
        ctx: C,
        action: &str,
    ) -> Result<Self, C::Error> {
        let target_shape = if let Some(fill) = ctx.fill::<T>() {
            while self.rank() <= other.rank() {
                self.shape.push(1);
            }
            let target_shape = max_shape(&self.shape, &other.shape);
            let row_shape = &target_shape[1..];
            self.fill_to_shape(&target_shape, fill.clone());
            other.fill_to_shape(row_shape, fill);
            target_shape
        } else {
            if self.rank() <= other.rank() || self.rank() - other.rank() > 1 {
                return Err(ctx.error(format!(
                    "Cannot {} rank {} array with rank {} array",
                    action,
                    self.rank(),
                    other.rank()
                )));
            }
            if &self.shape()[1..] != other.shape() {
                return Err(ctx.error(format!(
                    "Cannot {} arrays of shapes {} and {}",
                    action,
                    self.format_shape(),
                    other.format_shape()
                )));
            }
            take(&mut self.shape)
        };
        if let Some((a, b)) = self.data.last().zip(other.data.first()) {
            a.group_compatibility(b, ctx)?;
        }
        self.data.extend(other.data);
        self.shape = target_shape;
        self.shape[0] += 1;
        Ok(self)
    }
}

impl Value {
    pub fn couple(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, env)
    }
    pub fn couple_infallible(self, other: Self) -> Self {
        self.couple_impl(other, ()).unwrap()
    }
    fn couple_impl<C: FillContext>(self, other: Self, ctx: C) -> Result<Self, C::Error> {
        Ok(match (self, other) {
            (Value::Num(a), Value::Num(b)) => a.couple_impl(b, ctx)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.couple_impl(b, ctx)?.into(),
            (Value::Char(a), Value::Char(b)) => a.couple_impl(b, ctx)?.into(),
            (Value::Func(a), Value::Func(b)) => a.couple_impl(b, ctx)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.couple_impl(b.convert(), ctx)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().couple_impl(b, ctx)?.into(),
            (a, b) => a.coerce_to_functions(
                b,
                ctx,
                |a, b, ctx| Ok(a.couple_impl(b, ctx)?.into()),
                |a, b| format!("Cannot couple {a} array with {b} array"),
            )?,
        })
    }
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        match self {
            Value::Num(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Byte(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Char(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
            Value::Func(a) => a.uncouple(env).map(|(a, b)| (a.into(), b.into())),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn couple(self, other: Self, env: &Uiua) -> UiuaResult<Self> {
        self.couple_impl(other, env)
    }
    pub fn couple_infallible(self, other: Self) -> Self {
        self.couple_impl(other, ()).unwrap()
    }
    fn couple_impl<C: FillContext>(mut self, mut other: Self, ctx: C) -> Result<Self, C::Error> {
        crate::profile_function!();
        if self.shape != other.shape {
            if let Some(fill) = ctx.fill::<T>() {
                let new_shape = max_shape(&self.shape, &other.shape);
                self.fill_to_shape(&new_shape, fill.clone());
                other.fill_to_shape(&new_shape, fill);
            } else {
                return Err(ctx.error(format!(
                    "Cannot couple arrays with shapes {} and {}",
                    self.format_shape(),
                    other.format_shape()
                )));
            }
        }
        if let Some((a, b)) = self.data.last().zip(other.data.first()) {
            a.group_compatibility(b, ctx)?;
        }
        self.data.extend(other.data);
        self.shape.insert(0, 2);
        self.validate_shape();
        Ok(self)
    }
    pub fn uncouple(self, env: &Uiua) -> UiuaResult<(Self, Self)> {
        if self.row_count() != 2 {
            return Err(env.error(format!(
                "Cannot uncouple array with {} row{}",
                self.row_count(),
                if self.row_count() == 1 { "" } else { "s" }
            )));
        }
        let mut rows = self.into_rows();
        let first = rows.next().unwrap();
        let second = rows.next().unwrap();
        Ok((first, second))
    }
}

impl Value {
    pub fn from_row_values<V>(values: V, env: &Uiua) -> UiuaResult<Self>
    where
        V: IntoIterator,
        V::Item: Into<Value>,
    {
        Self::from_row_values_impl(values.into_iter().map(Into::into), env)
    }
    pub fn from_row_values_infallible<V>(values: V) -> Self
    where
        V: IntoIterator,
        V::Item: Into<Value>,
    {
        Self::from_row_values_impl(values.into_iter().map(Into::into), ()).unwrap()
    }
    fn from_row_values_impl<C: FillContext>(
        values: impl IntoIterator<Item = Value>,
        ctx: C,
    ) -> Result<Self, C::Error> {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Ok(Value::default());
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            value = if count == 2 {
                value.couple_impl(row, ctx)?
            } else {
                value.append(row, ctx, "append")?
            };
        }
        if count == 1 {
            value.shape_mut().insert(0, 1);
        }
        Ok(value)
    }
}

impl<T: ArrayValue> Array<T> {
    #[track_caller]
    pub fn from_row_arrays(values: impl IntoIterator<Item = Self>, env: &Uiua) -> UiuaResult<Self> {
        Self::from_row_arrays_impl(values, env)
    }
    #[track_caller]
    pub fn from_row_arrays_infallible(values: impl IntoIterator<Item = Self>) -> Self {
        Self::from_row_arrays_impl(values, ()).unwrap()
    }
    #[track_caller]
    fn from_row_arrays_impl<C: FillContext>(
        values: impl IntoIterator<Item = Self>,
        ctx: C,
    ) -> Result<Self, C::Error> {
        let mut row_values = values.into_iter();
        let Some(mut value) = row_values.next() else {
            return Ok(Self::default());
        };
        let mut count = 1;
        for row in row_values {
            count += 1;
            value = if count == 2 {
                value.couple_impl(row, ctx)?
            } else {
                value.append(row, ctx, "append")?
            };
        }
        if count == 1 {
            value.shape.insert(0, 1);
        }
        value.validate_shape();
        Ok(value)
    }
}

impl Value {
    pub fn reshape(&mut self, shape: &Self, env: &Uiua) -> UiuaResult {
        let target_shape = shape.as_naturals(
            env,
            "Shape should be a single natural number \
            or a list of natural numbers",
        )?;
        let target_shape = Shape::from(&*target_shape);
        match self {
            Value::Num(a) => a.reshape(target_shape),
            Value::Byte(a) => a.reshape(target_shape),
            Value::Char(a) => a.reshape(target_shape),
            Value::Func(a) => a.reshape(target_shape),
        }
        Ok(())
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn reshape(&mut self, shape: Shape) {
        if self.data.is_empty() {
            return;
        }
        let target_len: usize = shape.iter().product();
        self.shape = shape;
        if self.data.len() < target_len {
            let start = self.data.len();
            self.data.modify(|data| {
                data.reserve(target_len - data.len());
                for i in 0..target_len - start {
                    data.push(data[i % start].clone());
                }
            });
        } else {
            self.data.truncate(target_len);
        }
    }
}

impl Value {
    pub fn replicate(&self, replicated: Self, env: &Uiua) -> UiuaResult<Self> {
        let amount =
            self.as_naturals(env, "Replication amount must be a list of natural numbers")?;
        Ok(match replicated {
            Value::Num(a) => Value::Num(a.replicate(&amount, env)?),
            Value::Byte(a) => Value::Byte(a.replicate(&amount, env)?),
            Value::Char(a) => Value::Char(a.replicate(&amount, env)?),
            Value::Func(a) => Value::Func(a.replicate(&amount, env)?),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn replicate(mut self, amount: &[usize], env: &Uiua) -> UiuaResult<Self> {
        if self.row_count() != amount.len() {
            return Err(env.error(format!(
                "Cannot replicate array with shape {} with array of length {}",
                self.format_shape(),
                amount.len()
            )));
        }
        if self.rank() == 0 {
            if amount.len() != 1 {
                return Err(env.error("Scalar array can only be replicated with a single number"));
            }
            let mut new_data = Vec::with_capacity(amount[0]);
            for _ in 0..amount[0] {
                new_data.push(self.data[0].clone());
            }
            self = new_data.into();
        } else if amount.iter().all(|&n| n <= 1) {
            let row_len = self.row_len();
            let mut new_row_count = self.row_count();
            self.data.modify(|data| {
                for (r, &n) in amount.iter().enumerate().rev() {
                    if n == 0 {
                        let mut iter = take(data).into_iter();
                        *data = iter.by_ref().take(r * row_len).collect();
                        data.extend(iter.skip(row_len));
                        new_row_count -= 1;
                    }
                }
            });
            self.shape[0] = new_row_count;
        } else {
            let mut new_data = Vec::new();
            let mut new_len = 0;
            for (row, &n) in self.rows().zip(amount) {
                new_len += n;
                for _ in 0..n {
                    new_data.extend_from_slice(&row.data);
                }
            }
            self.shape[0] = new_len;
            self.data = new_data.into();
        }
        self.validate_shape();
        Ok(self)
    }
}

impl Value {
    pub(crate) fn into_shaped_indices(self, env: &Uiua) -> UiuaResult<(Shape, Vec<isize>)> {
        Ok(match self {
            Value::Num(arr) => {
                let mut index_data = Vec::with_capacity(arr.flat_len());
                for n in arr.data {
                    if n.fract() != 0.0 {
                        return Err(env.error(format!(
                            "Index must be an array of integers, but {n} is not an integer"
                        )));
                    }
                    index_data.push(n as isize);
                }
                (arr.shape, index_data)
            }
            Value::Byte(arr) => {
                let mut index_data = Vec::with_capacity(arr.flat_len());
                for n in arr.data {
                    index_data.push(n as isize);
                }
                (arr.shape, index_data)
            }
            value => {
                return Err(env.error(format!(
                    "Index must be an array of integers, not {}s",
                    value.type_name()
                )))
            }
        })
    }
    pub fn pick(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let (index_shape, index_data) = self.into_shaped_indices(env)?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Byte(a) => Value::Byte(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Char(a) => Value::Char(a.pick_shaped(&index_shape, &index_data, env)?),
            Value::Func(a) => Value::Func(a.pick_shaped(&index_shape, &index_data, env)?),
        })
    }
    pub fn unpick(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_indices(env, "Index must be an array of integers")?;
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => a.unpick_impl(&index, b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.unpick_impl(&index, b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.unpick_impl(&index, b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.unpick_impl(&index, b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.unpick_impl(&index, b.convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.convert().unpick_impl(&index, b, env)?.into(),
            (a, b) => a
                .coerce_to_functions(
                    b,
                    env,
                    |a, b, env| a.unpick_impl(&index, b, env),
                    |a, b| format!("Cannot unpick {a} array from {b} array"),
                )?
                .into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn pick_shaped(
        &self,
        index_shape: &[usize],
        index_data: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if index_shape.len() <= 1 {
            self.pick(index_data, env)
        } else {
            let (shape, data) = self.pick_shaped_impl(index_shape, index_data, env)?;
            Ok(Array::new(shape, data))
        }
    }
    fn pick_shaped_impl(
        &self,
        index_shape: &[usize],
        index_data: &[isize],
        env: &Uiua,
    ) -> UiuaResult<(Shape, Vec<T>)> {
        let index_row_len = index_shape[1..].iter().product();
        let mut new_data =
            Vec::with_capacity(index_shape[..index_shape.len() - 1].iter().product());
        for index_row in index_data.chunks(index_row_len) {
            let row = self.pick_shaped(&index_shape[1..], index_row, env)?;
            new_data.extend_from_slice(&row.data);
        }
        let mut new_shape = Shape::from(&index_shape[0..index_shape.len() - 1]);
        new_shape.extend_from_slice(&self.shape[*index_shape.last().unwrap()..]);
        Ok((new_shape, new_data))
    }
    pub fn pick(&self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        if index.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot pick from rank {} array with index of length {}",
                self.rank(),
                index.len()
            )));
        }
        let mut picked = self.data.clone();
        for (d, (&s, &i)) in self.shape.iter().zip(index).enumerate() {
            let row_len: usize = self.shape[d + 1..].iter().product();
            let s = s as isize;
            if i >= s || i < -s {
                if let Some(fill) = env.fill::<T>() {
                    picked = vec![fill; row_len].into();
                    continue;
                }
                return Err(env.error(format!(
                    "Index {i} is out of bounds of length {s} (dimension {d}) in shape {}",
                    self.format_shape()
                )));
            }
            let i = if i >= 0 { i as usize } else { (s + i) as usize };
            let start = i * row_len;
            let end = start + row_len;
            picked = picked.slice(start..end);
        }
        let shape = Shape::from(&self.shape[index.len()..]);
        Ok(Array::new(shape, picked))
    }
    fn unpick_impl(self, index: &[isize], mut from: Self, env: &Uiua) -> UiuaResult<Self> {
        let expected_shape = &from.shape()[index.len()..];
        if self.shape != expected_shape {
            return Err(
                env.error("Attempted to undo pick, but the shape of the selected array changed")
            );
        }
        let mut start = 0;
        for (i, (&ind, &f)) in index.iter().zip(from.shape()).enumerate() {
            let ind = if ind >= 0 {
                ind as usize
            } else {
                (f as isize + ind) as usize
            };
            start += ind * from.shape[i + 1..].iter().product::<usize>();
        }
        from.data.modify(|data| {
            for (f, i) in data.make_mut().iter_mut().skip(start).zip(self.data) {
                *f = i;
            }
        });
        Ok(from)
    }
}

impl Value {
    pub fn take(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_indices(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.take(&index, env)?),
            Value::Byte(a) => Value::Byte(a.take(&index, env)?),
            Value::Char(a) => Value::Char(a.take(&index, env)?),
            Value::Func(a) => Value::Func(a.take(&index, env)?),
        })
    }
    pub fn drop(self, from: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = self.as_indices(env, "Index must be a list of integers")?;
        Ok(match from {
            Value::Num(a) => Value::Num(a.drop(&index, env)?),
            Value::Byte(a) => Value::Byte(a.drop(&index, env)?),
            Value::Char(a) => Value::Char(a.drop(&index, env)?),
            Value::Func(a) => Value::Func(a.drop(&index, env)?),
        })
    }
    pub(crate) fn untake(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_indices(env, "Index must be a list of integers")?;
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.untake(&index, b, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.untake(&index, b, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.untake(&index, b, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.untake(&index, b, env)?),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.untake(&index, b.convert(), env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().untake(&index, b, env)?),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        })
    }
    pub(crate) fn undrop(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index = index.as_indices(env, "Index must be a list of integers")?;
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a.undrop(&index, b, env)?),
            (Value::Byte(a), Value::Byte(b)) => Value::Byte(a.undrop(&index, b, env)?),
            (Value::Char(a), Value::Char(b)) => Value::Char(a.undrop(&index, b, env)?),
            (Value::Func(a), Value::Func(b)) => Value::Func(a.undrop(&index, b, env)?),
            (Value::Num(a), Value::Byte(b)) => Value::Num(a.undrop(&index, b.convert(), env)?),
            (Value::Byte(a), Value::Num(b)) => Value::Num(a.convert().undrop(&index, b, env)?),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot undrop {} into {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn take(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(match index {
            [] => self,
            &[taking] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_taking = taking.unsigned_abs();
                let mut filled = false;
                self.data.modify(|data| {
                    if taking >= 0 {
                        if abs_taking > row_count {
                            if let Some(fill) = T::get_fill(env) {
                                filled = true;
                                data.extend(repeat(fill).take((abs_taking - row_count) * row_len));
                            } else {
                                return Err(env.error(format!(
                                    "Cannot take {} rows from array with {} row{} \
                                    outside a fill context",
                                    abs_taking,
                                    row_count,
                                    if row_count == 1 { "" } else { "s" }
                                )));
                            }
                        } else {
                            data.truncate(abs_taking * row_len);
                        }
                    } else {
                        *data = if abs_taking > row_count {
                            if let Some(fill) = T::get_fill(env) {
                                filled = true;
                                repeat(fill)
                                    .take((abs_taking - row_count) * row_len)
                                    .chain(take(data))
                                    .collect()
                            } else {
                                return Err(env.error(format!(
                                    "Cannot take {} rows from array with {} row{} \
                                    outside a fill context",
                                    abs_taking,
                                    row_count,
                                    if row_count == 1 { "" } else { "s" }
                                )));
                            }
                        } else {
                            take(data)
                                .into_iter()
                                .skip((row_count - abs_taking) * row_len)
                                .collect()
                        };
                    }
                    Ok(())
                })?;
                if let Some(s) = self.shape.get_mut(0) {
                    *s = if filled {
                        abs_taking
                    } else {
                        (*s).min(abs_taking)
                    };
                } else if filled {
                    self.shape.push(abs_taking);
                }
                self.validate_shape();
                self
            }
            &[taking, ref sub_index @ ..] => {
                if index.len() > self.rank() {
                    return Err(env.error(format!(
                        "Cannot take from rank {} array with index of length {}",
                        self.rank(),
                        index.len()
                    )));
                }
                let abs_taking = taking.unsigned_abs();
                if sub_index
                    .iter()
                    .zip(&self.shape[1..])
                    .all(|(&i, &s)| i.unsigned_abs() == s)
                {
                    return self.take(&[taking], env);
                }
                let mut new_rows = Vec::with_capacity(abs_taking);
                let mut arr = if taking >= 0 {
                    // Take in each row
                    for row in self.rows().take(abs_taking) {
                        new_rows.push(row.take(sub_index, env)?);
                    }
                    let mut arr = Array::from_row_arrays_infallible(new_rows);
                    // Extend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        if let Some(fill) = T::get_fill(env) {
                            let row_len = arr.row_len();
                            arr.data.extend(
                                repeat(fill).take((abs_taking - arr.row_count()) * row_len),
                            );
                        } else {
                            return Err(env.error(format!(
                                "Cannot take {} rows from array with {} row{} \
                                outside a fill context",
                                abs_taking,
                                arr.row_count(),
                                if arr.row_count() == 1 { "" } else { "s" }
                            )));
                        }
                    }
                    arr
                } else {
                    // Take in each row
                    let start = self.row_count().saturating_sub(abs_taking);
                    for row in self.rows().skip(start) {
                        new_rows.push(row.take(sub_index, env)?);
                    }
                    let mut arr = Array::from_row_arrays_infallible(new_rows);
                    // Prepend with fill values if necessary
                    if abs_taking > arr.row_count() {
                        if let Some(fill) = T::get_fill(env) {
                            let row_len = arr.row_len();
                            arr.data = repeat(fill)
                                .take((abs_taking - arr.row_count()) * row_len)
                                .chain(arr.data)
                                .collect();
                        } else {
                            return Err(env.error(format!(
                                "Cannot take {} rows from array with {} row{} \
                                outside a fill context",
                                abs_taking,
                                arr.row_count(),
                                if arr.row_count() == 1 { "" } else { "s" }
                            )));
                        }
                    }
                    arr
                };
                arr.shape[0] = abs_taking;
                arr.validate_shape();
                arr
            }
        })
    }
    pub fn drop(mut self, index: &[isize], env: &Uiua) -> UiuaResult<Self> {
        Ok(match index {
            [] => self,
            &[dropping] => {
                let row_len = self.row_len();
                let row_count = self.row_count();
                let abs_dropping = dropping.unsigned_abs();
                self.data.modify(|data| {
                    *data = if dropping >= 0 {
                        take(data)
                            .into_iter()
                            .skip(abs_dropping * row_len)
                            .collect()
                    } else {
                        take(data)
                            .into_iter()
                            .take((row_count.saturating_sub(abs_dropping)) * row_len)
                            .collect()
                    };
                });
                if self.shape.is_empty() {
                    self.shape.push(1);
                }
                self.shape[0] = self.shape[0].saturating_sub(abs_dropping);
                self.validate_shape();
                self
            }
            &[dropping, ref sub_index @ ..] => {
                if index.len() > self.rank() {
                    return Err(env.error(format!(
                        "Cannot drop from rank {} array with index of length {}",
                        self.rank(),
                        index.len()
                    )));
                }
                let abs_dropping = dropping.unsigned_abs();
                let mut new_rows = Vec::with_capacity(abs_dropping);
                let row_count = self.row_count();
                if dropping >= 0 {
                    for row in self.rows().skip(abs_dropping) {
                        new_rows.push(row.drop(sub_index, env)?);
                    }
                } else {
                    let end = row_count.saturating_sub(abs_dropping);
                    for row in self.rows().take(end) {
                        new_rows.push(row.drop(sub_index, env)?);
                    }
                };
                Array::from_row_arrays(new_rows, env)?
            }
        })
    }
    fn untake(self, index: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        let from = self;
        if from.rank() != into.rank() {
            return Err(env.error(format!(
                "Cannot untake rank {} array into rank {} array",
                from.rank(),
                into.rank()
            )));
        }
        Ok(match index {
            [] => into,
            &[untaking] => {
                let abs_untaking = untaking.unsigned_abs();
                if from.row_count() != abs_untaking {
                    return Err(env.error(format!(
                        "Attempted to undo take, but the taken section's \
                        row count was modified from {} to {}",
                        abs_untaking,
                        from.row_count()
                    )));
                }
                let into_row_len = into.row_len();
                let into_row_count = into.row_count();
                into.data.modify(|data| {
                    if untaking >= 0 {
                        for (from, into) in from
                            .row_slices()
                            .zip(data.make_mut().chunks_exact_mut(into_row_len))
                        {
                            into.clone_from_slice(from);
                        }
                    } else {
                        let start = into_row_count.saturating_sub(abs_untaking);
                        for (from, into) in from
                            .row_slices()
                            .zip(data.make_mut().chunks_exact_mut(into_row_len).skip(start))
                        {
                            into.clone_from_slice(from);
                        }
                    }
                });
                into
            }
            &[untaking, ref sub_index @ ..] => {
                let abs_untaking = untaking.unsigned_abs();
                if from.row_count() != abs_untaking {
                    return Err(env.error(format!(
                        "Attempted to undo take, but the taken section's \
                        row count was modified from {} to {}",
                        abs_untaking,
                        from.row_count()
                    )));
                }
                let into_row_count = into.row_count();
                let mut new_rows = Vec::with_capacity(into_row_count);
                if untaking >= 0 {
                    for (from, into) in from.rows().zip(into.rows()) {
                        new_rows.push(from.untake(sub_index, into, env)?);
                    }
                    new_rows.extend(into.rows().skip(abs_untaking));
                } else {
                    let start = into_row_count.saturating_sub(abs_untaking);
                    new_rows.extend(into.rows().take(start));
                    for (from, into) in from.rows().zip(into.rows().skip(start)) {
                        new_rows.push(from.untake(sub_index, into, env)?);
                    }
                }
                Array::from_row_arrays(new_rows, env)?
            }
        })
    }
    fn undrop(self, index: &[isize], into: Self, env: &Uiua) -> UiuaResult<Self> {
        let index: Vec<isize> = index
            .iter()
            .zip(&into.shape)
            .map(|(&i, &s)| {
                if i >= 0 {
                    i - s as isize
                } else {
                    i + s as isize
                }
            })
            .collect();
        self.untake(&index, into, env)
    }
}

impl Value {
    pub fn rotate(&self, mut rotated: Self, env: &Uiua) -> UiuaResult<Self> {
        let by = self.as_indices(env, "Rotation amount must be a list of integers")?;
        match &mut rotated {
            Value::Num(a) => a.rotate(&by, env)?,
            Value::Byte(a) => a.rotate(&by, env)?,
            Value::Char(a) => a.rotate(&by, env)?,
            Value::Func(a) => a.rotate(&by, env)?,
        }
        Ok(rotated)
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn rotate(&mut self, by: &[isize], env: &Uiua) -> UiuaResult {
        if by.len() > self.rank() {
            return Err(env.error(format!(
                "Cannot rotate rank {} array with index of length {}",
                self.rank(),
                by.len()
            )));
        }
        rotate(by, &self.shape, &mut self.data);
        Ok(())
    }
}

fn rotate<T>(by: &[isize], shape: &[usize], data: &mut [T]) {
    let row_count = shape[0];
    if row_count == 0 {
        return;
    }
    let row_len = shape[1..].iter().product();
    let offset = by[0];
    let mid = (row_count as isize + offset).rem_euclid(row_count as isize) as usize;
    let (left, right) = data.split_at_mut(mid * row_len);
    left.reverse();
    right.reverse();
    data.reverse();
    let index = &by[1..];
    let shape = &shape[1..];
    if index.is_empty() || shape.is_empty() {
        return;
    }
    for cell in data.chunks_mut(row_len) {
        rotate(index, shape, cell);
    }
}

impl Value {
    fn as_index_array<'a>(&'a self, env: &Uiua) -> UiuaResult<(&'a [usize], Vec<isize>)> {
        let mut indices = Vec::with_capacity(self.flat_len());
        match self {
            Value::Num(arr) => {
                for &i in arr.data.iter() {
                    if i.fract() != 0.0 {
                        return Err(
                            env.error(format!("Indices must be integers, but {} is not", i))
                        );
                    }
                    indices.push(i as isize);
                }
            }
            Value::Byte(arr) => {
                for &i in arr.data.iter() {
                    indices.push(i as isize);
                }
            }
            v => {
                return Err(env.error(format!(
                    "Indices must be an array of integers, but it is {}s",
                    v.type_name()
                )))
            }
        }
        Ok((self.shape(), indices))
    }
    pub fn select(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let (indices_shape, indices) = self.as_index_array(env)?;
        Ok(match from {
            Value::Num(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Byte(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Char(a) => a.select_impl(indices_shape, &indices, env)?.into(),
            Value::Func(a) => a.select_impl(indices_shape, &indices, env)?.into(),
        })
    }
    pub fn unselect(self, index: Self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        let (ind_shape, ind) = index.as_index_array(env)?;
        let mut sorted_indices = ind.clone();
        sorted_indices.sort();
        if sorted_indices.windows(2).any(|win| win[0] == win[1]) {
            return Err(env.error("Cannot undo selection with duplicate indices"));
        }
        Ok(match (self, into) {
            (Value::Num(a), Value::Num(b)) => a.unselect_impl(ind_shape, &ind, b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.unselect_impl(ind_shape, &ind, b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.unselect_impl(ind_shape, &ind, b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.unselect_impl(ind_shape, &ind, b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => {
                a.unselect_impl(ind_shape, &ind, b.convert(), env)?.into()
            }
            (Value::Byte(a), Value::Num(b)) => {
                a.convert().unselect_impl(ind_shape, &ind, b, env)?.into()
            }
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot untake {} into {}",
                    a.type_name(),
                    b.type_name()
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn select_impl(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            let row_count = indices_shape[0];
            let row_len = indices_shape[1..].iter().product();
            let mut rows = Vec::with_capacity(row_count);
            for indices_row in indices.chunks_exact(row_len) {
                rows.push(self.select_impl(&indices_shape[1..], indices_row, env)?);
            }
            Array::from_row_arrays(rows, env)
        } else {
            let mut res = self.select(indices, env)?;
            if indices_shape.is_empty() {
                res.shape.remove(0);
            }
            Ok(res)
        }
    }
    fn unselect_impl(
        &self,
        indices_shape: &[usize],
        indices: &[isize],
        into: Self,
        env: &Uiua,
    ) -> UiuaResult<Self> {
        if indices_shape.len() > 1 {
            Err(env.error("Cannot undo multi-dimensional selection"))
        } else {
            self.unselect(indices, into, env)
        }
    }
    fn select(&self, indices: &[isize], env: &Uiua) -> UiuaResult<Self> {
        let mut selected = Vec::with_capacity(self.row_len() * indices.len());
        let row_len = self.row_len();
        let row_count = self.row_count();
        for &i in indices {
            let i = if i >= 0 {
                let ui = i as usize;
                if ui >= row_count {
                    if let Some(fill) = env.fill::<T>() {
                        selected.extend(repeat(fill).take(row_len));
                        continue;
                    }
                    return Err(env.error(format!(
                        "Index {} is out of bounds of length {}",
                        i, row_count
                    )));
                }
                ui
            } else {
                let pos_i = (row_count as isize + i) as usize;
                if pos_i >= row_count {
                    if let Some(fill) = env.fill::<T>() {
                        selected.extend(repeat(fill).take(row_len));
                        continue;
                    }
                    return Err(env.error(format!(
                        "Index {} is out of bounds of length {}",
                        i, row_count
                    )));
                }
                pos_i
            };
            let start = i * row_len;
            let end = start + row_len;
            selected.extend_from_slice(&self.data[start..end]);
        }
        let mut shape = self.shape.clone();
        if let Some(s) = shape.get_mut(0) {
            *s = indices.len();
        } else {
            shape.push(indices.len());
        }
        let arr = Array::new(shape, selected);
        arr.validate_shape();
        Ok(arr)
    }
    fn unselect(&self, indices: &[isize], mut into: Self, env: &Uiua) -> UiuaResult<Self> {
        if self.row_count() != indices.len() {
            return Err(env.error(
                "Attempted to undo selection, but \
                the shape of the selected array changed",
            ));
        }
        let into_row_len = into.row_len();
        let into_row_count = into.row_count();
        for (&i, row) in indices.iter().zip(self.rows()) {
            let i = if i >= 0 {
                let ui = i as usize;
                if ui >= into_row_count {
                    return Err(env.error(format!(
                        "Index {} is out of bounds of length {}",
                        i, into_row_count
                    )));
                }
                ui
            } else {
                let pos_i = (into_row_count as isize + i) as usize;
                if pos_i >= into_row_count {
                    return Err(env.error(format!(
                        "Index {} is out of bounds of length {}",
                        i, into_row_count
                    )));
                }
                pos_i
            };
            let start = i * into_row_len;
            let end = start + into_row_len;
            for (i, x) in (start..end).zip(row.data) {
                into.data[i] = x.clone();
            }
        }
        Ok(into)
    }
}

impl Value {
    pub fn windows(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let size_spec = self.as_naturals(env, "Window size must be a list of natural numbers")?;
        Ok(match from {
            Value::Num(a) => a.windows(&size_spec, env)?.into(),
            Value::Byte(a) => a.windows(&size_spec, env)?.into(),
            Value::Char(a) => a.windows(&size_spec, env)?.into(),
            Value::Func(a) => a.windows(&size_spec, env)?.into(),
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn windows(&self, size_spec: &[usize], env: &Uiua) -> UiuaResult<Self> {
        if size_spec.len() > self.shape.len() {
            return Err(env.error(format!(
                "Window size {size_spec:?} has too many axes for shape {}",
                self.format_shape()
            )));
        }
        for (i, (size, sh)) in size_spec.iter().zip(&self.shape).enumerate() {
            if *size > *sh {
                return Err(env.error(format!(
                    "Cannot take window of size {size} along axis {i} of shape {}",
                    self.format_shape()
                )));
            }
        }
        // Determine the shape of the windows array
        let mut new_shape = Shape::with_capacity(self.shape.len() + size_spec.len());
        new_shape.extend(self.shape.iter().zip(size_spec).map(|(a, b)| a - b + 1));
        new_shape.extend_from_slice(size_spec);
        new_shape.extend_from_slice(&self.shape[size_spec.len()..]);
        // Make a new window shape with the same rank as the windowed array
        let mut true_size: Vec<usize> = Vec::with_capacity(self.shape.len());
        true_size.extend(size_spec);
        if true_size.len() < self.shape.len() {
            true_size.extend(&self.shape[true_size.len()..]);
        }

        let mut dst = Vec::new();
        let mut corner = vec![0; self.shape.len()];
        let mut curr = vec![0; self.shape.len()];
        'windows: loop {
            // Reset curr
            for i in curr.iter_mut() {
                *i = 0;
            }
            // Copy the window at the current corner
            'items: loop {
                // Copy the current item
                let mut src_index = 0;
                let mut stride = 1;
                for ((c, i), s) in corner.iter().zip(&curr).zip(&self.shape).rev() {
                    src_index += (*c + *i) * stride;
                    stride *= s;
                }
                dst.push(self.data[src_index].clone());
                // Go to the next item
                for i in (0..curr.len()).rev() {
                    if curr[i] == true_size[i] - 1 {
                        curr[i] = 0;
                    } else {
                        curr[i] += 1;
                        continue 'items;
                    }
                }
                break;
            }
            // Go to the next corner
            for i in (0..corner.len()).rev() {
                if corner[i] == self.shape[i] - true_size[i] {
                    corner[i] = 0;
                } else {
                    corner[i] += 1;
                    continue 'windows;
                }
            }
            break Ok(Array::new(new_shape, dst));
        }
    }
}

impl Value {
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, searched) {
            (Value::Num(a), Value::Num(b)) => a.find(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.find(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.find(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.find(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.find(&b.clone().convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().find(b, env)?.into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot find {} in {} array",
                    a.type_name(),
                    b.type_name(),
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn find(&self, searched: &Self, env: &Uiua) -> UiuaResult<Array<u8>> {
        if self.rank() > searched.rank() || self.row_count() > searched.row_count() {
            return Err(env.error(format!(
                "Cannot search for array of shape {} in array of shape {}",
                self.format_shape(),
                searched.format_shape()
            )));
        }

        // Pad the shape of the searched-for array
        let mut searched_for_shape = self.shape.clone();
        while searched_for_shape.len() < searched.shape.len() {
            searched_for_shape.insert(0, 1);
        }

        // Determine the ouput shape
        let output_shape: Shape = searched
            .shape
            .iter()
            .zip(&searched_for_shape)
            .map(|(a, b)| a + 1 - b)
            .collect();

        let mut data = Vec::new();
        let mut corner = vec![0; searched.shape.len()];
        let mut curr = vec![0; searched.shape.len()];

        'windows: loop {
            // Reset curr
            for i in curr.iter_mut() {
                *i = 0;
            }
            // Search the window whose top-left is the current corner
            'items: loop {
                // Get index for the current item in the searched array
                let mut searched_index = 0;
                let mut stride = 1;
                for ((c, i), s) in corner.iter().zip(&curr).zip(&searched.shape).rev() {
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
                let same = if let Some(searched_for) = self.data.get(search_for_index) {
                    searched.data[searched_index].array_eq(searched_for)
                } else {
                    false
                };
                if !same {
                    data.push(0);
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
                data.push(1);
                break;
            }
            // Go to the next corner
            for i in (0..corner.len()).rev() {
                if corner[i] == searched.shape[i] - searched_for_shape[i] {
                    corner[i] = 0;
                } else {
                    corner[i] += 1;
                    continue 'windows;
                }
            }
            let arr = Array::new(output_shape, data);
            arr.validate_shape();
            break Ok(arr);
        }
    }
}

impl Value {
    pub fn member(&self, of: &Self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, of) {
            (Value::Num(a), Value::Num(b)) => a.member(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.member(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.member(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.member(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.member(b, env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.member(b, env)?.into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot look for members of {} array in {} array",
                    a.type_name(),
                    b.type_name(),
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    pub fn member<U>(&self, of: &Array<U>, env: &Uiua) -> UiuaResult<Array<u8>>
    where
        T: ArrayCmp<U>,
        U: ArrayValue,
    {
        let elems = self;
        Ok(match elems.rank().cmp(&of.rank()) {
            Ordering::Equal => {
                let mut result_data = Vec::with_capacity(elems.row_count());
                'elem: for elem in elems.rows() {
                    for of in of.rows() {
                        if elem == of {
                            result_data.push(1);
                            continue 'elem;
                        }
                    }
                    result_data.push(0);
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data);
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(elems.row_count());
                for elem in elems.rows() {
                    rows.push(elem.member(of, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
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
        })
    }
}

impl Value {
    pub fn index_of(&self, searched_in: &Value, env: &Uiua) -> UiuaResult<Value> {
        Ok(match (self, searched_in) {
            (Value::Num(a), Value::Num(b)) => a.index_of(b, env)?.into(),
            (Value::Byte(a), Value::Byte(b)) => a.index_of(b, env)?.into(),
            (Value::Char(a), Value::Char(b)) => a.index_of(b, env)?.into(),
            (Value::Func(a), Value::Func(b)) => a.index_of(b, env)?.into(),
            (Value::Num(a), Value::Byte(b)) => a.index_of(&b.clone().convert(), env)?.into(),
            (Value::Byte(a), Value::Num(b)) => a.clone().convert().index_of(b, env)?.into(),
            (a, b) => {
                return Err(env.error(format!(
                    "Cannot look for indices of {} in {}",
                    a.type_name(),
                    b.type_name(),
                )))
            }
        })
    }
}

impl<T: ArrayValue> Array<T> {
    fn index_of(&self, searched_in: &Array<T>, env: &Uiua) -> UiuaResult<Array<f64>> {
        let searched_for = self;
        Ok(match searched_for.rank().cmp(&searched_in.rank()) {
            Ordering::Equal => {
                let mut result_data = Vec::with_capacity(searched_for.row_count());
                'elem: for elem in searched_for.rows() {
                    for (i, of) in searched_in.rows().enumerate() {
                        if elem == of {
                            result_data.push(i as f64);
                            continue 'elem;
                        }
                    }
                    result_data.push(searched_in.row_count() as f64);
                }
                let shape: Shape = self.shape.iter().cloned().take(1).collect();
                let res = Array::new(shape, result_data);
                res.validate_shape();
                res
            }
            Ordering::Greater => {
                let mut rows = Vec::with_capacity(searched_for.row_count());
                for elem in searched_for.rows() {
                    rows.push(elem.index_of(searched_in, env)?);
                }
                Array::from_row_arrays(rows, env)?
            }
            Ordering::Less => {
                if searched_in.rank() - searched_for.rank() == 1 {
                    (searched_in
                        .rows()
                        .position(|r| r == *searched_for)
                        .unwrap_or(searched_in.row_count()) as f64)
                        .into()
                } else {
                    let mut rows = Vec::with_capacity(searched_in.row_count());
                    for of in searched_in.rows() {
                        rows.push(searched_for.index_of(&of, env)?);
                    }
                    Array::from_row_arrays(rows, env)?
                }
            }
        })
    }
}
