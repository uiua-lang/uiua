use std::collections::{hash_map::Entry, HashMap};

use ecow::EcoVec;

use crate::{
    get_ops, grid_fmt::GridFmt, types::push_empty_rows_value, val_as_arr, Array, ArrayValue, Node,
    Ops, Primitive, SigNode, Uiua, UiuaResult, Value,
};

use super::{monadic::range, table::table_impl, validate_size};

pub fn tuples(ops: Ops, env: &mut Uiua) -> UiuaResult {
    let [f] = get_ops(ops, env)?;
    if f.sig.outputs() > 1 {
        return Err(env.error(format!(
            "{}'s function must have at most 1 output, \
            but its signature is {}",
            Primitive::Tuples.format(),
            f.sig
        )));
    } else if f.sig.args() > 1 && f.sig.outputs() == 0 {
        return Err(env.error(format!(
            "{}'s function must have 1 argument if it has 0 outputs, \
            but its signature is {}",
            Primitive::Tuples.format(),
            f.sig
        )));
    }
    match f.sig.args() {
        1 => tuple1(f, env)?,
        2 => tuple2(f, env)?,
        _ => {
            return Err(env.error(format!(
                "{}'s function must have 1 or 2 arguments, \
                but its signature is {}",
                Primitive::Tuples.format(),
                f.sig
            )))
        }
    }
    Ok(())
}

fn tuple1(f: SigNode, env: &mut Uiua) -> UiuaResult {
    let has_output = f.sig.outputs() == 1;
    let mut xs = env.pop(1)?;
    if xs.rank() == 0 {
        env.push(xs);
        return env.exec(f);
    }
    let mut results = Vec::new();
    let mut per_meta = xs.meta.take_per_meta();
    if xs.row_count() == 0 {
        xs.shape.insert(0, 0);
        if push_empty_rows_value(&f, [&xs], false, &mut per_meta, env) {
            return Ok(());
        } else {
            let mut proxy = xs.proxy_row(env);
            proxy.fix();
            env.push(proxy);
            _ = env.exec_maintain_sig(f);
            if has_output {
                results.push(env.pop("tuples' function result")?);
            }
        }
    } else {
        env.without_fill(|env| -> UiuaResult {
            for n in 1..=xs.row_count() {
                env.push(xs.slice_rows(0, n));
                env.exec(f.clone())?;
                if has_output {
                    results.push(env.pop("tuples's function result")?);
                }
            }
            Ok(())
        })?;
    }
    if has_output {
        let mut val = Value::from_row_values(results, env)?;
        if xs.row_count() == 0 {
            val.pop_row();
        }
        env.push(val);
    }
    Ok(())
}

fn tuple2(f: SigNode, env: &mut Uiua) -> UiuaResult {
    let k = env.pop(1)?;
    let mut xs = env.pop(2)?;

    let is_scalar = xs.rank() == 0;
    let n = if is_scalar {
        xs.as_nat(env, "Tuples of scalar must be a natural number")?
    } else {
        xs.row_count()
    };

    let k = if k.as_num(env, None).is_ok_and(|k| k == f64::INFINITY) {
        n
    } else {
        let k = k.as_int(env, "Tuple size must be an integer or infinity")?;
        if k >= 0 {
            k.unsigned_abs()
        } else {
            n.saturating_sub(k.unsigned_abs())
        }
    };

    'blk: {
        let res = match f.node.as_slice() {
            [Node::Prim(Primitive::Lt, _)] => xs.choose(k, false, false, env)?,
            [Node::Prim(Primitive::Le, _)] if n >= k => xs.choose(k, false, true, env)?,
            [Node::Prim(Primitive::Gt, _)] => xs.choose(k, true, false, env)?,
            [Node::Prim(Primitive::Ge, _)] if n >= k => xs.choose(k, true, true, env)?,
            [Node::Prim(Primitive::Ne, _)] => xs.permute(k, env)?,
            [Node::Prim(Primitive::Eq | Primitive::Match, _)] if is_scalar => {
                let n = xs.as_nat(env, "Tuples of scalar must be a natural number")?;
                env.push(n);
                return Ok(());
            }
            [Node::Prim(Primitive::Pop, _), Node::Prim(Primitive::Pop, _), Node::Push(val)] => {
                if let Ok(reps) = val.as_nat(env, None) {
                    if k > 2 && reps > 1 {
                        return Err(val
                            .as_bool(env, "tuples of 3 or more must return a boolean")
                            .unwrap_err());
                    }
                    if is_scalar {
                        ((n as f64).powi(k as i32) * reps as f64).into()
                    } else {
                        xs.permute_all(k, reps, env)?
                    }
                } else {
                    break 'blk;
                }
            }
            [Node::Prim(Primitive::Pop, _), Node::Prim(Primitive::Len, _)] => {
                if is_scalar {
                    ((n as f64).powi(k as i32)).into()
                } else {
                    xs.permute_all(k, 1, env)?
                }
            }
            _ => break 'blk,
        };
        env.push(res);
        return Ok(());
    }
    match k {
        0 if is_scalar => xs = 0.into(),
        0 => {
            xs = xs.first_dim_zero();
            xs.shape[0] = 1;
            xs.shape.insert(1, 0);
            xs.validate();
        }
        1 => {
            if is_scalar {
                xs = xs
                    .as_nat(env, "Tuples of scalar must be a natural number")?
                    .into();
            } else {
                xs.shape.insert(1, 1);
            }
        }
        2 => {
            let n = if is_scalar {
                xs.as_nat(env, "Tuples of scalar must be a natural number")?
            } else {
                xs.row_count()
            };
            let range: Value = match range(&[n as isize], env)? {
                Ok(data) => data.into(),
                Err(data) => data.into(),
            };
            env.push(range.clone());
            env.push(range);
            table_impl(f, env)?;
            let mut table = env.pop("table's function result")?;
            if table.rank() > 2 {
                return Err(env.error(format!(
                    "{}'s function must return a scalar, \
                    but the result is rank-{}",
                    Primitive::Tuples.format(),
                    table.rank() - 2
                )));
            }
            table.transpose();
            let table = table
                .as_natural_array(env, "tuples's function must return an array of naturals")?;
            if is_scalar {
                xs = table.data.into_iter().fold(0, usize::saturating_add).into();
            } else {
                let mut rows = Vec::new();
                for (i, counts) in table.row_slices().enumerate() {
                    for (j, &count) in counts.iter().enumerate() {
                        for _ in 0..count {
                            rows.push(xs.row(i));
                            rows.push(xs.row(j));
                        }
                    }
                }
                xs = Value::from_row_values(rows, env)?;
                xs.shape[0] /= 2;
                xs.shape.insert(1, 2);
                xs.validate();
            }
        }
        k => {
            fn inner<T: Clone>(
                arr: &Array<T>,
                k: usize,
                f: SigNode,
                is_scalar: bool,
                scalar: UiuaResult<usize>,
                env: &mut Uiua,
            ) -> UiuaResult<Value>
            where
                Value: From<Array<T>>,
            {
                let mut cache = HashMap::new();
                let mut curr = vec![0; k];
                let mut data = EcoVec::new();
                let mut count = 0;
                let row_count = if is_scalar { scalar? } else { arr.row_count() };
                let row_len = arr.row_len();
                'outer: loop {
                    // println!("curr: {curr:?}");
                    let mut add_it = true;
                    'ij: for (ii, &i) in curr.iter().enumerate() {
                        for &j in curr.iter().skip(ii + 1) {
                            // println!("i: {i}, j: {j}");
                            let entry = cache.entry((i, j));
                            add_it &= match entry {
                                Entry::Occupied(o) => *o.get(),
                                Entry::Vacant(v) => {
                                    env.push(i);
                                    env.push(j);
                                    env.exec(f.node.clone())?;
                                    *v.insert(env.pop("tuples's function result")?.as_bool(
                                        env,
                                        "tuples of 3 or more must return a boolean",
                                    )?)
                                }
                            };
                            if !add_it {
                                break 'ij;
                            }
                        }
                    }
                    if add_it {
                        if !is_scalar {
                            for &i in &curr {
                                data.extend_from_slice(&arr.data[i * row_len..][..row_len]);
                            }
                        }
                        count += 1;
                    }
                    // Increment curr
                    env.respect_execution_limit()?;
                    for i in (0..k).rev() {
                        if curr[i] == row_count - 1 {
                            curr[i] = 0;
                        } else {
                            curr[i] += 1;
                            continue 'outer;
                        }
                    }
                    break;
                }
                Ok(if is_scalar {
                    count.into()
                } else {
                    let mut shape = arr.shape.clone();
                    shape[0] = count;
                    shape.insert(1, k);
                    Array::new(shape, data).into()
                })
            }
            if xs.row_count() == 0 {
                env.push(if is_scalar {
                    0.into()
                } else {
                    xs.shape.insert(1, k);
                    xs
                });
                return Ok(());
            }
            let scalar = xs.as_nat(env, "Tuples of scalar must be a natural number");
            xs = match &xs {
                Value::Num(a) => inner(a, k, f, is_scalar, scalar, env)?,
                Value::Byte(a) => inner(a, k, f, is_scalar, scalar, env)?,
                Value::Complex(a) => inner(a, k, f, is_scalar, scalar, env)?,
                Value::Char(a) => inner(a, k, f, is_scalar, scalar, env)?,
                Value::Box(a) => inner(a, k, f, is_scalar, scalar, env)?,
            };
        }
    }
    env.push(xs);
    Ok(())
}

impl Value {
    /// `choose` all combinations of `k` rows from a value
    fn choose(self, k: usize, reverse: bool, same: bool, env: &Uiua) -> UiuaResult<Self> {
        if let Ok(n) = self.as_nat(env, None) {
            return Ok(combinations(n, k, same).into());
        }
        val_as_arr!(self, |a| a.choose(k, reverse, same, env).map(Into::into))
    }
    /// `permute` all combinations of `k` rows from a value
    fn permute(self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if let Ok(n) = self.as_nat(env, None) {
            return Ok(permutations(n, k).into());
        }
        val_as_arr!(self, |a| a.permute(k, env).map(Into::into))
    }
    fn permute_all(self, k: usize, reps: usize, env: &Uiua) -> UiuaResult<Self> {
        if let Ok(n) = self.as_nat(env, None) {
            return Ok((n as f64).powi(k as i32).into());
        }
        val_as_arr!(self, |a| a.permute_all(k, reps, env).map(Into::into))
    }
}

fn combinations(n: usize, k: usize, same: bool) -> f64 {
    if k > n {
        return 0.0;
    }
    let calc_n = if same { n + k - 1 } else { n };
    (1..=k.min(calc_n - k))
        .map(|i| (calc_n + 1 - i) as f64 / i as f64)
        .product::<f64>()
        .round()
}

fn permutations(n: usize, k: usize) -> f64 {
    if k > n {
        return 0.0;
    }
    (1..=n).rev().take(k).map(|i| i as f64).product()
}

impl<T: ArrayValue> Array<T> {
    /// `choose` all combinations of `k` rows from this array
    fn choose(&self, k: usize, rev: bool, same: bool, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot choose from scalar"));
        }
        let n = self.row_count();
        let mut shape = self.shape.clone();
        let combinations = combinations(n, k, same);
        if combinations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if combinations > usize::MAX as f64 {
            return Err(env.error(format!(
                "{} combinations would be too many",
                combinations.grid_string(false)
            )));
        }
        shape[0] = combinations.round() as usize;
        shape.insert(1, k);
        if n < k {
            return Ok(Array::new(shape, []));
        }
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let row_len = self.row_len();
        let at = |i| &self.data[i * row_len..][..row_len];
        Ok(match (k, n - k) {
            (1, _) => Array::new(shape, self.data.clone()),
            (_, 0) if !same => Array::new(shape, self.data.clone()),
            (_, 1) if !same => {
                let mut data = EcoVec::with_capacity(elem_count);
                if rev {
                    for i in (0..n).rev() {
                        for (j, row) in self.row_slices().enumerate().rev() {
                            if i != j {
                                data.extend_from_slice(row);
                            }
                        }
                    }
                } else {
                    for i in (0..n).rev() {
                        for (j, row) in self.row_slices().enumerate() {
                            if i != j {
                                data.extend_from_slice(row);
                            }
                        }
                    }
                }
                Array::new(shape, data)
            }
            (2, _) => {
                let mut data = EcoVec::with_capacity(elem_count);
                let mut add = |i, j| {
                    data.extend_from_slice(at(i));
                    data.extend_from_slice(at(j));
                };
                match (rev, same) {
                    (false, false) => (0..n - 1).for_each(|i| (i + 1..n).for_each(|j| add(i, j))),
                    (true, false) => (1..n).for_each(|i| (0..i).for_each(|j| add(i, j))),
                    (false, true) => (0..n).for_each(|i| (i..n).for_each(|j| add(i, j))),
                    (true, true) => (0..n).for_each(|i| (0..=i).for_each(|j| add(i, j))),
                }
                Array::new(shape, data)
            }
            (3, _) => {
                let mut data = EcoVec::with_capacity(elem_count);
                let mut add = |i, j, k| {
                    data.extend_from_slice(at(i));
                    data.extend_from_slice(at(j));
                    data.extend_from_slice(at(k));
                };
                match (rev, same) {
                    (false, false) => (0..n - 2).for_each(|i| {
                        (i + 1..n - 1).for_each(|j| (j + 1..n).for_each(|k| add(i, j, k)))
                    }),
                    (true, false) => {
                        (2..n).for_each(|i| (1..i).for_each(|j| (0..j).for_each(|k| add(i, j, k))))
                    }
                    (false, true) => {
                        (0..n).for_each(|i| (i..n).for_each(|j| (j..n).for_each(|k| add(i, j, k))))
                    }
                    (true, true) => (0..n)
                        .for_each(|i| (0..=i).for_each(|j| (0..=j).for_each(|k| add(i, j, k)))),
                }
                Array::new(shape, data)
            }
            _ => {
                let mut data = EcoVec::with_capacity(elem_count);
                let mut stack = vec![(Vec::new(), 0)];
                if rev {
                    while let Some((curr, start)) = stack.pop() {
                        if curr.len() == k {
                            for &i in &curr {
                                data.extend_from_slice(at(n - 1 - i));
                            }
                            continue;
                        }
                        for i in start..n {
                            let mut curr = curr.clone();
                            curr.push(i);
                            stack.push((curr, i + !same as usize));
                        }
                    }
                } else {
                    while let Some((curr, start)) = stack.pop() {
                        if curr.len() == k {
                            for &i in &curr {
                                data.extend_from_slice(at(i));
                            }
                            continue;
                        }
                        for i in (start..n).rev() {
                            let mut curr = curr.clone();
                            curr.push(i);
                            stack.push((curr, i + !same as usize));
                        }
                    }
                }
                Array::new(shape, data)
            }
        })
    }
    /// `permute` all combinations of `k` rows from this array
    fn permute(self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot permute scalar"));
        }
        let n = self.row_count();
        let mut shape = self.shape.clone();
        let permutations = permutations(n, k);
        if permutations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if permutations > usize::MAX as f64 {
            return Err(env.error(format!(
                "{} permutations would be too many",
                permutations.grid_string(false)
            )));
        }
        shape[0] = permutations.round() as usize;
        shape.insert(1, k);
        if n < k {
            return Ok(Array::new(shape, []));
        }
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let mut data = EcoVec::with_capacity(elem_count);
        let row_len = self.row_len();
        // It took me forever to find this algorithm
        let mut perm: Vec<usize> = (0..n).collect();
        let mut cycles: Vec<usize> = (n - k + 1..=n).rev().collect();
        for &i in &perm[..k] {
            data.extend_from_slice(&self.data[i * row_len..][..row_len]);
        }
        'outer: loop {
            for i in (0..k).rev() {
                cycles[i] -= 1;
                if cycles[i] == 0 {
                    perm[i..].rotate_left(1);
                    cycles[i] = n - i;
                } else {
                    let j = cycles[i];
                    perm.swap(i, n - j);
                    for &i in &perm[..k] {
                        data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                    }
                    continue 'outer;
                }
            }
            break;
        }
        Ok(Array::new(shape, data))
    }
    fn permute_all(self, k: usize, reps: usize, env: &Uiua) -> UiuaResult<Self> {
        let n = self.row_count();
        let permutations = (n as f64).powi(k as i32) * reps as f64;
        if permutations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if permutations > usize::MAX as f64 {
            return Err(env.error(format!(
                "{} permutations would be too many",
                permutations.grid_string(false)
            )));
        }
        let mut shape = self.shape.clone();
        shape[0] = permutations.round() as usize;
        shape.insert(1, k);
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let mut data = EcoVec::with_capacity(elem_count);
        let row_len = self.row_len();
        let mut curr = vec![0; k];
        'outer: loop {
            for _ in 0..reps {
                for &i in &curr {
                    data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                }
            }
            // Increment curr
            for i in (0..k).rev() {
                if curr[i] == n - 1 {
                    curr[i] = 0;
                } else {
                    curr[i] += 1;
                    continue 'outer;
                }
            }
            break;
        }
        Ok(Array::new(shape, data))
    }
}
