use std::collections::HashSet;

use ecow::EcoVec;

use crate::{val_as_arr, Array, ArrayValue, Uiua, UiuaResult, Value};

use super::validate_size;

impl Value {
    /// `choose` all combinations of `k` rows from a value
    pub fn choose(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let k = self.as_nat(env, "Choose k must be an integer")?;
        if let Ok(n) = from.as_nat(env, "") {
            return combinations(n, k, env).map(Into::into);
        }
        val_as_arr!(from, |a| a.choose(k, env).map(Into::into))
    }
    /// `permute` all combinations of `k` rows from a value
    pub fn permute(&self, from: &Self, env: &Uiua) -> UiuaResult<Self> {
        let k = self.as_nat(env, "Permute k must be an integer")?;
        if let Ok(n) = from.as_nat(env, "") {
            return permutations(n, k, env).map(Into::into);
        }
        val_as_arr!(from, |a| a.permute(k, env).map(Into::into))
    }
}

fn combinations(n: usize, k: usize, env: &Uiua) -> UiuaResult<f64> {
    if k > n {
        return Err(env.error(format!(
            "Cannot choose combinations of {k} rows \
            from array of shape {}",
            n
        )));
    }
    Ok((1..=k.min(n - k))
        .map(|i| (n + 1 - i) as f64 / i as f64)
        .product())
}

fn permutations(n: usize, k: usize, env: &Uiua) -> UiuaResult<f64> {
    if k > n {
        return Err(env.error(format!(
            "Cannot get permutations of {k} rows \
            from array of shape {}",
            n
        )));
    }
    Ok((1..=n).rev().take(k).map(|i| i as f64).product())
}

impl<T: ArrayValue> Array<T> {
    /// `choose` all combinations of `k` rows from this array
    pub fn choose(&self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot choose from scalar array"));
        }
        let n = self.row_count();
        let mut shape = self.shape.clone();
        let combinations = combinations(n, k, env)?;
        if combinations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if combinations > usize::MAX as f64 {
            return Err(env.error(format!("{combinations} combinations would be too many")));
        }
        shape[0] = combinations.round() as usize;
        shape.insert(1, k);
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let row_len = self.row_len();
        Ok(match (k, n - k) {
            (1, _) | (_, 0) => Array::new(shape, self.data.clone()),
            (_, 1) => {
                let mut data = EcoVec::with_capacity(elem_count);
                for i in (0..n).rev() {
                    for (j, row) in self.row_slices().enumerate() {
                        if i != j {
                            data.extend_from_slice(row);
                        }
                    }
                }
                Array::new(shape, data)
            }
            (2, _) => {
                let mut data = EcoVec::with_capacity(elem_count);
                for i in 0..n - 1 {
                    for j in i + 1..n {
                        data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                        data.extend_from_slice(&self.data[j * row_len..][..row_len]);
                    }
                }
                Array::new(shape, data)
            }
            (3, _) => {
                let mut data = EcoVec::with_capacity(elem_count);
                for i in 0..n - 2 {
                    for j in i + 1..n - 1 {
                        for k in j + 1..n {
                            data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                            data.extend_from_slice(&self.data[j * row_len..][..row_len]);
                            data.extend_from_slice(&self.data[k * row_len..][..row_len]);
                        }
                    }
                }
                Array::new(shape, data)
            }
            _ => {
                let mut data = EcoVec::with_capacity(elem_count);
                let mut indices = vec![0; k];
                'outer: loop {
                    env.respect_execution_limit()?;
                    if (indices.iter())
                        .zip(indices.iter().skip(1))
                        .all(|(a, b)| a > b)
                    {
                        for &i in indices.iter().rev() {
                            data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                        }
                    }
                    // Increment indices
                    for i in 0..k {
                        indices[i] += 1;
                        if indices[i] == n {
                            indices[i] = 0;
                        } else {
                            continue 'outer;
                        }
                    }
                    break;
                }
                Array::new(shape, data)
            }
        })
    }
    /// `permute` all combinations of `k` rows from this array
    pub fn permute(&self, k: usize, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return Err(env.error("Cannot permute scalar array"));
        }
        let n = self.row_count();
        let mut shape = self.shape.clone();
        let permutations = permutations(n, k, env)?;
        if permutations.is_nan() {
            return Err(env.error("Combinatorial explosion"));
        }
        if permutations > usize::MAX as f64 {
            return Err(env.error(format!("{permutations} permutations would be too many")));
        }
        shape[0] = permutations.round() as usize;
        shape.insert(1, k);
        let elem_count = validate_size::<T>(shape.iter().copied(), env)?;
        let mut data = EcoVec::with_capacity(elem_count);
        let row_len = self.row_len();
        let mut indices = vec![0; k];
        let mut set = HashSet::with_capacity(k);
        'outer: loop {
            env.respect_execution_limit()?;
            set.clear();
            if indices.iter().all(|&i| set.insert(i)) {
                for &i in indices.iter().rev() {
                    data.extend_from_slice(&self.data[i * row_len..][..row_len]);
                }
            }
            // Increment indices
            for i in 0..k {
                indices[i] += 1;
                if indices[i] == n {
                    indices[i] = 0;
                } else {
                    continue 'outer;
                }
            }
            break;
        }
        Ok(Array::new(shape, data))
    }
}
