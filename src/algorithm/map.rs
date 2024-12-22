use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::repeat,
    mem::{replace, take},
};

use ecow::EcoVec;
use serde::*;

use crate::{
    algorithm::ArrayCmpSlice, val_as_arr, Array, ArrayValue, Boxed, Complex, FormatShape, Uiua,
    UiuaResult, Value,
};

use super::{ErrorContext, FillContext};

impl<T: ArrayValue> Array<T> {
    /// Check if the array is a map
    pub fn is_map(&self) -> bool {
        self.meta().map_keys.as_ref().is_some()
    }
    /// Get the keys and values of a map array
    pub fn map_kv(&self) -> impl Iterator<Item = (Value, Array<T>)> {
        self.map_kv_inner().into_iter()
    }
    fn map_kv_inner(&self) -> Vec<(Value, Array<T>)> {
        let Some(map_keys) = self.meta().map_keys.as_ref() else {
            return Vec::new();
        };
        let mut kv = Vec::with_capacity(map_keys.len);
        let mut ki: Vec<_> = (map_keys.keys.rows())
            .zip(&map_keys.indices)
            .filter(|(k, _)| !k.is_all_empty_cell() && !k.is_all_tombstone())
            .collect();
        ki.sort_unstable_by_key(|(_, i)| *i);
        for (key, index) in ki {
            if *index < self.row_count() {
                kv.push((key, self.row(*index)));
            }
        }
        kv
    }
    /// Create a map array
    pub fn map<C: ErrorContext>(&mut self, keys: Value, ctx: &C) -> Result<(), C::Error> {
        let values = self;
        if keys.row_count() != values.row_count() {
            return Err(ctx.error(format!(
                "Map array's keys and values must have the same length, but they have lengths {} and {}",
                keys.row_count(),
                values.row_count()
            )));
        }
        let mut map_keys = MapKeys {
            keys: keys.clone(),
            indices: Vec::new(),
            len: 0,
            fix_stack: Vec::new(),
        };
        let mut to_remove = Vec::new();
        for (i, key) in keys.into_rows().enumerate() {
            let replaced = map_keys.insert(key, i, ctx)?;
            to_remove.extend(replaced);
        }
        for i in to_remove.into_iter().rev() {
            values.remove_row(i);
            for index in &mut map_keys.indices {
                if *index > i {
                    *index -= 1;
                }
            }
        }
        values.meta_mut().map_keys = Some(map_keys);
        Ok(())
    }
}

impl Value {
    /// Check if the value is a map
    pub fn is_map(&self) -> bool {
        self.meta().map_keys.as_ref().is_some()
    }
    /// Get the keys and values of a map array
    pub fn map_kv(&self) -> Vec<(Value, Value)> {
        val_as_arr!(self, |arr| arr
            .map_kv()
            .map(|(k, v)| (k, v.into()))
            .collect())
    }
    /// Create a map array
    pub fn map(&mut self, keys: Self, env: &Uiua) -> UiuaResult {
        val_as_arr!(self, |arr| arr.map(keys, env))
    }
    /// Turn a map array into its keys and values
    pub fn unmap(mut self, env: &Uiua) -> UiuaResult<(Value, Value)> {
        let keys = self
            .take_map_keys()
            .ok_or_else(|| env.error("Value is not a map"))?;
        Ok((keys.normalized(), self))
    }
    /// Get a value from a map array
    pub fn get(&self, key: &Value, env: &Uiua) -> UiuaResult<Value> {
        // Check for higher-ranked keys
        if let Some(keys) = self.meta().map_keys.as_ref() {
            if key.rank() == keys.keys.rank() && key.type_id() == keys.keys.type_id() {
                let mut values = Vec::with_capacity(key.row_count());
                for key in key.rows() {
                    values.push(self.get(&key, env)?);
                }
                return Value::from_row_values(values, env);
            }
        }
        // An empty array cannot have the key
        if self.row_count() == 0 {
            return (env.value_fill().cloned()).ok_or_else(|| env.error("Key not found in map"));
        }

        let keys =
            (self.meta().map_keys.as_ref()).ok_or_else(|| env.error("Value is not a map"))?;
        if keys.len != self.row_count() {
            return Err(env.error(format!(
                "Cannot read from map with {} keys and {} value(s)",
                keys.len,
                self.row_count()
            )));
        }
        if let Some(index) = keys.get(key) {
            if index >= self.row_count() {
                return Err(env.error("Map was corrupted"));
            }
            Ok(self.row(index))
        } else {
            env.value_fill()
                .cloned()
                .ok_or_else(|| env.error("Key not found in map"))
        }
    }
    /// Check if a map array contains a key
    pub fn has_key(&self, key: &Value, env: &Uiua) -> UiuaResult<Array<u8>> {
        if let Some(keys) = self.meta().map_keys.as_ref() {
            if key.rank() == keys.keys.rank() && key.type_id() == keys.keys.type_id() {
                let mut values = EcoVec::with_capacity(key.row_count());
                for key in key.rows() {
                    values.push(keys.get(&key).is_some().into());
                }
                return Ok(values.into());
            }
        }

        if self.row_count() == 0 {
            return Ok(0.into());
        }
        let keys =
            (self.meta().map_keys.as_ref()).ok_or_else(|| env.error("Value is not a map"))?;
        Ok(keys.get(key).is_some().into())
    }
    /// Insert a key-value pair into a map array
    #[allow(clippy::unit_arg)]
    pub fn insert(&mut self, key: Value, value: Value, env: &Uiua) -> UiuaResult {
        // if value.rank() == self.rank() && value.type_id() == self.type_id() {
        //     if key.row_count() != value.row_count() {
        //         return Err(env.error(format!(
        //             "You appear to be inserting multiple keys. \
        //             Inserted keys and values must have the same length, \
        //             but their shapes are {} and {}",
        //             key.shape(),
        //             value.shape()
        //         )));
        //     }
        //     for (key, value) in key.into_rows().zip(value.into_rows()) {
        //         self.insert(key, value, env)?;
        //     }
        //     return Ok(());
        // }

        if !self.is_map() && self.row_count() == 0 {
            self.map(Value::default(), env)?;
        }
        if self.rank() == 0 {
            self.shape_mut().insert(0, 1);
        }
        let row_count = self.row_count();
        let mut keys = self
            .take_map_keys()
            .ok_or_else(|| env.error("Value is not a map"))?;
        if keys.len != row_count {
            return Err(env.error(format!(
                "Cannot insert into map with {} keys and {} value(s)",
                keys.len, row_count
            )));
        }
        let curr_index = keys.get(&key);
        let index = curr_index.unwrap_or(row_count);
        keys.insert(key, index, env)?;
        let value = coerce_values(self, value, "insert", "value into map with", "values")
            .map_err(|e| env.error(e))?;
        if curr_index.is_some() {
            self.generic_bin_mut(
                value,
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |a, b| {
                    env.error(format!(
                        "Cannot insert {} value into map with {} values",
                        b.type_name(),
                        a.type_name()
                    ))
                },
            )?;
        } else {
            self.append(value, false, env)?;
        }
        self.meta_mut().map_keys = Some(keys);
        Ok(())
    }
    #[allow(clippy::unit_arg)]
    pub(crate) fn insert_at(
        &mut self,
        index: Value,
        key: Value,
        value: Value,
        env: &Uiua,
    ) -> UiuaResult {
        let index = index.as_nat(env, "Index must be a non-negative integer")?;
        if index > self.row_count() {
            return Err(env.error(format!(
                "Index {} is out of bounds for map with {} entries",
                index,
                self.row_count()
            )));
        }
        let mut keys = (self.take_map_keys()).ok_or_else(|| env.error("Value is not a map"))?;
        for i in &mut keys.indices {
            if *i >= index {
                *i += 1;
            }
        }
        if keys.insert(key, index, env)?.is_some() {
            self.generic_bin_mut(
                value,
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |arr, value| Ok(arr.set_row(index, value)),
                |a, b| {
                    env.error(format!(
                        "Cannot insert {} value into map with {} values",
                        b.type_name(),
                        a.type_name()
                    ))
                },
            )?;
        } else {
            self.generic_bin_mut(
                value,
                |arr, value| Ok(arr.insert_row(index, value)),
                |arr, value| Ok(arr.insert_row(index, value)),
                |arr, value| Ok(arr.insert_row(index, value)),
                |arr, value| Ok(arr.insert_row(index, value)),
                |arr, value| Ok(arr.insert_row(index, value)),
                |a, b| {
                    env.error(format!(
                        "Cannot insert {} value into map with {} values",
                        b.type_name(),
                        a.type_name()
                    ))
                },
            )?;
        };
        self.meta_mut().map_keys = Some(keys);
        Ok(())
    }
    /// Return a key's value to what it used to be, including if it didn't exist before
    pub fn undo_insert(&mut self, key: Value, original: &Self, env: &Uiua) -> UiuaResult {
        let orig_keys =
            (original.meta().map_keys.as_ref()).ok_or_else(|| env.error("Value was not a map"))?;
        if let Some(index) = orig_keys.get(&key) {
            self.insert_at(index.into(), key, original.row(index), env)?;
        } else {
            self.remove(key, env)?;
        }
        Ok(())
    }
    /// Remove a key-value pair from a map array
    pub fn remove(&mut self, key: Value, env: &Uiua) -> UiuaResult {
        if let Some(keys) = self.meta().map_keys.as_ref() {
            if key.rank() == keys.keys.rank() && key.type_id() == keys.keys.type_id() {
                for key in key.into_rows() {
                    self.remove(key, env)?;
                }
                return Ok(());
            }
        }

        if self.row_count() == 0 {
            return Ok(());
        }
        let row_count = self.row_count();
        let keys = (self.get_meta_mut().and_then(|m| m.map_keys.as_mut()))
            .ok_or_else(|| env.error("Value is not a map"))?;
        if keys.len != row_count {
            return Err(env.error(format!(
                "Cannot remove from map with {} keys and {} value(s)",
                keys.len, row_count
            )));
        }
        if let Some(index) = keys.remove(key, env)? {
            if index >= row_count {
                return Err(env.error("Map was corrupted"));
            }

            // Decrement indices greater than the removed index
            for i in &mut keys.indices {
                if *i > index {
                    *i -= 1;
                }
            }

            val_as_arr!(self, |arr| arr.remove_row(index))
        }
        Ok(())
    }
    /// Re-insert a key-value pair to a modified map array if it got removed
    pub fn undo_remove(&mut self, key: Value, original: &Self, env: &Uiua) -> UiuaResult {
        let keys =
            (original.meta().map_keys.as_ref()).ok_or_else(|| env.error("Value wasn't a map"))?;
        if let Some(index) = keys.get(&key) {
            self.insert_at(index.into(), key, original.row(index), env)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MapKeys {
    pub(crate) keys: Value,
    indices: Vec<usize>,
    len: usize,
    fix_stack: Vec<(usize, Vec<usize>)>,
}

impl MapKeys {
    fn capacity(&self) -> usize {
        self.indices.len()
    }
    fn grow(&mut self) {
        if self.capacity() == 0 || (self.len as f64 / self.capacity() as f64) > LOAD_FACTOR {
            self.grow_to((self.capacity() * 2).max(1));
        }
    }
    fn grow_to(&mut self, new_capacity: usize) {
        {
            if let Value::Byte(keys) = &self.keys {
                self.keys = Value::Num(keys.convert_ref());
            }
        }
        match &mut self.keys {
            Value::Num(a) => Self::grow_impl(a, &mut self.indices, new_capacity),
            Value::Complex(a) => Self::grow_impl(a, &mut self.indices, new_capacity),
            Value::Char(a) => Self::grow_impl(a, &mut self.indices, new_capacity),
            Value::Box(a) => Self::grow_impl(a, &mut self.indices, new_capacity),
            Value::Byte(_) => unreachable!(),
        }
    }
    fn grow_impl<K>(keys: &mut Array<K>, indices: &mut Vec<usize>, new_capacity: usize)
    where
        K: MapItem + ArrayValue,
    {
        let key_row_len = keys.row_len();
        let mut keys_shape = keys.shape.clone();
        if let Some(len) = keys_shape.first_mut() {
            *len = new_capacity;
        } else if new_capacity > 1 {
            keys_shape = new_capacity.into();
        }
        let old_keys = take(keys).into_rows();
        let old_indices = take(indices);
        *keys = Array::new(
            keys_shape,
            repeat(K::empty_cell())
                .take(new_capacity * key_row_len)
                .collect::<EcoVec<_>>(),
        );
        *indices = vec![0; new_capacity];
        let key_data = keys.data.as_mut_slice();
        for (key, index) in old_keys.zip(old_indices) {
            let start = hash_start(&key, new_capacity);
            let mut key_index = start;
            loop {
                let cell_key =
                    &mut key_data[key_index * key_row_len..(key_index + 1) * key_row_len];
                if cell_key.is_empty() || cell_key[0].is_any_empty_cell() {
                    cell_key.clone_from_slice(&key.data);
                    indices[key_index] = index;
                    break;
                }
                key_index = (key_index + 1) % new_capacity;
            }
        }
    }
    fn insert<C>(&mut self, key: Value, index: usize, ctx: &C) -> Result<Option<usize>, C::Error>
    where
        C: ErrorContext,
    {
        fn insert_impl<K>(
            keys: &mut Array<K>,
            indices: &mut [usize],
            key: Array<K>,
            index: usize,
            len: &mut usize,
            capacity: usize,
        ) -> Result<Option<usize>, (Array<K>, usize)>
        where
            K: MapItem + ArrayValue,
        {
            let start = hash_start(&key, capacity);
            let mut key_index = start;
            let key_row_len = keys.row_len();
            let key_data = keys.data.as_mut_slice();
            loop {
                let cell_key = &key_data[key_index * key_row_len..(key_index + 1) * key_row_len];
                let present = if cell_key.is_empty() {
                    *len == 1
                } else if cell_key[0].is_any_empty_cell() {
                    false
                } else if cell_key[0].is_any_tombstone() {
                    // Probe for the matching key
                    let orig = key_index;
                    loop {
                        key_index = (key_index + 1) % capacity;
                        if key_index == orig {
                            break false;
                        }
                        let cell_key =
                            &key_data[key_index * key_row_len..(key_index + 1) * key_row_len];
                        if cell_key[0].is_any_tombstone() {
                            continue;
                        } else if cell_key[0].is_any_empty_cell() {
                            key_index = orig;
                            break false;
                        } else if ArrayCmpSlice(cell_key) == ArrayCmpSlice(&key.data) {
                            break true;
                        } else {
                            continue;
                        }
                    }
                } else {
                    true
                };
                let cell_key =
                    &mut key_data[key_index * key_row_len..(key_index + 1) * key_row_len];
                if !present || ArrayCmpSlice(cell_key) == ArrayCmpSlice(&key.data) {
                    if !present {
                        *len += 1;
                    }
                    cell_key.clone_from_slice(&key.data);
                    let replaced = if present {
                        Some(indices[key_index])
                    } else {
                        None
                    };
                    indices[key_index] = index;
                    break Ok(replaced);
                }
                key_index = (key_index + 1) % capacity;
                if key_index == start {
                    break Err((key, index));
                }
            }
        }
        let key = coerce_values(&mut self.keys, key, "insert", "key into map with", "keys")
            .map_err(|e| ctx.error(e))?;
        if self.capacity() == 0 {
            self.grow();
        }
        let capacity = self.capacity();
        macro_rules! do_insert {
            ($($k:ident),*) => {
                match (&mut self.keys, key) {
                    $((Value::$k(keys), Value::$k(key)) => {
                        match insert_impl(keys, &mut self.indices, key, index, &mut self.len, capacity) {
                            Ok(replaced) => replaced,
                            Err((key, index)) => {
                                self.grow();
                                return self.insert(key.into(), index, ctx);
                            }
                        }
                    })*
                    _ => unreachable!(),
                }
            }
        }
        let replaced = do_insert!(Num, Complex, Char, Box);
        self.grow();
        Ok(replaced)
    }
    fn get(&self, key: &Value) -> Option<usize> {
        if self.keys.row_count() == 0 {
            return None;
        }
        let start = val_as_arr!(key, |a| hash_start(a, self.capacity()));
        let mut key_index = start;
        loop {
            let row_key = self.keys.row(key_index);
            if key.unpacked_ref() == row_key.unpacked_ref() {
                return Some(self.indices[key_index]);
            }
            if row_key.is_any_empty_cell() {
                return None;
            }
            key_index = (key_index + 1) % self.capacity();
            if key_index == start {
                break None;
            }
        }
    }
    fn remove(&mut self, key: Value, env: &Uiua) -> UiuaResult<Option<usize>> {
        fn remove_impl<K>(
            keys: &mut Array<K>,
            key: Array<K>,
            indices: &mut [usize],
            len: &mut usize,
            capacity: usize,
        ) -> Option<usize>
        where
            K: MapItem + ArrayValue,
        {
            let start = hash_start(&key, capacity);
            let mut key_index = start;
            let key_row_len = keys.row_len();
            let key_data = keys.data.as_mut_slice();
            loop {
                let cell_key =
                    &mut key_data[key_index * key_row_len..(key_index + 1) * key_row_len];
                if ArrayCmpSlice(cell_key) == ArrayCmpSlice(&key.data) {
                    *len -= 1;
                    for elem in cell_key {
                        *elem = K::tombstone_cell();
                    }
                    break Some(take(&mut indices[key_index]));
                }
                if cell_key[0].is_any_empty_cell() {
                    break None;
                }
                key_index = (key_index + 1) % capacity;
                if key_index == start {
                    break None;
                }
            }
        }
        let key = coerce_values(&mut self.keys, key, "remove", "key from map with", "keys")
            .map_err(|e| env.error(e))?;
        let capacity = self.capacity();
        macro_rules! do_remove {
            ($($k:ident),*) => {
                match (&mut self.keys, key) {
                    $((Value::$k(keys), Value::$k(key)) => {
                        Ok(remove_impl(keys, key, &mut self.indices, &mut self.len, capacity))
                    })*
                    (keys, key) => {
                        let keys = keys.type_name();
                        let key = key.type_name();
                        Err(env.error(format!(
                            "Cannot remove {key} key from map with {keys} keys",
                        )))
                    }
                }
            }
        }
        do_remove!(Num, Complex, Char, Box)
    }
    pub fn normalized(mut self) -> Value {
        let mut fix_count = 0;
        while self.unfix() {
            fix_count += 1;
        }
        let mut key_pairs: Vec<_> = self.keys.into_rows().zip(&self.indices).collect();
        key_pairs.sort_unstable_by_key(|(_, i)| *i);
        let mut keys = remove_empty_rows(key_pairs.into_iter().map(|(k, _)| k));
        for _ in 0..fix_count {
            keys.fix();
        }
        keys
    }
    pub(crate) fn fix(&mut self) {
        self.keys.fix();
        let indices = replace(&mut self.indices, vec![0]);
        let len = replace(&mut self.len, 1);
        self.fix_stack.push((len, indices));
    }
    pub(crate) fn unfix(&mut self) -> bool {
        if let Some((len, indices)) = self.fix_stack.pop() {
            self.len = len;
            self.indices = indices;
            self.keys.undo_fix();
            true
        } else {
            false
        }
    }
    fn present_indices(&self) -> Vec<usize> {
        let mut present_indices: Vec<_> = (self.keys.rows().enumerate())
            .filter(|(_, k)| !k.is_any_empty_cell() && !k.is_any_tombstone())
            .map(|(i, _)| (i, self.indices[i]))
            .collect();
        present_indices.sort_unstable_by_key(|(_, i)| *i);
        present_indices.into_iter().map(|(i, _)| i).collect()
    }
    pub(crate) fn reverse(&mut self) {
        let present_indices = self.present_indices();
        let mid = present_indices.len() / 2;
        for (&a, &b) in (present_indices.iter().take(mid)).zip(present_indices.iter().rev()) {
            self.indices.swap(a, b);
        }
    }
    pub(crate) fn rotate(&mut self, by: isize) {
        let present_indices = self.present_indices();
        let len = present_indices.len();
        if len == 0 {
            return;
        }
        let by = (-by).rem_euclid(len as isize) as usize;
        if by == 0 {
            return;
        }
        let old = self.indices.clone();
        for (i, &index) in present_indices.iter().enumerate() {
            let new_index = (i + by) % len;
            self.indices[index] = old[present_indices[new_index]];
        }
    }
    pub(crate) fn drop(&mut self, mut n: usize) {
        let present_indices = self.present_indices();
        n = n.min(present_indices.len());
        let dropped = &present_indices[..n];
        match &mut self.keys {
            Value::Num(keys) => set_tombstones(keys, dropped),
            Value::Complex(keys) => set_tombstones(keys, dropped),
            Value::Char(keys) => set_tombstones(keys, dropped),
            Value::Box(keys) => set_tombstones(keys, dropped),
            Value::Byte(keys) => {
                let mut nums = keys.convert_ref();
                set_tombstones(&mut nums, dropped);
                self.keys = Value::Num(nums);
            }
        }
        for &not_dropped in &present_indices[n..] {
            self.indices[not_dropped] -= n;
        }
        self.len -= n;
    }
    pub(crate) fn take(&mut self, mut n: usize) {
        let present_indices = self.present_indices();
        n = n.min(present_indices.len());
        let not_taken = &present_indices[n..];
        match &mut self.keys {
            Value::Num(keys) => set_tombstones(keys, not_taken),
            Value::Complex(keys) => set_tombstones(keys, not_taken),
            Value::Char(keys) => set_tombstones(keys, not_taken),
            Value::Box(keys) => set_tombstones(keys, not_taken),
            Value::Byte(keys) => {
                let mut nums = keys.convert_ref();
                set_tombstones(&mut nums, not_taken);
                self.keys = Value::Num(nums);
            }
        }
        self.len = n;
    }
    pub(crate) fn join<C>(&mut self, mut other: Self, ctx: &C) -> Result<Vec<usize>, C::Error>
    where
        C: FillContext,
    {
        for index in &mut other.indices {
            *index += self.len;
        }
        if self.keys.rank() == 0 {
            self.keys.shape_mut().insert(0, 1);
        }
        if other.keys.rank() == 0 {
            other.keys.shape_mut().insert(0, 1);
        }
        let mut to_remove = Vec::new();
        let mut to_insert: Vec<_> = (other.keys.into_rows())
            .zip(other.indices)
            .filter(|(k, _)| !k.is_any_empty_cell() && !k.is_any_tombstone())
            .collect();
        to_insert.sort_unstable_by_key(|(_, i)| *i);
        for (key, index) in to_insert {
            if let Some(replaced) = self.insert(key, index, ctx)? {
                to_remove.push(replaced);
            }
        }
        for &r in &to_remove {
            for i in &mut self.indices {
                if *i > r {
                    *i -= 1;
                }
            }
        }
        Ok(to_remove)
    }
}

impl PartialEq for MapKeys {
    fn eq(&self, other: &Self) -> bool {
        self.len == other.len
            && self.fix_stack == other.fix_stack
            && (self.keys.rows().zip(&self.indices))
                .filter(|(k, _)| !k.is_any_empty_cell() && !k.is_any_tombstone())
                .all(|(k, i)| other.get(&k) == Some(*i))
            && (other.keys.rows().zip(&other.indices))
                .filter(|(k, _)| !k.is_any_empty_cell() && !k.is_any_tombstone())
                .all(|(k, i)| self.get(&k) == Some(*i))
    }
}

impl Eq for MapKeys {}

impl Hash for MapKeys {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len.hash(state);
        self.fix_stack.hash(state);
        for (k, index) in self.keys.rows().zip(&self.indices) {
            if !k.is_any_empty_cell() && !k.is_any_tombstone() {
                k.hash(state);
                index.hash(state);
            }
        }
    }
}

fn set_tombstones<T: MapItem + Clone>(arr: &mut Array<T>, indices: &[usize]) {
    let row_len = arr.row_len();
    let data = arr.data.as_mut_slice();
    for &i in indices {
        let start = i * row_len;
        for elem in &mut data[start..start + row_len] {
            *elem = T::tombstone_cell();
        }
    }
}

/// Filter out placeholder map rows and collect into a new `Value`
pub fn remove_empty_rows(iter: impl ExactSizeIterator<Item = Value>) -> Value {
    Value::from_row_values_infallible(
        iter.filter(|row| !(row.is_any_empty_cell() || row.is_any_tombstone()))
            .collect::<Vec<_>>(),
    )
}

const LOAD_FACTOR: f64 = 0.75;

// A NaN value used as empty, not the standard NaN.
pub const EMPTY_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0001) };
// A NaN value used as a tombstone, not the standard NaN.
pub const TOMBSTONE_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0002) };
// A character value used as empty
pub const EMPTY_CHAR: char = '\u{100001}';
// A character value used as a tombstone
pub const TOMBSTONE_CHAR: char = '\u{100002}';

#[track_caller]
fn hash_start<T: ArrayValue>(arr: &Array<T>, capacity: usize) -> usize {
    let mut hasher = DefaultHasher::new();
    arr.hash(&mut hasher);
    hasher.finish() as usize % capacity.max(1)
}

fn coerce_values(
    a: &mut Value,
    #[allow(unused_mut)] mut b: Value,
    action1: &'static str,
    action2: &'static str,
    action3: &'static str,
) -> Result<Value, String> {
    {
        if let Value::Byte(keys) = a {
            *a = Value::Num(keys.convert_ref());
        }
        if let Value::Byte(values) = b {
            b = Value::Num(values.convert_ref());
        }
    }
    if a.shape() == [0] {
        let mut b_clone = b.clone();
        b_clone.shape_mut().insert(0, 1);
        *a = b_clone.first_dim_zero();
        return Ok(b);
    }
    if let Value::Box(a) = a {
        if a.rank() == 1 && !matches!(b, Value::Box(_)) {
            return Ok(Boxed(b).into());
        }
    }
    if let Value::Box(b_arr) = &b {
        if b_arr.rank() == 0 && !matches!(a, Value::Box(_)) {
            *a = Array::from_iter(a.rows().map(Boxed)).into();
            return Ok(b);
        }
    }
    match (&mut *a, b) {
        (Value::Num(arr), Value::Num(item)) if arr.rank() > 0 && arr.shape[1..] != item.shape => {
            Err(format!(
                "Cannot {action1} shape {} {action2} shape {} {action3}",
                item.shape(),
                FormatShape(&arr.shape()[1..])
            ))
        }
        (Value::Complex(arr), Value::Complex(item))
            if arr.rank() > 0 && arr.shape[1..] != item.shape =>
        {
            Err(format!(
                "Cannot {action1} shape {} {action2} shape {} {action3}",
                item.shape(),
                FormatShape(&arr.shape()[1..])
            ))
        }
        (Value::Box(arr), Value::Box(item)) if arr.rank() > 0 && arr.shape[1..] != item.shape => {
            Err(format!(
                "Cannot {action1} shape {} {action2} shape {} {action3}",
                item.shape(),
                FormatShape(&arr.shape()[1..])
            ))
        }
        (val @ Value::Num(_), owned @ Value::Num(_))
        | (val @ Value::Complex(_), owned @ Value::Complex(_))
        | (val @ Value::Char(_), owned @ Value::Char(_))
        | (val @ Value::Box(_), owned @ Value::Box(_)) => {
            if val.rank() > 0 && &val.shape()[1..] != owned.shape() {
                Err(format!(
                    "Cannot {action1} shape {} {action2} shape {} {action3}",
                    owned.shape(),
                    FormatShape(&val.shape()[1..])
                ))
            } else {
                Ok(owned)
            }
        }
        (Value::Box(_), Value::Num(num)) => Ok(Value::Box(Array::from(Boxed(Value::from(num))))),
        (Value::Box(_), Value::Char(ch)) => Ok(Value::Box(Array::from(Boxed(Value::from(ch))))),
        (Value::Box(_), Value::Complex(num)) => {
            Ok(Value::Box(Array::from(Boxed(Value::from(num)))))
        }
        (m, owned) => Err(format!(
            "Cannot {action1} {} {action2} {} {action3}",
            owned.type_name(),
            m.type_name()
        )),
    }
}

pub(crate) trait MapItem {
    fn empty_cell() -> Self;
    fn tombstone_cell() -> Self;
    fn is_any_empty_cell(&self) -> bool;
    fn is_any_tombstone(&self) -> bool;
    fn is_all_empty_cell(&self) -> bool {
        self.is_any_empty_cell()
    }
    fn is_all_tombstone(&self) -> bool {
        self.is_any_tombstone()
    }
}

impl MapItem for f64 {
    fn empty_cell() -> Self {
        EMPTY_NAN
    }
    fn tombstone_cell() -> Self {
        TOMBSTONE_NAN
    }
    fn is_any_empty_cell(&self) -> bool {
        self.to_bits() == EMPTY_NAN.to_bits()
    }
    fn is_any_tombstone(&self) -> bool {
        self.to_bits() == TOMBSTONE_NAN.to_bits()
    }
}

impl MapItem for Complex {
    fn empty_cell() -> Self {
        Complex::new(EMPTY_NAN, 0.0)
    }
    fn tombstone_cell() -> Self {
        Complex::new(TOMBSTONE_NAN, 0.0)
    }
    fn is_any_empty_cell(&self) -> bool {
        self.re.to_bits() == EMPTY_NAN.to_bits()
    }
    fn is_any_tombstone(&self) -> bool {
        self.re.to_bits() == TOMBSTONE_NAN.to_bits()
    }
}

impl MapItem for char {
    fn empty_cell() -> Self {
        EMPTY_CHAR
    }
    fn tombstone_cell() -> Self {
        TOMBSTONE_CHAR
    }
    fn is_any_empty_cell(&self) -> bool {
        *self == EMPTY_CHAR
    }
    fn is_any_tombstone(&self) -> bool {
        *self == TOMBSTONE_CHAR
    }
}

impl MapItem for Boxed {
    fn empty_cell() -> Self {
        Boxed(Value::empty_cell())
    }
    fn tombstone_cell() -> Self {
        Boxed(Value::tombstone_cell())
    }
    fn is_any_empty_cell(&self) -> bool {
        self.0.is_any_empty_cell()
    }
    fn is_any_tombstone(&self) -> bool {
        self.0.is_any_tombstone()
    }
}

impl MapItem for Value {
    fn empty_cell() -> Self {
        Value::from(EMPTY_NAN)
    }
    fn tombstone_cell() -> Self {
        Value::from(TOMBSTONE_NAN)
    }
    fn is_any_empty_cell(&self) -> bool {
        match self {
            Value::Num(num) => num.data.iter().any(|v| v.is_any_empty_cell()),
            Value::Byte(_) => false,
            Value::Complex(num) => num.data.iter().any(|v| v.is_any_empty_cell()),
            Value::Char(num) => num.data.iter().any(|v| v.is_any_empty_cell()),
            Value::Box(num) => num.data.iter().any(|v| v.is_any_empty_cell()),
        }
    }
    fn is_any_tombstone(&self) -> bool {
        match self {
            Value::Num(num) => num.data.iter().any(|v| v.is_any_tombstone()),
            Value::Byte(_) => false,
            Value::Complex(num) => num.data.iter().any(|v| v.is_any_tombstone()),
            Value::Char(num) => num.data.iter().any(|v| v.is_any_tombstone()),
            Value::Box(num) => num.data.iter().any(|v| v.is_any_tombstone()),
        }
    }
    fn is_all_empty_cell(&self) -> bool {
        match self {
            Value::Num(num) => num.data.iter().all(|v| v.is_any_empty_cell()),
            Value::Byte(_) => false,
            Value::Complex(num) => num.data.iter().all(|v| v.is_any_empty_cell()),
            Value::Char(num) => num.data.iter().all(|v| v.is_any_empty_cell()),
            Value::Box(num) => num.data.iter().all(|v| v.is_any_empty_cell()),
        }
    }
    fn is_all_tombstone(&self) -> bool {
        match self {
            Value::Num(num) => num.data.iter().all(|v| v.is_any_tombstone()),
            Value::Byte(_) => false,
            Value::Complex(num) => num.data.iter().all(|v| v.is_any_tombstone()),
            Value::Char(num) => num.data.iter().all(|v| v.is_any_tombstone()),
            Value::Box(num) => num.data.iter().all(|v| v.is_any_tombstone()),
        }
    }
}
