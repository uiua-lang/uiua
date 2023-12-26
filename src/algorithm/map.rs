use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::repeat,
    mem::take,
};

use ecow::EcoVec;

use crate::{
    algorithm::ArrayCmpSlice, Array, ArrayMeta, ArrayValue, Boxed, Complex, FormatShape, Uiua,
    UiuaResult, Value,
};

impl Value {
    /// Create a map array
    pub fn map(self, values: Self, env: &Uiua) -> UiuaResult<Value> {
        if self.row_count() != values.row_count() {
            return Err(env.error(format!(
                "Map array's keys and values must have the same length, but they have lengths {} and {}",
                self.row_count(),
                values.row_count()
            )));
        }
        Ok(Array::new(2, [Boxed(self), Boxed(values)]).into())
    }
    /// Turn a map array into its keys and values
    pub fn unmap(mut self, env: &Uiua) -> UiuaResult<(Value, Value)> {
        with_pair_mut(&mut self, env, |_| {})?;
        let mut rows = self.into_rows().map(|row| row.unboxed());
        let keys = (rows.next().unwrap().into_rows())
            .filter(|row| !(row.is_empty_cell() || row.is_tombstone()))
            .collect::<Vec<_>>();
        let values = (rows.next().unwrap().into_rows())
            .filter(|row| !(row.is_empty_cell() || row.is_tombstone()))
            .collect::<Vec<_>>();
        Ok((
            Value::from_row_values_infallible(keys),
            Value::from_row_values_infallible(values),
        ))
    }
    /// Get a value from a map array
    pub fn get(&self, key: &Value, env: &Uiua) -> UiuaResult<Value> {
        with_pair(self, env, |pair| pair.get(key))?.ok_or_else(|| env.error("Key not found in map"))
    }
    /// Check if a map array contains a key
    pub fn has_key(&self, key: &Value, env: &Uiua) -> UiuaResult<bool> {
        with_pair(self, env, |pair| pair.get(key).is_some())
    }
    /// Insert a key-value pair into a map array
    pub fn insert(&mut self, key: Value, value: Value, env: &Uiua) -> UiuaResult {
        with_pair_mut(self, env, |mut pair| pair.insert(key, value, env))?
    }
    /// Remove a key-value pair from a map array
    pub fn remove(&mut self, key: Value, env: &Uiua) -> UiuaResult {
        with_pair_mut(self, env, |mut pair| pair.remove(key, env))?
    }
}

const LOAD_FACTOR: f64 = 0.75;

// A NaN value used as empty, not the standard NaN.
pub const EMPTY_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0001) };
// A NaN value used as a tombstone, not the standard NaN.
pub const TOMBSTONE_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0002) };

fn hash_start<T: ArrayValue>(arr: &Array<T>, capacity: usize) -> usize {
    let mut hasher = DefaultHasher::new();
    arr.hash(&mut hasher);
    hasher.finish() as usize % capacity
}

enum HashMut<'a> {
    Num(&'a mut Array<f64>),
    Comp(&'a mut Array<Complex>),
    Box(&'a mut Array<Boxed>),
}

struct HashValue<'a>(&'a mut Value);

#[derive(Debug)]
enum HashOwned {
    Num(Array<f64>),
    Comp(Array<Complex>),
    Box(Array<Boxed>),
}

impl<'a> HashMut<'a> {
    fn type_name(&self) -> &'static str {
        match self {
            Self::Num(_) => "number",
            Self::Comp(_) => "complex",
            Self::Box(_) => "box",
        }
    }
}

impl<'a> HashValue<'a> {
    fn as_mut(&mut self) -> HashMut<'_> {
        match self.0 {
            Value::Num(arr) => HashMut::Num(arr),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => {
                let arr: Array<f64> = arr.convert_ref();
                *self.0 = arr.into();
                self.as_mut()
            }
            Value::Complex(arr) => HashMut::Comp(arr),
            Value::Char(arr) => {
                let arr: Array<_> = arr.rows().map(Value::from).map(Boxed).collect();
                *self.0 = arr.into();
                self.as_mut()
            }
            Value::Box(arr) => HashMut::Box(arr),
        }
    }
    fn coerce_with(
        &mut self,
        owned: HashOwned,
        action1: &'static str,
        action2: &'static str,
        action3: &'static str,
    ) -> Result<HashOwned, String> {
        match (self.as_mut(), owned) {
            (HashMut::Num(arr), HashOwned::Num(item)) if arr.shape[1..] != item.shape => {
                Err(format!(
                    "Cannot {action1} shape {} {action2} shape {} {action3}",
                    item.shape(),
                    FormatShape(&arr.shape()[1..])
                ))
            }
            (HashMut::Comp(arr), HashOwned::Comp(item)) if arr.shape[1..] != item.shape => {
                Err(format!(
                    "Cannot {action1} shape {} {action2} shape {} {action3}",
                    item.shape(),
                    FormatShape(&arr.shape()[1..])
                ))
            }
            (HashMut::Box(arr), HashOwned::Box(item)) if arr.shape[1..] != item.shape => {
                Err(format!(
                    "Cannot {action1} shape {} {action2} shape {} {action3}",
                    item.shape(),
                    FormatShape(&arr.shape()[1..])
                ))
            }
            (HashMut::Num(_), owned @ HashOwned::Num(_)) => Ok(owned),
            (HashMut::Comp(_), owned @ HashOwned::Comp(_)) => Ok(owned),
            (HashMut::Box(_), owned @ HashOwned::Box(_)) => Ok(owned),
            (HashMut::Num(arr), owned @ HashOwned::Comp(_)) if arr.row_count() == 0 => {
                *self.0 = arr.convert_ref::<Complex>().into();
                Ok(owned)
            }
            (HashMut::Num(arr), owned @ HashOwned::Box(_)) if arr.row_count() == 0 => {
                *self.0 = Array::<Boxed>::new([0], EcoVec::new()).into();
                Ok(owned)
            }
            (HashMut::Box(arr), HashOwned::Num(num)) if arr.row_count() == 0 => {
                let mut shape = num.shape.clone();
                shape.insert(0, 0);
                *self.0 = Array::<f64>::new(shape, EcoVec::new()).into();
                Ok(HashOwned::Num(num))
            }
            (HashMut::Box(arr), HashOwned::Comp(num)) if arr.row_count() == 0 => {
                let mut shape = num.shape.clone();
                shape.insert(0, 0);
                *self.0 = Array::<Complex>::new(shape, EcoVec::new()).into();
                Ok(HashOwned::Comp(num))
            }
            (HashMut::Box(_), HashOwned::Num(num)) => {
                Ok(HashOwned::Box(Array::from(Boxed(Value::from(num)))))
            }
            (HashMut::Box(_), HashOwned::Comp(num)) => {
                Ok(HashOwned::Box(Array::from(Boxed(Value::from(num)))))
            }
            (m, owned) => Err(format!(
                "Cannot {action1} {} {action2} {} {action3}",
                owned.type_name(),
                m.type_name()
            )),
        }
    }
}

impl HashOwned {
    fn new(value: Value) -> Self {
        match value {
            Value::Num(arr) => Self::Num(arr),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => Self::Num(arr.convert()),
            Value::Complex(arr) => Self::Comp(arr),
            Value::Char(_) => Self::new(Array::from(Boxed(value)).into()),
            Value::Box(arr) => Self::Box(arr),
        }
    }
    fn type_name(&self) -> &'static str {
        match self {
            Self::Num(_) => "number",
            Self::Comp(_) => "complex",
            Self::Box(_) => "box",
        }
    }
}

struct Pair<'a> {
    keys: &'a Value,
    values: &'a Value,
}

struct PairMut<'a> {
    meta: &'a mut ArrayMeta,
    keys: HashValue<'a>,
    values: HashValue<'a>,
}

fn with_pair<T>(val: &Value, env: &Uiua, f: impl FnOnce(Pair) -> T) -> UiuaResult<T> {
    match (val, val.shape().dims()) {
        (_, [] | [0, ..] | [1, ..]) => Ok(f(Pair {
            keys: val,
            values: val,
        })),
        (_, [2, ..]) => {
            let keys = val.row(0);
            let keys = keys.unpacked_ref();
            let values = val.row(1);
            let values = values.unpacked_ref();
            if keys.row_count() != values.row_count() {
                return Err(env.error(format!(
                    "Map array's keys and values must have the same length, but they have lengths {} and {}",
                    keys.row_count(),
                    values.row_count()
                )));
            }
            Ok(f(Pair { keys, values }))
        }
        (Value::Box(arr), _) if arr.element_count() == 2 => {
            let keys = arr.data[0].0.unpacked_ref();
            let values = arr.data[1].0.unpacked_ref();
            if keys.row_count() != values.row_count() {
                return Err(env.error(format!(
                    "Map array's keys and values must have the same length, but they have lengths {} and {}",
                    keys.row_count(),
                    values.row_count()
                )));
            }
            Ok(f(Pair { keys, values }))
        }
        _ => Err(env.error(format!(
            "Map array must have a length of 2, but its shape is {}",
            val.shape()
        ))),
    }
}

fn with_pair_mut<T>(val: &mut Value, env: &Uiua, f: impl FnOnce(PairMut) -> T) -> UiuaResult<T> {
    let row_count = val.row_count();
    let elem_count = val.element_count();
    match (val, row_count, elem_count) {
        (Value::Box(arr), _, 2) => {
            let data = arr.data.as_mut_slice();
            let (keys, values) = data.split_at_mut(1);
            let res = f(PairMut {
                meta: Array::<Boxed>::get_meta_mut(&mut arr.meta),
                keys: HashValue(&mut keys[0].0),
                values: HashValue(&mut values[0].0),
            });
            Ok(res)
        }
        (val, 0, _) | (val, _, 0) => {
            let mut arr: Array<_> = repeat(Boxed(Array::<Boxed>::default().into()))
                .take(2)
                .collect();
            arr.shape = val.shape().clone();
            arr.shape[0] = 2;
            let data = arr.data.as_mut_slice();
            let (keys, values) = data.split_at_mut(1);
            let res = f(PairMut {
                meta: Array::<Boxed>::get_meta_mut(&mut arr.meta),
                keys: HashValue(&mut keys[0].0),
                values: HashValue(&mut values[0].0),
            });
            *val = arr.into();
            Ok(res)
        }
        (val, 2, _) => match val {
            Value::Box(arr) => {
                let data = arr.data.as_mut_slice();
                let (keys, values) = data.split_at_mut(1);
                let res = f(PairMut {
                    meta: Array::<Boxed>::get_meta_mut(&mut arr.meta),
                    keys: HashValue(&mut keys[0].0),
                    values: HashValue(&mut values[0].0),
                });
                Ok(res)
            }
            value => {
                let mut arr: Array<_> = take(value).into_rows().map(Boxed).collect();
                let data = arr.data.as_mut_slice();
                let (keys, values) = data.split_at_mut(1);
                let res = f(PairMut {
                    meta: Array::<Boxed>::get_meta_mut(&mut arr.meta),
                    keys: HashValue(&mut keys[0].0),
                    values: HashValue(&mut values[0].0),
                });
                *value = arr.into();
                Ok(res)
            }
        },
        (val, ..) => Err(env.error(format!(
            "Map array must have a length of 2, but its shape is {}",
            val.shape()
        ))),
    }
}

impl<'a> Pair<'a> {
    fn capacity(&self) -> usize {
        self.keys.row_count()
    }
    fn hash_start(&self, key: &Value) -> usize {
        key.generic_ref_shallow(
            |arr| hash_start(arr, self.capacity()),
            |arr| hash_start(arr, self.capacity()),
            |arr| hash_start(arr, self.capacity()),
            |arr| hash_start(arr, self.capacity()),
            |arr| hash_start(arr, self.capacity()),
        )
    }
    fn get(&self, key: &Value) -> Option<Value> {
        if self.keys.shape() == [0] {
            return None;
        }
        let start = self.hash_start(key);
        let mut index = start;
        loop {
            let row_key = self.keys.row(index);
            if key.unpacked_ref() == row_key.unpacked_ref() {
                return Some(self.values.row(index));
            }
            if row_key.is_empty_cell() {
                return None;
            }
            index = (index + 1) % self.capacity();
            if index == start {
                break None;
            }
        }
    }
}

impl<'a> PairMut<'a> {
    fn capacity(&self) -> usize {
        self.keys.0.row_count()
    }
    fn len(&mut self) -> usize {
        if let Some(len) = self.meta.map_len {
            return len;
        }
        let len = match self.keys.as_mut() {
            HashMut::Num(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
            HashMut::Comp(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
            HashMut::Box(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
        };
        self.meta.map_len = Some(len);
        len
    }
    fn grow(&mut self) {
        if self.capacity() == 0 || self.len() as f64 / self.capacity() as f64 > LOAD_FACTOR {
            fn grow_impl<K, V>(keys: &mut Array<K>, values: &mut Array<V>, new_capacity: usize)
            where
                K: MapItem + ArrayValue,
                V: MapItem + ArrayValue,
            {
                let key_row_len = keys.row_len();
                let value_row_len = values.row_len();
                let mut keys_shape = keys.shape.clone();
                keys_shape[0] = new_capacity;
                let mut values_shape = values.shape.clone();
                values_shape[0] = new_capacity;
                let old_keys = take(keys).into_rows();
                let old_values = take(values).into_rows();
                *keys = Array::new(
                    keys_shape,
                    repeat(K::from_num(EMPTY_NAN))
                        .take(new_capacity * key_row_len)
                        .collect::<EcoVec<_>>(),
                );
                *values = Array::new(
                    values_shape,
                    repeat(V::from_num(EMPTY_NAN))
                        .take(new_capacity * value_row_len)
                        .collect::<EcoVec<_>>(),
                );
                let key_data = keys.data.as_mut_slice();
                let value_data = values.data.as_mut_slice();
                for (key, value) in old_keys.zip(old_values) {
                    let start = hash_start(&key, new_capacity);
                    let mut index = start;
                    loop {
                        let cell_key =
                            &mut key_data[index * key_row_len..(index + 1) * key_row_len];
                        if cell_key[0].is_empty_cell() {
                            cell_key.clone_from_slice(&key.data);
                            value_data[index * value_row_len..(index + 1) * value_row_len]
                                .clone_from_slice(&value.data);
                            break;
                        }
                        index = (index + 1) % new_capacity;
                    }
                }
            }

            let new_cap = (self.capacity() * 2).max(1);
            let len = self.len();
            self.meta.map_len = Some(len);
            match (self.keys.as_mut(), self.values.as_mut()) {
                (HashMut::Num(keys), HashMut::Num(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Num(keys), HashMut::Comp(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Num(keys), HashMut::Box(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Comp(keys), HashMut::Num(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Comp(keys), HashMut::Comp(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Comp(keys), HashMut::Box(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Box(keys), HashMut::Num(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Box(keys), HashMut::Comp(values)) => grow_impl(keys, values, new_cap),
                (HashMut::Box(keys), HashMut::Box(values)) => grow_impl(keys, values, new_cap),
            }
        }
    }
    fn insert(&mut self, key: Value, value: Value, env: &Uiua) -> UiuaResult {
        fn insert_impl<K, V>(
            keys: &mut Array<K>,
            values: &mut Array<V>,
            key: Array<K>,
            value: Array<V>,
            meta: &mut ArrayMeta,
            capacity: usize,
        ) -> Option<(Array<K>, Array<V>)>
        where
            K: MapItem + ArrayValue,
            V: MapItem + ArrayValue,
        {
            let start = hash_start(&key, capacity);
            let mut index = start;
            let key_row_len = keys.row_len();
            let value_row_len = values.row_len();
            let key_data = keys.data.as_mut_slice();
            let value_data = values.data.as_mut_slice();
            loop {
                let cell_key = &mut key_data[index * key_row_len..(index + 1) * key_row_len];
                let not_present = cell_key[0].is_empty_cell() || cell_key[0].is_tombstone();
                if not_present || ArrayCmpSlice(cell_key) == ArrayCmpSlice(&key.data) {
                    if not_present {
                        let len = meta.map_len.unwrap();
                        meta.map_len = Some(len + 1);
                    }
                    cell_key.clone_from_slice(&key.data);
                    value_data[index * value_row_len..(index + 1) * value_row_len]
                        .clone_from_slice(&value.data);
                    break None;
                }
                index = (index + 1) % capacity;
                if index == start {
                    break Some((key, value));
                }
            }
        }
        let key = self
            .keys
            .coerce_with(HashOwned::new(key), "insert", "key into map with", "keys")
            .map_err(|e| env.error(e))?;
        let value = self
            .values
            .coerce_with(
                HashOwned::new(value),
                "insert",
                "value into map with",
                "values",
            )
            .map_err(|e| env.error(e))?;
        if self.capacity() == 0 {
            self.grow();
        }
        let capacity = self.capacity();
        macro_rules! do_insert {
            ($(($k:ident, $v:ident),)*) => {
                match ((self.keys.as_mut(), key), (self.values.as_mut(), value)) {
                    $((
                        (HashMut::$k(keys), HashOwned::$k(key)),
                        (HashMut::$v(values), HashOwned::$v(value)),
                    ) => {
                        if let Some((key, value)) =
                            insert_impl(keys, values, key, value, self.meta, capacity)
                        {
                            self.grow();
                            return self.insert(key.into(), value.into(), env);
                        }
                    })*
                    ((keys, key), (values, value)) => {
                        let keys = keys.type_name();
                        let key = key.type_name();
                        let values = values.type_name();
                        let value = value.type_name();
                        if keys != key {
                            return Err(env.error(format!(
                                "Cannot insert {key} key into map with {keys} keys",
                            )));
                        }
                        if values != value {
                            return Err(env.error(format!(
                                "Cannot insert {value} value into map with {values} values",
                            )));
                        }
                    }
                }
            }
        }
        do_insert!(
            (Num, Num),
            (Num, Comp),
            (Num, Box),
            (Comp, Num),
            (Comp, Comp),
            (Comp, Box),
            (Box, Num),
            (Box, Comp),
            (Box, Box),
        );
        self.grow();
        Ok(())
    }
    fn remove(&mut self, key: Value, env: &Uiua) -> UiuaResult {
        fn remove_impl<K, V>(
            keys: &mut Array<K>,
            values: &mut Array<V>,
            key: Array<K>,
            meta: &mut ArrayMeta,
            capacity: usize,
        ) where
            K: MapItem + ArrayValue,
            V: MapItem + ArrayValue,
        {
            let start = hash_start(&key, capacity);
            let mut index = start;
            let key_row_len = keys.row_len();
            let value_row_len = values.row_len();
            let key_data = keys.data.as_mut_slice();
            let value_data = values.data.as_mut_slice();
            loop {
                let cell_key = &mut key_data[index * key_row_len..(index + 1) * key_row_len];
                if ArrayCmpSlice(cell_key) == ArrayCmpSlice(&key.data) {
                    if let Some(len) = meta.map_len {
                        meta.map_len = Some(len - 1);
                    }
                    for elem in cell_key {
                        *elem = K::from_num(TOMBSTONE_NAN);
                    }
                    for elem in &mut value_data[index * value_row_len..(index + 1) * value_row_len]
                    {
                        *elem = V::from_num(EMPTY_NAN);
                    }
                    break;
                }
                if cell_key[0].is_empty_cell() {
                    break;
                }
                index = (index + 1) % capacity;
                if index == start {
                    break;
                }
            }
        }
        let key = self
            .keys
            .coerce_with(HashOwned::new(key), "remove", "key from map with", "keys")
            .map_err(|e| env.error(e))?;
        let capacity = self.capacity();
        macro_rules! do_remove {
            ($(($k:ident, $v:ident),)*) => {
                match ((self.keys.as_mut(), key), self.values.as_mut()) {
                    $((
                        (HashMut::$k(keys), HashOwned::$k(key)),
                        HashMut::$v(values)
                    ) => {
                        remove_impl(keys, values, key, self.meta, capacity);
                    })*
                    ((keys, key), values) => {
                        let keys = keys.type_name();
                        let key = key.type_name();
                        let values = values.type_name();
                        if keys != key {
                            return Err(env.error(format!(
                                "Cannot remove {key} key from map with {keys} keys",
                            )));
                        }
                        if values != key {
                            return Err(env.error(format!(
                                "Cannot remove {key} key from map with {values} values",
                            )));
                        }
                    }
                }
            }
        }
        do_remove!(
            (Num, Num),
            (Num, Comp),
            (Num, Box),
            (Comp, Num),
            (Comp, Comp),
            (Comp, Box),
            (Box, Num),
            (Box, Comp),
            (Box, Box),
        );
        Ok(())
    }
}

trait MapItem {
    fn num(&self) -> f64;
    fn from_num(num: f64) -> Self;
    fn is_empty_cell(&self) -> bool {
        self.num().to_bits() == EMPTY_NAN.to_bits()
    }
    fn is_tombstone(&self) -> bool {
        self.num().to_bits() == TOMBSTONE_NAN.to_bits()
    }
}

impl MapItem for f64 {
    fn num(&self) -> f64 {
        *self
    }
    fn from_num(num: f64) -> Self {
        num
    }
}

impl MapItem for Complex {
    fn num(&self) -> f64 {
        self.re
    }
    fn from_num(num: f64) -> Self {
        Self::new(num, 0.0)
    }
}

impl MapItem for Boxed {
    fn num(&self) -> f64 {
        self.0.num()
    }
    fn from_num(num: f64) -> Self {
        Self(Value::from(num))
    }
}

impl MapItem for Value {
    fn num(&self) -> f64 {
        if self.element_count() == 0 {
            return 0.0;
        }
        match self {
            Value::Num(arr) => arr.data[0],
            _ => 0.0,
        }
    }
    fn from_num(num: f64) -> Self {
        Self::from(num)
    }
}
