use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::repeat,
    mem::take,
};

use ecow::EcoVec;

use crate::{
    algorithm::ArrayCmpSlice, cowslice::CowSlice, Array, ArrayMeta, ArrayValue, Boxed, Complex,
    FormatShape, Uiua, UiuaResult, Value,
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
        let capacity =
            ((self.row_count() as f64 / LOAD_FACTOR).ceil() as usize).next_power_of_two();
        let mut kv = EcoVec::with_capacity(2);
        for x in [&self, &values] {
            kv.push(Boxed(match x {
                Value::Num(_) => Array::<f64>::new(0, CowSlice::with_capacity(capacity)).into(),
                #[cfg(feature = "bytes")]
                Value::Byte(_) => Array::<f64>::new(0, CowSlice::with_capacity(capacity)).into(),
                Value::Complex(_) => {
                    Array::<Complex>::new(0, CowSlice::with_capacity(capacity)).into()
                }
                _ => Array::<Boxed>::new(0, CowSlice::with_capacity(capacity)).into(),
            }))
        }
        let mut map = Value::Box(Array::new(2, kv));
        map.meta_mut().map_len = Some(0);
        for (key, value) in self.into_rows().zip(values.into_rows()) {
            map.insert(key, value, env)?;
        }
        Ok(map)
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

fn coerce_values(
    a: &mut Value,
    mut b: Value,
    action1: &'static str,
    action2: &'static str,
    action3: &'static str,
) -> Result<Value, String> {
    #[cfg(feature = "bytes")]
    {
        if let Value::Byte(keys) = a {
            *a = Value::Num(keys.convert_ref());
        }
        if let Value::Byte(values) = b {
            b = Value::Num(values.convert_ref());
        }
    }
    match (&mut *a, b) {
        (Value::Num(arr), Value::Num(num)) if arr.row_count() == 0 => {
            let mut shape = num.shape.clone();
            shape.insert(0, 0);
            *a = Array::<f64>::new(shape, EcoVec::new()).into();
            Ok(Value::Num(num))
        }
        (Value::Num(arr), Value::Complex(comp)) if arr.row_count() == 0 => {
            let mut shape = comp.shape.clone();
            shape.insert(0, 0);
            *a = Array::<Complex>::new(shape, EcoVec::new()).into();
            Ok(Value::Complex(comp))
        }
        (Value::Num(arr), owned @ Value::Box(_)) if arr.row_count() == 0 => {
            *a = Array::<Boxed>::new(0, EcoVec::new()).into();
            Ok(owned)
        }
        (Value::Box(arr), Value::Num(num)) if arr.row_count() == 0 => {
            let mut shape = num.shape.clone();
            shape.insert(0, 0);
            *a = Array::<f64>::new(shape, EcoVec::new()).into();
            Ok(Value::Num(num))
        }
        (Value::Box(arr), Value::Char(ch)) if arr.row_count() == 0 => {
            let mut shape = ch.shape.clone();
            shape.insert(0, 0);
            *a = Array::<char>::new(shape, EcoVec::new()).into();
            Ok(Value::Char(ch))
        }
        (Value::Box(arr), Value::Complex(num)) if arr.row_count() == 0 => {
            let mut shape = num.shape.clone();
            shape.insert(0, 0);
            *a = Array::<Complex>::new(shape, EcoVec::new()).into();
            Ok(Value::Complex(num))
        }
        (Value::Num(arr), Value::Num(item)) if arr.shape[1..] != item.shape => Err(format!(
            "Cannot {action1} shape {} {action2} shape {} {action3}",
            item.shape(),
            FormatShape(&arr.shape()[1..])
        )),
        (Value::Complex(arr), Value::Complex(item)) if arr.shape[1..] != item.shape => {
            Err(format!(
                "Cannot {action1} shape {} {action2} shape {} {action3}",
                item.shape(),
                FormatShape(&arr.shape()[1..])
            ))
        }
        (Value::Box(arr), Value::Box(item)) if arr.shape[1..] != item.shape => Err(format!(
            "Cannot {action1} shape {} {action2} shape {} {action3}",
            item.shape(),
            FormatShape(&arr.shape()[1..])
        )),
        (Value::Num(_), owned @ Value::Num(_))
        | (Value::Complex(_), owned @ Value::Complex(_))
        | (Value::Char(_), owned @ Value::Char(_))
        | (Value::Box(_), owned @ Value::Box(_)) => Ok(owned),
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

struct Pair<'a> {
    keys: &'a Value,
    values: &'a Value,
}

struct PairMut<'a> {
    meta: &'a mut ArrayMeta,
    keys: &'a mut Value,
    values: &'a mut Value,
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
                keys: &mut keys[0].0,
                values: &mut values[0].0,
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
                keys: &mut keys[0].0,
                values: &mut values[0].0,
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
                    keys: &mut keys[0].0,
                    values: &mut values[0].0,
                });
                Ok(res)
            }
            value => {
                let mut arr: Array<_> = take(value).into_rows().map(Boxed).collect();
                let data = arr.data.as_mut_slice();
                let (keys, values) = data.split_at_mut(1);
                let res = f(PairMut {
                    meta: Array::<Boxed>::get_meta_mut(&mut arr.meta),
                    keys: &mut keys[0].0,
                    values: &mut values[0].0,
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
        self.keys.row_count()
    }
    fn len(&mut self) -> usize {
        if let Some(len) = self.meta.map_len {
            return len;
        }
        let len = match self.keys {
            Value::Num(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
            #[cfg(feature = "bytes")]
            Value::Byte(arr) => arr.row_count(),
            Value::Complex(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
            Value::Char(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
            Value::Box(arr) => (arr.rows())
                .filter(|row| !(row.data[0].is_empty_cell() || row.data[0].is_tombstone()))
                .count(),
        };
        self.meta.map_len = Some(len);
        len
    }
    fn grow(&mut self) {
        if self.capacity() == 0 || (self.len() as f64 / self.capacity() as f64) > LOAD_FACTOR {
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
                    repeat(K::empty_cell())
                        .take(new_capacity * key_row_len)
                        .collect::<EcoVec<_>>(),
                );
                *values = Array::new(
                    values_shape,
                    repeat(V::empty_cell())
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
            #[cfg(feature = "bytes")]
            {
                if let Value::Byte(keys) = self.keys {
                    *self.keys = Value::Num(keys.convert_ref());
                }
                if let Value::Byte(values) = self.values {
                    *self.values = Value::Num(values.convert_ref());
                }
            }
            match (&mut *self.keys, &mut *self.values) {
                (Value::Num(a), Value::Num(b)) => grow_impl(a, b, new_cap),
                (Value::Num(a), Value::Complex(b)) => grow_impl(a, b, new_cap),
                (Value::Num(a), Value::Char(b)) => grow_impl(a, b, new_cap),
                (Value::Num(a), Value::Box(b)) => grow_impl(a, b, new_cap),
                (Value::Complex(a), Value::Num(b)) => grow_impl(a, b, new_cap),
                (Value::Complex(a), Value::Complex(b)) => grow_impl(a, b, new_cap),
                (Value::Complex(a), Value::Char(b)) => grow_impl(a, b, new_cap),
                (Value::Complex(a), Value::Box(b)) => grow_impl(a, b, new_cap),
                (Value::Char(a), Value::Num(b)) => grow_impl(a, b, new_cap),
                (Value::Char(a), Value::Complex(b)) => grow_impl(a, b, new_cap),
                (Value::Char(a), Value::Char(b)) => grow_impl(a, b, new_cap),
                (Value::Char(a), Value::Box(b)) => grow_impl(a, b, new_cap),
                (Value::Box(a), Value::Num(b)) => grow_impl(a, b, new_cap),
                (Value::Box(a), Value::Complex(b)) => grow_impl(a, b, new_cap),
                (Value::Box(a), Value::Char(b)) => grow_impl(a, b, new_cap),
                (Value::Box(a), Value::Box(b)) => grow_impl(a, b, new_cap),
                (Value::Num(_), Value::Byte(_))
                | (Value::Byte(_), Value::Num(_))
                | (Value::Byte(_), Value::Byte(_))
                | (Value::Byte(_), Value::Complex(_))
                | (Value::Byte(_), Value::Char(_))
                | (Value::Byte(_), Value::Box(_))
                | (Value::Complex(_), Value::Byte(_))
                | (Value::Char(_), Value::Byte(_))
                | (Value::Box(_), Value::Byte(_)) => unreachable!(),
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
        let key = coerce_values(self.keys, key, "insert", "key into map with", "keys")
            .map_err(|e| env.error(e))?;
        let value = coerce_values(
            self.values,
            value,
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
                match ((&mut *self.keys, key), (&mut *self.values, value)) {
                    $((
                        (Value::$k(keys), Value::$k(key)),
                        (Value::$v(values), Value::$v(value)),
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
            (Num, Complex),
            (Num, Box),
            (Complex, Num),
            (Complex, Complex),
            (Complex, Box),
            (Char, Char),
            (Char, Box),
            (Box, Num),
            (Box, Complex),
            (Box, Char),
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
                        *elem = K::tombstone_cell();
                    }
                    for elem in &mut value_data[index * value_row_len..(index + 1) * value_row_len]
                    {
                        *elem = V::empty_cell();
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
        let key = coerce_values(self.keys, key, "remove", "key from map with", "keys")
            .map_err(|e| env.error(e))?;
        let capacity = self.capacity();
        macro_rules! do_remove {
            ($(($k:ident, $v:ident),)*) => {
                match ((&mut *self.keys, key), &mut *self.values) {
                    $((
                        (Value::$k(keys), Value::$k(key)),
                        Value::$v(values)
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
            (Num, Complex),
            (Num, Box),
            (Complex, Num),
            (Complex, Complex),
            (Complex, Box),
            (Char, Char),
            (Char, Box),
            (Box, Num),
            (Box, Complex),
            (Box, Char),
            (Box, Box),
        );
        Ok(())
    }
}

pub(crate) trait MapItem {
    fn empty_cell() -> Self;
    fn is_empty_cell(&self) -> bool;
    fn tombstone_cell() -> Self;
    fn is_tombstone(&self) -> bool;
}

impl MapItem for f64 {
    fn empty_cell() -> Self {
        EMPTY_NAN
    }
    fn is_empty_cell(&self) -> bool {
        self.to_bits() == EMPTY_NAN.to_bits()
    }
    fn tombstone_cell() -> Self {
        TOMBSTONE_NAN
    }
    fn is_tombstone(&self) -> bool {
        self.to_bits() == TOMBSTONE_NAN.to_bits()
    }
}

impl MapItem for Complex {
    fn empty_cell() -> Self {
        Complex::new(EMPTY_NAN, 0.0)
    }
    fn is_empty_cell(&self) -> bool {
        self.re.to_bits() == EMPTY_NAN.to_bits()
    }
    fn tombstone_cell() -> Self {
        Complex::new(TOMBSTONE_NAN, 0.0)
    }
    fn is_tombstone(&self) -> bool {
        self.re.to_bits() == TOMBSTONE_NAN.to_bits()
    }
}

impl MapItem for char {
    fn empty_cell() -> Self {
        '\0'
    }
    fn is_empty_cell(&self) -> bool {
        *self == '\0'
    }
    fn tombstone_cell() -> Self {
        '\u{1}'
    }
    fn is_tombstone(&self) -> bool {
        *self == '\u{1}'
    }
}

impl MapItem for Boxed {
    fn empty_cell() -> Self {
        Boxed(Value::empty_cell())
    }
    fn is_empty_cell(&self) -> bool {
        self.0.is_empty_cell()
    }
    fn tombstone_cell() -> Self {
        Boxed(Value::tombstone_cell())
    }
    fn is_tombstone(&self) -> bool {
        self.0.is_tombstone()
    }
}

impl MapItem for Value {
    fn empty_cell() -> Self {
        Value::from(EMPTY_NAN)
    }
    fn is_empty_cell(&self) -> bool {
        match self {
            Value::Num(num) => num.as_scalar().is_some_and(f64::is_empty_cell),
            #[cfg(feature = "bytes")]
            Value::Byte(_) => false,
            Value::Complex(num) => num.as_scalar().is_some_and(Complex::is_empty_cell),
            Value::Char(num) => num.as_scalar().is_some_and(char::is_empty_cell),
            Value::Box(num) => num.as_scalar().is_some_and(Boxed::is_empty_cell),
        }
    }
    fn tombstone_cell() -> Self {
        Value::from(TOMBSTONE_NAN)
    }
    fn is_tombstone(&self) -> bool {
        match self {
            Value::Num(num) => num.as_scalar().is_some_and(f64::is_tombstone),
            #[cfg(feature = "bytes")]
            Value::Byte(_) => false,
            Value::Complex(num) => num.as_scalar().is_some_and(Complex::is_tombstone),
            Value::Char(num) => num.as_scalar().is_some_and(char::is_tombstone),
            Value::Box(num) => num.as_scalar().is_some_and(Boxed::is_tombstone),
        }
    }
}
