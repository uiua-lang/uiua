use std::{
    collections::hash_map::DefaultHasher,
    hash::{Hash, Hasher},
    iter::repeat,
    mem::take,
};

use ecow::{eco_vec, EcoVec};

use crate::{Array, ArrayValue, Boxed, Uiua, UiuaResult, Value};

impl Value {
    /// Check if a map array contains a key
    pub fn has_key(&self, key: &Value, env: &Uiua) -> UiuaResult<bool> {
        let map = MapRef::new(self, env)?;
        Ok(map.get(key).is_some())
    }
    /// Get the keys of a map array
    pub fn keys(&self, env: &Uiua) -> UiuaResult<Array<Boxed>> {
        let MapRef(arr) = MapRef::new(self, env)?;
        match arr.rank() {
            1 => Ok(arr.data.first().cloned().into_iter().collect()),
            2 => {
                let mut keys = EcoVec::with_capacity(arr.row_count());
                for row in arr.rows() {
                    let key = &row.data[0].0;
                    if !is_empty(key) && !is_tombstone(key) {
                        keys.push(Boxed(key.clone()));
                    }
                }
                Ok(Array::new([keys.len()].as_slice(), keys))
            }
            _ => unreachable!(),
        }
    }
    /// Get the values of a map array
    pub fn values(&self, env: &Uiua) -> UiuaResult<Array<Boxed>> {
        let MapRef(arr) = MapRef::new(self, env)?;
        match arr.rank() {
            1 => Ok(arr.data.last().cloned().into_iter().collect()),
            2 => {
                let mut values = EcoVec::with_capacity(arr.row_count());
                for row in arr.rows() {
                    let key = &row.data[0].0;
                    if !is_empty(key) && !is_tombstone(key) {
                        values.push(Boxed(row.data[1].0.clone()));
                    }
                }
                Ok(Array::new([values.len()].as_slice(), values))
            }
            _ => unreachable!(),
        }
    }
    /// Get a value from a map array
    pub fn get(&self, key: &Value, env: &Uiua) -> UiuaResult<Value> {
        let map = MapRef::new(self, env)?;
        map.get(key)
            .ok_or_else(|| env.error("Key not found in map"))
    }
    /// Insert a key-value pair into a map array
    pub fn insert(&mut self, key: Value, value: Value, env: &Uiua) -> UiuaResult<()> {
        let mut map = MapRefMut::new(self, env)?;
        map.insert(key, value);
        Ok(())
    }
    /// Remove a key-value pair from a map array
    pub fn remove(&mut self, key: &Value, env: &Uiua) -> UiuaResult {
        let mut map = MapRefMut::new(self, env)?;
        map.remove(key);
        Ok(())
    }
    /// Remove all empty and tombstone values from a map array
    pub fn shrink(&mut self, env: &Uiua) -> UiuaResult {
        let MapRefMut(arr) = MapRefMut::new(self, env)?;
        let new_data: EcoVec<_> = take(arr)
            .into_rows()
            .filter(|row| {
                let key = &row.data[0].0;
                !is_empty(key) && !is_tombstone(key)
            })
            .flat_map(|row| row.data)
            .collect();
        *arr = Array::new([new_data.len() / 2, 2].as_slice(), new_data);
        arr.meta_mut().map_len = Some(arr.row_count());
        Ok(())
    }
}

struct MapRef<'a>(&'a Array<Boxed>);
struct MapRefMut<'a>(&'a mut Array<Boxed>);

const LOAD_FACTOR: f64 = 0.75;

// A NaN value used as empty, not the standard NaN.
pub const EMPTY_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0001) };
// A NaN value used as a tombstone, not the standard NaN.
pub const TOMBSTONE_NAN: f64 =
    unsafe { std::mem::transmute(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0002) };

fn is_empty(val: &Value) -> bool {
    if val.rank() > 0 {
        return false;
    }
    match val {
        Value::Num(n) if n.rank() == 0 => n.data[0].to_bits() == EMPTY_NAN.to_bits(),
        _ => false,
    }
}

fn is_tombstone(val: &Value) -> bool {
    if val.rank() > 0 {
        return false;
    }
    match val {
        Value::Num(n) if n.rank() == 0 => n.data[0].to_bits() == TOMBSTONE_NAN.to_bits(),
        _ => false,
    }
}

fn hash_start(value: &Value, capacity: usize) -> usize {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish() as usize % capacity
}

impl<'a> MapRef<'a> {
    fn new(value: &'a Value, env: &Uiua) -> UiuaResult<Self> {
        let arr = match value {
            Value::Box(arr) => arr,
            value => {
                return Err(env.error(format!(
                    "Map must be an array of boxes, but it is {}",
                    value.type_name_plural()
                )))
            }
        };
        if arr.rank() == 0 {
            return Err(env.error("Map array cannot be a scalar"));
        }
        if arr.rank() > 2 {
            return Err(env.error("Map array cannot have a rank greater than 2"));
        }
        if arr.shape() != [0] && !arr.shape.ends_with(&[2]) {
            return Err(env.error("Map array must have a shape ending in 2"));
        }
        Ok(MapRef(arr))
    }
    fn capacity(&self) -> usize {
        self.0.row_count()
    }
    fn hash_start(&self, value: &Value) -> usize {
        hash_start(value, self.capacity())
    }
    fn get(&self, key: &Value) -> Option<Value> {
        if self.0.rank() == 1 {
            if self.0.shape() == [0] {
                return None;
            }
            let row_key = &self.0.data[0].0;
            return if key.unpacked_ref() == row_key.unpacked_ref() {
                Some(self.0.data[1].0.clone())
            } else {
                None
            };
        }
        let start = self.hash_start(key);
        let mut index = start;
        loop {
            let row = self.0.row(index);
            let row_key = &row.data[0].0;
            if key.unpacked_ref() == row_key.unpacked_ref() {
                return Some(row.data[1].0.clone());
            }
            if is_empty(row_key) {
                return None;
            }
            index = (index + 1) % self.capacity();
            if index == start {
                break None;
            }
        }
    }
}
impl<'a> MapRefMut<'a> {
    fn new(value: &'a mut Value, env: &Uiua) -> UiuaResult<Self> {
        let arr = match value {
            Value::Box(arr) => arr,
            value => {
                return Err(env.error(format!(
                    "Map must be an array of boxes, but it is {}",
                    value.type_name_plural()
                )))
            }
        };
        if arr.rank() == 0 {
            return Err(env.error("Map array cannot be a scalar"));
        }
        if arr.rank() > 2 {
            return Err(env.error("Map array cannot have a rank greater than 2"));
        }
        if arr.shape.as_slice() == [0] {
            arr.shape.push(2)
        }
        if !arr.shape.ends_with(&[2]) {
            return Err(env.error("Map array must have a shape ending in 2"));
        }
        if arr.rank() == 1 {
            arr.shape.insert(0, 1);
        }
        Ok(MapRefMut(arr))
    }
    fn as_ref(&self) -> MapRef {
        MapRef(self.0)
    }
    fn len(&mut self) -> usize {
        if let Some(len) = self.0.meta().map_len {
            return len;
        }
        let len = self
            .0
            .rows()
            .filter(|row| !(is_empty(&row.data[0].0) || is_tombstone(&row.data[0].0)))
            .count();
        self.0.meta_mut().map_len = Some(len);
        len
    }
    fn capacity(&self) -> usize {
        self.as_ref().capacity()
    }
    fn grow(&mut self) {
        if self.capacity() == 0 || self.len() as f64 / self.capacity() as f64 > LOAD_FACTOR {
            let new_capacity = (self.capacity() * 2).max(1);
            let len = self.len();
            let old_rows = take(self.0).into_rows();
            *self.0 = Array::new(
                [new_capacity, 2].as_slice(),
                repeat(Boxed(Value::from(EMPTY_NAN)))
                    .take(new_capacity * 2)
                    .collect::<EcoVec<_>>(),
            );
            self.0.meta_mut().map_len = Some(len);
            let data = self.0.data.as_mut_slice();
            for row in old_rows {
                let mut kv = row.data.into_iter();
                let key = kv.next().unwrap().0;
                let value = kv.next().unwrap().0;
                let start = hash_start(&key, new_capacity);
                let mut index = start;
                loop {
                    let row = &mut data[index * 2..(index + 1) * 2];
                    let row_key = &row[0].0;
                    if is_empty(row_key) || is_tombstone(row_key) {
                        row[0] = Boxed(key);
                        row[1] = Boxed(value);
                        break;
                    }
                    index = (index + 1) % new_capacity;
                }
            }
        }
    }
    fn insert(&mut self, key: Value, value: Value) {
        if self.0.row_count() == 0 {
            self.grow();
        }
        let start = self.as_ref().hash_start(&key);
        let mut index = start;
        loop {
            let row = self.0.row(index);
            let row_key = &row.data[0].0;
            let not_present = is_empty(row_key) || is_tombstone(row_key);
            if not_present || key.unpacked_ref() == row_key.unpacked_ref() {
                if not_present {
                    let len = self.len();
                    self.0.meta_mut().map_len = Some(len + 1);
                }
                self.0.set_row(
                    index,
                    Array::new(
                        [2].as_slice(),
                        eco_vec![key.boxed_if_not(), value.boxed_if_not()],
                    ),
                );
                break;
            }
            index = (index + 1) % self.capacity();
            if index == start {
                self.grow();
                self.insert(key, value);
                return;
            }
        }
        self.grow();
    }
    fn remove(&mut self, key: &Value) {
        let start = self.as_ref().hash_start(key);
        let mut index = start;
        loop {
            let row = self.0.row(index);
            let row_key = &row.data[0].0;
            if key.unpacked_ref() == row_key.unpacked_ref() {
                let len = self.len();
                self.0.meta_mut().map_len = Some(len - 1);
                self.0.set_row(
                    index,
                    Array::new(
                        [2].as_slice(),
                        eco_vec![
                            Boxed(Value::from(TOMBSTONE_NAN)),
                            Boxed(Value::from(EMPTY_NAN))
                        ],
                    ),
                );
            }
            if is_empty(row_key) {
                break;
            }
            index = (index + 1) % self.capacity();
            if index == start {
                break;
            }
        }
    }
}

impl<T: ArrayValue> Array<T> {
    fn set_row(&mut self, index: usize, row: Self) {
        let row_len = self.row_len();
        assert_eq!(row_len, row.data.len());
        let start = index * row_len;
        let end = start + row_len;
        self.data.as_mut_slice()[start..end].clone_from_slice(&row.data);
    }
}
