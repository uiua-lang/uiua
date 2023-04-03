use std::{
    cmp::Ordering,
    fmt,
    mem::{take, ManuallyDrop},
    rc::Rc,
};

use crate::{function::Function, value::Value, Uiua, UiuaResult};

pub struct Array {
    ty: ArrayType,
    shape: Vec<usize>,
    data: Data,
}

impl Default for Array {
    fn default() -> Self {
        Array {
            ty: ArrayType::Num,
            shape: Vec::new(),
            data: Data {
                numbers: ManuallyDrop::new(Vec::new()),
            },
        }
    }
}

pub union Data {
    numbers: ManuallyDrop<Vec<f64>>,
    bytes: ManuallyDrop<Vec<u8>>,
    chars: ManuallyDrop<Vec<char>>,
    values: ManuallyDrop<Vec<Value>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum ArrayType {
    #[default]
    Num,
    Byte,
    Char,
    Value,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayType::Num => write!(f, "numbers"),
            ArrayType::Byte => write!(f, "bytes"),
            ArrayType::Char => write!(f, "characters"),
            ArrayType::Value => write!(f, "values"),
        }
    }
}

impl Array {
    #[track_caller]
    pub fn from_cells(cells: Vec<Array>) -> Self {
        if cells.len() == 1 {
            return cells.into_iter().next().unwrap();
        }
        let mut shape = cells[0].shape.clone();
        shape.insert(0, cells.len());
        assert!(
            cells.windows(2).all(|w| w[0].shape == w[1].shape),
            "all arrays must have the same shape"
        );
        let values: Vec<Value> = cells
            .into_iter()
            .flat_map(Array::into_flat_values)
            .collect();
        Self {
            shape,
            ty: ArrayType::Value,
            data: Data {
                values: ManuallyDrop::new(values),
            },
        }
        .normalized_type()
    }
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.shape.first().copied().unwrap_or(1)
    }
    pub fn data_len(&self) -> usize {
        self.shape.iter().product()
    }
    pub fn shape(&self) -> &[usize] {
        &self.shape
    }
    pub fn shape_mut(&mut self) -> &mut Vec<usize> {
        &mut self.shape
    }
    pub(crate) fn take_shape(&mut self) -> Vec<usize> {
        take(&mut self.shape)
    }
    pub(crate) fn set_shape(&mut self, shape: impl Into<Vec<usize>>) {
        self.shape = shape.into();
    }
    pub fn ty(&self) -> ArrayType {
        self.ty
    }
    pub fn is_numbers(&self) -> bool {
        self.ty == ArrayType::Num
    }
    pub fn is_bytes(&self) -> bool {
        self.ty == ArrayType::Byte
    }
    pub fn is_chars(&self) -> bool {
        self.ty == ArrayType::Char
    }
    pub fn is_values(&self) -> bool {
        self.ty == ArrayType::Value
    }
    pub fn numbers(&self) -> &[f64] {
        assert_eq!(self.ty, ArrayType::Num);
        unsafe { &self.data.numbers }
    }
    pub fn numbers_mut(&mut self) -> &mut Vec<f64> {
        assert_eq!(self.ty, ArrayType::Num);
        unsafe { &mut self.data.numbers }
    }
    pub fn try_iter_numbers(
        &self,
        env: &Uiua,
        requirement: &'static str,
        mut f: impl FnMut(f64, &Uiua) -> UiuaResult,
    ) -> UiuaResult {
        match self.ty {
            ArrayType::Num => {
                for &num in self.numbers() {
                    f(num, env)?;
                }
            }
            ArrayType::Byte => {
                for &byte in self.bytes() {
                    f(byte as f64, env)?;
                }
            }
            ty => return Err(env.error(format!("{requirement}, it is {ty}"))),
        }
        Ok(())
    }
    pub fn bytes(&self) -> &[u8] {
        assert_eq!(self.ty, ArrayType::Byte);
        unsafe { &self.data.bytes }
    }
    pub fn bytes_mut(&mut self) -> &mut Vec<u8> {
        assert_eq!(self.ty, ArrayType::Byte);
        unsafe { &mut self.data.bytes }
    }
    pub fn chars(&self) -> &[char] {
        assert_eq!(self.ty, ArrayType::Char);
        unsafe { &self.data.chars }
    }
    pub fn chars_mut(&mut self) -> &mut Vec<char> {
        assert_eq!(self.ty, ArrayType::Char);
        unsafe { &mut self.data.chars }
    }
    pub fn values(&self) -> &[Value] {
        assert_eq!(self.ty, ArrayType::Value);
        unsafe { &self.data.values }
    }
    pub fn values_mut(&mut self) -> &mut Vec<Value> {
        assert_eq!(self.ty, ArrayType::Value);
        unsafe { &mut self.data.values }
    }
    pub fn data<N, B, C, V, T>(&self, nums: N, bytes: B, chars: C, values: V) -> T
    where
        N: FnOnce(&[usize], &[f64]) -> T,
        B: FnOnce(&[usize], &[u8]) -> T,
        C: FnOnce(&[usize], &[char]) -> T,
        V: FnOnce(&[usize], &[Value]) -> T,
    {
        match self.ty {
            ArrayType::Num => nums(&self.shape, unsafe { &self.data.numbers }),
            ArrayType::Byte => bytes(&self.shape, unsafe { &self.data.bytes }),
            ArrayType::Char => chars(&self.shape, unsafe { &self.data.chars }),
            ArrayType::Value => values(&self.shape, unsafe { &self.data.values }),
        }
    }
    pub fn data_with<D, N, B, C, V, T>(&self, with: D, nums: N, bytes: B, chars: C, values: V) -> T
    where
        N: FnOnce(D, &[usize], &[f64]) -> T,
        B: FnOnce(D, &[usize], &[u8]) -> T,
        C: FnOnce(D, &[usize], &[char]) -> T,
        V: FnOnce(D, &[usize], &[Value]) -> T,
    {
        match self.ty {
            ArrayType::Num => nums(with, &self.shape, unsafe { &self.data.numbers }),
            ArrayType::Byte => bytes(with, &self.shape, unsafe { &self.data.bytes }),
            ArrayType::Char => chars(with, &self.shape, unsafe { &self.data.chars }),
            ArrayType::Value => values(with, &self.shape, unsafe { &self.data.values }),
        }
    }
    pub fn data_mut<N, B, C, V, T>(&mut self, nums: N, bytes: B, chars: C, values: V) -> T
    where
        N: FnOnce(&mut [usize], &mut [f64]) -> T,
        B: FnOnce(&mut [usize], &mut [u8]) -> T,
        C: FnOnce(&mut [usize], &mut [char]) -> T,
        V: FnOnce(&mut [usize], &mut [Value]) -> T,
    {
        match self.ty {
            ArrayType::Num => nums(&mut self.shape, unsafe { &mut self.data.numbers }),
            ArrayType::Byte => bytes(&mut self.shape, unsafe { &mut self.data.bytes }),
            ArrayType::Char => chars(&mut self.shape, unsafe { &mut self.data.chars }),
            ArrayType::Value => values(&mut self.shape, unsafe { &mut self.data.values }),
        }
    }
    pub fn take_flat_values(&mut self) -> Vec<Value> {
        match self.ty {
            ArrayType::Num => take(unsafe { &mut *self.data.numbers })
                .into_iter()
                .map(Into::into)
                .collect(),
            ArrayType::Byte => take(unsafe { &mut *self.data.bytes })
                .into_iter()
                .map(Into::into)
                .collect(),
            ArrayType::Char => take(unsafe { &mut *self.data.chars })
                .into_iter()
                .map(Into::into)
                .collect(),
            ArrayType::Value => take(unsafe { &mut *self.data.values }),
        }
    }
    pub fn into_flat_values(mut self) -> Vec<Value> {
        self.take_flat_values()
    }
    pub fn into_shape_flat_values(mut self) -> (Vec<usize>, Vec<Value>) {
        (take(&mut self.shape), self.into_flat_values())
    }
    pub fn into_shape_values(mut self) -> (Vec<usize>, Vec<Value>) {
        (take(&mut self.shape), self.into_values())
    }
    pub fn into_numbers(mut self) -> Vec<f64> {
        assert_eq!(self.ty, ArrayType::Num);
        take(unsafe { &mut *self.data.numbers })
    }
    pub fn into_bytes(mut self) -> Vec<u8> {
        assert_eq!(self.ty, ArrayType::Byte);
        take(unsafe { &mut *self.data.bytes })
    }
    pub fn into_chars(mut self) -> Vec<char> {
        assert_eq!(self.ty, ArrayType::Char);
        take(unsafe { &mut *self.data.chars })
    }
    pub fn make_values(&mut self) -> &mut Vec<Value> {
        if self.ty != ArrayType::Value {
            self.data = Data {
                values: ManuallyDrop::new(self.take_flat_values()),
            };
            self.ty = ArrayType::Value;
        }
        self.values_mut()
    }
    pub fn normalize_type(&mut self) {
        match self.ty {
            ArrayType::Value => {
                if self.values().is_empty() {
                    return;
                }
                let values = self.values().iter();
                if self.values().iter().all(Value::is_char) {
                    self.data = values.map(Value::char).collect::<Vec<_>>().into();
                    self.ty = ArrayType::Char;
                } else if self.values().iter().all(Value::is_number) {
                    self.data = values.map(Value::number).collect::<Vec<_>>().into();
                    self.ty = ArrayType::Num;
                } else if self.values().iter().all(Value::is_byte) {
                    self.data = values.map(Value::byte).collect::<Vec<_>>().into();
                    self.ty = ArrayType::Byte;
                }
            }
            ArrayType::Num => {
                if self
                    .numbers()
                    .iter()
                    .all(|n| (0.0..=255.0).contains(n) && n.fract() == 0.0)
                {
                    let numbers = self.numbers().iter().map(|&n| n as u8).collect::<Vec<_>>();
                    self.data = numbers.into();
                    self.ty = ArrayType::Byte;
                }
            }
            _ => (),
        }
    }
    pub fn normalize(&mut self) -> Option<(Vec<usize>, Vec<usize>)> {
        if !self.is_values() {
            self.normalize_type();
            return None;
        }
        let mut shape = None;
        for val in self.values() {
            let acc = shape.get_or_insert_with(|| val.shape());
            if val.shape() != *acc {
                return Some((acc.to_vec(), val.shape().to_vec()));
            }
        }
        let mut shape = shape.unwrap_or(&[]).to_vec();
        if shape.is_empty() {
            return None;
        }
        let values: Vec<Value> = self
            .take_flat_values()
            .into_iter()
            .map(Value::into_array)
            .flat_map(Array::into_flat_values)
            .collect();
        self.shape.append(&mut shape);
        let shape = take(&mut self.shape);
        *self = Self {
            ty: ArrayType::Value,
            shape,
            data: Data {
                values: ManuallyDrop::new(values),
            },
        }
        .normalized_type();
        None
    }
    pub fn normalized(mut self) -> Self {
        self.normalize();
        self
    }
    pub fn normalized_type(mut self) -> Self {
        self.normalize_type();
        self
    }
    pub fn deshape(&mut self) {
        let data_len: usize = self.shape.iter().product();
        self.shape = vec![data_len];
    }
    /// If rank <= 1, return the atom values, otherwise return the array cell values.
    pub fn into_values(mut self) -> Vec<Value> {
        if self.shape.is_empty() {
            return self.into_flat_values();
        }
        if self.shape.len() == 1 {
            return self.into_flat_values();
        }
        let mut shape = self.shape.clone();
        let cell_count = shape.remove(0);
        match self.ty {
            ArrayType::Num => into_cells(
                cell_count,
                take(unsafe { &mut self.data.numbers }),
                |numbers| Value::from(Array::from((shape.clone(), numbers))),
            ),
            ArrayType::Byte => {
                into_cells(cell_count, take(unsafe { &mut self.data.bytes }), |bytes| {
                    Value::from(Array::from((shape.clone(), bytes)))
                })
            }
            ArrayType::Char => {
                into_cells(cell_count, take(unsafe { &mut self.data.chars }), |chars| {
                    Value::from(Array::from((shape.clone(), chars)))
                })
            }
            ArrayType::Value => into_cells(
                cell_count,
                take(unsafe { &mut self.data.values }),
                |values| Value::from(Array::from((shape.clone(), values))),
            ),
        }
    }
    pub fn into_cells(mut self) -> Vec<Self> {
        if self.shape.is_empty() {
            return vec![self];
        }
        let mut shape = self.shape.clone();
        let cell_count = shape.remove(0);
        match self.ty {
            ArrayType::Num => into_cells(
                cell_count,
                take(unsafe { &mut self.data.numbers }),
                |numbers| Array::from((shape.clone(), numbers)),
            ),
            ArrayType::Byte => {
                into_cells(cell_count, take(unsafe { &mut self.data.bytes }), |bytes| {
                    Array::from((shape.clone(), bytes))
                })
            }
            ArrayType::Char => {
                into_cells(cell_count, take(unsafe { &mut self.data.chars }), |chars| {
                    Array::from((shape.clone(), chars))
                })
            }
            ArrayType::Value => into_cells(
                cell_count,
                take(unsafe { &mut self.data.values }),
                |values| Array::from((shape.clone(), values)),
            ),
        }
    }
    pub fn first(&self) -> Option<Value> {
        if self.shape.is_empty() {
            fn first<T>(_: &[usize], items: &[T]) -> Option<Value>
            where
                T: Clone + Into<Value>,
            {
                items.first().cloned().map(Into::into)
            }
            return self.data(first, first, first, first);
        }
        if self.shape == [0] {
            return None;
        }
        let cell_count = self.shape[0];
        let shape = &self.shape[1..];
        fn first<T>(cell_count: usize, shape: &[usize], items: &[T]) -> Value
        where
            T: Clone + Into<Value>,
            Array: From<(Vec<usize>, Vec<T>)>,
        {
            if shape.is_empty() {
                return items.first().unwrap().clone().into();
            }
            let cell_size = items.len() / cell_count;
            let items = items[..cell_size].to_vec();
            Array::from((shape.to_vec(), items)).into()
        }
        Some(match self.ty {
            ArrayType::Num => first(cell_count, shape, self.numbers()),
            ArrayType::Byte => first(cell_count, shape, self.bytes()),
            ArrayType::Char => first(cell_count, shape, self.chars()),
            ArrayType::Value => first(cell_count, shape, self.values()),
        })
    }
    pub fn last(&self) -> Option<Value> {
        if self.shape.is_empty() {
            fn last<T>(_: &[usize], items: &[T]) -> Option<Value>
            where
                T: Clone + Into<Value>,
            {
                items.last().cloned().map(Into::into)
            }
            return self.data(last, last, last, last);
        }
        if self.shape == [0] {
            return None;
        }
        let cell_count = self.shape[0];
        let shape = &self.shape[1..];
        fn last<T>(cell_count: usize, shape: &[usize], items: &[T]) -> Value
        where
            T: Clone + Into<Value>,
            Array: From<(Vec<usize>, Vec<T>)>,
        {
            if shape.is_empty() {
                return items.last().unwrap().clone().into();
            }
            let cell_size = items.len() / cell_count;
            let items = items[items.len() - cell_size..].to_vec();
            Array::from((shape.to_vec(), items)).into()
        }
        Some(match self.ty {
            ArrayType::Num => last(cell_count, shape, self.numbers()),
            ArrayType::Byte => last(cell_count, shape, self.bytes()),
            ArrayType::Char => last(cell_count, shape, self.chars()),
            ArrayType::Value => last(cell_count, shape, self.values()),
        })
    }
}

fn into_cells<T, F, R>(cell_count: usize, mut items: Vec<T>, f: F) -> Vec<R>
where
    F: Fn(Vec<T>) -> R,
{
    let cell_size = items.len() / cell_count;
    let mut cells = Vec::with_capacity(cell_count);
    for _ in 0..cell_count {
        cells.push(f(items.drain(items.len() - cell_size..).collect()));
    }
    cells.reverse();
    cells
}

impl Drop for Array {
    fn drop(&mut self) {
        match self.ty {
            ArrayType::Num => unsafe { ManuallyDrop::drop(&mut self.data.numbers) },
            ArrayType::Byte => unsafe { ManuallyDrop::drop(&mut self.data.bytes) },
            ArrayType::Char => unsafe { ManuallyDrop::drop(&mut self.data.chars) },
            ArrayType::Value => unsafe { ManuallyDrop::drop(&mut self.data.values) },
        }
    }
}

impl Clone for Array {
    fn clone(&self) -> Self {
        match self.ty {
            ArrayType::Num => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    numbers: ManuallyDrop::new(self.numbers().to_vec()),
                },
            },
            ArrayType::Byte => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    bytes: ManuallyDrop::new(self.bytes().to_vec()),
                },
            },
            ArrayType::Char => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    chars: ManuallyDrop::new(self.chars().to_vec()),
                },
            },
            ArrayType::Value => Self {
                ty: self.ty,
                shape: self.shape.clone(),
                data: Data {
                    values: ManuallyDrop::new(self.values().to_vec()),
                },
            },
        }
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        if self.data_len() != other.data_len() {
            return false;
        }
        match (self.ty, other.ty) {
            (ArrayType::Num, ArrayType::Byte) => {
                self.numbers().iter().zip(other.bytes()).all(|(n, b)| {
                    let b = *b as f64;
                    *n == b || n.is_nan() && b.is_nan()
                })
            }
            (ArrayType::Byte, ArrayType::Num) => {
                self.bytes().iter().zip(other.numbers()).all(|(b, n)| {
                    let b = *b as f64;
                    *n == b || n.is_nan() && b.is_nan()
                })
            }
            _ => {
                if self.ty != other.ty {
                    return false;
                }
                if self.shape != other.shape {
                    return false;
                }
                match self.ty {
                    ArrayType::Num => self.numbers() == other.numbers(),
                    ArrayType::Byte => self.bytes() == other.bytes(),
                    ArrayType::Char => self.chars() == other.chars(),
                    ArrayType::Value => self.values() == other.values(),
                }
            }
        }
    }
}

impl Eq for Array {}

impl PartialOrd for Array {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Array {
    fn cmp(&self, other: &Self) -> Ordering {
        self.ty
            .cmp(&other.ty)
            .then_with(|| self.shape.cmp(&other.shape))
            .then_with(|| match self.ty {
                ArrayType::Num => {
                    let a = self.numbers();
                    let b = other.numbers();
                    a.len().cmp(&b.len()).then_with(|| {
                        for (a, b) in a.iter().zip(b) {
                            let ordering = match (a.is_nan(), b.is_nan()) {
                                (true, true) => Ordering::Equal,
                                (true, false) => Ordering::Greater,
                                (false, true) => Ordering::Less,
                                (false, false) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
                            };
                            if ordering != Ordering::Equal {
                                return ordering;
                            }
                        }
                        Ordering::Equal
                    })
                }
                ArrayType::Byte => self.bytes().cmp(other.bytes()),
                ArrayType::Char => self.chars().cmp(other.chars()),
                ArrayType::Value => self.values().cmp(other.values()),
            })
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            ArrayType::Num => write!(f, "{:?}", self.numbers()),
            ArrayType::Byte => write!(f, "{:?}", self.bytes()),
            ArrayType::Char if self.rank() == 1 => {
                write!(f, "{:?}", self.chars().iter().collect::<String>())
            }
            ArrayType::Char => write!(f, "{:?}", self.chars()),
            ArrayType::Value => write!(f, "{:?}", self.values()),
        }
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            ArrayType::Num => write!(f, "{:?}", self.numbers()),
            ArrayType::Byte => write!(f, "{:?}", self.bytes()),
            ArrayType::Char if self.rank() == 1 => {
                write!(f, "{}", self.chars().iter().collect::<String>())
            }
            ArrayType::Char => write!(f, "{:?}", self.chars()),
            ArrayType::Value => write!(f, "{:?}", self.values()),
        }
    }
}

impl From<ArrayType> for Array {
    fn from(ty: ArrayType) -> Self {
        Self {
            shape: vec![],
            ty,
            data: match ty {
                ArrayType::Num => Vec::<f64>::new().into(),
                ArrayType::Byte => Vec::<u8>::new().into(),
                ArrayType::Char => Vec::<char>::new().into(),
                ArrayType::Value => Vec::<Value>::new().into(),
            },
        }
    }
}

impl From<u8> for Array {
    fn from(n: u8) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Byte,
            data: vec![n].into(),
        }
    }
}

impl From<f64> for Array {
    fn from(n: f64) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Num,
            data: vec![n].into(),
        }
    }
}

impl From<char> for Array {
    fn from(c: char) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Char,
            data: vec![c].into(),
        }
    }
}

impl From<Function> for Array {
    fn from(f: Function) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: vec![Value::from(f)].into(),
        }
    }
}

impl From<Rc<Function>> for Array {
    fn from(f: Rc<Function>) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: vec![Value::from(f)].into(),
        }
    }
}

impl From<Value> for Array {
    fn from(v: Value) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: vec![v].into(),
        }
    }
}

impl<T> From<(Vec<usize>, T)> for Array
where
    Array: From<T>,
{
    fn from((shape, data): (Vec<usize>, T)) -> Self {
        let mut arr = Array::from(data);
        arr.shape = shape;
        arr
    }
}

impl From<Vec<u8>> for Array {
    fn from(v: Vec<u8>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Byte,
            data: v.into(),
        }
    }
}

impl From<Vec<f64>> for Array {
    fn from(v: Vec<f64>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Num,
            data: v.into(),
        }
    }
}

impl From<Vec<char>> for Array {
    fn from(v: Vec<char>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Char,
            data: v.into(),
        }
    }
}

impl<'a> From<&'a str> for Array {
    fn from(s: &'a str) -> Self {
        Self::from(s.chars().collect::<Vec<_>>())
    }
}

impl From<String> for Array {
    fn from(s: String) -> Self {
        Self::from(s.as_str())
    }
}

impl From<Vec<Value>> for Array {
    fn from(v: Vec<Value>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Value,
            data: v.into(),
        }
        .normalized_type()
    }
}

impl<T> FromIterator<T> for Array
where
    Self: From<Vec<T>>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self::from(iter.into_iter().collect::<Vec<_>>())
    }
}

impl From<Vec<f64>> for Data {
    fn from(v: Vec<f64>) -> Self {
        Data {
            numbers: ManuallyDrop::new(v),
        }
    }
}

impl From<Vec<u8>> for Data {
    fn from(v: Vec<u8>) -> Self {
        Data {
            bytes: ManuallyDrop::new(v),
        }
    }
}

impl From<Vec<char>> for Data {
    fn from(v: Vec<char>) -> Self {
        Data {
            chars: ManuallyDrop::new(v),
        }
    }
}

impl From<Vec<Value>> for Data {
    fn from(v: Vec<Value>) -> Self {
        Data {
            values: ManuallyDrop::new(v),
        }
    }
}
