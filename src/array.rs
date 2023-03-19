use std::{
    cmp::Ordering,
    fmt,
    mem::{swap, take, ManuallyDrop},
};

use crate::{
    algorithm::*,
    function::Function,
    pervade::{self, *},
    value::Value,
    vm::Env,
    RuntimeResult,
};

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
    chars: ManuallyDrop<Vec<char>>,
    values: ManuallyDrop<Vec<Value>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum ArrayType {
    #[default]
    Num,
    Char,
    Value,
}

impl fmt::Display for ArrayType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrayType::Num => write!(f, "numbers"),
            ArrayType::Char => write!(f, "characters"),
            ArrayType::Value => write!(f, "values"),
        }
    }
}

impl Array {
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
    pub fn ty(&self) -> ArrayType {
        self.ty
    }
    pub fn is_numbers(&self) -> bool {
        self.ty == ArrayType::Num
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
    pub fn data<N, C, V, T>(&self, nums: N, chars: C, values: V) -> T
    where
        N: FnOnce(&[usize], &[f64]) -> T,
        C: FnOnce(&[usize], &[char]) -> T,
        V: FnOnce(&[usize], &[Value]) -> T,
    {
        match self.ty {
            ArrayType::Num => nums(&self.shape, unsafe { &self.data.numbers }),
            ArrayType::Char => chars(&self.shape, unsafe { &self.data.chars }),
            ArrayType::Value => values(&self.shape, unsafe { &self.data.values }),
        }
    }
    pub fn data_mut<N, C, V, T>(&mut self, nums: N, chars: C, values: V) -> T
    where
        N: FnOnce(&mut [usize], &mut [f64]) -> T,
        C: FnOnce(&mut [usize], &mut [char]) -> T,
        V: FnOnce(&mut [usize], &mut [Value]) -> T,
    {
        match self.ty {
            ArrayType::Num => nums(&mut self.shape, unsafe { &mut self.data.numbers }),
            ArrayType::Char => chars(&mut self.shape, unsafe { &mut self.data.chars }),
            ArrayType::Value => values(&mut self.shape, unsafe { &mut self.data.values }),
        }
    }
    fn take_flat_values(&mut self) -> Vec<Value> {
        match self.ty {
            ArrayType::Num => take(unsafe { &mut *self.data.numbers })
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
    pub fn into_parts(mut self) -> (Vec<usize>, Vec<Value>) {
        (take(&mut self.shape), self.into_flat_values())
    }
    pub fn into_numbers(mut self) -> Vec<f64> {
        assert_eq!(self.ty, ArrayType::Num);
        take(unsafe { &mut *self.data.numbers })
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
    pub fn normalize(&mut self, array_depth: usize) {
        if let ArrayType::Value = self.ty {
            if self.values().is_empty() {
                return;
            }
            if self.values().iter().all(Value::is_char) {
                let shape = take(&mut self.shape);
                *self = Self::from(self.values().iter().map(Value::char).collect::<Vec<_>>());
                self.shape = shape;
            } else if self.values().iter().all(Value::is_num) {
                let shape = take(&mut self.shape);
                *self = Self::from(self.values().iter().map(Value::number).collect::<Vec<_>>());
                self.shape = shape;
            } else if array_depth > 0 && self.values().iter().all(Value::is_array) {
                let mut shape = None;
                for arr in self.values().iter().map(Value::array) {
                    if arr.shape != *shape.get_or_insert_with(|| arr.shape()) {
                        return;
                    }
                }
                let mut shape = shape.unwrap_or(&[]).to_vec();
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
                .normalized(array_depth - 1);
            }
        }
    }
    pub fn normalized(mut self, array_depth: usize) -> Self {
        self.normalize(array_depth);
        self
    }
    pub fn sort(&mut self) {
        let shape = self.shape.clone();
        match self.ty {
            ArrayType::Num => sort_array(&shape, self.numbers_mut(), |a, b| {
                a.partial_cmp(b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
            }),
            ArrayType::Char => sort_array(&shape, self.chars_mut(), Ord::cmp),
            ArrayType::Value => sort_array(&shape, self.values_mut(), Ord::cmp),
        }
    }
    pub fn deshape(&mut self) {
        let data_len: usize = self.shape.iter().product();
        self.shape = vec![data_len];
    }
    pub fn reshape(&mut self, shape: impl IntoIterator<Item = usize>) {
        self.shape = shape.into_iter().collect();
        let new_len: usize = self.shape.iter().product();
        match self.ty {
            ArrayType::Num => force_length(self.numbers_mut(), new_len),
            ArrayType::Char => force_length(self.chars_mut(), new_len),
            ArrayType::Value => force_length(self.values_mut(), new_len),
        }
    }
    pub fn reverse(&mut self) {
        let shape = self.shape.clone();
        match self.ty {
            ArrayType::Num => reverse(&shape, self.numbers_mut()),
            ArrayType::Char => reverse(&shape, self.chars_mut()),
            ArrayType::Value => reverse(&shape, self.values_mut()),
        }
    }
    pub fn join(&mut self, mut other: Self, env: &Env) -> RuntimeResult {
        if self.shape.is_empty() && other.shape.is_empty() {
            // Atom case
            self.take_values_from(other);
            self.shape = vec![2];
        } else {
            let rank_diff = self.rank() as isize - other.rank() as isize;
            if rank_diff.abs() > 1 {
                return Err(env.error(format!(
                    "Joined values cannot have a rank difference greater than 1, \
                    but ranks are {} and {}",
                    self.rank(),
                    other.rank()
                )));
            }
            match self.rank().cmp(&other.rank()) {
                Ordering::Equal => {
                    if self.shape[1..] != other.shape[1..] {
                        return Err(env.error(format!(
                            "Joined arrays of the same rank must have the same \
                            non-leading shape, but the shapes are {:?} and {:?}",
                            self.shape, other.shape
                        )));
                    }
                    self.shape[0] += other.shape[0];
                    self.take_values_from(other);
                }
                Ordering::Greater => {
                    if self.shape[1..] != other.shape {
                        return Err(env.error(format!(
                            "Appended arrays must have the same non-leading shape, \
                            but the shapes are {:?} and {:?}",
                            self.shape, other.shape
                        )));
                    }
                    self.shape[0] += 1;
                    self.take_values_from(other);
                }
                Ordering::Less => {
                    if self.shape != other.shape[1..] {
                        return Err(env.error(format!(
                            "Prepended arrays must have the same non-leading shape, \
                            but the shapes are {:?} and {:?}",
                            self.shape, other.shape
                        )));
                    }
                    self.reverse();
                    other.reverse();
                    swap(self, &mut other);
                    self.take_values_from(other);
                    self.shape[0] += 1;
                    self.reverse();
                }
            }
        }
        Ok(())
    }
    /// Simply take the values from the other array and append them to this one.
    /// Does not update the shape.
    fn take_values_from(&mut self, other: Self) {
        match (self.ty, other.ty) {
            (ArrayType::Num, ArrayType::Num) => {
                self.numbers_mut().extend(other.into_numbers());
            }
            (ArrayType::Char, ArrayType::Char) => {
                self.chars_mut().extend(other.into_chars());
            }
            (ArrayType::Value, ArrayType::Value) => {
                self.values_mut().extend(other.into_flat_values());
            }
            _ => {
                let shape = take(&mut self.shape);
                let mut values = self.take_flat_values();
                values.append(&mut other.into_flat_values());
                *self = Array::from((shape, values));
            }
        }
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
    pub fn into_first(mut self) -> Option<Value> {
        if self.shape.is_empty() {
            return self.into_flat_values().pop();
        }
        if self.shape == [0] {
            return None;
        }
        let mut shape = take(&mut self.shape);
        let cell_count = shape.remove(0);
        fn into_first<T>(
            cell_count: usize,
            shape: Vec<usize>,
            items: &mut ManuallyDrop<Vec<T>>,
        ) -> Value
        where
            T: Into<Value>,
            Array: From<(Vec<usize>, Vec<T>)>,
        {
            let mut items = take(&mut **items);
            if shape.is_empty() {
                return items.into_iter().next().unwrap().into();
            }
            let cell_size = items.len() / cell_count;
            items.truncate(cell_size);
            Array::from((shape, items)).into()
        }
        Some(match self.ty {
            ArrayType::Num => into_first(cell_count, shape, unsafe { &mut self.data.numbers }),
            ArrayType::Char => into_first(cell_count, shape, unsafe { &mut self.data.chars }),
            ArrayType::Value => into_first(cell_count, shape, unsafe { &mut self.data.values }),
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

macro_rules! array_un_impl {
    ($name:ident,
        $(($ty:ident, $get:ident, $f:ident)),*
    $(,)?) => {
        impl Array {
            #[allow(unreachable_patterns)]
            pub fn $name(&self, env: &Env) -> RuntimeResult<Self> {
                let shape = self.shape.clone();
                Ok(match self.ty {
                    $(ArrayType::$ty => (shape, un_pervade(self.$get(), pervade::$name::$f)).into(),)*
                    ArrayType::Value => {
                        (shape, un_pervade_fallible(self.values(), env, Value::$name)?).into()
                    }
                    ty => return Err(pervade::$name::error(ty, env)),
                })
            }
        }
    };
}

array_un_impl!(not, (Num, numbers, num));
array_un_impl!(neg, (Num, numbers, num));
array_un_impl!(abs, (Num, numbers, num));
array_un_impl!(sqrt, (Num, numbers, num));
array_un_impl!(sin, (Num, numbers, num));
array_un_impl!(cos, (Num, numbers, num));
array_un_impl!(asin, (Num, numbers, num));
array_un_impl!(acos, (Num, numbers, num));
array_un_impl!(floor, (Num, numbers, num));
array_un_impl!(ceil, (Num, numbers, num));
array_un_impl!(round, (Num, numbers, num));

macro_rules! array_bin_impl {
    ($name:ident,
        $(($a_ty:ident, $af:ident, $b_ty:ident, $bf:ident, $ab:ident)),*
    $(,)?) => {
        impl Array {
            #[allow(unreachable_patterns)]
            pub fn $name(&self, other: &Self, env: &Env) -> RuntimeResult<Self> {
                let ash = self.shape();
                let bsh = other.shape();
                Ok(match (self.ty, other.ty) {
                    $((ArrayType::$a_ty, ArrayType::$b_ty) =>
                        bin_pervade(ash, self.$af(), bsh, other.$bf(), env, pervade::$name::$ab)?.into(),)*
                    (ArrayType::Value, ArrayType::Value) => {
                        bin_pervade_fallible(ash, self.values(), bsh, other.values(), env, Value::$name)?.into()
                    }
                    $((ArrayType::Value, ArrayType::$b_ty) => {
                        bin_pervade_fallible(ash, self.values(), bsh, other.$bf(), env,
                            |a, b, env| Value::$name(a, &b.clone().into(), env))?.into()
                    },)*
                    $((ArrayType::$a_ty, ArrayType::Value) => {
                        bin_pervade_fallible(ash, self.$af(), bsh, other.values(), env,
                            |a, b, env| Value::$name(&a.clone().into(), b, env))?.into()
                    },)*
                    (a, b) => return Err(pervade::$name::error(a, b, env)),
                })
            }
        }
    };
}

array_bin_impl!(
    add,
    (Num, numbers, Num, numbers, num_num),
    (Num, numbers, Char, chars, num_char),
    (Char, chars, Num, numbers, char_num),
);

array_bin_impl!(
    sub,
    (Num, numbers, Num, numbers, num_num),
    (Num, numbers, Char, chars, num_char),
    (Char, chars, Char, chars, char_char),
);

array_bin_impl!(mul, (Num, numbers, Num, numbers, num_num));
array_bin_impl!(div, (Num, numbers, Num, numbers, num_num));
array_bin_impl!(modulus, (Num, numbers, Num, numbers, num_num));
array_bin_impl!(pow, (Num, numbers, Num, numbers, num_num));
array_bin_impl!(root, (Num, numbers, Num, numbers, num_num));
array_bin_impl!(atan2, (Num, numbers, Num, numbers, num_num));

array_bin_impl!(
    min,
    (Num, numbers, Num, numbers, num_num),
    (Char, chars, Char, chars, char_char),
    (Char, chars, Num, numbers, char_num),
    (Num, numbers, Char, chars, num_char),
);

array_bin_impl!(
    max,
    (Num, numbers, Num, numbers, num_num),
    (Char, chars, Char, chars, char_char),
    (Char, chars, Num, numbers, char_num),
    (Num, numbers, Char, chars, num_char),
);

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            array_bin_impl!(
                $name,
                (Num, numbers, Num, numbers, num_num),
                (Char, chars, Char, chars, generic),
                (Num, numbers, Char, chars, always_less),
                (Char, chars, Num, numbers, always_greater),
            );
        )*
    };
}

cmp_impls!(is_eq, is_ne, is_lt, is_le, is_gt, is_ge);

impl Drop for Array {
    fn drop(&mut self) {
        match self.ty {
            ArrayType::Num => unsafe {
                ManuallyDrop::drop(&mut self.data.numbers);
            },
            ArrayType::Char => unsafe {
                ManuallyDrop::drop(&mut self.data.chars);
            },
            ArrayType::Value => unsafe {
                ManuallyDrop::drop(&mut self.data.values);
            },
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
        if self.ty != other.ty {
            return false;
        }
        if self.shape != other.shape {
            return false;
        }
        match self.ty {
            ArrayType::Num => self.numbers() == other.numbers(),
            ArrayType::Char => self.chars() == other.chars(),
            ArrayType::Value => self.values() == other.values(),
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
                ArrayType::Char => self.chars().cmp(other.chars()),
                ArrayType::Value => self.values().cmp(other.values()),
            })
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.ty {
            ArrayType::Num => write!(f, "{:?}", self.numbers()),
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
                ArrayType::Num => Data {
                    numbers: ManuallyDrop::new(vec![]),
                },
                ArrayType::Char => Data {
                    chars: ManuallyDrop::new(vec![]),
                },
                ArrayType::Value => Data {
                    values: ManuallyDrop::new(vec![]),
                },
            },
        }
    }
}

impl From<f64> for Array {
    fn from(n: f64) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Num,
            data: Data {
                numbers: ManuallyDrop::new(vec![n]),
            },
        }
    }
}

impl From<char> for Array {
    fn from(c: char) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Char,
            data: Data {
                chars: ManuallyDrop::new(vec![c]),
            },
        }
    }
}

impl From<Function> for Array {
    fn from(f: Function) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: Data {
                values: ManuallyDrop::new(vec![Value::from(f)]),
            },
        }
    }
}

impl From<Value> for Array {
    fn from(v: Value) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: Data {
                values: ManuallyDrop::new(vec![v]),
            },
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

impl From<Vec<f64>> for Array {
    fn from(v: Vec<f64>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Num,
            data: Data {
                numbers: ManuallyDrop::new(v),
            },
        }
    }
}

impl From<Vec<char>> for Array {
    fn from(v: Vec<char>) -> Self {
        Self {
            shape: vec![v.len()],
            ty: ArrayType::Char,
            data: Data {
                chars: ManuallyDrop::new(v),
            },
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
            data: Data {
                values: ManuallyDrop::new(v),
            },
        }
        .normalized(0)
    }
}

impl From<Vec<Array>> for Array {
    #[track_caller]
    fn from(v: Vec<Array>) -> Self {
        if v.len() == 1 {
            return v.into_iter().next().unwrap();
        }
        let mut shape = v[0].shape.clone();
        shape.insert(0, v.len());
        assert!(
            v.windows(2).all(|w| w[0].shape == w[1].shape),
            "all arrays must have the same shape"
        );
        let values: Vec<Value> = v.into_iter().flat_map(Array::into_flat_values).collect();
        Self {
            shape,
            ty: ArrayType::Value,
            data: Data {
                values: ManuallyDrop::new(values),
            },
        }
        .normalized(0)
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
