use std::{
    cmp::Ordering,
    fmt,
    mem::{take, ManuallyDrop},
};

use crate::{
    algorithm::*,
    function::{Function, Partial},
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

impl Array {
    pub fn rank(&self) -> usize {
        self.shape.len()
    }
    pub fn len(&self) -> usize {
        self.shape.first().copied().unwrap_or(0)
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn data_len(&self) -> usize {
        self.shape.iter().product()
    }
    pub fn shape(&self) -> &[usize] {
        &self.shape
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
    pub fn clone_values(&self) -> Vec<Value> {
        match self.ty {
            ArrayType::Num => self.numbers().iter().copied().map(Into::into).collect(),
            ArrayType::Char => self.chars().iter().copied().map(Into::into).collect(),
            ArrayType::Value => self.values().to_vec(),
        }
    }
    pub fn values_mut(&mut self) -> &mut Vec<Value> {
        assert_eq!(self.ty, ArrayType::Value);
        unsafe { &mut self.data.values }
    }
    pub fn range(n: usize) -> Self {
        Self::from((0..n).map(|n| n as f64).collect::<Vec<_>>())
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
    pub fn shape_prefix_matches(&self, other: &Self) -> bool {
        self.shape.iter().zip(&other.shape).all(|(a, b)| a == b)
    }
    pub fn normalize(&mut self, arrays: bool) {
        if let ArrayType::Value = self.ty {
            if self.values().iter().all(Value::is_char) {
                let shape = take(&mut self.shape);
                *self = Self::from(self.values().iter().map(Value::char).collect::<Vec<_>>());
                self.shape = shape;
            } else if self.values().iter().all(Value::is_num) {
                let shape = take(&mut self.shape);
                *self = Self::from(self.values().iter().map(Value::number).collect::<Vec<_>>());
                self.shape = shape;
            } else if arrays && self.values().iter().all(Value::is_array) {
                let mut shape = None;
                for arr in self.values().iter().map(Value::array) {
                    if arr.shape != *shape.get_or_insert_with(|| arr.shape()) {
                        return;
                    }
                }
                let mut shape = shape.unwrap_or(&[]).to_vec();
                let values: Vec<Value> = self
                    .values()
                    .iter()
                    .map(Value::array)
                    .flat_map(Array::clone_values)
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
                .normalized(arrays);
            }
        }
    }
    pub fn normalized(mut self, arrays: bool) -> Self {
        self.normalize(arrays);
        self
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
}

macro_rules! array_impl {
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
                        bin_pervade(ash, self.$af(), bsh, other.$bf(), pervade::$name::$ab)?.into(),)*
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

array_impl!(
    add,
    (Num, numbers, Num, numbers, num_num),
    (Num, numbers, Char, chars, num_char),
    (Char, chars, Num, numbers, char_num),
);

array_impl!(
    sub,
    (Num, numbers, Num, numbers, num_num),
    (Char, chars, Num, numbers, char_num),
);

array_impl!(mul, (Num, numbers, Num, numbers, num_num));
array_impl!(div, (Num, numbers, Num, numbers, num_num));
array_impl!(modulus, (Num, numbers, Num, numbers, num_num));
array_impl!(pow, (Num, numbers, Num, numbers, num_num));
array_impl!(atan2, (Num, numbers, Num, numbers, num_num));

array_impl!(
    min,
    (Num, numbers, Num, numbers, num_num),
    (Char, chars, Char, chars, char_char),
    (Char, chars, Num, numbers, char_num),
    (Num, numbers, Char, chars, num_char),
);

array_impl!(
    max,
    (Num, numbers, Num, numbers, num_num),
    (Char, chars, Char, chars, char_char),
    (Char, chars, Num, numbers, char_num),
    (Num, numbers, Char, chars, num_char),
);

macro_rules! cmp_impls {
    ($($name:ident),*) => {
        $(
            array_impl!(
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

impl From<Partial> for Array {
    fn from(p: Partial) -> Self {
        Self {
            shape: vec![],
            ty: ArrayType::Value,
            data: Data {
                values: ManuallyDrop::new(vec![Value::from(p)]),
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
        .normalized(false)
    }
}

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self::from(iter.into_iter().collect::<Vec<_>>())
    }
}
