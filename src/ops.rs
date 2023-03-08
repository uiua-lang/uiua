use std::{cmp::Ordering, fmt::Debug};

use crate::{compile::Assembly, RuntimeError};

#[derive(Clone, Copy)]
pub struct Env<'a> {
    pub span: usize,
    pub assembly: &'a Assembly,
}

impl<'a> Env<'a> {
    pub fn error(&self, message: impl Into<String>) -> RuntimeError {
        self.assembly.spans[self.span].error(message)
    }
}

macro_rules! cmp_impl {
    ($name:ident $eq:tt $ordering:expr) => {
        pub mod $name {
            use super::*;
            pub fn num_num(a: &f64, b: &f64) -> f64 {
                (a.partial_cmp(b)
                    .unwrap_or_else(|| a.is_nan().cmp(&b.is_nan()))
                    $eq $ordering) as u8 as f64
            }
            pub fn generic<T: Ord>(a: &T, b: &T) -> f64 {
                (a.cmp(b) $eq $ordering) as u8 as f64
            }
            pub fn error<T: Debug>(_a: T, _b: T, _env: &Env) -> RuntimeError {
                unreachable!("Comparisons cannot fail")
            }
        }
    };
}

cmp_impl!(is_eq == Ordering::Equal);
cmp_impl!(is_ne != Ordering::Equal);
cmp_impl!(is_lt == Ordering::Less);
cmp_impl!(is_le != Ordering::Greater);
cmp_impl!(is_gt == Ordering::Greater);
cmp_impl!(is_ge != Ordering::Less);

pub mod add {
    use super::*;
    pub fn num_num(a: &f64, b: &f64) -> f64 {
        a + b
    }
    pub fn num_char(a: &f64, b: &char) -> char {
        char::from_u32(*a as u32 + *b as u32).unwrap_or('\0')
    }
    pub fn char_num(a: &char, b: &f64) -> char {
        char::from_u32(*a as u32 + *b as u32).unwrap_or('\0')
    }
    pub fn error<T: Debug>(a: T, b: T, env: &Env) -> RuntimeError {
        env.error(format!("Cannot add {a:?} and {b:?}"))
    }
}

pub mod sub {
    use super::*;
    pub fn num_num(a: &f64, b: &f64) -> f64 {
        a - b
    }
    pub fn char_num(a: &char, b: &f64) -> char {
        char::from_u32((*a as u32).saturating_sub(*b as u32)).unwrap_or('\0')
    }
    pub fn error<T: Debug>(a: T, b: T, env: &Env) -> RuntimeError {
        env.error(format!("Cannot subtract {b:?} from {a:?}"))
    }
}

pub mod mul {
    use super::*;
    pub fn num_num(a: &f64, b: &f64) -> f64 {
        a * b
    }
    pub fn error<T: Debug>(a: T, b: T, env: &Env) -> RuntimeError {
        env.error(format!("Cannot multiply {a:?} and {b:?}"))
    }
}

pub mod div {
    use super::*;
    pub fn num_num(a: &f64, b: &f64) -> f64 {
        a / b
    }
    pub fn error<T: Debug>(a: T, b: T, env: &Env) -> RuntimeError {
        env.error(format!("Cannot divide {a:?} by {b:?}"))
    }
}
