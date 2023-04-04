use std::{cmp::Ordering, fmt::Display, slice};

use crate::{Uiua, UiuaError, UiuaResult};

use super::array::{Array, ArrayValue, Arrayish};

fn unit_arrayish<T: ArrayValue>(value: &T) -> (&[usize], &[T]) {
    (&[], slice::from_ref(value))
}

pub fn bin_pervade<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: &Array<A>,
    b: &Array<B>,
    env: &Uiua,
    f: impl Fn(A, B) -> C,
) -> UiuaResult<Array<C>> {
    if !a.shape_prefixes_match(&b) {
        return Err(env.error(format!(
            "Shapes {:?} and {:?} do not match",
            a.shape(),
            b.shape()
        )));
    }
    let shape = a.shape().max(b.shape()).to_vec();
    let mut data = Vec::with_capacity(a.flat_len().max(b.flat_len()));
    bin_pervade_recursive(a, b, &mut data, f);
    Ok(Array::new(shape, data))
}

pub fn bin_pervade_fallible<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: &Array<A>,
    b: &Array<B>,
    env: &Uiua,
    f: impl Fn(A, B, &Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult<Array<C>> {
    if !a.shape_prefixes_match(&b) {
        return Err(env.error(format!(
            "Shapes {:?} and {:?} do not match",
            a.shape(),
            b.shape()
        )));
    }
    let shape = a.shape().max(b.shape()).to_vec();
    let mut data = Vec::with_capacity(a.flat_len().max(b.flat_len()));
    bin_pervade_recursive_fallible(a, b, &mut data, env, f)?;
    Ok(Array::new(shape, data))
}

fn bin_pervade_recursive<A: Arrayish, B: Arrayish, C: ArrayValue>(
    a: A,
    b: B,
    c: &mut Vec<C>,
    f: impl Fn(A::Value, B::Value) -> C,
) {
    match (a.shape(), b.shape()) {
        ([], []) => c.push(f(a.data()[0].clone(), b.data()[0].clone())),
        (ash, bsh) if ash == bsh => {
            for (a, b) in a.data().iter().zip(b.data().iter()) {
                c.push(f(a.clone(), b.clone()));
            }
        }
        ([], bsh) => {
            for (a, b) in a.data().iter().zip(b.rows()) {
                bin_pervade_recursive(unit_arrayish(a), (&bsh[1..], b), c, &f);
            }
        }
        (ash, []) => {
            for (a, b) in a.rows().zip(b.data().iter()) {
                bin_pervade_recursive((&ash[1..], a), unit_arrayish(b), c, &f);
            }
        }
        (ash, bsh) => {
            for (a, b) in a.rows().zip(b.rows()) {
                bin_pervade_recursive((&ash[1..], a), (&bsh[1..], b), c, &f);
            }
        }
    }
}

fn bin_pervade_recursive_fallible<A: Arrayish, B: Arrayish, C: ArrayValue>(
    a: A,
    b: B,
    c: &mut Vec<C>,
    env: &Uiua,
    f: impl Fn(A::Value, B::Value, &Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult {
    match (a.shape(), b.shape()) {
        ([], []) => c.push(f(a.data()[0].clone(), b.data()[0].clone(), env)?),
        ([], bsh) => {
            for (a, b) in a.data().iter().zip(b.rows()) {
                bin_pervade_recursive_fallible(unit_arrayish(a), (&bsh[1..], b), c, env, f)?;
            }
        }
        (ash, []) => {
            for (a, b) in a.rows().zip(b.data().iter()) {
                bin_pervade_recursive_fallible((&ash[1..], a), unit_arrayish(b), c, env, f)?;
            }
        }
        (ash, bsh) => {
            for (a, b) in a.rows().zip(b.rows()) {
                bin_pervade_recursive_fallible((&ash[1..], a), (&bsh[1..], b), c, env, f)?;
            }
        }
    }
    Ok(())
}

pub mod not {
    use super::*;
    pub fn num(a: f64) -> f64 {
        1.0 - a
    }
    pub fn byte(a: u8) -> u8 {
        1u8.saturating_sub(a)
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot negate {a}"))
    }
}

pub mod neg {
    use super::*;
    pub fn num(a: f64) -> f64 {
        -a
    }
    pub fn byte(a: u8) -> f64 {
        -(a as f64)
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot negate {a}"))
    }
}
pub mod abs {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.abs()
    }
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the absolute value of {a}"))
    }
}
pub mod sign {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.signum()
    }
    pub fn byte(a: u8) -> u8 {
        (a != 0) as u8
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the sign of {a}"))
    }
}
pub mod sqrt {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.sqrt()
    }
    pub fn byte(a: u8) -> u8 {
        (a as f64).sqrt() as u8
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the square root of {a}"))
    }
}
pub mod sin {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.sin()
    }
    pub fn byte(a: u8) -> f64 {
        (a as f64).sin()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the sine of {a}"))
    }
}
pub mod cos {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.cos()
    }
    pub fn byte(a: u8) -> f64 {
        (a as f64).cos()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the cosine of {a}"))
    }
}
pub mod asin {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.asin()
    }
    pub fn byte(a: u8) -> f64 {
        (a as f64).asin()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the arcsine of {a}"))
    }
}
pub mod acos {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.acos()
    }
    pub fn byte(a: u8) -> f64 {
        (a as f64).acos()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the arccosine of {a}"))
    }
}
pub mod floor {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.floor()
    }
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the floor of {a}"))
    }
}
pub mod ceil {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.ceil()
    }
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the ceiling of {a}"))
    }
}
pub mod round {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.round()
    }
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the rounded value of {a}"))
    }
}

macro_rules! cmp_impl {
    ($name:ident $eq:tt $ordering:expr) => {
        pub mod $name {
            use super::*;
            pub fn always_greater<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Less) as u8
            }
            pub fn always_less<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Greater) as u8
            }
            pub fn num_num(a: f64, b: f64) -> u8 {
                (b.partial_cmp(&a)
                    .unwrap_or_else(|| b.is_nan().cmp(&a.is_nan()))
                    $eq $ordering) as u8
            }
            pub fn byte_num(a: u8, b: f64) -> u8 {
                (b.partial_cmp(&(a as f64))
                    .unwrap_or_else(|| b.is_nan().cmp(&false))
                    $eq $ordering) as u8
            }
            pub fn num_byte(a: f64, b: u8) -> u8 {
                ((b as f64).partial_cmp(&a)
                    .unwrap_or_else(|| false.cmp(&a.is_nan()))
                    $eq $ordering) as u8
            }
            pub fn generic<T: Ord>(a: T, b: T) -> u8 {
                (b.cmp(&a) $eq $ordering) as u8
            }
            pub fn error<T: Display>(a: T, b: T, _env: &Uiua) -> UiuaError {
                unreachable!("Comparisons cannot fail, failed to compare {a} and {b}")
            }
        }
    };
}

cmp_impl!(is_eq == std::cmp::Ordering::Equal);
cmp_impl!(is_ne != Ordering::Equal);
cmp_impl!(is_lt == Ordering::Less);
cmp_impl!(is_le != Ordering::Greater);
cmp_impl!(is_gt == Ordering::Greater);
cmp_impl!(is_ge != Ordering::Less);

pub mod add {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b + a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        (b as u16 + a as u16) as f64
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b + a as f64
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        a + b as f64
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn char_num(a: char, b: f64) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn byte_char(a: u8, b: char) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn char_byte(a: char, b: u8) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot add {a} and {b}"))
    }
}

pub mod sub {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b - a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        (b as i16 - a as i16) as f64
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b - a as f64
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        b as f64 - a
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0')
    }
    pub fn char_char(a: char, b: char) -> f64 {
        ((b as i64) - (a as i64)) as f64
    }
    pub fn byte_char(a: u8, b: char) -> char {
        char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0')
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot subtract {a} from {b}"))
    }
}

pub mod mul {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b * a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        b as f64 * a as f64
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b * a as f64
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        b as f64 * a
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot multiply {a} and {b}"))
    }
}

pub mod div {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b / a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        b as f64 / a as f64
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b / a as f64
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        b as f64 / a
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot divide {a} by {b}"))
    }
}

pub mod modulus {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        (b % a + a) % a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        (b % a) as f64
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        let a = a as f64;
        (b % a + a) % a
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        (b as f64 % a + a) % a
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the modulus of {a} by {b}"))
    }
}

pub mod atan2 {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        a.atan2(b)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the atan2 of {a} and {b}"))
    }
}

pub mod pow {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.powf(a)
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        (b as f64).powf(a as f64)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b.powf(a as f64)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        (b as f64).powf(a)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the power of {a} to {b}"))
    }
}

pub mod log {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.log(a)
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        (b as f64).log(a as f64)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b.log(a as f64)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        (b as f64).log(a)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the root of {a} to {b}"))
    }
}

pub mod max {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        a.max(b)
    }
    pub fn byte_byte(a: u8, b: u8) -> u8 {
        a.max(b)
    }
    pub fn char_char(a: char, b: char) -> char {
        a.max(b)
    }
    pub fn num_char(_a: f64, b: char) -> char {
        b
    }
    pub fn char_num(a: char, _b: f64) -> char {
        a
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        (a).max(b as f64)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        (a as f64).max(b)
    }
    pub fn byte_char(_a: u8, b: char) -> char {
        b
    }
    pub fn char_byte(a: char, _b: u8) -> char {
        a
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the max of {a} and {b}"))
    }
}

pub mod min {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        a.min(b)
    }
    pub fn byte_byte(a: u8, b: u8) -> u8 {
        a.min(b)
    }
    pub fn char_char(a: char, b: char) -> char {
        a.min(b)
    }
    pub fn num_char(a: f64, _b: char) -> f64 {
        a
    }
    pub fn char_num(_a: char, b: f64) -> f64 {
        b
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        (a).min(b as f64)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        (a as f64).min(b)
    }
    pub fn byte_char(a: u8, _b: char) -> u8 {
        a
    }
    pub fn char_byte(_a: char, b: u8) -> u8 {
        b
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the min of {a} and {b}"))
    }
}
