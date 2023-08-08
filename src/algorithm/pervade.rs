use std::{
    cmp::{self, Ordering},
    fmt::Display,
    slice,
};

use crate::{array::*, Byte, Uiua, UiuaError, UiuaResult};

pub fn bin_pervade<A: ArrayValue, B: ArrayValue, C: ArrayValue>(
    a: &Array<A>,
    b: &Array<B>,
    env: &Uiua,
    f: impl Fn(A, B) -> C + Copy,
) -> UiuaResult<Array<C>> {
    if !a.shape_prefixes_match(&b) {
        return Err(env.error(format!(
            "Shapes {} and {} do not match",
            a.format_shape(),
            b.format_shape()
        )));
    }
    let shape = a.shape().max(b.shape()).to_vec();
    let mut data = Vec::with_capacity(a.flat_len().max(b.flat_len()));
    bin_pervade_recursive(a, b, &mut data, f);
    Ok((shape, data).into())
}

fn bin_pervade_recursive<A: Arrayish, B: Arrayish, C: ArrayValue>(
    a: &A,
    b: &B,
    c: &mut Vec<C>,
    f: impl Fn(A::Value, B::Value) -> C + Copy,
) {
    match (a.shape(), b.shape()) {
        ([], []) => c.push(f(a.data()[0].clone(), b.data()[0].clone())),
        (ash, bsh) if ash == bsh => {
            for (a, b) in a.data().iter().zip(b.data().iter()) {
                c.push(f(a.clone(), b.clone()));
            }
        }
        ([], bsh) => {
            for brow in b.rows() {
                bin_pervade_recursive(a, &(&bsh[1..], brow), c, f);
            }
        }
        (ash, []) => {
            for arow in a.rows() {
                bin_pervade_recursive(&(&ash[1..], arow), b, c, f);
            }
        }
        (ash, bsh) => {
            for (arow, brow) in a.rows().zip(b.rows()) {
                bin_pervade_recursive(&(&ash[1..], arow), &(&bsh[1..], brow), c, f);
            }
        }
    }
}

pub mod not {
    use super::*;
    pub fn num(a: f64) -> f64 {
        1.0 - a
    }
    pub fn byte(a: Byte) -> Byte {
        a.map(|a| 1 - a)
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
    pub fn byte(a: Byte) -> f64 {
        -f64::from(a)
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
    pub fn byte(a: Byte) -> Byte {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the absolute value of {a}"))
    }
}
pub mod sign {
    use super::*;
    pub fn num(a: f64) -> f64 {
        if a.is_nan() {
            f64::NAN
        } else if a == 0.0 {
            0.0
        } else {
            a.signum()
        }
    }
    pub fn byte(a: Byte) -> Byte {
        a.map(|a| a.signum())
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
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).sqrt()
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
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).sin()
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
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).cos()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the cosine of {a}"))
    }
}
pub mod tan {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.tan()
    }
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).tan()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the tangent of {a}"))
    }
}
pub mod asin {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.asin()
    }
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).asin()
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
    pub fn byte(a: Byte) -> f64 {
        f64::from(a).acos()
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
    pub fn byte(a: Byte) -> Byte {
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
    pub fn byte(a: Byte) -> Byte {
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
    pub fn byte(a: Byte) -> Byte {
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
            pub fn always_greater<A, B>(_: A, _: B) -> Byte {
                ($ordering $eq Ordering::Less).into()
            }
            pub fn always_less<A, B>(_: A, _: B) -> Byte {
                ($ordering $eq Ordering::Greater).into()
            }
            pub fn num_num(a: f64, b: f64) -> Byte {
                (b.partial_cmp(&a)
                    .unwrap_or_else(|| b.is_nan().cmp(&a.is_nan()))
                    $eq $ordering).into()
            }
            pub fn byte_num(a: Byte, b: f64) -> Byte {
                (b.partial_cmp(&a.into())
                    .unwrap_or_else(|| b.is_nan().cmp(&false))
                    $eq $ordering).into()
            }
            pub fn num_byte(a: f64, b: Byte) -> Byte {
                (f64::from(b).partial_cmp(&a)
                    .unwrap_or_else(|| false.cmp(&a.is_nan()))
                    $eq $ordering).into()
            }
            pub fn generic<T: Ord>(a: T, b: T) -> Byte {
                (b.cmp(&a) $eq $ordering).into()
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(a) + f64::from(b)
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b + f64::from(a)
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        a + f64::from(b)
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn char_num(a: char, b: f64) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn byte_char(a: Byte, b: char) -> char {
        a.value()
            .map(|a| char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0'))
            .unwrap_or_default()
    }
    pub fn char_byte(a: char, b: Byte) -> char {
        b.value()
            .map(|b| char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0'))
            .unwrap_or_default()
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b) - f64::from(a)
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b - f64::from(a)
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        f64::from(b) - a
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0')
    }
    pub fn char_char(a: char, b: char) -> f64 {
        ((b as i64) - (a as i64)) as f64
    }
    pub fn byte_char(a: Byte, b: char) -> char {
        a.value()
            .map(|a| char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0'))
            .unwrap_or_default()
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b) * f64::from(a)
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b * f64::from(a)
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        f64::from(b) * a
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b) / f64::from(a)
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b / f64::from(a)
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        f64::from(b) / a
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b) % f64::from(a)
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        let a = f64::from(a);
        (b % a + a) % a
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        (f64::from(b) % a + a) % a
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b).powf(f64::from(a))
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b.powf(f64::from(a))
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        f64::from(b).powf(a)
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
    pub fn byte_byte(a: Byte, b: Byte) -> f64 {
        f64::from(b).log(f64::from(a))
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        b.log(f64::from(a))
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        f64::from(b).log(a)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the log base {b} of {a}"))
    }
}

pub mod max {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        if a.is_nan() || b.is_nan() {
            f64::NAN
        } else {
            a.max(b)
        }
    }
    pub fn byte_byte(a: Byte, b: Byte) -> Byte {
        a.op(b, i16::max)
    }
    pub fn char_char(a: char, b: char) -> char {
        if a.is_fill_value() || b.is_fill_value() {
            char::fill_value()
        } else {
            a.max(b)
        }
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        num_num(a, b.into())
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the max of {a} and {b}"))
    }
}

pub mod min {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        if a.is_nan() || b.is_nan() {
            f64::NAN
        } else {
            a.min(b)
        }
    }
    pub fn byte_byte(a: Byte, b: Byte) -> Byte {
        a.op(b, i16::min)
    }
    pub fn char_char(a: char, b: char) -> char {
        if a.is_fill_value() || b.is_fill_value() {
            char::fill_value()
        } else {
            a.min(b)
        }
    }
    pub fn num_byte(a: f64, b: Byte) -> f64 {
        num_num(a, b.into())
    }
    pub fn byte_num(a: Byte, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the min of {a} and {b}"))
    }
}

pub trait PervasiveInput: IntoIterator + Sized {
    type OwnedItem: Clone;
    fn len(&self) -> usize;
    fn only(&self) -> Self::OwnedItem;
    fn item(item: <Self as IntoIterator>::Item) -> Self::OwnedItem;
    fn as_slice(&self) -> &[Self::OwnedItem];
    fn into_only(self) -> Self::OwnedItem {
        Self::item(self.into_iter().next().unwrap())
    }
}

impl<T: Clone> PervasiveInput for Vec<T> {
    type OwnedItem = T;
    fn len(&self) -> usize {
        Vec::len(self)
    }
    fn only(&self) -> T {
        self.first().unwrap().clone()
    }
    fn item(item: <Self as IntoIterator>::Item) -> T {
        item
    }
    fn as_slice(&self) -> &[T] {
        self.as_slice()
    }
}

impl<'a, T: Clone> PervasiveInput for &'a [T] {
    type OwnedItem = T;
    fn len(&self) -> usize {
        <[T]>::len(self)
    }
    fn only(&self) -> T {
        self.first().unwrap().clone()
    }
    fn item(item: <Self as IntoIterator>::Item) -> T {
        item.clone()
    }
    fn as_slice(&self) -> &[T] {
        self
    }
}

impl<T: Clone> PervasiveInput for Option<T> {
    type OwnedItem = T;
    fn len(&self) -> usize {
        self.is_some() as usize
    }
    fn only(&self) -> T {
        self.as_ref().unwrap().clone()
    }
    fn item(item: <Self as IntoIterator>::Item) -> T {
        item
    }
    fn as_slice(&self) -> &[T] {
        slice::from_ref(self.as_ref().unwrap())
    }
}

pub fn bin_pervade_generic<A: PervasiveInput, B: PervasiveInput, C: Default>(
    a_shape: &[usize],
    a: A,
    b_shape: &[usize],
    b: B,
    env: &mut Uiua,
    f: impl FnMut(A::OwnedItem, B::OwnedItem, &mut Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult<(Vec<usize>, Vec<C>)> {
    let c_shape = cmp::max(a_shape, b_shape).to_vec();
    let c_len: usize = c_shape.iter().product();
    let mut c: Vec<C> = Vec::with_capacity(c_len);
    for _ in 0..c_len {
        c.push(C::default());
    }
    bin_pervade_recursive_generic(a_shape, a, b_shape, b, &mut c, env, f)?;
    Ok((c_shape, c))
}

fn bin_pervade_recursive_generic<A: PervasiveInput, B: PervasiveInput, C>(
    a_shape: &[usize],
    a: A,
    b_shape: &[usize],
    b: B,
    c: &mut [C],
    env: &mut Uiua,
    mut f: impl FnMut(A::OwnedItem, B::OwnedItem, &mut Uiua) -> UiuaResult<C> + Copy,
) -> UiuaResult {
    if a_shape == b_shape {
        for ((a, b), c) in a.into_iter().zip(b).zip(c) {
            *c = f(A::item(a), B::item(b), env)?;
        }
        return Ok(());
    }
    match (a_shape.is_empty(), b_shape.is_empty()) {
        (true, true) => c[0] = f(a.into_only(), b.into_only(), env)?,
        (false, true) => {
            for (a, c) in a.into_iter().zip(c) {
                *c = f(A::item(a), b.only(), env)?;
            }
        }
        (true, false) => {
            for (b, c) in b.into_iter().zip(c) {
                *c = f(a.only(), B::item(b), env)?;
            }
        }
        (false, false) => {
            let a_cells = a_shape[0];
            let b_cells = b_shape[0];
            if a_cells != b_cells {
                return Err(env.error(format!("Shapes {a_shape:?} and {b_shape:?} do not match")));
            }
            let a_chunk_size = a.len() / a_cells;
            let b_chunk_size = b.len() / b_cells;
            match (a_shape.len() == 1, b_shape.len() == 1) {
                (true, true) => {
                    for ((a, b), c) in a.into_iter().zip(b).zip(c) {
                        *c = f(A::item(a), B::item(b), env)?;
                    }
                }
                (true, false) => {
                    for ((a, b), c) in a
                        .into_iter()
                        .zip(b.as_slice().chunks_exact(b_chunk_size))
                        .zip(c.chunks_exact_mut(b_chunk_size))
                    {
                        bin_pervade_recursive_generic(
                            &a_shape[1..],
                            Some(A::item(a)),
                            &b_shape[1..],
                            b,
                            c,
                            env,
                            f,
                        )?;
                    }
                }
                (false, true) => {
                    for ((a, b), c) in a
                        .as_slice()
                        .chunks_exact(a_chunk_size)
                        .zip(b.into_iter())
                        .zip(c.chunks_exact_mut(a_chunk_size))
                    {
                        bin_pervade_recursive_generic(
                            &a_shape[1..],
                            a,
                            &b_shape[1..],
                            Some(B::item(b)),
                            c,
                            env,
                            f,
                        )?;
                    }
                }
                (false, false) => {
                    for ((a, b), c) in a
                        .as_slice()
                        .chunks_exact(a_chunk_size)
                        .zip(b.as_slice().chunks_exact(b_chunk_size))
                        .zip(c.chunks_exact_mut(cmp::max(a_chunk_size, b_chunk_size)))
                    {
                        bin_pervade_recursive_generic(
                            &a_shape[1..],
                            a,
                            &b_shape[1..],
                            b,
                            c,
                            env,
                            f,
                        )?;
                    }
                }
            }
        }
    }
    Ok(())
}
