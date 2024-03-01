//! Algorithms for pervasive array operations

use std::{
    cmp::{self, Ordering},
    convert::Infallible,
    fmt::Display,
    marker::PhantomData,
    slice::{self, ChunksExact},
};

use crate::{array::*, cowslice::CowSlice, Uiua, UiuaError, UiuaResult};
use crate::{Complex, Shape};

use super::fill_array_shapes;

#[allow(clippy::len_without_is_empty)]
pub trait Arrayish {
    type Value: ArrayValue;
    fn shape(&self) -> &[usize];
    fn data(&self) -> &[Self::Value];
    fn rank(&self) -> usize {
        self.shape().len()
    }
    fn flat_len(&self) -> usize {
        self.data().len()
    }
    fn row_len(&self) -> usize {
        self.shape().iter().skip(1).product()
    }
    fn rows(&self) -> ChunksExact<Self::Value> {
        self.data().chunks_exact(self.row_len().max(1))
    }
}

impl<'a, T> Arrayish for &'a T
where
    T: Arrayish,
{
    type Value = T::Value;
    fn shape(&self) -> &[usize] {
        T::shape(self)
    }
    fn data(&self) -> &[Self::Value] {
        T::data(self)
    }
}

impl<T: ArrayValue> Arrayish for Array<T> {
    type Value = T;
    fn shape(&self) -> &[usize] {
        &self.shape
    }
    fn data(&self) -> &[Self::Value] {
        &self.data
    }
}

impl<T: ArrayValue> Arrayish for (&[usize], &[T]) {
    type Value = T;
    fn shape(&self) -> &[usize] {
        self.0
    }
    fn data(&self) -> &[Self::Value] {
        self.1
    }
}

impl<T: ArrayValue> Arrayish for (&[usize], &mut [T]) {
    type Value = T;
    fn shape(&self) -> &[usize] {
        self.0
    }
    fn data(&self) -> &[Self::Value] {
        self.1
    }
}

pub trait PervasiveFn<A, B> {
    type Output;
    type Error;
    fn call(&self, a: A, b: B, env: &Uiua) -> Result<Self::Output, Self::Error>;
}

#[derive(Clone)]
pub struct InfalliblePervasiveFn<A, B, C, F>(F, PhantomData<(A, B, C)>);

impl<A, B, C, F> InfalliblePervasiveFn<A, B, C, F> {
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }
}

impl<A, B, C, F> PervasiveFn<A, B> for InfalliblePervasiveFn<A, B, C, F>
where
    F: Fn(A, B) -> C,
{
    type Output = C;
    type Error = Infallible;
    fn call(&self, a: A, b: B, _env: &Uiua) -> Result<Self::Output, Self::Error> {
        Ok((self.0)(a, b))
    }
}

#[derive(Clone)]
pub struct FalliblePerasiveFn<A, B, C, F>(F, PhantomData<(A, B, C)>);

impl<A, B, C, F> FalliblePerasiveFn<A, B, C, F> {
    pub fn new(f: F) -> Self {
        Self(f, PhantomData)
    }
}

impl<A, B, C, F> PervasiveFn<A, B> for FalliblePerasiveFn<A, B, C, F>
where
    F: Fn(A, B, &Uiua) -> UiuaResult<C>,
{
    type Output = C;
    type Error = UiuaError;
    fn call(&self, a: A, b: B, env: &Uiua) -> UiuaResult<Self::Output> {
        (self.0)(a, b, env)
    }
}

fn reshape_depths<A, B>(a: &mut Array<A>, b: &mut Array<B>, mut a_depth: usize, mut b_depth: usize)
where
    A: ArrayValue,
    B: ArrayValue,
{
    a_depth = a_depth.min(a.rank());
    b_depth = b_depth.min(b.rank());
    match a_depth.cmp(&b_depth) {
        Ordering::Equal => {}
        Ordering::Less => {
            for b_dim in b.shape[..b_depth - a_depth].iter().rev() {
                a.reshape_scalar(Ok(*b_dim as isize));
            }
        }
        Ordering::Greater => {
            for a_dim in a.shape[..a_depth - b_depth].iter().rev() {
                b.reshape_scalar(Ok(*a_dim as isize));
            }
        }
    }
}

pub fn bin_pervade<A, B, C, F>(
    mut a: Array<A>,
    mut b: Array<B>,
    a_depth: usize,
    b_depth: usize,
    env: &Uiua,
    f: F,
) -> UiuaResult<Array<C>>
where
    A: ArrayValue,
    B: ArrayValue,
    C: ArrayValue,
    F: PervasiveFn<A, B, Output = C> + Clone,
    F::Error: Into<UiuaError>,
{
    // Account for depths
    reshape_depths(&mut a, &mut b, a_depth, b_depth);
    // Fill
    fill_array_shapes(&mut a, &mut b, env)?;
    // Pervade
    let shape = a.shape().max(b.shape()).clone();
    let mut data = CowSlice::with_capacity(a.element_count().max(b.element_count()));
    bin_pervade_recursive(&a, &b, &mut data, env, f).map_err(Into::into)?;
    Ok(Array::new(shape, data))
}

fn bin_pervade_recursive<A, B, C, F>(
    a: &A,
    b: &B,
    c: &mut CowSlice<C>,
    env: &Uiua,
    f: F,
) -> Result<(), F::Error>
where
    A: Arrayish,
    B: Arrayish,
    C: ArrayValue,
    F: PervasiveFn<A::Value, B::Value, Output = C> + Clone,
{
    match (a.shape(), b.shape()) {
        ([], []) => c.modify(|c| {
            c.push(f.call(a.data()[0].clone(), b.data()[0].clone(), env)?);
            Ok(())
        })?,
        (ash, bsh) if ash == bsh => {
            c.try_extend(
                a.data()
                    .iter()
                    .zip(b.data())
                    .map(|(a, b)| f.call(a.clone(), b.clone(), env)),
            )?;
        }
        ([], bsh) => {
            for brow in b.rows() {
                bin_pervade_recursive(a, &(&bsh[1..], brow), c, env, f.clone())?;
            }
        }
        (ash, []) => {
            for arow in a.rows() {
                bin_pervade_recursive(&(&ash[1..], arow), b, c, env, f.clone())?;
            }
        }
        (ash, bsh) => {
            for (arow, brow) in a.rows().zip(b.rows()) {
                bin_pervade_recursive(&(&ash[1..], arow), &(&bsh[1..], brow), c, env, f.clone())?;
            }
        }
    }
    Ok(())
}

pub fn bin_pervade_mut<T>(
    a: &mut Array<T>,
    mut b: Array<T>,
    a_depth: usize,
    b_depth: usize,
    env: &Uiua,
    f: impl Fn(T, T) -> T + Copy,
) -> UiuaResult
where
    T: ArrayValue + Copy,
{
    // Account for depths
    reshape_depths(a, &mut b, a_depth, b_depth);
    // Fill
    fill_array_shapes(a, &mut b, env)?;
    // Pervade
    let ash = a.shape.dims();
    let bsh = b.shape.dims();
    // Try to avoid copying when possible
    if ash == bsh {
        if a.data.is_copy_of(&b.data) {
            drop(b);
            let a_data = a.data.as_mut_slice();
            for a in a_data {
                *a = f(*a, *a);
            }
        } else if b.data.is_unique() {
            let a_data = a.data.as_slice();
            let b_data = b.data.as_mut_slice();
            for (a, b) in a_data.iter().zip(b_data) {
                *b = f(*a, *b);
            }
            *a = b;
        } else {
            let a_data = a.data.as_mut_slice();
            let b_data = b.data.as_slice();
            for (a, b) in a_data.iter_mut().zip(b_data) {
                *a = f(*a, *b);
            }
        }
    } else if ash.contains(&0) || bsh.contains(&0) {
        if ash.len() < bsh.len() {
            *a = b;
        }
    } else {
        match ash.len().cmp(&bsh.len()) {
            Ordering::Greater => {
                let a_data = a.data.as_mut_slice();
                let b_data = b.data.as_slice();
                bin_pervade_recursive_mut_left(a_data, ash, b_data, bsh, f);
            }
            Ordering::Less => {
                let a_data = a.data.as_slice();
                let b_data = b.data.as_mut_slice();
                bin_pervade_recursive_mut_right(a_data, ash, b_data, bsh, f);
                *a = b;
            }
            Ordering::Equal => {
                let a_data = a.data.as_mut_slice();
                let b_data = b.data.as_mut_slice();
                bin_pervade_recursive_mut(a_data, ash, b_data, bsh, f);
            }
        }
    }
    Ok(())
}

fn bin_pervade_recursive_mut<T>(
    a_data: &mut [T],
    a_shape: &[usize],
    b_data: &mut [T],
    b_shape: &[usize],
    f: impl Fn(T, T) -> T + Copy,
) where
    T: Copy,
{
    match (a_shape, b_shape) {
        ([], []) => {
            panic!("should never call `bin_pervade_recursive_mut` with scalars")
        }
        (_, []) => {
            let b_scalar = b_data[0];
            for a in a_data {
                *a = f(*a, b_scalar);
            }
        }
        ([], _) => {
            let a_scalar = a_data[0];
            for b in b_data {
                *b = f(a_scalar, *b);
            }
        }
        (ash, bsh) => {
            let a_row_len = a_data.len() / ash[0];
            let b_row_len = b_data.len() / bsh[0];
            for (a, b) in a_data
                .chunks_exact_mut(a_row_len)
                .zip(b_data.chunks_exact_mut(b_row_len))
            {
                bin_pervade_recursive_mut(a, &ash[1..], b, &bsh[1..], f);
            }
        }
    }
}

fn bin_pervade_recursive_mut_left<T>(
    a_data: &mut [T],
    a_shape: &[usize],
    b_data: &[T],
    b_shape: &[usize],
    f: impl Fn(T, T) -> T + Copy,
) where
    T: Copy,
{
    match (a_shape, b_shape) {
        ([], _) => {
            panic!("should never call `bin_pervade_recursive_mut_left` with scalar left")
        }
        (_, []) => {
            let b_scalar = b_data[0];
            for a in a_data {
                *a = f(*a, b_scalar);
            }
        }
        (ash, bsh) => {
            let a_row_len = a_data.len() / ash[0];
            let b_row_len = b_data.len() / bsh[0];
            for (a, b) in a_data
                .chunks_exact_mut(a_row_len)
                .zip(b_data.chunks_exact(b_row_len))
            {
                bin_pervade_recursive_mut_left(a, &ash[1..], b, &bsh[1..], f);
            }
        }
    }
}

fn bin_pervade_recursive_mut_right<T>(
    a_data: &[T],
    a_shape: &[usize],
    b_data: &mut [T],
    b_shape: &[usize],
    f: impl Fn(T, T) -> T + Copy,
) where
    T: Copy,
{
    match (a_shape, b_shape) {
        (_, []) => {
            panic!("should never call `bin_pervade_recursive_mut_right` with scalar right")
        }
        ([], _) => {
            let a_scalar = a_data[0];
            for b in b_data {
                *b = f(a_scalar, *b);
            }
        }
        (ash, bsh) => {
            let a_row_len = a_data.len() / ash[0];
            let b_row_len = b_data.len() / bsh[0];
            for (a, b) in a_data
                .chunks_exact(a_row_len)
                .zip(b_data.chunks_exact_mut(b_row_len))
            {
                bin_pervade_recursive_mut_right(a, &ash[1..], b, &bsh[1..], f);
            }
        }
    }
}

pub mod not {
    use super::*;
    pub fn num(a: f64) -> f64 {
        1.0 - a
    }
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        num(a.into())
    }
    #[cfg(feature = "bytes")]
    pub fn bool(a: u8) -> u8 {
        a ^ 1u8
    }
    pub fn com(a: Complex) -> Complex {
        1.0 - a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot not {a}"))
    }
}

pub mod neg {
    use super::*;
    pub fn num(a: f64) -> f64 {
        -a
    }
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        -f64::from(a)
    }
    pub fn char(a: char) -> char {
        if a.is_lowercase() {
            let mut upper = a.to_uppercase();
            if upper.len() == 1 {
                upper.next().unwrap()
            } else {
                a
            }
        } else if a.is_uppercase() {
            let mut lower = a.to_lowercase();
            if lower.len() == 1 {
                lower.next().unwrap()
            } else {
                a
            }
        } else {
            a
        }
    }
    pub fn com(a: Complex) -> Complex {
        -a
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn char(a: char) -> char {
        if a.is_lowercase() {
            let mut upper = a.to_uppercase();
            if upper.len() == 1 {
                upper.next().unwrap()
            } else {
                a
            }
        } else {
            a
        }
    }
    pub fn com(a: Complex) -> f64 {
        a.abs()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> u8 {
        (a > 0) as u8
    }
    pub fn char(a: char) -> f64 {
        if a.is_uppercase() {
            1.0
        } else if a.is_lowercase() {
            -1.0
        } else {
            0.0
        }
    }
    pub fn com(a: Complex) -> Complex {
        a.normalize()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        f64::from(a).sqrt()
    }
    #[cfg(feature = "bytes")]
    pub fn bool(a: u8) -> u8 {
        a
    }
    pub fn com(a: Complex) -> Complex {
        a.sqrt()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        f64::from(a).sin()
    }
    pub fn com(a: Complex) -> Complex {
        a.sin()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        f64::from(a).cos()
    }
    pub fn com(a: Complex) -> Complex {
        a.cos()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> f64 {
        f64::from(a).asin()
    }
    pub fn com(a: Complex) -> Complex {
        a.asin()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the arcsine of {a}"))
    }
}
pub mod floor {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.floor()
    }
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn com(a: Complex) -> Complex {
        a.floor()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn com(a: Complex) -> Complex {
        a.ceil()
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
    #[cfg(feature = "bytes")]
    pub fn byte(a: u8) -> u8 {
        a
    }
    pub fn com(a: Complex) -> Complex {
        a.round()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the rounded value of {a}"))
    }
}

pub mod complex_re {
    use super::*;

    pub fn com(a: Complex) -> f64 {
        a.re
    }
    pub fn generic<T>(a: T) -> T {
        a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the real part of {a}"))
    }
}
pub mod complex_im {
    use super::*;

    pub fn com(a: Complex) -> f64 {
        a.im
    }
    pub fn num(_a: f64) -> f64 {
        0.0
    }
    #[cfg(feature = "bytes")]
    pub fn byte(_a: u8) -> u8 {
        0
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the imaginary part of {a}"))
    }
}

macro_rules! eq_impl {
    ($name:ident $eq:tt $ordering:expr) => {
        pub mod $name {
            use super::*;
            pub fn always_greater<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Less).into()
            }
            pub fn always_less<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Greater).into()
            }
            pub fn num_num(a: f64, b: f64) -> u8 {
                (b.array_cmp(&a) $eq $ordering) as u8
            }
            pub fn com_x(a: Complex, b: impl Into<Complex>) -> u8 {
                (b.into().array_cmp(&a) $eq $ordering) as u8
            }
            pub fn x_com(a: impl Into<Complex>, b: Complex) -> u8 {
                (b.array_cmp(&a.into()) $eq $ordering) as u8
            }
            #[cfg(feature = "bytes")]
            pub fn byte_num(a: u8, b: f64) -> u8 {
                (b.array_cmp(&f64::from(a)) $eq $ordering) as u8
            }
            #[cfg(feature = "bytes")]
            pub fn num_byte(a: f64, b: u8) -> u8 {
                (f64::from(b).array_cmp(&a) $eq $ordering) as u8
            }
            pub fn generic<T: Ord>(a: T, b: T) -> u8 {
                (b.cmp(&a) $eq $ordering).into()
            }
            pub fn same_type<T: ArrayCmp + From<u8>>(a: T, b: T) -> T {
               ((b.array_cmp(&a) $eq $ordering) as u8).into()
            }
            pub fn error<T: Display>(a: T, b: T, _env: &Uiua) -> UiuaError {
                unreachable!("Comparisons cannot fail, failed to compare {a} and {b}")
            }
        }
    };
}

macro_rules! cmp_impl {
    ($name:ident $eq:tt $ordering:expr) => {
        pub mod $name {
            use super::*;
            pub fn always_greater<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Less).into()
            }
            pub fn always_less<A, B>(_: A, _: B) -> u8 {
                ($ordering $eq Ordering::Greater).into()
            }
            pub fn num_num(a: f64, b: f64) -> u8 {
                (b.array_cmp(&a) $eq $ordering) as u8
            }
            pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
                let b = b.into();
                Complex::new(
                    (b.re.array_cmp(&a.re) $eq $ordering) as u8 as f64,
                    (b.im.array_cmp(&a.im) $eq $ordering) as u8 as f64
                )
            }
            pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
                let a = a.into();
                Complex::new(
                    (b.re.array_cmp(&a.re) $eq $ordering) as u8 as f64,
                    (b.im.array_cmp(&a.im) $eq $ordering) as u8 as f64
                )
            }
            #[cfg(feature = "bytes")]
            pub fn byte_num(a: u8, b: f64) -> u8 {
                (b.array_cmp(&f64::from(a)) $eq $ordering) as u8
            }
            #[cfg(feature = "bytes")]
            pub fn num_byte(a: f64, b: u8) -> u8 {
                (f64::from(b).array_cmp(&a) $eq $ordering) as u8
            }
            pub fn generic<T: Ord>(a: T, b: T) -> u8 {
                (b.cmp(&a) $eq $ordering).into()
            }
            pub fn same_type<T: ArrayCmp + From<u8>>(a: T, b: T) -> T {
               ((b.array_cmp(&a) $eq $ordering) as u8).into()
            }
            pub fn error<T: Display>(a: T, b: T, _env: &Uiua) -> UiuaError {
                unreachable!("Comparisons cannot fail, failed to compare {a} and {b}")
            }
        }
    };
}

eq_impl!(is_eq == Ordering::Equal);
eq_impl!(is_ne != Ordering::Equal);
cmp_impl!(is_lt == Ordering::Less);
cmp_impl!(is_le != Ordering::Greater);
cmp_impl!(is_gt == Ordering::Greater);
cmp_impl!(is_ge != Ordering::Less);

pub mod add {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b + a
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(a) + f64::from(b)
    }
    #[cfg(feature = "bytes")]
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        b + a
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b + f64::from(a)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        a + f64::from(b)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() + a
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b + a.into()
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    pub fn char_num(a: char, b: f64) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    #[cfg(feature = "bytes")]
    pub fn byte_char(a: u8, b: char) -> char {
        char::from_u32((b as i64 + a as i64) as u32).unwrap_or('\0')
    }
    #[cfg(feature = "bytes")]
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
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b) - f64::from(a)
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b - f64::from(a)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b) - a
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() - a
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b - a.into()
    }
    pub fn num_char(a: f64, b: char) -> char {
        char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0')
    }
    pub fn char_char(a: char, b: char) -> f64 {
        ((b as i64) - (a as i64)) as f64
    }
    #[cfg(feature = "bytes")]
    pub fn byte_char(a: u8, b: char) -> char {
        char::from_u32(((b as i64) - (a as i64)) as u32).unwrap_or('\0')
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot subtract {a} from {b}"))
    }
}

macro_rules! bin_op_mod {
    ($name:ident, $a:ident, $b:ident, $byte_convert:expr, $byte_ret:ty, $f:expr, $err:literal) => {
        pub mod $name {
            use super::*;
            pub fn num_num($a: f64, $b: f64) -> f64 {
                $f
            }
            #[cfg(feature = "bytes")]
            pub fn byte_byte($a: u8, $b: u8) -> f64 {
                let $a = $byte_convert($a);
                let $b = $byte_convert($b);
                $f
            }
            #[cfg(feature = "bytes")]
            pub fn byte_num($a: u8, $b: f64) -> f64 {
                let $a = $byte_convert($a);
                $f
            }
            #[cfg(feature = "bytes")]
            pub fn num_byte($a: f64, $b: u8) -> f64 {
                let $b = $byte_convert($b);
                $f
            }

            pub fn com_x($a: Complex, $b: impl Into<Complex>) -> Complex {
                let $b = $b.into();
                $f
            }

            pub fn x_com($a: impl Into<Complex>, $b: Complex) -> Complex {
                let $a = $a.into();
                $f
            }
            pub fn error<T: Display>($a: T, $b: T, env: &Uiua) -> UiuaError {
                env.error(format!($err))
            }
        }
    };
}

pub mod mul {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b * a
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b) * f64::from(a)
    }
    #[cfg(feature = "bytes")]
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        b & a
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b * f64::from(a)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b) * a
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() * a
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b * a.into()
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot multiply {a} and {b}"))
    }
}

bin_op_mod!(div, a, b, f64::from, f64, b / a, "Cannot divide {b} by {a}");

pub mod modulus {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.rem_euclid(a)
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        num_num(a.into(), b.into())
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b.into())
    }
    pub fn com_com(a: Complex, b: Complex) -> Complex {
        b % a
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() % a
    }
    pub fn x_com(a: impl Into<f64>, b: Complex) -> Complex {
        b % a.into()
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot modulo {a} and {b}"))
    }
}
bin_op_mod!(
    atan2,
    a,
    b,
    f64::from,
    f64,
    a.atan2(b),
    "Cannot get the atan2 of {a} and {b}"
);
pub mod pow {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.powf(a)
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b).powf(f64::from(a))
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b.powi(a as i32)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b).powf(a)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into().powc(a)
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b.powc(a.into())
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the power of {a} to {b}"))
    }
}
bin_op_mod!(
    log,
    a,
    b,
    f64::from,
    f64,
    b.log(a),
    "Cannot get the log base {b} of {a}"
);
pub mod complex {
    use super::*;

    pub fn num_num(a: f64, b: f64) -> Complex {
        Complex::new(b, a)
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> Complex {
        Complex::new(b.into(), a.into())
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> Complex {
        Complex::new(b, a.into())
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> Complex {
        Complex::new(b.into(), a)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() + a * Complex::I
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b + a.into() * Complex::I
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the form a complex number with {b} as the real part and {a} as the imaginary part"))
    }
}

pub mod max {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        a.max(b)
    }
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> u8 {
        a.max(b)
    }
    #[cfg(feature = "bytes")]
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        a | b
    }
    pub fn char_char(a: char, b: char) -> char {
        a.max(b)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b.into())
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        a.max(b.into())
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        a.into().max(b)
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
    #[cfg(feature = "bytes")]
    pub fn byte_byte(a: u8, b: u8) -> u8 {
        a.min(b)
    }
    #[cfg(feature = "bytes")]
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        a & b
    }
    pub fn char_char(a: char, b: char) -> char {
        a.min(b)
    }
    #[cfg(feature = "bytes")]
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b.into())
    }
    #[cfg(feature = "bytes")]
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        a.min(b.into())
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        a.into().min(b)
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
) -> UiuaResult<(Shape, Vec<C>)> {
    let c_shape = Shape::from(cmp::max(a_shape, b_shape));
    let c_len: usize = c_shape.iter().product();
    let mut c: Vec<C> = Vec::with_capacity(c_len);
    for _ in 0..c_len {
        c.push(C::default());
    }
    bin_pervade_recursive_generic(a_shape, a, b_shape, b, &mut c, env, f)?;
    Ok((c_shape, c))
}

#[allow(unused_mut)] // for a rust-analyzer false-positive
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
        for ((a, b), mut c) in a.into_iter().zip(b).zip(c) {
            *c = f(A::item(a), B::item(b), env)?;
        }
        return Ok(());
    }
    match (a_shape.is_empty(), b_shape.is_empty()) {
        (true, true) => c[0] = f(a.into_only(), b.into_only(), env)?,
        (false, true) => {
            for (a, mut c) in a.into_iter().zip(c) {
                *c = f(A::item(a), b.only(), env)?;
            }
        }
        (true, false) => {
            for (b, mut c) in b.into_iter().zip(c) {
                *c = f(a.only(), B::item(b), env)?;
            }
        }
        (false, false) => {
            let a_cells = a_shape[0];
            let b_cells = b_shape[0];
            if a_cells != b_cells {
                return Err(env.error(format!(
                    "Shapes {} and {} do not match",
                    FormatShape(a_shape),
                    FormatShape(b_shape)
                )));
            }
            let a_chunk_size = a.len() / a_cells;
            let b_chunk_size = b.len() / b_cells;
            match (a_shape.len() == 1, b_shape.len() == 1) {
                (true, true) => {
                    for ((a, b), mut c) in a.into_iter().zip(b).zip(c) {
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
