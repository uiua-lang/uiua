//! Algorithms for pervasive array operations

use std::{
    cmp::{self, Ordering},
    convert::Infallible,
    fmt::Display,
    iter::repeat,
    marker::PhantomData,
    slice,
};

use ecow::eco_vec;

use crate::{algorithm::loops::flip, array::*, Uiua, UiuaError, UiuaResult};
use crate::{Complex, Shape};

use super::FillContext;

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

fn pervade_dim(a: usize, b: usize) -> usize {
    if a == b {
        a
    } else if a == 1 {
        b
    } else if b == 1 {
        a
    } else {
        a.max(b)
    }
}

pub fn bin_pervade<A, B, C, F>(
    a: Array<A>,
    b: Array<B>,
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
    let _a_depth = a_depth.min(a.rank());
    let _b_depth = b_depth.min(b.rank());

    let new_rank = a.rank().max(b.rank());
    let mut new_shape = Shape::with_capacity(new_rank);
    let a_fill = env.scalar_fill::<A>();
    let b_fill = env.scalar_fill::<B>();
    for i in 0..new_rank {
        let c = match (a.shape.get(i).copied(), b.shape.get(i).copied()) {
            (None, None) => unreachable!(),
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (Some(ad), Some(bd)) => {
                if ad == bd || ad == 1 || bd == 1 {
                    pervade_dim(ad, bd)
                } else if ad < bd {
                    match &a_fill {
                        Ok(_) => pervade_dim(ad, bd),
                        Err(e) => {
                            return Err(env.error(format!(
                                "Shapes {} and {} are not compatible{e}",
                                a.shape, b.shape
                            )))
                        }
                    }
                } else {
                    match &b_fill {
                        Ok(_) => pervade_dim(ad, bd),
                        Err(e) => {
                            return Err(env.error(format!(
                                "Shapes {} and {} are not compatible{e}",
                                a.shape, b.shape
                            )))
                        }
                    }
                }
            }
        };
        new_shape.push(c);
    }

    // dbg!(&a.shape, &b.shape, &new_shape);

    let mut new_data = eco_vec![C::default(); new_shape.elements()];
    if !new_shape.contains(&0) {
        let slice = new_data.make_mut();
        let a_fill = a_fill.as_ref().ok();
        let b_fill = b_fill.as_ref().ok();
        bin_pervade_recursive(
            (&a.data, &a.shape),
            (&b.data, &b.shape),
            slice,
            a_fill,
            b_fill,
            f,
            env,
        )
        .map_err(Into::into)?;
    }
    Ok(Array::new(new_shape, new_data))
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn bin_pervade_recursive<A, B, C, F>(
    (a, ash): (&[A], &[usize]),
    (b, bsh): (&[B], &[usize]),
    c: &mut [C],
    a_fill: Option<&A>,
    b_fill: Option<&B>,
    f: F,
    env: &Uiua,
) -> Result<(), F::Error>
where
    A: ArrayValue + Clone,
    B: ArrayValue + Clone,
    C: ArrayValue + Clone,
    F: PervasiveFn<A, B, Output = C> + Clone,
{
    let recur = |a: &[A], ash: &[usize], b: &[B], bsh: &[usize], c: &mut [C]| {
        bin_pervade_recursive((a, ash), (b, bsh), c, a_fill, b_fill, f.clone(), env)
    };
    match (ash, bsh) {
        ([], []) => {
            let (a, b) = (a[0].clone(), b[0].clone());
            c[0] = f.call(a, b, env)?;
        }
        // Scalar A
        ([], [_, ..]) => {
            let a = &a[0];
            for (b, c) in b.iter().zip(c) {
                *c = f.call(a.clone(), b.clone(), env)?;
            }
        }
        // Scalar B
        ([_, ..], []) => {
            let b = &b[0];
            for (a, c) in a.iter().zip(c) {
                *c = f.call(a.clone(), b.clone(), env)?;
            }
        }
        ([al, ash @ ..], [bl, bsh @ ..]) => {
            let a_row_len = a.len() / al;
            let b_row_len = b.len() / bl;
            let c_row_len = c.len() / al.max(bl);
            if al == bl {
                if ash == bsh {
                    for ((a, b), c) in a.iter().zip(b).zip(c) {
                        *c = f.call(a.clone(), b.clone(), env)?;
                    }
                } else {
                    for ((a, b), c) in a
                        .chunks_exact(a_row_len)
                        .zip(b.chunks_exact(b_row_len))
                        .zip(c.chunks_exact_mut(c_row_len))
                    {
                        recur(a, ash, b, bsh, c)?;
                    }
                }
            } else if al < bl {
                if let Some(a_fill) = a_fill {
                    let a_fill_row = vec![a_fill.clone(); a_row_len];
                    let a_iter = a
                        .chunks_exact(a_row_len)
                        .chain(repeat(a_fill_row.as_slice()));
                    for ((a, b), c) in a_iter
                        .zip(b.chunks_exact(b_row_len))
                        .zip(c.chunks_exact_mut(c_row_len))
                    {
                        recur(a, ash, b, bsh, c)?;
                    }
                } else if ash == bsh {
                    for (b, c) in b.chunks_exact(b_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                        for ((a, b), c) in a.iter().zip(b).zip(c) {
                            *c = f.call(a.clone(), b.clone(), env)?;
                        }
                    }
                } else {
                    for (b, c) in b.chunks_exact(b_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                        recur(a, ash, b, bsh, c)?;
                    }
                }
            } else if let Some(b_fill) = b_fill {
                let b_fill_row = vec![b_fill.clone(); b_row_len];
                let b_iter = b
                    .chunks_exact(b_row_len)
                    .chain(repeat(b_fill_row.as_slice()));
                for ((a, b), c) in a
                    .chunks_exact(a_row_len)
                    .zip(b_iter)
                    .zip(c.chunks_exact_mut(c_row_len))
                {
                    recur(a, ash, b, bsh, c)?;
                }
            } else if ash == bsh {
                for (a, c) in a.chunks_exact(a_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                    for ((a, b), c) in a.iter().zip(b).zip(c) {
                        *c = f.call(a.clone(), b.clone(), env)?;
                    }
                }
            } else {
                for (a, c) in a.chunks_exact(a_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                    recur(a, ash, b, bsh, c)?;
                }
            }
        }
    }
    Ok(())
}

/// Pervade an operation where both input types and the output type are all the same
pub fn bin_pervade_mut<T>(
    mut a: Array<T>,
    b: &mut Array<T>,
    a_depth: usize,
    b_depth: usize,
    env: &Uiua,
    f: impl Fn(T, T) -> T + Copy,
) -> UiuaResult
where
    T: ArrayValue + Copy,
{
    let _a_depth = a_depth.min(a.rank());
    let _b_depth = b_depth.min(b.rank());

    let new_rank = a.rank().max(b.rank());
    let mut new_shape = Shape::with_capacity(new_rank);
    let fill = env.scalar_fill::<T>();
    let mut requires_fill = false;
    for i in 0..new_rank {
        let c = match (a.shape.get(i).copied(), b.shape.get(i).copied()) {
            (None, None) => unreachable!(),
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (Some(ad), Some(bd)) => {
                if ad == bd || ad == 1 || bd == 1 {
                    requires_fill |= ad != bd && fill.is_ok();
                    pervade_dim(ad, bd)
                } else {
                    match &fill {
                        Ok(_) => {
                            requires_fill = true;
                            pervade_dim(ad, bd)
                        }
                        Err(e) => {
                            return Err(env.error(format!(
                                "Shapes {} and {} are not compatible{e}",
                                a.shape, b.shape
                            )))
                        }
                    }
                }
            }
        };
        new_shape.push(c);
    }

    let fill = if requires_fill { fill.ok() } else { None };

    fn reuse_no_fill<T: ArrayValue + Copy>(
        a: &[T],
        b: &mut [T],
        ash: &[usize],
        bsh: &[usize],
        f: impl Fn(T, T) -> T + Copy,
    ) {
        match (ash, bsh) {
            ([], _) => {
                for b in b {
                    *b = f(a[0], *b);
                }
            }
            ([1, ash @ ..], [bl, bsh @ ..]) => {
                let b_row_len = b.len() / bl;
                if ash == bsh {
                    for b in b.chunks_exact_mut(b_row_len) {
                        for (a, b) in a.iter().zip(b) {
                            *b = f(*a, *b);
                        }
                    }
                } else {
                    for b in b.chunks_exact_mut(b_row_len) {
                        reuse_no_fill(a, b, ash, bsh, f);
                    }
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => {
                debug_assert_eq!(al, bl);
                if ash == bsh {
                    for (a, b) in a.iter().zip(b) {
                        *b = f(*a, *b);
                    }
                } else {
                    let a_row_len = a.len() / al;
                    let b_row_len = b.len() / bl;
                    for (a, b) in a.chunks_exact(a_row_len).zip(b.chunks_exact_mut(b_row_len)) {
                        reuse_no_fill(a, b, ash, bsh, f);
                    }
                }
            }
            _ => unreachable!(),
        }
    }
    fn reuse_fill<T: ArrayValue + Copy>(
        a: &[T],
        b: &mut [T],
        ash: &[usize],
        bsh: &[usize],
        fill: T,
        f: impl Fn(T, T) -> T + Copy,
    ) {
        match (ash, bsh) {
            ([], _) => {
                for b in b {
                    *b = f(a[0], *b);
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => {
                let a_row_len = a.len() / al;
                let b_row_len = b.len() / bl;
                if al == bl {
                    for (a, b) in a.chunks_exact(a_row_len).zip(b.chunks_exact_mut(b_row_len)) {
                        reuse_fill(a, b, ash, bsh, fill, f);
                    }
                } else {
                    let fill_row = vec![fill; a_row_len];
                    for (a, b) in a
                        .chunks_exact(a_row_len)
                        .chain(repeat(fill_row.as_slice()))
                        .zip(b.chunks_exact_mut(b_row_len))
                    {
                        reuse_fill(a, b, ash, bsh, fill, f);
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn use_new_fill<T: ArrayValue + Copy>(
        a: &[T],
        b: &[T],
        c: &mut [T],
        ash: &[usize],
        bsh: &[usize],
        fill: T,
        f: impl Fn(T, T) -> T + Copy,
    ) {
        match (ash, bsh) {
            ([], []) => c[0] = f(a[0], b[0]),
            ([], [_, ..]) => {
                let a = a[0];
                for (b, c) in b.iter().zip(c) {
                    *c = f(a, *b);
                }
            }
            ([_, ..], []) => {
                let b = b[0];
                for (a, c) in a.iter().zip(c) {
                    *c = f(*a, b);
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => {
                let a_row_len = a.len() / al;
                let b_row_len = b.len() / bl;
                let c_row_len = c.len() / al.max(bl);
                match al.cmp(bl) {
                    Ordering::Equal => {
                        if ash == bsh {
                            for ((a, b), c) in a.iter().zip(b).zip(c) {
                                *c = f(*a, *b);
                            }
                        } else {
                            for ((a, b), c) in a
                                .chunks_exact(a_row_len)
                                .zip(b.chunks_exact(b_row_len))
                                .zip(c.chunks_exact_mut(c_row_len))
                            {
                                use_new_fill(a, b, c, ash, bsh, fill, f);
                            }
                        }
                    }
                    Ordering::Less => {
                        let a_fill_row = vec![fill; a_row_len];
                        let a_iter = a
                            .chunks_exact(a_row_len)
                            .chain(repeat(a_fill_row.as_slice()));
                        for ((a, b), c) in a_iter
                            .zip(b.chunks_exact(b_row_len))
                            .zip(c.chunks_exact_mut(c_row_len))
                        {
                            use_new_fill(a, b, c, ash, bsh, fill, f);
                        }
                    }
                    Ordering::Greater => {
                        let b_fill_row = vec![fill; b_row_len];
                        let b_iter = b
                            .chunks_exact(b_row_len)
                            .chain(repeat(b_fill_row.as_slice()));
                        for ((a, b), c) in a
                            .chunks_exact(a_row_len)
                            .zip(b_iter)
                            .zip(c.chunks_exact_mut(c_row_len))
                        {
                            use_new_fill(a, b, c, ash, bsh, fill, f);
                        }
                    }
                }
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn use_new_no_fill<T: ArrayValue + Copy>(
        a: &[T],
        b: &[T],
        c: &mut [T],
        ash: &[usize],
        bsh: &[usize],
        f: impl Fn(T, T) -> T + Copy,
    ) {
        match (ash, bsh) {
            ([], []) => c[0] = f(a[0], b[0]),
            ([], [_, ..]) => {
                let a = a[0];
                for (b, c) in b.iter().zip(c) {
                    *c = f(a, *b);
                }
            }
            ([_, ..], []) => {
                let b = b[0];
                for (a, c) in a.iter().zip(c) {
                    *c = f(*a, b);
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => {
                let a_row_len = a.len() / al;
                let b_row_len = b.len() / bl;
                let c_row_len = c.len() / al.max(bl);
                match al.cmp(bl) {
                    Ordering::Equal => {
                        if ash == bsh {
                            for ((a, b), c) in a.iter().zip(b).zip(c) {
                                *c = f(*a, *b);
                            }
                        } else {
                            for ((a, b), c) in a
                                .chunks_exact(a_row_len)
                                .zip(b.chunks_exact(b_row_len))
                                .zip(c.chunks_exact_mut(c_row_len))
                            {
                                use_new_no_fill(a, b, c, ash, bsh, f);
                            }
                        }
                    }
                    Ordering::Less => {
                        for (b, c) in b.chunks_exact(b_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                            use_new_no_fill(a, b, c, ash, bsh, f);
                        }
                    }
                    Ordering::Greater => {
                        for (a, c) in a.chunks_exact(a_row_len).zip(c.chunks_exact_mut(c_row_len)) {
                            use_new_no_fill(a, b, c, ash, bsh, f);
                        }
                    }
                }
            }
        }
    }

    if new_shape.contains(&0) {
        b.shape = new_shape;
        b.data = Default::default();
    } else if new_shape == b.shape {
        // The existing array can be used
        if a.shape == b.shape {
            // Try to avoid copying when possible
            if a.data.is_copy_of(&b.data) {
                drop(a);
                let b_data = b.data.as_mut_slice();
                for b in b_data {
                    *b = f(*b, *b);
                }
            } else if a.data.is_unique() {
                let a_data = a.data.as_mut_slice();
                let b_data = b.data.as_slice();
                for (a, b) in a_data.iter_mut().zip(b_data) {
                    *a = f(*a, *b);
                }
                b.data = a.data;
            } else {
                let a_data = a.data.as_slice();
                let b_data = b.data.as_mut_slice();
                for (a, b) in a_data.iter().zip(b_data) {
                    *b = f(*a, *b);
                }
            }
        } else if let Some(fill) = fill {
            reuse_fill(&a.data, b.data.as_mut_slice(), &a.shape, &b.shape, fill, f);
        } else {
            reuse_no_fill(&a.data, b.data.as_mut_slice(), &a.shape, &b.shape, f);
        }
    } else if new_shape == a.shape {
        // An existing array can be used, but things need to be flipped
        if let Some(fill) = fill {
            reuse_fill(
                &b.data,
                a.data.as_mut_slice(),
                &b.shape,
                &a.shape,
                fill,
                flip(f),
            );
        } else {
            reuse_no_fill(&b.data, a.data.as_mut_slice(), &b.shape, &a.shape, flip(f));
        }
        *b = a;
    } else {
        // Allocate a new array
        let mut new_data = eco_vec![T::default(); new_shape.elements()];
        let slice = new_data.make_mut();
        if let Some(fill) = fill {
            use_new_fill(&a.data, &b.data, slice, &a.shape, &b.shape, fill, f);
        } else {
            use_new_no_fill(&a.data, &b.data, slice, &a.shape, &b.shape, f);
        }
        b.data = new_data.into();
        b.shape = new_shape;
    }

    Ok(())
}

pub mod not {
    use super::*;
    pub fn num(a: f64) -> f64 {
        1.0 - a
    }
    pub fn byte(a: u8) -> f64 {
        num(a.into())
    }
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

fn toggle_char_case(a: char) -> char {
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

pub mod scalar_neg {
    use super::*;
    pub fn num(a: f64) -> f64 {
        -a
    }
    pub fn byte(a: u8) -> f64 {
        -f64::from(a)
    }
    pub fn char(a: char) -> char {
        toggle_char_case(a)
    }
    pub fn com(a: Complex) -> Complex {
        -a
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot negate {a}"))
    }
}
pub mod scalar_abs {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.abs()
    }
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

fn character_sign(a: char) -> f64 {
    if a.is_uppercase() {
        1.0
    } else if a.is_lowercase() {
        -1.0
    } else {
        0.0
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
    pub fn byte(a: u8) -> u8 {
        (a > 0) as u8
    }
    pub fn char(a: char) -> f64 {
        character_sign(a)
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
    pub fn byte(a: u8) -> f64 {
        f64::from(a).sqrt()
    }
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
            pub fn byte_num(a: u8, b: f64) -> u8 {
                (b.array_cmp(&f64::from(a)) $eq $ordering) as u8
            }
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
            pub fn byte_num(a: u8, b: f64) -> u8 {
                (b.array_cmp(&f64::from(a)) $eq $ordering) as u8
            }
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
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(a) + f64::from(b)
    }
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        b + a
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b + f64::from(a)
    }
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
        f64::from(b) - f64::from(a)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b - f64::from(a)
    }
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
            pub fn byte_byte($a: u8, $b: u8) -> f64 {
                let $a = $byte_convert($a);
                let $b = $byte_convert($b);
                $f
            }
            pub fn byte_num($a: u8, $b: f64) -> f64 {
                let $a = $byte_convert($a);
                $f
            }
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
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b) * f64::from(a)
    }
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        b & a
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b * f64::from(a)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b) * a
    }
    pub fn num_char(a: f64, b: char) -> char {
        if a < 0.0 {
            toggle_char_case(b)
        } else {
            b
        }
    }
    pub fn char_num(a: char, b: f64) -> char {
        if b < 0.0 {
            toggle_char_case(a)
        } else {
            a
        }
    }
    pub fn byte_char(_: u8, b: char) -> char {
        b
    }
    pub fn char_byte(a: char, _: u8) -> char {
        a
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

pub mod div {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b / a
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b) / f64::from(a)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b / f64::from(a)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b) / a
    }
    pub fn num_char(a: f64, b: char) -> char {
        if a < 0.0 {
            toggle_char_case(b)
        } else {
            b
        }
    }
    pub fn byte_char(_: u8, b: char) -> char {
        b
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into() / a
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b / a.into()
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot divide {b} by {a}"))
    }
}

pub mod modulus {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.rem_euclid(a)
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        num_num(a.into(), b.into())
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
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
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b).powf(f64::from(a))
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b.powi(a as i32)
    }
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
    pub fn byte_byte(a: u8, b: u8) -> Complex {
        Complex::new(b.into(), a.into())
    }
    pub fn byte_num(a: u8, b: f64) -> Complex {
        Complex::new(b, a.into())
    }
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
        env.error(format!(
            "Cannot make a complex number with {b} as the real part and {a} as the imaginary part"
        ))
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
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        a | b
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b.into())
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        a.max(b.into())
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        a.into().max(b)
    }
    pub fn generic<T: Ord>(a: T, b: T) -> T {
        a.max(b)
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
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        a & b
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b.into())
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        a.min(b.into())
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        a.into().min(b)
    }
    pub fn generic<T: Ord>(a: T, b: T) -> T {
        a.min(b)
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
