//! Algorithms for pervasive array operations

use std::iter::repeat_n;
use std::{
    cmp::Ordering, convert::Infallible, fmt::Display, iter::repeat, marker::PhantomData, mem::swap,
};

use ecow::eco_vec;

use crate::{
    Complex, Shape, Uiua, UiuaError, UiuaResult, Value,
    algorithm::{loops::flip, validate_size},
    array::*,
    fill::FillValue,
};

use super::{FillContext, MultiOutput, multi_output};

pub trait PervasiveFn<A, B> {
    type Output;
    type Error;
    fn call(&self, a: A, b: B, env: &Uiua) -> Result<Self::Output, Self::Error>;
}

#[derive(Clone, Copy)]
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

pub(crate) fn pervade_dim(a: usize, b: usize) -> usize {
    if a == 1 {
        b
    } else if b == 1 {
        a
    } else {
        a.max(b)
    }
}

pub fn derive_new_shape(
    ash: &Shape,
    bsh: &Shape,
    a_fill_sh: Result<&Shape, &'static str>,
    b_fill_sh: Result<&Shape, &'static str>,
    env: &Uiua,
) -> UiuaResult<Shape> {
    let new_rank = ash.len().max(bsh.len());
    let mut new_shape = Shape::with_capacity(new_rank);
    for i in 0..new_rank {
        let c = match (ash.get(i).copied(), bsh.get(i).copied()) {
            (None, None) => unreachable!(),
            (Some(a), None) => a,
            (None, Some(b)) => b,
            (Some(ad), Some(bd)) => {
                if ad == bd || (ad == 1 && a_fill_sh.is_err()) || (bd == 1 && b_fill_sh.is_err()) {
                    pervade_dim(ad, bd)
                } else if ad < bd {
                    match a_fill_sh {
                        Ok(sh) if !ash.row_slice().ends_with(sh) => {
                            return Err(env.error(format!(
                                "Fill shape {sh} cannot be used to fill array with shape {ash}"
                            )));
                        }
                        Ok(_) => ad.max(bd),
                        Err(e) => {
                            return Err(
                                env.error(format!("Shapes {ash} and {bsh} are not compatible{e}"))
                            );
                        }
                    }
                } else {
                    match b_fill_sh {
                        Ok(sh) if !bsh.row_slice().ends_with(sh) => {
                            return Err(env.error(format!(
                                "Fill shape {sh} cannot be used to fill array with shape {bsh}"
                            )));
                        }
                        Ok(_) => ad.max(bd),
                        Err(e) => {
                            return Err(
                                env.error(format!("Shapes {ash} and {bsh} are not compatible{e}"))
                            );
                        }
                    }
                }
            }
        };
        new_shape.push(c);
    }
    Ok(new_shape)
}

pub fn bin_pervade<A, B, C, F>(a: Array<A>, b: Array<B>, env: &Uiua, f: F) -> UiuaResult<Array<C>>
where
    A: ArrayValue,
    B: ArrayValue,
    C: ArrayValue,
    F: PervasiveFn<A, B, Output = C> + Clone,
    F::Error: Into<UiuaError>,
{
    let a_fill = env.scalar_fill::<A>();
    let b_fill = env.scalar_fill::<B>();
    let empty = Shape::SCALAR;
    let new_shape = derive_new_shape(
        &a.shape,
        &b.shape,
        a_fill.as_ref().map(|_| &empty).map_err(|&e| e),
        b_fill.as_ref().map(|_| &empty).map_err(|&e| e),
        env,
    )?;

    // dbg!(&a.shape, &b.shape, &new_shape);

    let elem_count = validate_size::<C>(new_shape.iter().copied(), env)?;
    let mut new_data = eco_vec![C::default(); elem_count];
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

pub(crate) fn bin_pervade_recursive<A, B, C, F>(
    (a, ash): (&[A], &[usize]),
    (b, bsh): (&[B], &[usize]),
    c: &mut [C],
    a_fill: Option<&FillValue<A>>,
    b_fill: Option<&FillValue<B>>,
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
            let a_row_len: usize = ash.iter().product();
            let b_row_len: usize = bsh.iter().product();
            let c_row_len = c.len() / al.max(bl);
            if al == bl {
                if ash == bsh {
                    for ((a, b), c) in a.iter().zip(b).zip(c) {
                        *c = f.call(a.clone(), b.clone(), env)?;
                    }
                } else {
                    match (a_row_len, b_row_len) {
                        (0, 0) => {}
                        (0, _) => {
                            let a = vec![a_fill.unwrap().value.clone(); b_row_len];
                            for (b, c) in
                                b.chunks_exact(b_row_len).zip(c.chunks_exact_mut(c_row_len))
                            {
                                recur(&a, ash, b, bsh, c)?;
                            }
                        }
                        (_, 0) => {
                            let b = vec![b_fill.unwrap().value.clone(); a_row_len];
                            for (a, c) in
                                a.chunks_exact(a_row_len).zip(c.chunks_exact_mut(c_row_len))
                            {
                                recur(a, ash, &b, bsh, c)?;
                            }
                        }
                        _ => {
                            for ((a, b), c) in a
                                .chunks_exact(a_row_len)
                                .zip(b.chunks_exact(b_row_len))
                                .zip(c.chunks_exact_mut(c_row_len))
                            {
                                recur(a, ash, b, bsh, c)?;
                            }
                        }
                    }
                }
            } else if al < bl {
                if let Some(a_fill) = a_fill {
                    let a_fill_row = vec![a_fill.value.clone(); a_row_len];
                    if a_fill.is_left() {
                        let a_iter = repeat_n(a_fill_row.as_slice(), *bl - *al)
                            .chain(a.chunks_exact(a_row_len.max(1)));
                        for ((a, b), c) in a_iter
                            .zip(b.chunks_exact(b_row_len))
                            .zip(c.chunks_exact_mut(c_row_len))
                        {
                            recur(a, ash, b, bsh, c)?;
                        }
                    } else {
                        let a_iter = a
                            .chunks_exact(a_row_len.max(1))
                            .chain(repeat(a_fill_row.as_slice()));
                        for ((a, b), c) in a_iter
                            .zip(b.chunks_exact(b_row_len))
                            .zip(c.chunks_exact_mut(c_row_len))
                        {
                            recur(a, ash, b, bsh, c)?;
                        }
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
                let b_fill_row = vec![b_fill.value.clone(); b_row_len];
                if b_fill.is_left() {
                    let b_iter = repeat_n(b_fill_row.as_slice(), *al - *bl)
                        .chain(b.chunks_exact(b_row_len.max(1)));
                    for ((a, b), c) in a
                        .chunks_exact(a_row_len)
                        .zip(b_iter)
                        .zip(c.chunks_exact_mut(c_row_len))
                    {
                        recur(a, ash, b, bsh, c)?;
                    }
                } else {
                    let b_iter = b
                        .chunks_exact(b_row_len.max(1))
                        .chain(repeat(b_fill_row.as_slice()));
                    for ((a, b), c) in a
                        .chunks_exact(a_row_len)
                        .zip(b_iter)
                        .zip(c.chunks_exact_mut(c_row_len))
                    {
                        recur(a, ash, b, bsh, c)?;
                    }
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
    use_fill: bool,
    env: &Uiua,
    f: impl Fn(T, T) -> T + Copy,
) -> UiuaResult
where
    T: ArrayValue + Copy,
{
    fn derive_new_shape(
        ash: &Shape,
        bsh: &Shape,
        fill_err: Option<&str>,
        env: &Uiua,
    ) -> UiuaResult<(Shape, bool)> {
        let new_rank = ash.len().max(bsh.len());
        let mut new_shape = Shape::with_capacity(new_rank);
        let mut requires_fill = false;
        for i in 0..new_rank {
            let c = match (ash.get(i).copied(), bsh.get(i).copied()) {
                (None, None) => unreachable!(),
                (Some(a), None) => a,
                (None, Some(b)) => b,
                (Some(ad), Some(bd)) => {
                    if ad == bd || ad == 1 || bd == 1 {
                        match fill_err {
                            None => {
                                requires_fill |= ad != bd;
                                ad.max(bd)
                            }
                            Some(_) => pervade_dim(ad, bd),
                        }
                    } else {
                        match &fill_err {
                            None => {
                                requires_fill = true;
                                pervade_dim(ad, bd)
                            }
                            Some(e) => {
                                return Err(env.error(format!(
                                    "Shapes {ash} and {bsh} are not compatible{e}"
                                )));
                            }
                        }
                    }
                }
            };
            new_shape.push(c);
        }
        Ok((new_shape, requires_fill))
    }

    let fill = if use_fill {
        env.scalar_fill::<T>()
    } else {
        Err("")
    };
    let (new_shape, requires_fill) =
        derive_new_shape(&a.shape, &b.shape, fill.as_ref().err().copied(), env)?;
    validate_size::<T>(new_shape.iter().copied(), env)?;
    let fill = if requires_fill { fill.ok() } else { None };

    // dbg!(a.shape, b.shape, &new_shape, &fill);

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
        fill: FillValue<T>,
        f: impl Fn(T, T) -> T + Copy,
    ) {
        match (ash, bsh) {
            ([], _) => {
                for b in b {
                    *b = f(a[0], *b);
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => {
                let a_row_len: usize = ash.iter().product();
                let b_row_len = b.len() / bl;
                if al == bl {
                    if a_row_len == 0 {
                        let a = vec![fill.value; b_row_len];
                        for b in b.chunks_exact_mut(b_row_len) {
                            reuse_fill(&a, b, ash, bsh, fill, f);
                        }
                    } else {
                        for (a, b) in a.chunks_exact(a_row_len).zip(b.chunks_exact_mut(b_row_len)) {
                            reuse_fill(a, b, ash, bsh, fill, f);
                        }
                    }
                } else if a_row_len == 0 {
                    for b in b.chunks_exact_mut(b_row_len) {
                        reuse_fill(&[], b, ash, bsh, fill, f);
                    }
                } else {
                    let fill_row = vec![fill.value; a_row_len];
                    if fill.is_left() {
                        for (a, b) in repeat_n(fill_row.as_slice(), *bl - *al)
                            .chain(a.chunks_exact(a_row_len))
                            .zip(b.chunks_exact_mut(b_row_len))
                        {
                            reuse_fill(a, b, ash, bsh, fill, f);
                        }
                    } else {
                        for (a, b) in a
                            .chunks_exact(a_row_len)
                            .chain(repeat(fill_row.as_slice()))
                            .zip(b.chunks_exact_mut(b_row_len))
                        {
                            reuse_fill(a, b, ash, bsh, fill, f);
                        }
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
        fill: FillValue<T>,
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
                let a_row_len = ash.iter().product();
                let b_row_len = bsh.iter().product();
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
                        let a_fill_row = vec![fill.value; a_row_len];
                        if fill.is_left() {
                            let a_iter = repeat_n(a_fill_row.as_slice(), *bl - *al)
                                .chain(a.chunks_exact(a_row_len));
                            for ((a, b), c) in a_iter
                                .zip(b.chunks_exact(b_row_len))
                                .zip(c.chunks_exact_mut(c_row_len))
                            {
                                use_new_fill(a, b, c, ash, bsh, fill, f);
                            }
                        } else {
                            let a_iter = a
                                .chunks_exact(a_row_len)
                                .chain(repeat(a_fill_row.as_slice()));
                            if b_row_len == 0 {
                                let b = vec![fill.value; a_row_len];
                                for (a, c) in a_iter.zip(c.chunks_exact_mut(c_row_len)) {
                                    use_new_fill(a, &b, c, ash, bsh, fill, f);
                                }
                            } else {
                                for ((a, b), c) in a_iter
                                    .zip(b.chunks_exact(b_row_len))
                                    .zip(c.chunks_exact_mut(c_row_len))
                                {
                                    use_new_fill(a, b, c, ash, bsh, fill, f);
                                }
                            }
                        }
                    }
                    Ordering::Greater => {
                        let b_fill_row = vec![fill.value; b_row_len];
                        if fill.is_left() {
                            let b_iter = repeat_n(b_fill_row.as_slice(), *al - *bl)
                                .chain(b.chunks_exact(b_row_len));
                            for ((a, b), c) in a
                                .chunks_exact(a_row_len)
                                .zip(b_iter)
                                .zip(c.chunks_exact_mut(c_row_len))
                            {
                                use_new_fill(a, b, c, ash, bsh, fill, f);
                            }
                        } else {
                            let b_iter = b
                                .chunks_exact(b_row_len)
                                .chain(repeat(b_fill_row.as_slice()));
                            if a_row_len == 0 {
                                let a = vec![fill.value; b_row_len];
                                for (b, c) in b_iter.zip(c.chunks_exact_mut(c_row_len)) {
                                    use_new_fill(&a, b, c, ash, bsh, fill, f);
                                }
                            } else {
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

pub(crate) fn bin_pervade_values(
    a: Value,
    b: Value,
    a_fill: Result<Value, &'static str>,
    b_fill: Result<Value, &'static str>,
    outputs: usize,
    env: &mut Uiua,
    f: impl FnMut(Value, Value, &mut Uiua) -> UiuaResult<MultiOutput<Value>> + Clone,
) -> UiuaResult<MultiOutput<Value>> {
    let new_shape = derive_new_shape(
        &a.shape,
        &b.shape,
        a_fill.as_ref().map(|a| &a.shape).map_err(|&e| e),
        b_fill.as_ref().map(|b| &b.shape).map_err(|&e| e),
        env,
    )?;

    #[allow(clippy::too_many_arguments)]
    fn recur(
        mut a: Value,
        mut b: Value,
        a_fill: Option<&Value>,
        b_fill: Option<&Value>,
        outputs: &mut MultiOutput<Vec<Value>>,
        env: &mut Uiua,
        mut f: impl FnMut(Value, Value, &mut Uiua) -> UiuaResult<MultiOutput<Value>> + Clone,
    ) -> UiuaResult
where {
        let mut add_outputs = |news: MultiOutput<Value>| {
            for (output, new) in outputs.iter_mut().zip(news) {
                output.push(new);
            }
        };
        match (&*a.shape, &*b.shape) {
            ([], []) => add_outputs(f(a, b, env)?),
            ([], [_]) => {
                for b in b.into_rows() {
                    add_outputs(f(a.clone(), b, env)?);
                }
            }
            ([_], []) => {
                for a in a.into_rows() {
                    add_outputs(f(a, b.clone(), env)?);
                }
            }
            ([], [_, ..]) => {
                for b in b.into_rows() {
                    recur(a.clone(), b, a_fill, b_fill, outputs, env, f.clone())?;
                }
            }
            ([_, ..], []) => {
                for a in a.into_rows() {
                    recur(a, b.clone(), a_fill, b_fill, outputs, env, f.clone())?;
                }
            }
            ([al, ash @ ..], [bl, bsh @ ..]) => match (al.cmp(bl), a_fill, b_fill) {
                (Ordering::Equal, _, _) => {
                    for (a, b) in a.into_rows().zip(b.into_rows()) {
                        recur(a, b, a_fill, b_fill, outputs, env, f.clone())?;
                    }
                }
                (Ordering::Less, Some(a_fill), _) => {
                    let mut a_fill_val = a_fill.clone();
                    if a_fill_val.rank() + 1 < a.rank() {
                        for &d in ash[..a.rank() - a_fill_val.rank() - 1].iter().rev() {
                            a_fill_val.reshape_scalar(Ok(d as isize), true, env)?;
                        }
                    }
                    let a_iter = a.into_rows().chain(repeat(a_fill_val));
                    for (a, b) in a_iter.zip(b.into_rows()) {
                        recur(a, b, Some(a_fill), b_fill, outputs, env, f.clone())?;
                    }
                }
                (Ordering::Less, None, _) => {
                    debug_assert_eq!(a.row_count(), 1);
                    a.shape.remove(0);
                    for b in b.into_rows() {
                        recur(a.clone(), b, a_fill, b_fill, outputs, env, f.clone())?;
                    }
                }
                (Ordering::Greater, _, Some(b_fill)) => {
                    let mut b_fill_val = b_fill.clone();
                    if b_fill_val.rank() + 1 < b.rank() {
                        for &d in bsh[..b.rank() - b_fill_val.rank() - 1].iter().rev() {
                            b_fill_val.reshape_scalar(Ok(d as isize), true, env)?;
                        }
                    }
                    let b_iter = b.into_rows().chain(repeat(b_fill_val));
                    for (a, b) in a.into_rows().zip(b_iter) {
                        recur(a, b, a_fill, Some(b_fill), outputs, env, f.clone())?
                    }
                }
                (Ordering::Greater, _, None) => {
                    debug_assert_eq!(b.row_count(), 1);
                    b.shape.remove(0);
                    for a in a.into_rows() {
                        recur(a, b.clone(), a_fill, b_fill, outputs, env, f.clone())?;
                    }
                }
            },
        }
        Ok(())
    }

    let mut outputs = multi_output(outputs, Vec::new());
    recur(
        a,
        b,
        a_fill.ok().as_ref(),
        b_fill.ok().as_ref(),
        &mut outputs,
        env,
        f,
    )?;

    let mut new_values = MultiOutput::new();
    for output in outputs {
        let mut new_val = Value::from_row_values(output, env)?;
        let mut this_shape = new_shape.clone();
        this_shape.extend_from_slice(&new_val.shape[1..]);
        new_val.shape = this_shape;
        new_val.validate();
        new_values.push(new_val);
    }
    Ok(new_values)
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
        if a == 0.0 { 0.0 } else { a.signum() }
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
pub mod recip {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.recip()
    }
    pub fn byte(a: u8) -> f64 {
        f64::from(a).recip()
    }
    pub fn com(a: Complex) -> Complex {
        a.recip()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the reciprocal of {a}"))
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
pub mod exp {
    use std::f64::consts::E;

    use super::*;
    pub fn num(a: f64) -> f64 {
        E.powf(a)
    }
    pub fn byte(a: u8) -> f64 {
        num(a.into())
    }
    pub fn com(a: Complex) -> Complex {
        Complex::new(E, 0.0).powc(a)
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the exponential of {a}"))
    }
}
pub mod ln {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.ln()
    }
    pub fn byte(a: u8) -> f64 {
        f64::from(a).ln()
    }
    pub fn com(a: Complex) -> Complex {
        a.ln()
    }
    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot take the natural logarithm of {a}"))
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
pub mod acos {
    use super::*;
    pub fn num(a: f64) -> f64 {
        a.acos()
    }
    pub fn byte(a: u8) -> f64 {
        f64::from(a).acos()
    }
    pub fn com(a: Complex) -> Complex {
        a.acos()
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

pub mod exp2 {
    use super::*;

    pub fn byte(a: u8) -> f64 {
        num(a as f64)
    }
    pub fn num(a: f64) -> f64 {
        a.exp2()
    }
    pub fn com(a: Complex) -> Complex {
        Complex::from(2.0).powc(a)
    }

    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot raise {a} to the 2nd"))
    }
}
pub mod exp10 {
    use super::*;

    pub fn byte(a: u8) -> f64 {
        10f64.powi(a as i32)
    }
    pub fn num(a: f64) -> f64 {
        10f64.powf(a)
    }
    pub fn com(a: Complex) -> Complex {
        Complex::from(10.0).powc(a)
    }

    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot raise {a} to the 10th"))
    }
}
pub mod log2 {
    use super::*;

    pub fn byte(a: u8) -> f64 {
        num(a as f64)
    }
    pub fn num(a: f64) -> f64 {
        a.log2()
    }
    pub fn com(a: Complex) -> Complex {
        a.log(2.0)
    }

    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the log₂ of {a}"))
    }
}
pub mod log10 {
    use super::*;

    pub fn byte(a: u8) -> f64 {
        num(a as f64)
    }
    pub fn num(a: f64) -> f64 {
        a.log10()
    }
    pub fn com(a: Complex) -> Complex {
        a.log(10.0)
    }

    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the log₁₀ of {a}"))
    }
}

pub mod square_abs {
    use super::*;

    pub fn byte(a: u8) -> f64 {
        num(a as f64)
    }
    pub fn num(a: f64) -> f64 {
        a * a
    }
    pub fn com(a: Complex) -> f64 {
        a.re * a.re + a.im * a.im
    }

    pub fn error<T: Display>(a: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot square {a}"))
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
cmp_impl!(other_is_lt == Ordering::Less);
cmp_impl!(other_is_le != Ordering::Greater);
cmp_impl!(other_is_gt == Ordering::Greater);
cmp_impl!(other_is_ge != Ordering::Less);

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
        char::from_u32(((b as i64).saturating_add(a as i64)).clamp(0, char::MAX as i64) as u32)
            .unwrap_or('\0')
    }
    pub fn char_num(a: char, b: f64) -> char {
        char::from_u32(((b as i64).saturating_add(a as i64)).clamp(0, char::MAX as i64) as u32)
            .unwrap_or('\0')
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
        char::from_u32(((b as i64).saturating_sub(a as i64)).clamp(0, char::MAX as i64) as u32)
            .unwrap_or('\0')
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
        if a < 0.0 { toggle_char_case(b) } else { b }
    }
    pub fn char_num(a: char, b: f64) -> char {
        if b < 0.0 { toggle_char_case(a) } else { a }
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

pub mod set_sign {
    use super::*;

    pub fn num_num(a: f64, b: f64) -> f64 {
        mul::num_num(a, b.abs())
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        num_num(a as f64, b as f64)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a as f64, b)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(a, b as f64)
    }
    pub fn num_char(a: f64, b: char) -> char {
        if a > 0.0 {
            scalar_abs::char(b)
        } else if a < 0.0 {
            scalar_neg::char(scalar_abs::char(b))
        } else {
            b
        }
    }
    pub fn char_num(a: char, b: f64) -> char {
        num_char(b, a)
    }
    pub fn byte_char(a: u8, b: char) -> char {
        if a > 0 { scalar_abs::char(b) } else { b }
    }
    pub fn char_byte(a: char, b: u8) -> char {
        byte_char(b, a)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        a * b.into().abs()
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        a.into() * b.abs()
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot set sign of {b} to {a}"))
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
        if a < 0.0 { toggle_char_case(b) } else { b }
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

pub mod modulo {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.rem_euclid(a).abs()
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

pub mod or {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        if a == 0.0 {
            return b;
        }
        if b == 0.0 {
            return a;
        }
        if a.is_nan() || b.is_nan() {
            return f64::NAN;
        }
        if a.is_infinite() {
            return a.signum() * b;
        }
        if b.is_infinite() {
            return b.signum() * a;
        }
        if (1.0..=u128::MAX as f64).contains(&a)
            && (1.0..=u128::MAX as f64).contains(&b)
            && a.fract() == 0.0
            && b.fract() == 0.0
        {
            let mut a = a as u128;
            let mut b = b as u128;
            let shift = (a | b).trailing_zeros();
            a >>= shift;
            b >>= shift;
            a >>= a.trailing_zeros();
            loop {
                b >>= b.trailing_zeros();
                if a > b {
                    swap(&mut a, &mut b);
                }
                b -= a;
                if b == 0 {
                    break;
                }
            }
            return (a << shift) as f64;
        }
        fn recurse(a: f64, b: f64) -> f64 {
            if b <= 8.0 * f64::EPSILON {
                return a;
            }
            recurse(b, a.rem_euclid(b))
        }
        a.signum() * b.signum() * recurse(a.abs(), b.abs())
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        num_num(b.into(), a)
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        num_num(a.into(), b)
    }
    pub fn byte_byte(mut a: u8, mut b: u8) -> u8 {
        if a == 0 {
            return b;
        }
        if b == 0 {
            return a;
        }
        let shift = (a | b).trailing_zeros();
        a >>= shift;
        b >>= shift;
        a >>= a.trailing_zeros();
        loop {
            b >>= b.trailing_zeros();
            if a > b {
                swap(&mut a, &mut b);
            }
            b -= a;
            if b == 0 {
                break;
            }
        }
        a << shift
    }
    pub fn bool_bool(a: u8, b: u8) -> u8 {
        a | b
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        let b = b.into();
        Complex::new(num_num(a.re, b.re), num_num(a.im, b.im))
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        let a = a.into();
        Complex::new(num_num(a.re, b.re), num_num(a.im, b.im))
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot or {a} and {b}"))
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
pub mod scalar_pow {
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
pub mod root {
    use super::*;
    pub fn num_num(a: f64, b: f64) -> f64 {
        b.powf(1.0 / a)
    }
    pub fn byte_byte(a: u8, b: u8) -> f64 {
        f64::from(b).powf(1.0 / f64::from(a))
    }
    pub fn byte_num(a: u8, b: f64) -> f64 {
        b.powf(1.0 / a as f64)
    }
    pub fn num_byte(a: f64, b: u8) -> f64 {
        f64::from(b).powf(1.0 / a)
    }
    pub fn com_x(a: Complex, b: impl Into<Complex>) -> Complex {
        b.into().powc(1.0 / a)
    }
    pub fn x_com(a: impl Into<Complex>, b: Complex) -> Complex {
        b.powc(1.0 / a.into())
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        env.error(format!("Cannot get the {a} root of {b}"))
    }
}
bin_op_mod!(
    log,
    a,
    b,
    f64::from,
    f64,
    b.log(a),
    "Cannot get the log base {a} of {b}"
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

pub mod abs_complex {
    use super::*;

    pub fn num(a: impl Into<f64>, b: impl Into<f64>) -> f64 {
        complex::num_num(a.into(), b.into()).abs()
    }
    pub fn com(a: impl Into<Complex>, b: impl Into<Complex>) -> f64 {
        complex::com_x(a.into(), b).abs()
    }
    pub fn error<T: Display>(a: T, b: T, env: &Uiua) -> UiuaError {
        complex::error(a, b, env)
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
