//! Algorithms for monadic array operations

use core::str;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    convert::identity,
    f64::consts::{PI, TAU},
    fmt,
    io::Write,
    iter::{self, once},
    mem::size_of,
    ptr, slice,
    time::Duration,
};

use ecow::{eco_vec, EcoVec};
use enum_iterator::{all, Sequence};
use rayon::prelude::*;
use time::{Date, Month, OffsetDateTime, Time};
use unicode_segmentation::UnicodeSegmentation;

use crate::{
    array::*,
    cowslice::{cowslice, CowSlice},
    grid_fmt::GridFmt,
    val_as_arr,
    value::Value,
    Boxed, Complex, Primitive, Shape, Uiua, UiuaResult,
};

use super::{validate_size, ArrayCmpSlice, FillContext};

impl Value {
    /// Make the value 1-dimensional
    pub fn deshape(&mut self) {
        self.deshape_depth(0);
    }
    pub(crate) fn deshape_depth(&mut self, mut depth: usize) {
        if let Value::Box(arr) = self {
            if let Some(Boxed(val)) = arr.as_scalar_mut() {
                val.deshape_depth(depth);
                return;
            }
        }
        if self.is_map() {
            self.take_map_keys();
        }
        depth = depth.min(self.rank());
        let deshaped = self.shape_mut().split_off(depth).into_iter().product();
        self.shape_mut().push(deshaped);
    }
    pub(crate) fn deshape_sub(&mut self, irank: i32, extend: bool, env: &Uiua) -> UiuaResult {
        if irank > 0 && irank as usize == self.rank() {
            return Ok(());
        }
        self.take_map_keys();
        let shape = self.shape_mut();
        let rank = irank.unsigned_abs() as usize;
        match irank.cmp(&0) {
            Ordering::Equal => {
                // First scalar
                if shape.contains(&0) {
                    if let Some(fill) = env.value_fill() {
                        *self = fill.clone();
                    } else {
                        return Err(env.error(format!(
                            "Cannot get first scalar of an empty array (shape {shape})"
                        )));
                    }
                } else {
                    *shape = [].into();
                    val_as_arr!(self, |arr| arr.data.truncate(1));
                }
            }
            Ordering::Greater => {
                // Positive rank
                match rank.cmp(&shape.len()) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        let mid = shape.len() + 1 - rank;
                        let new_first_dim: usize = shape[..mid].iter().product();
                        *shape = once(new_first_dim)
                            .chain(shape[mid..].iter().copied())
                            .collect();
                    }
                    Ordering::Greater => {
                        if extend {
                            for _ in 0..rank - shape.len() {
                                shape.insert(0, 1);
                            }
                        } else {
                            shape.insert(0, 1);
                        }
                    }
                }
            }
            Ordering::Less => {
                // Negative rank
                if rank + 1 > shape.len() {
                    return if extend {
                        Err(env.error(format!(
                            "Negative {} has magnitude {}, but the \
                        rank-{} array cannot be reduced that much",
                            Primitive::Deshape.format(),
                            rank,
                            shape.len()
                        )))
                    } else {
                        Ok(())
                    };
                }
                let new_first_dim: usize = shape[..=rank].iter().product();
                *shape = once(new_first_dim)
                    .chain(shape[rank + 1..].iter().copied())
                    .collect();
            }
        }
        self.validate_shape();
        Ok(())
    }
    pub(crate) fn undo_deshape(
        &mut self,
        sub: Option<i32>,
        orig_shape: &Shape,
        env: &Uiua,
    ) -> UiuaResult {
        if let Some(irank) = sub {
            if self.rank() == 0 {
                if let Value::Box(arr) = self {
                    arr.data.as_mut_slice()[0]
                        .0
                        .undo_deshape(sub, orig_shape, env)?;
                }
                return Ok(());
            }
            if irank == 0 {
                return Ok(());
            }
            let rank = irank.unsigned_abs() as usize;
            let new_shape: Shape = if irank >= 0 {
                // Positive rank
                (orig_shape.iter())
                    .take((orig_shape.len() + 1).saturating_sub(rank))
                    .chain((self.shape().iter()).skip(rank.saturating_sub(orig_shape.len()).max(1)))
                    .copied()
                    .collect()
            } else {
                // Negative rank
                (orig_shape.iter().take(rank + 1))
                    .chain(self.shape().iter().skip(1))
                    .copied()
                    .collect()
            };
            if validate_size::<u8>(new_shape.iter().copied(), env)? != self.element_count() {
                return Ok(());
            }
            *self.shape_mut() = new_shape;
            self.validate_shape();
            Ok(())
        } else {
            let mut new_shape = self.shape().clone();
            if new_shape.len() > 0 {
                new_shape.remove(0);
            }
            for &d in orig_shape.iter().rev() {
                new_shape.insert(0, d);
            }
            if new_shape.elements() == self.element_count() {
                *self.shape_mut() = new_shape;
                Ok(())
            } else {
                let spec: Vec<Result<isize, bool>> =
                    orig_shape.iter().map(|&d| Ok(d as isize)).collect();
                self.reshape_impl(&spec, env)
            }
        }
    }
    pub(crate) fn box_depth(mut self, depth: usize) -> Array<Boxed> {
        let depth = depth.min(self.rank());
        if depth == 0 {
            return Boxed(self).into();
        }
        let per_meta = self.take_per_meta();
        let new_shape: Shape = self.shape()[..depth].into();
        let row_shape: Shape = self.shape()[depth..].into();
        let data: EcoVec<Boxed> = self.into_row_shaped_slices(row_shape).map(Boxed).collect();
        let mut arr = Array::new(new_shape, data);
        arr.set_per_meta(per_meta);
        arr
    }
    /// Attempt to parse the value into a number
    pub fn parse_num(&self, env: &Uiua) -> UiuaResult<Self> {
        Ok(match (self, self.shape().dims()) {
            (Value::Char(arr), [] | [_]) => {
                let s: String = arr.data.iter().copied().collect();
                match (
                    s.strip_suffix("i").and_then(|s| s.split_once("r")),
                    s.strip_suffix("i").and_then(|s| s.split_once("+")),
                    s.strip_suffix("i").and_then(|s| s.split_once("-")),
                ) {
                    (Some((re, im)), None, _) | (None, Some((re, im)), _) => {
                        let re = parse_uiua_num(re.into(), env);
                        let im = parse_uiua_num(im.into(), env);
                        re.and_then(|re| im.map(|im| Complex { re, im }.into()))
                            .or_else(|e| env.value_fill().cloned().ok_or(e))?
                    }
                    (_, _, Some((re, im))) => {
                        let re = parse_uiua_num(re.into(), env);
                        let im = parse_uiua_num(im.into(), env);
                        re.and_then(|re| im.map(|im| Complex { re, im: -im }.into()))
                            .or_else(|e| env.value_fill().cloned().ok_or(e))?
                    }
                    _ => parse_uiua_num(s.into(), env)
                        .map(Into::into)
                        .or_else(|e| env.value_fill().cloned().ok_or(e))?,
                }
            }
            (Value::Box(arr), []) => {
                let value = &arr.data[0].0;
                value.parse_num(env)?
            }
            (Value::Char(_) | Value::Box(_), _) => {
                let mut rows = Vec::with_capacity(self.row_count());
                for row in self.rows() {
                    rows.push(row.parse_num(env)?);
                }
                Value::from_row_values(rows, env)?
            }
            (val, _) => return Err(env.error(format!("Cannot parse {} array", val.type_name()))),
        })
    }
    pub(crate) fn unparse(&self, env: &Uiua) -> UiuaResult<Self> {
        if self.rank() == 0 {
            return match self {
                Value::Box(b) => b.as_scalar().unwrap().as_value().unparse(env),
                value => Ok(value.format().into()),
            };
        }

        fn padded<T: fmt::Display>(c: char, arr: &Array<T>, env: &Uiua) -> UiuaResult<Array<char>> {
            let mut buf = Vec::new();
            let mut max_whole = 0;
            let mut max_dec = 0;
            for v in &arr.data {
                buf.clear();
                write!(&mut buf, "{v}").unwrap();
                if let Some(i) = buf.iter().position(|&c| c == b'.') {
                    max_whole = max_whole.max(i);
                    max_dec = max_dec.max(buf.len() - i - 1);
                } else {
                    max_whole = max_whole.max(buf.len());
                }
            }
            let max_len = if max_dec == 0 {
                max_whole
            } else {
                max_whole + max_dec + 1
            };
            let mut new_shape = arr.shape().clone();
            new_shape.push(max_len);
            let elem_count = validate_size::<char>(new_shape.iter().copied(), env)?;
            let mut new_data = eco_vec![c; elem_count];
            if max_len > 0 {
                for (i, s) in new_data.make_mut().chunks_exact_mut(max_len).enumerate() {
                    let n = arr.data[i].to_string();
                    let dot_pos = n.find('.');
                    let skip = if max_dec == 0 {
                        0
                    } else {
                        dot_pos
                            .map(|i| max_dec - (n.len() - i - 1))
                            .unwrap_or(max_dec + 1)
                    };
                    for (s, c) in s.iter_mut().rev().skip(skip).zip(n.chars().rev()) {
                        *s = c;
                    }
                    if dot_pos.is_none() && max_dec > 0 && c.is_ascii_digit() {
                        s[max_whole] = '.';
                    }
                }
            }
            Ok(Array::new(new_shape, new_data))
        }

        Ok(match self {
            Value::Num(arr) => {
                if let Ok(c) = env.scalar_fill::<char>() {
                    padded(c, arr, env)?.into()
                } else {
                    let new_data: CowSlice<Boxed> = (arr.data.iter().map(|v| v.to_string()))
                        .map(Value::from)
                        .map(Boxed)
                        .collect();
                    Array::new(arr.shape.clone(), new_data).into()
                }
            }
            Value::Byte(arr) => {
                if let Ok(c) = env.scalar_fill::<char>() {
                    padded(c, arr, env)?.into()
                } else {
                    let new_data: CowSlice<Boxed> = (arr.data.iter().map(|v| v.to_string()))
                        .map(Value::from)
                        .map(Boxed)
                        .collect();
                    Array::new(arr.shape.clone(), new_data).into()
                }
            }
            Value::Complex(arr) => {
                let new_data: CowSlice<Boxed> = (arr.data.iter().map(|v| v.to_string()))
                    .map(Value::from)
                    .map(Boxed)
                    .collect();
                Array::new(arr.shape.clone(), new_data).into()
            }
            val => return Err(env.error(format!("Cannot unparse {} array", val.type_name()))),
        })
    }
}

fn parse_uiua_num(mut s: Cow<str>, env: &Uiua) -> UiuaResult<f64> {
    let mut mul = 1.0;
    if s.contains('¯') {
        s = s.replace('¯', "-").into();
    }
    if s.contains('`') {
        s = s.replace('`', "-").into();
    }
    'glyphs: for (names, constant) in [
        (["η", "eta"], PI * 0.5),
        (["π", "pi"], PI),
        (["τ", "tau"], TAU),
        (["∞", "inf"], f64::INFINITY),
    ] {
        if let Some((before, after)) = s.split_once('/') {
            for name in names {
                if before.trim_start_matches('-') == name {
                    s = format!("{}/{}", before.replace(name, &constant.to_string()), after).into();
                    break 'glyphs;
                } else if let Some(start) = before.strip_suffix(name) {
                    mul = constant;
                    s = format!("{}/{}", start, after).into();
                    break 'glyphs;
                }
            }
        } else {
            for name in names {
                if s.trim_start_matches('-') == name {
                    s = s.replace(name, &constant.to_string()).into();
                    break 'glyphs;
                } else if let Some(start) = s.strip_suffix(name) {
                    mul = constant;
                    s = start.to_string().into();
                    break 'glyphs;
                }
            }
        }
    }
    match s.split_once('/') {
        Some((numer, denom)) => numer
            .parse::<f64>()
            .and_then(|n| denom.parse::<f64>().map(|d| n / d)),
        None => s.parse::<f64>(),
    }
    .map(|n| n * mul)
    .map_err(|e| env.error(format!("Cannot parse into number: {}", e)))
}

impl<T: ArrayValue> Array<T> {
    /// Make the array 1-dimensional
    pub fn deshape(&mut self) {
        if self.rank() == 1 {
            return;
        }
        if self.is_map() {
            self.take_map_keys();
        }
        self.shape.deshape();
    }
    /// Add a 1-length dimension to the front of the array's shape
    pub fn fix(&mut self) {
        self.fix_depth(0);
    }
    pub(crate) fn fix_depth(&mut self, depth: usize) {
        let depth = self.shape.fix_depth(depth);
        if depth == 0 {
            if let Some(keys) = self.map_keys_mut() {
                keys.fix();
            }
        }
    }
    /// Remove a 1-length dimension from the front of the array's shape
    pub fn unfix(&mut self, env: &Uiua) -> UiuaResult {
        if let Some(keys) = self.map_keys_mut() {
            keys.unfix();
        }
        self.shape.unfix().map_err(|e| env.error(e))
    }
    /// Collapse the top two dimensions of the array's shape
    pub fn undo_fix(&mut self) {
        if let Some(keys) = self.map_keys_mut() {
            keys.unfix();
        }
        _ = self.shape.unfix();
    }
}

impl<T: ArrayValue> Array<T>
where
    Value: From<Array<T>>,
{
    pub(crate) fn box_depth(mut self, depth: usize) -> Array<Boxed> {
        let depth = depth.min(self.rank());
        if depth == 0 {
            return Boxed(self.into()).into();
        }
        let per_meta = self.take_per_meta();
        let new_shape: Shape = self.shape()[..depth].into();
        let row_shape: Shape = self.shape()[depth..].into();
        let data: EcoVec<Boxed> = self
            .into_row_shaped_slices(row_shape)
            .map(Value::from)
            .map(Boxed)
            .collect();
        let mut arr = Array::new(new_shape, data);
        arr.set_per_meta(per_meta);
        arr
    }
}

impl Value {
    /// Create a `range` array
    pub fn range(&self, env: &Uiua) -> UiuaResult<Self> {
        let ishape = self.as_ints(
            env,
            "Range max should be a single integer \
            or a list of integers",
        )?;
        if self.rank() == 0 {
            let max = ishape[0];
            return Ok(if max >= 0 {
                if max <= 256 {
                    (0..max).map(|i| i as u8).collect()
                } else {
                    validate_size::<f64>([max.unsigned_abs()], env)?;
                    (0..max).map(|i| i as f64).collect()
                }
            } else {
                validate_size::<f64>([max.unsigned_abs()], env)?;
                (max..0).map(|i| i as f64).rev().collect()
            });
        }
        if ishape.is_empty() {
            return Ok(Array::<f64>::new(0, CowSlice::new()).into());
        }
        let mut shape = Shape::from_iter(ishape.iter().map(|d| d.unsigned_abs()));
        shape.push(shape.len());
        let data = range(&ishape, env)?;
        Ok(match data {
            Ok(data) => Array::new(shape, data).into(),
            Err(data) => Array::new(shape, data).into(),
        })
    }
    pub(crate) fn unshape(&self, env: &Uiua) -> UiuaResult<Self> {
        let ishape = self.as_ints(
            env,
            "Shape should be a single integer or a list of integers",
        )?;
        let shape = Shape::from_iter(ishape.iter().map(|n| n.unsigned_abs()));
        let elems: usize = validate_size::<f64>(shape.iter().copied(), env)?;
        let data = EcoVec::from_iter((0..elems).map(|i| i as f64));
        let mut arr = Array::new(shape, data);
        for (i, s) in ishape.into_iter().enumerate() {
            if s < 0 {
                arr.reverse_depth(i);
            }
        }
        Ok(arr.into())
    }
}

pub(crate) fn range(
    shape: &[isize],
    env: &Uiua,
) -> UiuaResult<Result<CowSlice<f64>, CowSlice<u8>>> {
    if shape.is_empty() {
        return Ok(Err(cowslice![0]));
    }
    let mut prod = 1usize;
    for &d in shape {
        if d != 0 {
            let (new, overflow) = prod.overflowing_mul(d.unsigned_abs());
            if overflow {
                let mut starting_with = "[".to_string();
                for (i, d) in shape.iter().take(3).enumerate() {
                    if i > 0 {
                        starting_with.push_str(" × ");
                    }
                    starting_with.push_str(&d.to_string());
                }
                if shape.len() > 3 {
                    starting_with.push_str(" × …");
                }
                starting_with.push(']');
                return Err(env.error(format!(
                    "{} of length-{} shape {} would be too large",
                    Primitive::Range.format(),
                    shape.len(),
                    starting_with
                )));
            }
            prod = new;
        }
    }
    if shape.contains(&0) {
        return Ok(Err(CowSlice::new()));
    }
    let mut len = shape.len();
    for &item in shape {
        let (new, overflow) = len.overflowing_mul(item.unsigned_abs());
        if overflow || new > 2usize.pow(30) / size_of::<f64>() {
            let len = shape.len() as f64 * shape.iter().map(|d| *d as f64).product::<f64>();
            return Err(env.error(format!(
                "Attempting to make a range from shape {} would \
                create an array with {} elements, which is too large",
                FormatShape(&shape.iter().map(|d| d.unsigned_abs()).collect::<Vec<_>>()),
                len
            )));
        }
        len = new;
    }
    let mut scan = shape
        .iter()
        .rev()
        .scan(1, |acc, &d| {
            let old = *acc;
            *acc *= d.unsigned_abs();
            Some(old)
        })
        .collect::<Vec<usize>>();
    scan.reverse();
    let elem_count = shape.len() * shape.iter().map(|d| d.unsigned_abs()).product::<usize>();
    let any_neg = shape.iter().any(|&d| d < 0);
    let max = shape.iter().map(|d| d.unsigned_abs()).max().unwrap();
    if max <= 256 && !any_neg {
        validate_size::<u8>([len], env)?;
        let mut data: EcoVec<u8> = eco_vec![0; len];
        let data_slice = data.make_mut();
        for i in 0..elem_count {
            let dim = i % shape.len();
            let index = i / shape.len();
            data_slice[i] = (index / scan[dim] % shape[dim].unsigned_abs()) as u8;
        }
        Ok(Err(data.into()))
    } else {
        validate_size::<f64>([len], env)?;
        let mut data: EcoVec<f64> = eco_vec![0.0; len];
        let data_slice = data.make_mut();
        for i in 0..elem_count {
            let dim = i % shape.len();
            let index = i / shape.len();
            data_slice[i] = (index / scan[dim] % shape[dim].unsigned_abs()) as f64;
            if shape[dim] < 0 {
                data_slice[i] = -1.0 - data_slice[i];
            }
        }
        Ok(Ok(data.into()))
    }
}

impl Value {
    /// Get the first row of the value
    pub fn first(self, env: &Uiua) -> UiuaResult<Self> {
        self.first_depth(0, env)
    }
    pub(crate) fn first_depth(mut self, depth: usize, env: &Uiua) -> UiuaResult<Self> {
        self.match_fill(env);
        val_as_arr!(self, |a| a.first_depth(depth, env).map(Into::into))
    }
    /// Get the last row of the value
    pub fn last(self, env: &Uiua) -> UiuaResult<Self> {
        self.last_depth(0, env)
    }
    pub(crate) fn last_depth(mut self, depth: usize, env: &Uiua) -> UiuaResult<Self> {
        self.match_fill(env);
        val_as_arr!(self, |a| a.last_depth(depth, env).map(Into::into))
    }
    pub(crate) fn undo_first(self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        into.try_map_boxed(|into| {
            self.generic_bin_into(
                into.unboxed(),
                |a, b| a.undo_first(b, env).map(Into::into),
                |a, b| a.undo_first(b, env).map(Into::into),
                |a, b| a.undo_first(b, env).map(Into::into),
                |a, b| a.undo_first(b, env).map(Into::into),
                |a, b| a.undo_first(b, env).map(Into::into),
                |a, b| {
                    env.error(format!(
                        "Cannot unfirst {} into {}",
                        a.type_name(),
                        b.type_name()
                    ))
                },
            )
        })
    }
    pub(crate) fn undo_last(self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        into.try_map_boxed(|into| {
            self.generic_bin_into(
                into.unboxed(),
                |a, b| a.undo_last(b, env).map(Into::into),
                |a, b| a.undo_last(b, env).map(Into::into),
                |a, b| a.undo_last(b, env).map(Into::into),
                |a, b| a.undo_last(b, env).map(Into::into),
                |a, b| a.undo_last(b, env).map(Into::into),
                |a, b| {
                    env.error(format!(
                        "Cannot unlast {} into {}",
                        a.type_name(),
                        b.type_name()
                    ))
                },
            )
        })
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the first row of the array
    pub fn first(mut self, env: &Uiua) -> UiuaResult<Self> {
        match &*self.shape {
            [] => Ok(self),
            [0, rest @ ..] => match env.scalar_fill() {
                Ok(fill) => {
                    self.data.extend_repeat(&fill, self.row_len());
                    self.shape = rest.into();
                    Ok(self)
                }
                Err(e) => Err(env
                    .error(format!(
                        "Cannot take {} of an empty array{e}",
                        Primitive::First.format()
                    ))
                    .fill()),
            },
            _ => {
                let row_len = self.row_len();
                self.shape.remove(0);
                self.data.truncate(row_len);
                self.take_map_keys();
                self.take_label();
                Ok(self)
            }
        }
    }
    pub(crate) fn first_depth(mut self, mut depth: usize, env: &Uiua) -> UiuaResult<Self> {
        depth = depth.min(self.rank());
        if depth == 0 {
            return self.first(env);
        }
        match &self.shape[depth..] {
            [] => Ok(self),
            [0, rest @ ..] => match env.scalar_fill() {
                Ok(fill) => {
                    self.shape = self.shape[..depth].iter().chain(rest).copied().collect();
                    self.data.extend_repeat(&fill, self.shape.elements());
                    self.validate_shape();
                    Ok(self)
                }
                Err(e) => Err(env
                    .error(format!(
                        "Cannot take {} of an empty array{e}",
                        Primitive::First.format()
                    ))
                    .fill()),
            },
            [1, ..] => {
                self.shape.remove(depth);
                self.validate_shape();
                Ok(self)
            }
            [n, rest @ ..] => {
                let row_len: usize = rest.iter().product();
                let row_count: usize = self.shape[..depth].iter().product();
                let slice = self.data.as_mut_slice();
                for i in 1..row_count {
                    let dest = i * row_len;
                    let src = i * *n * row_len;
                    unsafe {
                        ptr::swap_nonoverlapping(
                            slice.as_mut_ptr().add(dest),
                            slice.as_mut_ptr().add(src),
                            row_len,
                        )
                    };
                }
                self.shape.remove(depth);
                self.data.truncate(self.shape.elements());
                self.validate_shape();
                Ok(self)
            }
        }
    }
    /// Get the last row of the array
    pub fn last(mut self, env: &Uiua) -> UiuaResult<Self> {
        match &*self.shape {
            [] => Ok(self),
            [0, rest @ ..] => match env.scalar_fill() {
                Ok(fill) => {
                    self.data.extend_repeat(&fill, self.row_len());
                    self.shape = rest.into();
                    Ok(self)
                }
                Err(e) => Err(env
                    .error(format!("Cannot take last of an empty array{e}"))
                    .fill()),
            },
            _ => {
                let row_len = self.row_len();
                self.shape.remove(0);
                let prefix_len = self.data.len() - row_len;
                self.data = self.data[prefix_len..].into();
                self.take_map_keys();
                self.take_label();
                Ok(self)
            }
        }
    }
    pub(crate) fn last_depth(mut self, mut depth: usize, env: &Uiua) -> UiuaResult<Self> {
        depth = depth.min(self.rank());
        if depth == 0 {
            return self.last(env);
        }
        match &self.shape[depth..] {
            [] => Ok(self),
            [0, rest @ ..] => match env.scalar_fill() {
                Ok(fill) => {
                    self.shape = self.shape[..depth].iter().chain(rest).copied().collect();
                    self.data.extend_repeat(&fill, self.shape.elements());
                    Ok(self)
                }
                Err(e) => Err(env
                    .error(format!("Cannot take last of an empty array{e}"))
                    .fill()),
            },
            [1, ..] => {
                self.shape.remove(depth);
                self.validate_shape();
                Ok(self)
            }
            [n, rest @ ..] => {
                let row_len: usize = rest.iter().product();
                let row_count: usize = self.shape[..depth].iter().product();
                let slice = self.data.as_mut_slice();
                for i in 0..row_count {
                    let dest = i * row_len;
                    let src = (i + 1) * *n * row_len - row_len;
                    unsafe {
                        ptr::swap_nonoverlapping(
                            slice.as_mut_ptr().add(dest),
                            slice.as_mut_ptr().add(src),
                            row_len,
                        )
                    };
                }
                self.shape.remove(depth);
                self.data.truncate(self.shape.elements());
                self.validate_shape();
                Ok(self)
            }
        }
    }
    pub(crate) fn undo_first(self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        self.join(into.drop(&[Ok(1)], env)?, true, env)
    }
    pub(crate) fn undo_last(self, into: Self, env: &Uiua) -> UiuaResult<Self> {
        into.drop(&[Ok(-1)], env)?.join(self, true, env)
    }
}

impl Value {
    /// Reverse the rows of the value
    pub fn reverse(&mut self) {
        self.reverse_depth(0);
    }
    pub(crate) fn reverse_depth(&mut self, depth: usize) {
        self.generic_mut_deep(
            |a| a.reverse_depth(depth),
            |a| a.reverse_depth(depth),
            |a| a.reverse_depth(depth),
            |a| a.reverse_depth(depth),
            |a| a.reverse_depth(depth),
        )
    }
}

impl<T: Clone> Array<T> {
    /// Reverse the rows of the array
    pub fn reverse(&mut self) {
        self.reverse_depth(0);
    }
    pub(crate) fn reverse_depth(&mut self, mut depth: usize) {
        depth = depth.min(self.rank());
        let row_shape = &self.shape[depth..];
        if row_shape.is_empty() {
            return;
        }
        let chunk_size = row_shape.iter().product();
        if chunk_size == 0 {
            return;
        }
        let data = self.data.as_mut_slice();
        let chunk_row_count = self.shape[depth];
        let chunk_row_len = chunk_size / chunk_row_count;
        for data in data.chunks_exact_mut(chunk_size) {
            for i in 0..chunk_row_count / 2 {
                let left = i * chunk_row_len;
                let right = (chunk_row_count - i - 1) * chunk_row_len;
                let left = &mut data[left] as *mut T;
                let right = &mut data[right] as *mut T;
                unsafe {
                    ptr::swap_nonoverlapping(left, right, chunk_row_len);
                }
            }
        }

        // Reverse map keys
        if depth == 0 {
            if let Some(meta) = self.get_meta_mut() {
                if let Some(keys) = &mut meta.map_keys {
                    keys.reverse();
                }
            }
        }
    }
}

impl Value {
    /// Transpose the value
    pub fn transpose(&mut self) {
        self.generic_mut_deep(
            Array::transpose,
            Array::transpose,
            Array::transpose,
            Array::transpose,
            Array::transpose,
        )
    }
    pub(crate) fn transpose_depth(&mut self, depth: usize, amnt: i32) {
        match self {
            Value::Num(n) => n.transpose_depth(depth, amnt),
            Value::Byte(b) => b.transpose_depth(depth, amnt),
            Value::Complex(c) => c.transpose_depth(depth, amnt),
            Value::Char(c) => c.transpose_depth(depth, amnt),
            Value::Box(b) => {
                if depth == b.rank() {
                    for b in b.data.as_mut_slice() {
                        b.0.transpose();
                    }
                } else {
                    b.transpose_depth(depth, amnt);
                }
            }
        }
    }
}

impl<T: ArrayValue> Array<T> {
    /// Transpose the array
    pub fn transpose(&mut self) {
        self.transpose_depth(0, 1);
    }
    pub(crate) fn transpose_depth(&mut self, mut depth: usize, amnt: i32) {
        crate::profile_function!();
        if depth == 0 && self.is_map() {
            self.take_map_keys();
        }
        if self.rank() == 0 {
            return;
        }
        depth = depth.min(self.rank());
        let trans_count = amnt.unsigned_abs() as usize % self.rank();
        let trans_rank = self.rank() - depth;
        // Early return if nothing would actually happen
        if trans_rank < 2 || depth + trans_count == self.rank() || trans_count == 0 {
            return;
        }
        let forward = amnt.is_positive();
        // Early return if any dimension is 0, because there are no elements
        if self.shape[depth..].iter().any(|&d| d == 0) || depth > 0 && self.shape[depth - 1] == 0 {
            if forward {
                self.shape[depth..].rotate_left(trans_count);
            } else {
                self.shape[depth..].rotate_right(trans_count);
            }
            return;
        }
        let square_matrix = trans_rank == 2 && self.shape[depth] == self.shape[depth + 1];
        let s = self.shape[depth];
        // Count the number of subarrays
        let subs: usize = if forward {
            self.shape[depth..].iter().take(trans_count).product()
        } else {
            self.shape[depth..].iter().rev().skip(trans_count).product()
        };
        let data_slice = self.data.as_mut_slice();
        // Divide the array into chunks at the given depth
        for data in data_slice.chunks_exact_mut(self.shape[depth..].iter().product()) {
            // Special in-place case for square matrices
            if square_matrix {
                if subs > 500 {
                    // This is pretty unsafe, but no indices should collide, so it's fine? 🤷
                    let ptr: usize = data.as_mut_ptr() as usize;
                    (0..s - 1).into_par_iter().for_each(|i| {
                        let ptr: *mut T = ptr as *mut _;
                        for j in i + 1..s {
                            unsafe {
                                ptr::swap_nonoverlapping(ptr.add(i * s + j), ptr.add(j * s + i), 1);
                            }
                        }
                    });
                } else {
                    for i in 0..s - 1 {
                        for j in i + 1..s {
                            data.swap(i * s + j, j * s + i);
                        }
                    }
                }
                continue;
            }
            let stride = data.len() / subs;
            // The operation to perform on each subarray
            let op = |(temp_chunk_i, chunk): (usize, &mut [T])| {
                for (chunk_i, item) in chunk.iter_mut().enumerate() {
                    *item = data[chunk_i * stride + temp_chunk_i].clone();
                }
            };
            // Perform the operation on each subarray
            let mut temp = data.to_vec();
            if subs > 500 {
                temp.par_chunks_mut(subs).enumerate().for_each(op);
            } else {
                temp.chunks_mut(subs).enumerate().for_each(op);
            }
            data.clone_from_slice(&temp);
        }
        if forward {
            self.shape[depth..].rotate_left(trans_count);
        } else {
            self.shape[depth..].rotate_right(trans_count);
        }
    }
}

impl Value {
    /// Get the `rise` of the value
    pub fn rise(&self) -> Array<f64> {
        val_as_arr!(self, Array::rise)
    }
    /// Get the `fall` of the value
    pub fn fall(&self) -> Array<f64> {
        val_as_arr!(self, Array::fall)
    }
    /// Sort the value ascending
    pub fn sort_up(&mut self) {
        self.sort_up_depth(0);
    }
    pub(crate) fn sort_up_depth(&mut self, depth: usize) {
        val_as_arr!(self, |a| a.sort_up_depth(depth))
    }
    /// Sort the value descending
    pub fn sort_down(&mut self) {
        self.sort_down_depth(0);
    }
    pub(crate) fn sort_down_depth(&mut self, depth: usize) {
        val_as_arr!(self, |a| a.sort_down_depth(depth))
    }
    /// `classify` the rows of the value
    pub fn classify(&self) -> Self {
        if self.rank() == 0 {
            return 0.into();
        }
        let map_keys = self.map_keys().cloned();
        let mut val: Value = val_as_arr!(self, Array::classify).into_iter().collect();
        if let Some(map_keys) = map_keys {
            val.meta_mut().map_keys = Some(map_keys);
        }
        val
    }
    pub(crate) fn classify_depth(&self, depth: usize) -> Self {
        let map_keys = self.map_keys().cloned();
        let mut val = val_as_arr!(self, |a| a.classify_depth(depth));
        if let Some(map_keys) = map_keys {
            val.meta_mut().map_keys = Some(map_keys);
        }
        val
    }
    /// `deduplicate` the rows of the value
    pub fn deduplicate(&mut self, env: &Uiua) -> UiuaResult {
        val_as_arr!(self, |a| a.deduplicate(env))
    }
    /// Mask the `unique` rows of the value
    pub fn unique(&self) -> Self {
        val_as_arr!(self, Array::unique).into()
    }
    /// Count the unique rows of the value
    pub fn count_unique(&self) -> usize {
        val_as_arr!(self, Array::count_unique)
    }
    /// Check that all values are true
    pub fn all_true(&self) -> bool {
        match self {
            Value::Num(arr) => arr.data.iter().all(|&n| n == 1.0),
            Value::Byte(arr) => arr.data.iter().all(|&b| b == 1),
            Value::Char(_) => false,
            Value::Box(arr) => arr.data.iter().all(|Boxed(val)| val.all_true()),
            Value::Complex(arr) => arr.data.iter().all(|&c| c.re == 1.0 && c.im == 1.0),
        }
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get the `rise` of the array
    pub fn rise(&self) -> Array<f64> {
        if self.rank() == 0 {
            return Array::scalar(0.0);
        }
        if self.row_count() == 0 {
            return Array::default();
        }
        let mut indices = (0..self.row_count())
            .map(|i| i as f64)
            .collect::<EcoVec<_>>();
        indices.make_mut().par_sort_by(|&a, &b| {
            self.row_slice(a as usize)
                .iter()
                .zip(self.row_slice(b as usize))
                .map(|(a, b)| a.array_cmp(b))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices.into()
    }
    pub(crate) fn rise_indices(&self) -> Vec<usize> {
        if self.rank() == 0 {
            return vec![0];
        }
        if self.row_count() == 0 {
            return Vec::new();
        }
        let mut indices: Vec<usize> = (0..self.row_count()).collect();
        indices.par_sort_by(|&a, &b| {
            self.row_slice(a)
                .iter()
                .zip(self.row_slice(b))
                .map(|(a, b)| a.array_cmp(b))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices
    }
    /// Get the `fall` of the array
    pub fn fall(&self) -> Array<f64> {
        if self.rank() == 0 {
            return Array::scalar(0.0);
        }
        if self.row_count() == 0 {
            return Array::default();
        }
        let mut indices = (0..self.row_count())
            .map(|i| i as f64)
            .collect::<EcoVec<_>>();
        indices.make_mut().par_sort_by(|&a, &b| {
            self.row_slice(a as usize)
                .iter()
                .zip(self.row_slice(b as usize))
                .map(|(a, b)| b.array_cmp(a))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices.into()
    }
    pub(crate) fn fall_indices(&self) -> Vec<usize> {
        if self.rank() == 0 {
            return vec![0];
        }
        if self.row_count() == 0 {
            return Vec::new();
        }
        let mut indices: Vec<usize> = (0..self.row_count()).collect();
        indices.par_sort_by(|&a, &b| {
            self.row_slice(a)
                .iter()
                .zip(self.row_slice(b))
                .map(|(a, b)| b.array_cmp(a))
                .find(|x| x != &Ordering::Equal)
                .unwrap_or(Ordering::Equal)
        });
        indices
    }
    /// Sort an array ascending
    pub fn sort_up(&mut self) {
        self.sort_up_depth(0);
    }
    /// Sort an array descending
    pub fn sort_down(&mut self) {
        self.sort_down_depth(0);
    }
    pub(crate) fn sort_up_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        if self.rank() == depth || self.element_count() == 0 {
            return;
        }
        if let Some(Some(keys)) = (depth == 0).then(|| self.take_map_keys()) {
            let keys = keys.normalized();
            let rise = self.rise_indices();
            self.sort_up();
            let new_keys = Value::from_row_values_infallible(
                rise.into_iter().map(|i| keys.row(i)).collect::<Vec<_>>(),
            );
            self.map(new_keys, &()).unwrap();
            return;
        }
        let chunk_len: usize = self.shape[depth..].iter().product();
        let subrow_len: usize = self.shape[depth + 1..].iter().product();
        if chunk_len == 0 || subrow_len == 0 {
            return;
        }
        let is_list = self.rank() == depth + 1;
        let mut new_chunk = Vec::with_capacity(chunk_len);
        let mut indices = Vec::with_capacity(chunk_len / subrow_len);
        for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
            if is_list {
                chunk.par_sort_by(T::array_cmp);
            } else {
                indices.extend(0..chunk.len() / subrow_len);
                indices.par_sort_by(|&a, &b| {
                    chunk[a * subrow_len..(a + 1) * subrow_len]
                        .iter()
                        .zip(&chunk[b * subrow_len..(b + 1) * subrow_len])
                        .map(|(a, b)| a.array_cmp(b))
                        .find(|x| x != &Ordering::Equal)
                        .unwrap_or(Ordering::Equal)
                });
                new_chunk.clear();
                for i in indices.drain(..) {
                    new_chunk.extend_from_slice(&chunk[i * subrow_len..(i + 1) * subrow_len]);
                }
                chunk.clone_from_slice(&new_chunk);
            }
        }
    }
    pub(crate) fn sort_down_depth(&mut self, depth: usize) {
        let depth = depth.min(self.rank());
        if self.rank() == depth || self.element_count() == 0 {
            return;
        }
        if let Some(Some(keys)) = (depth == 0).then(|| self.take_map_keys()) {
            let keys = keys.normalized();
            let fall = self.fall_indices();
            self.sort_down();
            let new_keys = Value::from_row_values_infallible(
                fall.into_iter().map(|i| keys.row(i)).collect::<Vec<_>>(),
            );
            self.map(new_keys, &()).unwrap();
            return;
        }
        let chunk_len: usize = self.shape[depth..].iter().product();
        let subrow_len: usize = self.shape[depth + 1..].iter().product();
        if chunk_len == 0 || subrow_len == 0 {
            return;
        }
        let is_list = self.rank() == depth + 1;
        let mut new_chunk = Vec::with_capacity(chunk_len);
        let mut indices = Vec::with_capacity(chunk_len / subrow_len);
        for chunk in self.data.as_mut_slice().chunks_exact_mut(chunk_len) {
            if is_list {
                chunk.par_sort_by(|a, b| b.array_cmp(a));
            } else {
                indices.extend(0..chunk.len() / subrow_len);
                indices.par_sort_by(|&a, &b| {
                    chunk[a * subrow_len..(a + 1) * subrow_len]
                        .iter()
                        .zip(&chunk[b * subrow_len..(b + 1) * subrow_len])
                        .map(|(a, b)| b.array_cmp(a))
                        .find(|x| x != &Ordering::Equal)
                        .unwrap_or(Ordering::Equal)
                });
                new_chunk.clear();
                for i in indices.drain(..) {
                    new_chunk.extend_from_slice(&chunk[i * subrow_len..(i + 1) * subrow_len]);
                }
                chunk.clone_from_slice(&new_chunk);
            }
        }
    }
    /// `classify` the rows of the array
    pub fn classify(&self) -> Vec<usize> {
        let mut classes = HashMap::new();
        let mut classified = Vec::with_capacity(self.row_count());
        for row in self.row_slices() {
            let new_class = classes.len();
            let class = *classes.entry(ArrayCmpSlice(row)).or_insert(new_class);
            classified.push(class);
        }
        classified
    }
    fn classify_depth(&self, mut depth: usize) -> Value {
        if self.rank() == 0 {
            return 0.into();
        }
        depth = depth.min(self.rank());
        let row_shape = Shape::from(&self.shape[depth..]);
        let mut classes = HashMap::new();
        let row_row_count = row_shape.row_count();
        let classified_shape = Shape::from(&self.shape[..=depth.min(self.rank() - 1)]);
        let mut i = 0;
        if row_row_count < 256 {
            // Fits in a u8
            let mut classified = eco_vec![0u8; classified_shape.elements()];
            if row_shape.elements() == 0 || row_shape.row_len() == 0 {
                return Array::new(classified_shape, classified).into();
            }
            let classified_slice = classified.make_mut();
            for row in self.data.chunks_exact(row_shape.elements()) {
                classes.clear();
                for row in row.chunks_exact(row_shape.row_len()) {
                    let new_class = classes.len();
                    let class = *classes.entry(ArrayCmpSlice(row)).or_insert(new_class);
                    classified_slice[i] = class as u8;
                    i += 1;
                }
            }
            Array::new(classified_shape, classified).into()
        } else {
            // Doesn't fit in a u8
            let mut classified = eco_vec![0.0; classified_shape.elements()];
            if row_shape.elements() == 0 || row_shape.row_len() == 0 {
                return Array::new(classified_shape, classified).into();
            }
            let classified_slice = classified.make_mut();
            for row in self.data.chunks_exact(row_shape.elements()) {
                classes.clear();
                for row in row.chunks_exact(row_shape.row_len()) {
                    let new_class = classes.len();
                    let class = *classes.entry(ArrayCmpSlice(row)).or_insert(new_class);
                    classified_slice[i] = class as f64;
                    i += 1;
                }
            }
            Array::new(classified_shape, classified).into()
        }
    }
    /// `deduplicate` the rows of the array
    pub fn deduplicate(&mut self, env: &Uiua) -> UiuaResult {
        if self.rank() == 0 {
            return Ok(());
        }
        let map_keys_unique = self
            .take_map_keys()
            .map(|keys| (keys.normalized(), self.unique()));
        let mut deduped = CowSlice::new();
        let mut seen = HashSet::new();
        let mut new_len = 0;
        for row in self.row_slices() {
            if seen.insert(ArrayCmpSlice(row)) {
                deduped.extend_from_slice(row);
                new_len += 1;
            }
        }
        self.data = deduped;
        self.shape[0] = new_len;
        if let Some((keys, unique)) = map_keys_unique {
            let keys = Value::from(unique).keep(keys, env)?;
            self.map(keys, env)?;
        }
        Ok(())
    }
    /// Mask the `unique` rows of the array
    pub fn unique(&self) -> Array<u8> {
        if self.rank() == 0 {
            return 1u8.into();
        }
        let map_keys = self.map_keys().cloned();
        let mut seen = HashSet::new();
        let mut mask = eco_vec![0u8; self.row_count()];
        let mask_slice = mask.make_mut();
        for (i, row) in self.row_slices().enumerate() {
            if seen.insert(ArrayCmpSlice(row)) {
                mask_slice[i] = 1;
            }
        }
        let mut arr = Array::new([self.row_count()], mask);
        arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
        arr.meta_mut().map_keys = map_keys;
        arr
    }
    /// Count the number of unique rows in the array
    pub fn count_unique(&self) -> usize {
        let mut seen = HashSet::new();
        self.row_slices()
            .filter(|row| seen.insert(ArrayCmpSlice(row)))
            .count()
    }
}

impl Value {
    /// Encode the `bits` of the value
    pub fn bits(&self, env: &Uiua) -> UiuaResult<Value> {
        match self {
            Value::Byte(n) => n.bits(env),
            Value::Num(n) => n.bits(env),
            _ => Err(env.error("Argument to bits must be an array of natural numbers")),
        }
    }
    /// Decode the `bits` of the value
    pub fn unbits(&self, env: &Uiua) -> UiuaResult<Value> {
        match self {
            Value::Byte(n) => n.un_bits(env),
            Value::Num(n) => n.un_bits(env),
            _ => Err(env.error("Argument to un bits must be an array of integers")),
        }
    }
    pub(crate) fn undo_un_bits(&self, orig_shape: &Self, env: &Uiua) -> UiuaResult<Value> {
        let min_bits_len = orig_shape
            .as_nats(env, "Shape must be an array of natural numbers")?
            .pop()
            .unwrap_or(0);
        match self {
            Value::Byte(n) => n.bits_impl(min_bits_len, env),
            Value::Num(n) => n.bits_impl(min_bits_len, env),
            _ => Err(env.error("Argument to undo un bits must be an array of integers")),
        }
    }
}

impl<T: RealArrayValue> Array<T> {
    /// Encode the `bits` of the array
    pub fn bits(&self, env: &Uiua) -> UiuaResult<Value> {
        self.bits_impl(0, env)
    }
    fn bits_impl(&self, min_bits_len: usize, env: &Uiua) -> UiuaResult<Value> {
        let mut nats = Vec::with_capacity(self.data.len());
        let mut negatives = Vec::with_capacity(self.data.len());
        let mut any_neg = false;
        for &n in &self.data {
            if !n.is_int() {
                return Err(env.error(format!(
                    "Array must be a list of integers, but {n} is not an integer"
                )));
            }
            let n = n.to_f64();
            if n.abs() > u128::MAX as f64 {
                return Err(env.error(format!(
                    "{n} is too large for the {} algorithm",
                    Primitive::Bits.format()
                )));
            }
            nats.push(n.abs().round() as u128);
            negatives.push(n < 0.0);
            any_neg |= n < 0.0;
        }
        let mut max = if let Some(max) = nats.iter().max() {
            *max
        } else {
            let mut shape = self.shape.clone();
            shape.push(0);
            return Ok(Array::<u8>::new(shape, CowSlice::new()).into());
        };
        let mut max_bits = 0;
        while max != 0 {
            max_bits += 1;
            max >>= 1;
        }
        max_bits = max_bits.max(min_bits_len);
        let mut shape = self.shape.clone();
        shape.push(max_bits);
        let val: Value = if any_neg {
            // If any number is negative, make a f64 array
            let mut new_data = eco_vec![0.0; self.data.len() * max_bits];
            let new_data_slice = new_data.make_mut();
            // LSB first
            for (i, (n, is_neg)) in nats.into_iter().zip(negatives).enumerate() {
                for j in 0..max_bits {
                    let index = i * max_bits + j;
                    new_data_slice[index] = u8::from(n & (1 << j) != 0) as f64;
                    if is_neg {
                        new_data_slice[index] = -new_data_slice[index];
                    }
                }
            }
            Array::new(shape, new_data).into()
        } else {
            // If all numbers are natural, make a u8 array
            let mut new_data = eco_vec![0; self.data.len() * max_bits];
            let new_data_slice = new_data.make_mut();
            // LSB first
            for (i, n) in nats.into_iter().enumerate() {
                for j in 0..max_bits {
                    let index = i * max_bits + j;
                    new_data_slice[index] = u8::from(n & (1 << j) != 0);
                }
            }
            let mut arr = Array::new(shape, new_data);
            arr.meta_mut().flags.set(ArrayFlags::BOOLEAN, true);
            arr.into()
        };
        val.validate_shape();
        Ok(val)
    }
}

impl<T> Array<T>
where
    T: RealArrayValue,
    Array<T>: Into<Value>,
{
    /// Decode the `bits` of the array
    pub fn un_bits(&self, env: &Uiua) -> UiuaResult<Value> {
        for &n in &self.data {
            if !n.is_int() {
                return Err(env.error(format!(
                    "Array must be a list of integers, but {n} is not an integer"
                )));
            }
        }
        if self.rank() == 0 {
            return Ok(self.clone().into());
        }
        let mut shape = self.shape.clone();
        let bits_slice_len = shape.pop().unwrap();
        let elems = shape.elements();
        if bits_slice_len == 0 {
            return Ok(Array::<u8>::new(shape, eco_vec![0; elems]).into());
        }
        let mut new_data = eco_vec![0.0; elems];
        let new_data_slice = new_data.make_mut();
        // LSB first
        for (i, bits) in self.data.chunks_exact(bits_slice_len).enumerate() {
            let mut n = 0.0;
            for (j, bit) in bits.iter().enumerate() {
                n += bit.to_f64() * 2.0f64.powi(j as i32);
            }
            new_data_slice[i] = n;
        }
        Ok(Array::new(shape, new_data).into())
    }
}

impl Value {
    /// Get the indices `where` the value is nonzero
    pub fn wher(&self, env: &Uiua) -> UiuaResult<Value> {
        let counts =
            self.as_natural_array(env, "Argument to where must be an array of naturals")?;
        let total: usize = counts.data.iter().fold(0, |acc, &b| acc.saturating_add(b));
        Ok(match self.rank() {
            0 => {
                validate_size::<u8>([total], env)?;
                let data = eco_vec![0u8; total];
                Array::new([total], data).into()
            }
            1 => {
                validate_size::<f64>([total], env)?;
                let mut data = EcoVec::with_capacity(total);
                for (i, &b) in counts.data.iter().enumerate() {
                    for _ in 0..b {
                        let i = i as f64;
                        data.push(i);
                    }
                }
                Array::from(data).into()
            }
            _ => {
                validate_size::<f64>([total, counts.rank()], env)?;
                let mut data = EcoVec::with_capacity(total * counts.rank());
                for (i, &b) in counts.data.iter().enumerate() {
                    for _ in 0..b {
                        let mut i = i;
                        let start = data.len();
                        for &d in counts.shape.iter().rev() {
                            data.insert(start, (i % d) as f64);
                            i /= d;
                        }
                    }
                }
                let shape = Shape::from([total, counts.rank()].as_ref());
                Array::new(shape, data).into()
            }
        })
    }
    /// Get the `first` index `where` the value is nonzero
    pub fn first_where(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        self.first_where_impl(env, identity, identity)
    }
    /// Get the last index `where` the value is nonzero
    pub fn last_where(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        self.first_where_impl(env, Iterator::rev, Iterator::rev)
    }
    fn first_where_impl<'a, B, N>(
        &'a self,
        env: &Uiua,
        byte_iter: impl Fn(iter::Enumerate<slice::Iter<'a, u8>>) -> B,
        num_iter: impl Fn(iter::Enumerate<slice::Iter<'a, f64>>) -> N,
    ) -> UiuaResult<Array<f64>>
    where
        B: Iterator<Item = (usize, &'a u8)>,
        N: Iterator<Item = (usize, &'a f64)>,
    {
        if self.rank() <= 1 {
            match self {
                Value::Num(nums) => {
                    for (i, n) in num_iter(nums.data.iter().enumerate()) {
                        if n.fract() != 0.0 || *n < 0.0 {
                            return Err(env.error("Argument to where must be an array of naturals"));
                        }
                        if *n != 0.0 {
                            return Ok(Array::scalar(i as f64));
                        }
                    }
                    env.scalar_fill::<f64>()
                        .map(Array::scalar)
                        .map_err(|e| env.error(format!("Cannot take first of an empty array{e}")))
                }
                Value::Byte(bytes) => {
                    for (i, n) in byte_iter(bytes.data.iter().enumerate()) {
                        if *n != 0 {
                            return Ok(Array::scalar(i as f64));
                        }
                    }
                    env.scalar_fill::<f64>()
                        .map(Array::scalar)
                        .map_err(|e| env.error(format!("Cannot take first of an empty array{e}")))
                }
                value => Err(env.error(format!(
                    "Argument to where must be an array of naturals, but it is {}",
                    value.type_name_plural()
                ))),
            }
        } else {
            match self {
                Value::Num(nums) => {
                    for (i, n) in num_iter(nums.data.iter().enumerate()) {
                        if n.fract() != 0.0 || *n < 0.0 {
                            return Err(env.error("Argument to where must be an array of naturals"));
                        }
                        if *n != 0.0 {
                            let mut i = i;
                            let mut res = Vec::with_capacity(nums.rank());
                            for &d in nums.shape.iter().rev() {
                                res.insert(0, (i % d) as f64);
                                i /= d;
                            }
                            return Ok(Array::from_iter(res));
                        }
                    }
                    env.scalar_fill::<f64>()
                        .map(Array::scalar)
                        .map_err(|e| env.error(format!("Cannot take first of an empty array{e}")))
                }
                Value::Byte(bytes) => {
                    for (i, n) in byte_iter(bytes.data.iter().enumerate()) {
                        if *n != 0 {
                            let mut i = i;
                            let mut res = Vec::with_capacity(bytes.rank());
                            for &d in bytes.shape.iter().rev() {
                                res.insert(0, (i % d) as f64);
                                i /= d;
                            }
                            return Ok(Array::from_iter(res));
                        }
                    }
                    env.scalar_fill::<f64>()
                        .map(Array::scalar)
                        .map_err(|e| env.error(format!("Cannot take first of an empty array{e}")))
                }
                value => Err(env.error(format!(
                    "Argument to where must be an array of naturals, but it is {}",
                    value.type_name_plural()
                ))),
            }
        }
    }
    pub(crate) fn len_where(&self, env: &Uiua) -> UiuaResult<f64> {
        match self {
            Value::Num(nums) => {
                let mut len = 0.0;
                let mut ok = true;
                for &n in &nums.data {
                    if n.fract() != 0.0 || n < 0.0 {
                        ok = false;
                        break;
                    }
                    len += n;
                }
                ok.then_some(len)
                    .ok_or_else(|| env.error("Argument to where must be an array of naturals"))
            }
            Value::Byte(bytes) => {
                // `abs` in needed to fix some weird behavior on WASM
                Ok(bytes.data.iter().map(|&n| n as f64).sum::<f64>().abs())
            }
            value => Err(env.error(format!(
                "Argument to where must be an array of naturals, but it is {}",
                value.type_name_plural()
            ))),
        }
    }
    /// `un` `where`
    pub fn unwhere(&self, env: &Uiua) -> UiuaResult<Self> {
        self.unwhere_impl(&[], env)
    }
    fn unwhere_impl(&self, min_size: &[usize], env: &Uiua) -> UiuaResult<Self> {
        const INDICES_ERROR: &str = "Argument to ° un ⊚ where must be an array of naturals";
        Ok(match self.shape().dims() {
            [] | [_] => {
                let indices = self.as_nats(env, INDICES_ERROR)?;
                let is_sorted = indices
                    .iter()
                    .zip(indices.iter().skip(1))
                    .all(|(&a, &b)| a <= b);
                let mut size = indices.iter().max().map(|&i| i + 1).unwrap_or(0);
                if let Some(&min) = min_size.first() {
                    size = size.max(min);
                }
                validate_size::<f64>([size], env)?;
                let mut data = eco_vec![0.0; size];
                let data_slice = data.make_mut();
                if is_sorted {
                    let mut j = 0;
                    for i in 0..size {
                        while indices.get(j).is_some_and(|&n| n < i) {
                            j += 1;
                        }
                        let mut count: usize = 0;
                        while indices.get(j).copied() == Some(i) {
                            j += 1;
                            count += 1;
                        }
                        data_slice[i] = count as f64;
                    }
                } else {
                    let mut counts = HashMap::new();
                    for &i in &indices {
                        *counts.entry(i).or_insert(0) += 1;
                    }
                    for i in 0..size {
                        data_slice[i] = counts.get(&i).copied().unwrap_or(0) as f64;
                    }
                }
                Array::from(data).into()
            }
            [_, trailing] => {
                let indices = self.as_natural_array(env, INDICES_ERROR)?;
                let mut counts: HashMap<&[usize], usize> = HashMap::new();
                for row in indices.row_slices() {
                    *counts.entry(row).or_default() += 1;
                }
                let mut shape = Shape::with_capacity(*trailing);
                for _ in 0..*trailing {
                    shape.push(0);
                }
                for row in counts.keys() {
                    for (a, r) in shape.iter_mut().zip(*row) {
                        *a = (*a).max(*r + 1);
                    }
                }
                for (s, &m) in shape.iter_mut().zip(min_size) {
                    *s = (*s).max(m);
                }
                if counts.values().all(|&n| n < 256) {
                    let data_len = validate_size::<u8>(shape.iter().copied(), env)?;
                    let mut data = eco_vec![0u8; data_len];
                    let data_slice = data.make_mut();
                    for (key, count) in counts {
                        let mut i = 0;
                        let mut row_len = 1;
                        for (d, &n) in shape.iter().zip(key).rev() {
                            i += n * row_len;
                            row_len *= d;
                        }
                        data_slice[i] = count as u8;
                    }
                    Array::new(shape, data).into()
                } else {
                    let data_len = validate_size::<f64>(shape.iter().copied(), env)?;
                    let mut data = eco_vec![0.0; data_len];
                    let data_slice = data.make_mut();
                    for (key, count) in counts {
                        let mut i = 0;
                        let mut row_len = 1;
                        for (d, &n) in shape.iter().zip(key).rev() {
                            i += n * row_len;
                            row_len *= d;
                        }
                        data_slice[i] = count as f64;
                    }
                    Array::new(shape, data).into()
                }
            }
            shape => return Err(env.error(format!("Cannot unwhere rank-{} array", shape.len()))),
        })
    }
    pub(crate) fn undo_where(&self, shape: &[usize], env: &Uiua) -> UiuaResult<Self> {
        self.unwhere_impl(shape, env)
    }
    pub(crate) fn all_same(&self) -> bool {
        if self.row_count() <= 1 {
            return true;
        }
        val_as_arr!(self, |arr| {
            if arr.rank() == 1 {
                return arr.data.windows(2).all(|win| win[1].array_eq(&win[0]));
            }
            let row_len = arr.row_len();
            arr.data.windows(2 * row_len).all(|win| {
                win.iter()
                    .zip(win[row_len..].iter())
                    .all(|(a, b)| a.array_eq(b))
            })
        })
    }
}

impl Value {
    /// Convert a string value to a list of UTF-8 bytes
    pub fn utf8(&self, env: &Uiua) -> UiuaResult<Self> {
        let s = self.as_string(env, "Argument to utf₈ must be a string")?;
        Ok(Array::<u8>::from_iter(s.into_bytes()).into())
    }
    /// Convert a string value to a list of UTF-16 code units
    pub fn utf16(&self, env: &Uiua) -> UiuaResult<Self> {
        let s = self.as_string(env, "Argument to utf₁₆ must be a string")?;
        Ok(Array::<f64>::from_iter(s.encode_utf16().map(|u| u as f64)).into())
    }
    /// Convert a list of UTF-8 bytes to a string value
    pub fn unutf8(&self, env: &Uiua) -> UiuaResult<Self> {
        let bytes = self.as_bytes(env, "Argument to °utf₈ must be a list of bytes")?;
        let s = String::from_utf8(bytes).map_err(|e| env.error(e))?;
        Ok(s.into())
    }
    /// Convert a list of UTF-16 code units to a string value
    pub fn unutf16(&self, env: &Uiua) -> UiuaResult<Self> {
        let code_units = self.as_u16s(env, "Argument to °utf₁₆ must be a list of code units")?;
        let s = String::from_utf16(&code_units).map_err(|e| env.error(e))?;
        Ok(s.into())
    }
    /// Convert a string value to a list of boxed UTF-8 grapheme clusters
    pub fn graphemes(&self, env: &Uiua) -> UiuaResult<Self> {
        let s = self.as_string(env, "Argument to graphemes must be a string")?;
        let mut data = EcoVec::new();
        for grapheme in s.graphemes(true) {
            data.push(Boxed(grapheme.into()));
        }
        Ok(Array::from(data).into())
    }
    /// Convert a list of boxed UTF-8 grapheme clusters to a string value
    pub fn ungraphemes(self, env: &Uiua) -> UiuaResult<Self> {
        let mut data = EcoVec::new();
        for val in self.into_rows().map(Value::unboxed) {
            let s = val.as_string(env, "Argument to ungraphemes must be a list of strings")?;
            data.extend(s.chars());
        }
        Ok(Array::from(data).into())
    }
}

impl Value {
    pub(crate) fn first_min_index(&self, env: &Uiua) -> UiuaResult<Self> {
        val_as_arr!(self, env, Array::first_min_index).map(Into::into)
    }
    pub(crate) fn first_max_index(&self, env: &Uiua) -> UiuaResult<Self> {
        val_as_arr!(self, env, Array::first_max_index).map(Into::into)
    }
    pub(crate) fn last_min_index(&self, env: &Uiua) -> UiuaResult<Self> {
        val_as_arr!(self, env, Array::last_min_index).map(Into::into)
    }
    pub(crate) fn last_max_index(&self, env: &Uiua) -> UiuaResult<Self> {
        val_as_arr!(self, env, Array::last_max_index).map(Into::into)
    }
}

impl<T: ArrayValue> Array<T> {
    pub(crate) fn first_min_index(&self, env: &Uiua) -> UiuaResult<f64> {
        if self.rank() == 0 {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return env
                .scalar_fill::<f64>()
                .map_err(|e| env.error(format!("Cannot get min index of an empty array{e}")));
        }
        let index = self
            .row_slices()
            .map(ArrayCmpSlice)
            .enumerate()
            .min_by(|(_, a), (_, b)| a.cmp(b))
            .unwrap()
            .0;
        Ok(index as f64)
    }
    pub(crate) fn first_max_index(&self, env: &Uiua) -> UiuaResult<f64> {
        if self.rank() == 0 {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return env
                .scalar_fill::<f64>()
                .map_err(|e| env.error(format!("Cannot get max index of an empty array{e}")));
        }
        let index = self
            .row_slices()
            .map(ArrayCmpSlice)
            .enumerate()
            .min_by(|(_, a), (_, b)| a.cmp(b).reverse())
            .unwrap()
            .0;
        Ok(index as f64)
    }
    pub(crate) fn last_min_index(&self, env: &Uiua) -> UiuaResult<f64> {
        if self.rank() == 0 {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return env
                .scalar_fill::<f64>()
                .map_err(|e| env.error(format!("Cannot get min index of an empty array{e}")));
        }
        let index = self
            .row_slices()
            .map(ArrayCmpSlice)
            .enumerate()
            .max_by(|(_, a), (_, b)| a.cmp(b).reverse())
            .unwrap()
            .0;
        Ok(index as f64)
    }
    pub(crate) fn last_max_index(&self, env: &Uiua) -> UiuaResult<f64> {
        if self.rank() == 0 {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return env
                .scalar_fill::<f64>()
                .map_err(|e| env.error(format!("Cannot get max index of an empty array{e}")));
        }
        let index = self
            .row_slices()
            .map(ArrayCmpSlice)
            .enumerate()
            .max_by(|(_, a), (_, b)| a.cmp(b))
            .unwrap()
            .0;
        Ok(index as f64)
    }
}

impl Value {
    pub(crate) fn primes(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        match self {
            Value::Num(n) => n.primes(env),
            Value::Byte(b) => b.convert_ref::<f64>().primes(env),
            value => Err(env.error(format!("Cannot get primes of {} array", value.type_name()))),
        }
    }
}

impl Array<f64> {
    pub(crate) fn primes(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        let mut max = 0;
        // Validate nums and calc max
        for &n in &self.data {
            if n <= 0.0 {
                return Err(env.error(format!(
                    "Cannot get primes of non-positive number {}",
                    n.grid_string(true)
                )));
            }
            if n.fract() != 0.0 {
                return Err(env.error(format!(
                    "Cannot get primes of non-integer number {}",
                    n.grid_string(true)
                )));
            }
            if n > usize::MAX as f64 {
                return Err(env.error(format!(
                    "Cannot get primes of {} because it is too large",
                    n.grid_string(true)
                )));
            }
            max = max.max(n as usize);
        }

        if self.element_count() == 1 {
            let mut n = self.data[0] as usize;
            let mut primes = EcoVec::new();
            for d in 2..=self.data[0].sqrt().ceil() as usize {
                while n % d == 0 {
                    primes.push(d as f64);
                    n /= d;
                }
            }
            if n > 1 {
                primes.push(n as f64);
            }
            let mut shape = self.shape.clone();
            shape.insert(0, primes.len());
            return Ok(Array::new(shape, primes));
        }

        validate_size::<usize>([max, 2], env)?;

        // Sieve of Eratosthenes
        let mut spf = vec![1; max + 1];
        for i in 2..=max {
            spf[i] = i;
        }
        for i in 2..=max {
            if spf[i] == i {
                let (ii, overflow) = i.overflowing_mul(i);
                if !overflow && ii <= max {
                    for j in (ii..=max).step_by(i) {
                        if spf[j] == j {
                            spf[j] = i;
                        }
                    }
                }
            }
        }

        // Factorize
        let mut lengths: Vec<usize> = Vec::with_capacity(self.data.len());
        let mut factors: Vec<usize> = Vec::with_capacity(self.data.len());
        for &n in &self.data {
            let mut m = n as usize;
            if m == 1 {
                lengths.push(0);
                continue;
            }
            let mut len = 0;
            while m != 1 {
                factors.push(spf[m]);
                m /= spf[m];
                len += 1;
            }
            lengths.push(len);
        }

        // Pad and create array
        let longest = lengths.iter().max().copied().unwrap_or(0);
        let mut data = eco_vec![1.0; self.data.len() * longest];
        let data_slice = data.make_mut();
        let mut k = 0;
        for (i, len) in lengths.into_iter().enumerate() {
            for j in (longest - len)..longest {
                data_slice[j * self.data.len() + i] = factors[k] as f64;
                k += 1;
            }
        }
        let mut shape = self.shape.clone();
        shape.insert(0, longest);
        Ok(Array::new(shape, data))
    }
}

impl Value {
    pub(crate) fn to_json_string(&self, env: &Uiua) -> UiuaResult<String> {
        let json = self.to_json_value(env)?;
        serde_json::to_string(&json).map_err(|e| env.error(e))
    }
    pub(crate) fn to_json_value(&self, env: &Uiua) -> UiuaResult<serde_json::Value> {
        Ok(match self {
            Value::Num(n) if n.rank() == 0 => {
                let n = n.data[0];
                if n.fract() == 0.0 && n.abs() < i64::MAX as f64 {
                    serde_json::Value::Number((n as i64).into())
                } else {
                    serde_json::Number::from_f64(n)
                        .map(Into::into)
                        .unwrap_or(serde_json::Value::Null)
                }
            }
            Value::Byte(bytes) if bytes.rank() == 0 => {
                let b = bytes.data[0];
                if bytes.meta().flags.contains(ArrayFlags::BOOLEAN_LITERAL) {
                    serde_json::Value::Bool(b != 0)
                } else {
                    serde_json::Value::Number(b.into())
                }
            }
            Value::Complex(_) => return Err(env.error("Cannot convert complex numbers to JSON")),
            Value::Char(c) if c.rank() == 0 => serde_json::Value::String(c.data[0].to_string()),
            Value::Char(c) if c.rank() == 1 => serde_json::Value::String(c.data.iter().collect()),
            Value::Box(b) if b.rank() == 0 => b.data[0].0.to_json_value(env)?,
            value => {
                if value.is_map() {
                    let mut map = serde_json::Map::with_capacity(value.row_count());
                    for (k, v) in value.map_kv() {
                        let k = k.as_string(env, "JSON map keys must be strings")?;
                        let v = v.to_json_value(env)?;
                        map.insert(k, v);
                    }
                    serde_json::Value::Object(map)
                } else {
                    serde_json::Value::Array(
                        value
                            .rows()
                            .map(|row| row.to_json_value(env))
                            .collect::<Result<_, _>>()?,
                    )
                }
            }
        })
    }
    pub(crate) fn from_json_string(json: &str, env: &Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "json5"))]
        let json_value: serde_json::Value = serde_json::from_str(json).map_err(|e| env.error(e))?;
        #[cfg(feature = "json5")]
        let json_value: serde_json::Value = json5::from_str(json).map_err(|e| env.error(e))?;
        Self::from_json_value(json_value, env)
    }
    pub(crate) fn from_json_value(json_value: serde_json::Value, _env: &Uiua) -> UiuaResult<Self> {
        Ok(match json_value {
            serde_json::Value::Null => f64::NAN.into(),
            serde_json::Value::Bool(b) => b.into(),
            serde_json::Value::Number(n) => {
                if let Some(n) = n.as_f64() {
                    if n >= 0.0 && n.fract() == 0.0 && n < u8::MAX as f64 {
                        (n as u8).into()
                    } else {
                        n.into()
                    }
                } else {
                    0.0.into()
                }
            }
            serde_json::Value::String(s) => s.into(),
            serde_json::Value::Array(arr) => {
                let mut rows = Vec::with_capacity(arr.len());
                for value in arr {
                    let mut value = Value::from_json_value(value, _env)?;
                    if value.map_keys().is_some() {
                        value = Boxed(value).into();
                    }
                    rows.push(value);
                }
                if rows.iter().all(|val| val.shape().is_empty())
                    && rows
                        .windows(2)
                        .all(|win| win[0].type_id() == win[1].type_id())
                {
                    Value::from_row_values_infallible(rows)
                } else {
                    Array::from(
                        rows.into_iter()
                            .map(Value::boxed_if_not)
                            .collect::<EcoVec<_>>(),
                    )
                    .into()
                }
            }
            serde_json::Value::Object(map) => {
                let mut keys = EcoVec::with_capacity(map.len());
                let mut values = Vec::with_capacity(map.len());
                for (k, v) in map {
                    keys.push(Boxed(k.into()));
                    let mut value = Value::from_json_value(v, _env)?;
                    if value.map_keys().is_some() {
                        value = Boxed(value).into();
                    }
                    values.push(value);
                }
                let mut values = if values.windows(2).all(|win| {
                    win[0].shape() == win[1].shape() && win[0].type_name() == win[1].type_name()
                }) {
                    Value::from_row_values_infallible(values)
                } else {
                    Array::from(values.into_iter().map(Boxed).collect::<EcoVec<_>>()).into()
                };
                values.map(keys.into(), _env)?;
                values
            }
        })
    }
}

impl Value {
    pub(crate) fn to_csv(&self, env: &Uiua) -> UiuaResult<String> {
        #[cfg(not(feature = "csv"))]
        return Err(env.error("CSV support is not enabled in this environment"));
        #[cfg(feature = "csv")]
        {
            let delimiter = u8::try_from(env.scalar_fill::<char>().unwrap_or(','))
                .map_err(|_| env.error("CSV delimiter must be ASCII"))?;

            let mut buf = Vec::new();
            let mut writer = csv::WriterBuilder::new()
                .flexible(true)
                .delimiter(delimiter)
                .from_writer(&mut buf);

            match self.rank() {
                0 => writer
                    .write_record([self.format()])
                    .map_err(|e| env.error(e))?,
                1 => {
                    for row in self.rows() {
                        writer
                            .write_record(row.unboxed().rows().map(|v| v.format()))
                            .map_err(|e| env.error(e))?;
                    }
                }
                2 => {
                    for row in self.rows() {
                        writer
                            .write_record(row.rows().map(|v| v.format()))
                            .map_err(|e| env.error(e))?;
                    }
                }
                n => return Err(env.error(format!("Cannot write a rank-{n} array to CSV"))),
            }
            writer.flush().map_err(|e| env.error(e))?;
            drop(writer);
            let s = String::from_utf8(buf).map_err(|e| env.error(e))?;
            Ok(s)
        }
    }
    pub(crate) fn to_xlsx(&self, env: &Uiua) -> UiuaResult<Vec<u8>> {
        #[cfg(not(feature = "simple_excel_writer"))]
        return Err(env.error("XLSX encoding is not enabled in this environment"));
        #[cfg(feature = "simple_excel_writer")]
        {
            use simple_excel_writer::*;
            if self.rank() > 3 {
                return Err(env.error(format!(
                    "Cannot write a rank-{} array to an XLSX workbook",
                    self.rank()
                )));
            }
            let sheet_arrays = if self.is_map() {
                let mut sheet_arrays = Vec::new();
                for (k, v) in self.map_kv() {
                    let name = k.as_string(env, "Sheet name must be a string")?;
                    if v.rank() > 2 {
                        return Err(env.error(format!(
                            "Cannot write a rank-{} array to an XLSX sheet",
                            v.rank()
                        )));
                    }
                    sheet_arrays.push((name, v));
                }
                sheet_arrays
            } else if self.rank() == 3 {
                self.rows()
                    .enumerate()
                    .map(|(i, row)| (format!("Sheet {}", i + 1), row))
                    .collect()
            } else {
                vec![("Sheet1".into(), self.clone())]
            };
            let mut workbook = Workbook::create_in_memory();
            for (sheet_name, value) in sheet_arrays {
                let mut sheet = workbook.create_sheet(&sheet_name);
                workbook
                    .write_sheet(&mut sheet, |writer| {
                        for row in value.unboxed().into_rows() {
                            let mut sheet_row = Row::new();
                            for cell in row.unboxed().into_rows() {
                                match cell {
                                    Value::Num(n) => sheet_row.add_cell(n.data[0]),
                                    Value::Byte(b) => sheet_row.add_cell(b.data[0] as f64),
                                    Value::Char(c) => sheet_row.add_cell(c.data[0].to_string()),
                                    Value::Complex(c) => sheet_row.add_cell(c.data[0].to_string()),
                                    Value::Box(b) => {
                                        let Boxed(b) = &b.data[0];
                                        if b.row_count() == 0 {
                                            sheet_row.add_empty_cells(1);
                                        } else {
                                            sheet_row.add_cell(b.format())
                                        }
                                    }
                                }
                            }
                            writer.append_row(sheet_row)?;
                        }
                        Ok(())
                    })
                    .map_err(|e| env.error(e))?;
            }
            workbook
                .close()
                .map(Option::unwrap)
                .map_err(|e| env.error(e))
        }
    }
    pub(crate) fn from_csv(csv_str: &str, env: &mut Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "csv"))]
        return Err(env.error("CSV support is not enabled in this environment"));
        #[cfg(feature = "csv")]
        {
            let delimiter = u8::try_from(env.scalar_unfill::<char>().unwrap_or(','))
                .map_err(|_| env.error("CSV delimiter must be ASCII"))?;

            let mut reader = csv::ReaderBuilder::new()
                .has_headers(false)
                .flexible(true)
                .delimiter(delimiter)
                .from_reader(csv_str.as_bytes());

            let fill = env.value_fill().cloned().unwrap_or_else(|| "".into());
            env.with_fill(fill, |env| {
                let mut rows = Vec::new();
                for result in reader.records() {
                    let record = result.map_err(|e| env.error(e))?;
                    let mut row = EcoVec::new();
                    for field in record.iter() {
                        row.push(Boxed(field.into()));
                    }
                    rows.push(Array::new(row.len(), row));
                }
                Array::from_row_arrays(rows, env).map(Into::into)
            })
        }
    }
    pub(crate) fn from_xlsx(_xlsx: &[u8], env: &mut Uiua) -> UiuaResult<Self> {
        #[cfg(not(feature = "calamine"))]
        return Err(env.error("XLSX decoding is not enabled in this environment"));
        #[cfg(feature = "calamine")]
        {
            use calamine::*;

            let mut workbook: Xlsx<_> =
                open_workbook_from_rs(std::io::Cursor::new(_xlsx)).map_err(|e| env.error(e))?;
            let sheet_names = workbook.sheet_names();
            let fill = env.value_fill().cloned().unwrap_or_else(|| "".into());
            let mut sheet_values = EcoVec::new();
            env.with_fill(fill, |env| {
                for sheet_name in &sheet_names {
                    let sheet = workbook
                        .worksheet_range(sheet_name)
                        .map_err(|e| env.error(e))?;
                    let mut rows = Vec::new();
                    for row in sheet.rows() {
                        let mut cells = EcoVec::new();
                        for cell in row {
                            cells.push(Boxed(match cell {
                                &Data::Int(i) => i.into(),
                                &Data::Float(f) => f.into(),
                                Data::String(s) => s.clone().into(),
                                &Data::Bool(b) => b.into(),
                                Data::DateTime(dt) => {
                                    ((dt.as_f64() - 2.0) * 24.0 * 60.0 * 60.0 - 2208988800.0).into()
                                }
                                Data::DateTimeIso(dt) => dt.clone().into(),
                                Data::DurationIso(dur) => dur.clone().into(),
                                Data::Error(e) => e.to_string().into(),
                                Data::Empty => String::new().into(),
                            }));
                        }
                        rows.push(Array::from(cells));
                    }
                    sheet_values.push(Boxed(Array::from_row_arrays(rows, env)?.into()));
                }
                Ok(())
            })?;
            let keys: Value = sheet_names.into_iter().map(|s| Boxed(s.into())).collect();
            let mut values: Value = Array::from(sheet_values).into();
            values.map(keys, env)?;
            Ok(values)
        }
    }
}

fn f64_repr(n: f64) -> String {
    let abs = n.abs();
    let pos = if abs == PI / 2.0 {
        "η".into()
    } else if abs == PI {
        "π".into()
    } else if abs == TAU {
        "τ".into()
    } else if abs.is_infinite() {
        "∞".into()
    } else {
        abs.to_string()
    };
    if n < 0.0 {
        format!("¯{}", pos)
    } else {
        pos
    }
}

impl Value {
    /// Get the `repr` of a value
    pub fn representation(&self) -> String {
        const MAX_SINGLE_LINE_LEN: usize = 40;
        let mut s = match self.rank() {
            0 => match self {
                Value::Num(arr) => {
                    let n = arr.data[0];
                    let bool_lit = arr.meta().flags.contains(ArrayFlags::BOOLEAN_LITERAL);
                    if n == 0.0 && bool_lit {
                        "False".into()
                    } else if n == 1.0 && bool_lit {
                        "True".into()
                    } else {
                        f64_repr(n)
                    }
                }
                Value::Byte(arr) => {
                    let b = arr.data[0];
                    let bool_lit = arr.meta().flags.contains(ArrayFlags::BOOLEAN_LITERAL);
                    if b == 0 && bool_lit {
                        "False".into()
                    } else if b == 1 && bool_lit {
                        "True".into()
                    } else {
                        b.to_string()
                    }
                }
                Value::Complex(arr) => {
                    let c = arr.data[0];
                    if c == Complex::I {
                        "i".into()
                    } else if c == -Complex::I {
                        "¯i".into()
                    } else {
                        format!("ℂ{} {}", f64_repr(c.im), f64_repr(c.re))
                    }
                }
                Value::Char(arr) => {
                    let c = arr.data[0];
                    match c {
                        ' ' => "@\\s".into(),
                        c => c.grid_string(false),
                    }
                }
                Value::Box(arr) => format!("□{}", arr.data[0].0.representation()),
            },
            1 => match self {
                Value::Char(arr) => format!("{:?}", arr.data.iter().collect::<String>()),
                Value::Box(arr) => {
                    let mut s = '{'.to_string();
                    for (i, v) in arr.data.iter().enumerate() {
                        if i > 0 {
                            s.push(' ');
                        }
                        s.push_str(&v.0.representation());
                    }
                    s.push('}');
                    s
                }
                value => {
                    let mut s = '['.to_string();
                    for (i, v) in value.rows().enumerate() {
                        if i > 0 {
                            s.push(' ');
                        }
                        s.push_str(&v.representation());
                    }
                    s.push(']');
                    s
                }
            },
            _ => {
                let mut s = '['.to_string();
                let rows: Vec<String> = self.rows().map(|v| v.representation()).collect();
                let max_row_len = rows.iter().map(String::len).max().unwrap_or(0);
                for (i, row) in rows.iter().enumerate() {
                    if i > 0 {
                        if max_row_len > MAX_SINGLE_LINE_LEN {
                            s.push_str("\n  ");
                        } else {
                            s.push(' ');
                        }
                    }
                    s.push_str(row);
                }
                s.push(']');
                s
            }
        };
        if let Some(map_keys) = self.map_keys() {
            s = format!(
                "map {} {}",
                map_keys.clone().normalized().representation(),
                s
            );
        }
        if let Some(label) = &self.meta().label {
            s = format!("${label} {s}");
        }
        s
    }
    /// Get the `datetime` of a value
    pub fn datetime(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        let mut arr = match self {
            Value::Num(arr) => arr.clone(),
            Value::Byte(arr) => arr.convert_ref(),
            value => return Err(env.error(format!("Cannot get datetime of {}", value.type_name()))),
        };
        let size = validate_size::<f64>(arr.shape.iter().copied().chain([6]), env)?;
        let mut new_data = eco_vec![0.0; size];
        let slice = new_data.make_mut();
        for (i, &n) in arr.data.iter().enumerate() {
            let dur = time::Duration::checked_seconds_f64(n)
                .ok_or_else(|| env.error(format!("{n} is not a valid time")))?;
            let dt = if n >= 0.0 {
                OffsetDateTime::UNIX_EPOCH.checked_add(dur)
            } else {
                OffsetDateTime::UNIX_EPOCH.checked_sub(dur)
            }
            .ok_or_else(|| env.error(format!("{n} is not a valid time")))?;
            slice[i * 6] = dt.year() as f64;
            slice[i * 6 + 1] = dt.month() as u8 as f64;
            slice[i * 6 + 2] = dt.day() as f64;
            slice[i * 6 + 3] = dt.hour() as f64;
            slice[i * 6 + 4] = dt.minute() as f64;
            slice[i * 6 + 5] = dt.second() as f64;
        }
        arr.data = new_data.into();
        arr.shape.push(6);
        arr.validate_shape();
        Ok(arr)
    }
    pub(crate) fn undatetime(&self, env: &Uiua) -> UiuaResult<Array<f64>> {
        let mut arr = match self {
            Value::Num(arr) => arr.clone(),
            Value::Byte(arr) => arr.convert_ref(),
            value => {
                return Err(env.error(format!("Cannot decode datetime from {}", value.type_name())))
            }
        };
        let convert = |chunk: &[f64]| -> UiuaResult<f64> {
            let mut year = chunk.first().copied().unwrap_or(0.0);
            let mut month = chunk.get(1).copied().unwrap_or(1.0) - 1.0;
            let mut day = chunk.get(2).copied().unwrap_or(1.0) - 1.0;
            let mut hour = chunk.get(3).copied().unwrap_or(0.0);
            let mut minute = chunk.get(4).copied().unwrap_or(0.0);
            let mut second = chunk.get(5).copied().unwrap_or(0.0);
            let mut frac = chunk.get(6).copied().unwrap_or(0.0);
            frac += second.fract();
            second = second.floor();
            second += minute.fract() * 60.0;
            minute = minute.floor();
            minute += hour.fract() * 60.0;
            hour = hour.floor();
            hour += day.fract() * 24.0;
            day = day.floor();
            day += month.fract() * 30.0;
            month = month.floor();
            month += year.fract() * 12.0;
            year = year.floor();
            if frac >= 1.0 {
                second += frac.floor();
                frac = 1.0 - frac.fract();
            } else if frac < 0.0 {
                second += frac;
                frac = -frac.fract();
            }
            if second >= 60.0 {
                minute += (second / 60.0).floor();
                second %= 60.0;
            } else if second < 0.0 {
                minute += second / 60.0;
                second = 60.0 + (second % 60.0);
            }
            if minute >= 60.0 {
                hour += (minute / 60.0).floor();
                minute %= 60.0;
            } else if minute < 0.0 {
                hour += minute / 60.0;
                minute = 60.0 + (minute % 60.0);
            }
            if hour >= 24.0 {
                day += (hour / 24.0).floor();
                hour %= 24.0;
            } else if hour < 0.0 {
                day += hour / 24.0;
                hour = 24.0 + (hour % 24.0);
            }
            let day_delta = if day >= 28.0 {
                let delta = day - 27.0;
                day -= delta;
                delta
            } else if day < 0.0 {
                let delta = day;
                day = 0.0;
                delta
            } else {
                day.fract()
            };
            if month >= 12.0 {
                year += (month / 12.0).floor();
                month %= 12.0;
            } else if month < 0.0 {
                year += month / 12.0;
                month = 12.0 + (month % 12.0);
            }
            let year = year as i32;
            let month = month as u8 + 1;
            let day = day as u8 + 1;
            let hour = hour as u8;
            let minute = minute as u8;
            let second = second as u8;
            let mut dt = OffsetDateTime::new_utc(
                Date::from_calendar_date(year, Month::January.nth_next(month - 1), day).map_err(
                    |e| env.error(format!("Invalid date {year:04}-{month:02}-{day:02}: {e}")),
                )?,
                Time::from_hms(hour, minute, second).map_err(|e| {
                    env.error(format!(
                        "Invalid time {hour:02}:{minute:02}:{second:02}: {e}"
                    ))
                })?,
            );
            if day_delta > 0.0 {
                dt += Duration::from_secs_f64(day_delta * 86400.0);
            } else if day_delta < 0.0 {
                dt -= Duration::from_secs_f64(day_delta.abs() * 86400.0);
            }
            Ok(dt.unix_timestamp() as f64 + frac)
        };
        let [shape_pre @ .., n] = &*arr.shape else {
            return Err(env.error("Cannot decode datetime from scalar"));
        };
        arr.data = if *n == 0 {
            let size = validate_size::<f64>(shape_pre.iter().copied(), env)?;
            eco_vec![0.0; size].into()
        } else {
            let mut new_data = eco_vec![0.0; arr.data.len() / *n];
            let slice = new_data.make_mut();
            if *n > 0 {
                for (i, chunk) in arr.data.chunks_exact(*n).enumerate() {
                    slice[i] = convert(chunk)?;
                }
            }
            new_data.into()
        };
        arr.shape.pop();
        arr.validate_shape();
        Ok(arr)
    }
}

#[derive(Clone, Copy, Sequence)]
#[repr(u8)]
enum BinType {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    I8 = 4,
    I16 = 5,
    I32 = 6,
    I64 = 7,
    F32 = 8,
    F64 = 9,
    Char = 16,
    Box = 32,
    Complex = 48,
}

const MAX_BINARY_DEPTH: usize = if cfg!(debug_assertions) { 10 } else { 32 };

impl Value {
    pub(crate) fn to_binary(&self, env: &Uiua) -> UiuaResult<Vec<u8>> {
        let mut bytes = Vec::new();
        self.to_binary_impl(&mut bytes, 0, env)?;
        Ok(bytes)
    }
    fn to_binary_impl(&self, bytes: &mut Vec<u8>, depth: usize, env: &Uiua) -> UiuaResult {
        if depth > MAX_BINARY_DEPTH {
            return Err(env.error("Nested structure is too deep"));
        }
        if self.rank() > u8::MAX as usize {
            return Err(env.error(format!("Rank {} is too large", self.rank())));
        }
        fn write_shape(shape: &Shape, bytes: &mut Vec<u8>) {
            bytes.push(shape.len() as u8);
            for &dim in shape {
                bytes.extend((dim as u32).to_le_bytes());
            }
        }
        fn write_ty_meta(
            ty: BinType,
            meta: &ArrayMeta,
            bytes: &mut Vec<u8>,
            depth: usize,
            env: &Uiua,
        ) -> UiuaResult {
            let mut ty_u = ty as u8;
            if meta == &ArrayMeta::default() {
                bytes.push(ty_u);
            } else {
                ty_u |= 128u8;
                bytes.push(ty_u);
                // Flags
                bytes.push(meta.flags.bits());
                // Label
                let label = meta.label.as_deref().unwrap_or("");
                bytes.extend((label.len() as u32).to_le_bytes());
                bytes.extend(label.as_bytes());
                // Pointer
                if meta.pointer.is_some() {
                    return Err(env.error("Cannot serialize pointers"));
                }
                // Handle kind
                if meta.handle_kind.is_some() {
                    return Err(env.error("Cannot serialize I/O handles"));
                }
                // Map keys
                bytes.push(meta.map_keys.is_some() as u8);
                if let Some(keys) = &meta.map_keys {
                    keys.clone()
                        .normalized()
                        .to_binary_impl(bytes, depth + 1, env)?;
                }
            }
            Ok(())
        }
        match self {
            Value::Num(arr) => {
                let mut all_non_neg = true;
                let mut all_int = true;
                let mut all_f32 = true;
                let mut min = 0f64;
                let mut max = 0f64;
                for &n in &arr.data {
                    all_non_neg &= n >= 0.0;
                    all_int &= n.fract() == 0.0;
                    all_f32 &= (n as f32 as f64).to_bits() == n.to_bits();
                    min = min.min(n);
                    max = max.max(n);
                }
                let ty = if all_non_neg && all_int {
                    if max <= u8::MAX as f64 {
                        BinType::U8
                    } else if max <= u16::MAX as f64 {
                        BinType::U16
                    } else if max <= u32::MAX as f64 {
                        BinType::U32
                    } else if max <= u64::MAX as f64 {
                        BinType::U64
                    } else if max <= 2f32.powf(24.0) as f64 {
                        BinType::F32
                    } else {
                        BinType::F64
                    }
                } else if all_int {
                    if min >= i8::MIN as f64 && max <= i8::MAX as f64 {
                        BinType::I8
                    } else if min >= i16::MIN as f64 && max <= i16::MAX as f64 {
                        BinType::I16
                    } else if min >= i32::MIN as f64 && max <= i32::MAX as f64 {
                        BinType::I32
                    } else if min >= i64::MIN as f64 && max <= i64::MAX as f64 {
                        BinType::I64
                    } else if min >= -(2f32.powf(24.0)) as f64 && max <= 2f32.powf(24.0) as f64 {
                        BinType::F32
                    } else {
                        BinType::F64
                    }
                } else if all_f32 {
                    BinType::F32
                } else {
                    BinType::F64
                };
                write_ty_meta(ty, arr.meta(), bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                fn write(nums: &[f64], bytes: &mut Vec<u8>, f: impl Fn(f64, &mut Vec<u8>)) {
                    for &n in nums {
                        f(n, bytes);
                    }
                }
                let data = &arr.data;
                match ty {
                    BinType::U8 => write(data, bytes, |n, b| b.extend((n as u8).to_le_bytes())),
                    BinType::U16 => write(data, bytes, |n, b| b.extend((n as u16).to_le_bytes())),
                    BinType::U32 => write(data, bytes, |n, b| b.extend((n as u32).to_le_bytes())),
                    BinType::U64 => write(data, bytes, |n, b| b.extend((n as u64).to_le_bytes())),
                    BinType::I8 => write(data, bytes, |n, b| b.extend((n as i8).to_le_bytes())),
                    BinType::I16 => write(data, bytes, |n, b| b.extend((n as i16).to_le_bytes())),
                    BinType::I32 => write(data, bytes, |n, b| b.extend((n as i32).to_le_bytes())),
                    BinType::I64 => write(data, bytes, |n, b| b.extend((n as i64).to_le_bytes())),
                    BinType::F32 => write(data, bytes, |n, b| b.extend((n as f32).to_le_bytes())),
                    BinType::F64 => write(data, bytes, |n, b| b.extend(n.to_le_bytes())),
                    _ => unreachable!(),
                }
            }
            Value::Byte(arr) => {
                write_ty_meta(BinType::U8, arr.meta(), bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                bytes.extend(&arr.data);
            }
            Value::Char(arr) => {
                write_ty_meta(BinType::Char, arr.meta(), bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                let s: String = arr.data.iter().copied().collect();
                bytes.extend((s.as_bytes().len() as u32).to_le_bytes());
                bytes.extend(s.as_bytes());
            }
            Value::Box(arr) => {
                write_ty_meta(BinType::Box, arr.meta(), bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                for Boxed(v) in &arr.data {
                    v.to_binary_impl(bytes, depth + 1, env)?;
                }
            }
            Value::Complex(arr) => {
                write_ty_meta(BinType::Complex, arr.meta(), bytes, depth, env)?;
                write_shape(&arr.shape, bytes);
                for Complex { re, im } in &arr.data {
                    bytes.extend(re.to_le_bytes());
                    bytes.extend(im.to_le_bytes());
                }
            }
        }
        Ok(())
    }
    pub(crate) fn from_binary(mut bytes: &[u8], env: &Uiua) -> UiuaResult<Self> {
        Self::from_binary_impl(&mut bytes, 0, env)
    }
    fn from_binary_impl(bytes: &mut &[u8], depth: usize, env: &Uiua) -> UiuaResult<Self> {
        if depth > MAX_BINARY_DEPTH {
            return Err(env.error("Nested structure is too deep"));
        }
        // Type
        let mut ty_u = *bytes
            .first()
            .ok_or_else(|| env.error("Missing type identifier"))?;
        let has_meta = ty_u & 128u8 != 0;
        ty_u &= 127u8;
        let ty = all::<BinType>()
            .find(|&ty| ty as u8 == ty_u)
            .ok_or_else(|| env.error(format!("Invalid binary type {ty_u}")))?;
        *bytes = &bytes[1..];

        let meta = if has_meta {
            let mut meta = ArrayMeta::default();
            // Flags
            if bytes.is_empty() {
                return Err(env.error("Missing flags length"));
            }
            let flags_u = *bytes.first().unwrap();
            *bytes = &bytes[1..];
            let flags = ArrayFlags::from_bits(flags_u)
                .ok_or_else(|| env.error(format!("Invalid array flags {:08b}", flags_u)))?;
            meta.flags = flags;

            // Label
            if bytes.len() < size_of::<u32>() {
                return Err(env.error("Missing label length"));
            }
            let label_len = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
            *bytes = &bytes[size_of::<u32>()..];
            if label_len > 0 {
                if bytes.len() < label_len as usize {
                    return Err(env.error("Missing label data"));
                }
                let label = str::from_utf8(&bytes[..label_len as usize])
                    .map_err(|e| env.error(format!("Failed to parse label: {e}")))?;
                *bytes = &bytes[label_len as usize..];
                meta.label = Some(label.into());
            }

            // Map keys
            if bytes.is_empty() {
                return Err(env.error("Missing map keys check"));
            }
            let has_map_keys = *bytes.first().unwrap() != 0;
            *bytes = &bytes[1..];
            let keys = if has_map_keys {
                Some(Self::from_binary_impl(bytes, depth + 1, env)?)
            } else {
                None
            };

            Some((meta, keys))
        } else {
            None
        };

        // Rank
        if bytes.is_empty() {
            return Err(env.error("Missing rank"));
        }
        let rank = *bytes.first().unwrap();
        *bytes = &bytes[1..];

        // Shape
        let mut shape = Shape::with_capacity(rank as usize);
        for i in 0..rank {
            if bytes.len() < size_of::<u32>() {
                return Err(env.error(format!("Missing shape dimension {i}")));
            }
            let len = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
            shape.push(len as usize);
            *bytes = &bytes[size_of::<u32>()..];
        }

        // Data
        fn make<'a, A: TryFrom<&'a [u8]>, T, E: Clone>(
            bytes: &mut &'a [u8],
            shape: Shape,
            env: &Uiua,
            f: impl Fn(A) -> T,
            g: impl Fn(T) -> E,
        ) -> UiuaResult<Array<E>> {
            validate_size::<E>(shape.iter().copied(), env)?;
            let mut data = EcoVec::with_capacity(shape.elements());
            for i in 0..shape.elements() {
                if bytes.len() < size_of::<A>() {
                    return Err(env.error(format!("Missing data for element {i}")));
                }
                let elem =
                    f(A::try_from(&bytes[..size_of::<A>()]).unwrap_or_else(|_| unreachable!()));
                data.push(g(elem));
                *bytes = &bytes[size_of::<A>()..];
            }
            Ok(Array::new(shape, data))
        }
        let mut val: Value = match ty {
            BinType::U8 => make(bytes, shape, env, u8::from_le_bytes, |x| x)?.into(),
            BinType::U16 => make(bytes, shape, env, u16::from_le_bytes, |x| x as f64)?.into(),
            BinType::U32 => make(bytes, shape, env, u32::from_le_bytes, |x| x as f64)?.into(),
            BinType::U64 => make(bytes, shape, env, u64::from_le_bytes, |x| x as f64)?.into(),
            BinType::I8 => make(bytes, shape, env, i8::from_le_bytes, |x| x as f64)?.into(),
            BinType::I16 => make(bytes, shape, env, i16::from_le_bytes, |x| x as f64)?.into(),
            BinType::I32 => make(bytes, shape, env, i32::from_le_bytes, |x| x as f64)?.into(),
            BinType::I64 => make(bytes, shape, env, i64::from_le_bytes, |x| x as f64)?.into(),
            BinType::F32 => make(bytes, shape, env, f32::from_le_bytes, |x| x as f64)?.into(),
            BinType::F64 => make(bytes, shape, env, f64::from_le_bytes, |x| x)?.into(),
            BinType::Char => {
                if bytes.len() < size_of::<u32>() {
                    return Err(env.error("Missing byte count"));
                }
                let byte_count = u32::from_le_bytes(bytes[..size_of::<u32>()].try_into().unwrap());
                *bytes = &bytes[size_of::<u32>()..];
                if bytes.len() < byte_count as usize {
                    return Err(env.error("Missing character bytes"));
                }
                let s = str::from_utf8(&bytes[..byte_count as usize])
                    .map_err(|e| env.error(format!("Failed to parse string: {e}")))?;
                *bytes = &bytes[byte_count as usize..];
                let data: EcoVec<char> = s.chars().collect();
                if shape.elements() != data.len() {
                    return Err(env.error(format!(
                        "Shape implies {shape} characters, but got {}",
                        data.len()
                    )));
                }
                Array::new(shape, data).into()
            }
            BinType::Complex => make(bytes, shape, env, u128::from_le_bytes, |u| {
                let bytes = u.to_le_bytes();
                let re = f64::from_le_bytes(bytes[..size_of::<f64>()].try_into().unwrap());
                let im = f64::from_le_bytes(bytes[size_of::<f64>()..].try_into().unwrap());
                Complex::new(re, im)
            })?
            .into(),
            BinType::Box => {
                let mut data = EcoVec::with_capacity(shape.elements());
                for i in 0..shape.elements() {
                    let val = Self::from_binary_impl(bytes, depth + 1, env)
                        .map_err(|e| env.error(format!("Failed to parse box element {i}: {e}")))?;
                    data.push(Boxed(val));
                }
                Array::new(shape, data).into()
            }
        };
        if let Some((meta, map_keys)) = meta {
            *val.meta_mut() = meta;
            if let Some(keys) = map_keys {
                val.map(keys, env)?;
            }
        }
        Ok(val)
    }
}
