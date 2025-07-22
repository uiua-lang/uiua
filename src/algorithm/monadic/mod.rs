//! Algorithms for monadic array operations

mod sort;

use core::str;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    convert::identity,
    f64::consts::{PI, TAU},
    io::Write,
    iter::{self, once},
    mem::{size_of, take},
    ptr, slice,
    time::Duration,
};

use ecow::{eco_vec, EcoVec};
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
            self.meta.take_map_keys();
        }
        depth = depth.min(self.rank());
        let deshaped = self.shape.split_off(depth).into_iter().product();
        self.shape.push(deshaped);
        self.meta.take_sorted_flags();
        self.validate();
    }
    pub(crate) fn deshape_sub(
        &mut self,
        irank: i32,
        mut depth: usize,
        extend: bool,
        env: &Uiua,
    ) -> UiuaResult {
        depth = depth.min(self.rank());
        let deep_rank = self.rank() - depth;
        if irank > 0 && irank as usize == deep_rank {
            return Ok(());
        }
        self.meta.take_map_keys();
        let shape = &mut self.shape;
        let rank = irank.unsigned_abs() as usize;
        match irank.cmp(&0) {
            Ordering::Equal => {
                // First scalar
                if depth == 0 {
                    if shape.contains(&0) {
                        if let Some(fv) = env.value_fill() {
                            *self = fv.value.clone();
                        } else {
                            return Err(env.error(format!(
                                "Cannot get first scalar of an empty array (shape {shape})"
                            )));
                        }
                    } else {
                        *shape = [].into();
                        val_as_arr!(self, |arr| arr.data.truncate(1));
                    }
                } else {
                    let row_len: usize = shape.iter().skip(depth).product();
                    shape.truncate(depth);
                    let elem_count = shape.elements();
                    val_as_arr!(self, |arr| {
                        let slice = arr.data.as_mut_slice();
                        for i in 1..elem_count {
                            slice[i] = take(&mut slice[i * row_len]);
                        }
                        arr.data.truncate(elem_count);
                    });
                }
            }
            Ordering::Greater => {
                // Positive rank
                match rank.cmp(&deep_rank) {
                    Ordering::Equal => {}
                    Ordering::Less => {
                        let mut new_shape: Shape = shape.iter().take(depth).copied().collect();
                        let mid = shape.len() + 1 - rank;
                        let new_first_dim: usize = shape[depth..mid].iter().product();
                        new_shape.extend(once(new_first_dim).chain(shape[mid..].iter().copied()));
                        *shape = new_shape;
                    }
                    Ordering::Greater => {
                        if extend {
                            for _ in 0..rank - shape.len() {
                                shape.insert(depth, 1);
                            }
                        } else {
                            shape.insert(depth, 1);
                        }
                    }
                }
            }
            Ordering::Less => {
                // Negative rank
                if rank + 1 > deep_rank {
                    return if extend {
                        Err(env.error(format!(
                            "Negative {} has magnitude {}, but the \
                            rank-{} array cannot be reduced that much",
                            Primitive::Deshape.format(),
                            rank,
                            deep_rank
                        )))
                    } else {
                        Ok(())
                    };
                }
                let mut new_shape: Shape = shape.iter().take(depth).copied().collect();
                let new_first_dim: usize = shape[depth..=depth + rank].iter().product();
                new_shape
                    .extend(once(new_first_dim).chain(shape[depth + rank + 1..].iter().copied()));
                *shape = new_shape;
            }
        }
        self.meta.take_sorted_flags();
        self.validate();
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
                    .chain((self.shape.iter()).skip(rank.saturating_sub(orig_shape.len()).max(1)))
                    .copied()
                    .collect()
            } else {
                // Negative rank
                (orig_shape.iter().take(rank + 1))
                    .chain(self.shape.iter().skip(1))
                    .copied()
                    .collect()
            };
            if validate_size::<u8>(new_shape.iter().copied(), env)? != self.shape.elements() {
                return Ok(());
            }
            self.shape = new_shape;
            self.validate();
            Ok(())
        } else {
            let mut new_shape = self.shape.clone();
            if !new_shape.is_empty() {
                new_shape.remove(0);
            }
            for &d in orig_shape.iter().rev() {
                new_shape.prepend(d);
            }
            if new_shape.elements() == self.shape.elements() {
                self.shape = new_shape;
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
        let per_meta = self.meta.take_per_meta();
        let new_shape: Shape = self.shape[..depth].into();
        let row_shape: Shape = self.shape[depth..].into();
        let data: EcoVec<Boxed> = self.into_row_shaped_slices(row_shape).map(Boxed).collect();
        let mut arr = Array::new(new_shape, data);
        arr.meta.set_per_meta(per_meta);
        arr
    }
    /// Attempt to parse the value into a number
    pub fn parse_num(mut self, env: &Uiua) -> UiuaResult<Self> {
        let per_meta = self.meta.take_per_meta();
        let mut parsed = match (self.rank(), self) {
            (0 | 1, Value::Char(arr)) => {
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
                            .or_else(|e| env.value_fill().map(|fv| fv.value.clone()).ok_or(e))?
                    }
                    (_, _, Some((re, im))) => {
                        let re = parse_uiua_num(re.into(), env);
                        let im = parse_uiua_num(im.into(), env);
                        re.and_then(|re| im.map(|im| Complex { re, im: -im }.into()))
                            .or_else(|e| env.value_fill().map(|fv| fv.value.clone()).ok_or(e))?
                    }
                    _ => parse_uiua_num(s.into(), env)
                        .map(Into::into)
                        .or_else(|e| env.value_fill().map(|fv| fv.value.clone()).ok_or(e))?,
                }
            }
            (0, Value::Box(arr)) => {
                let Boxed(value) = arr.into_scalar().unwrap();
                value.parse_num(env)?
            }
            (_, val @ (Value::Char(_) | Value::Box(_))) => {
                let mut rows = Vec::with_capacity(val.row_count());
                for row in val.into_rows() {
                    rows.push(row.parse_num(env)?);
                }
                Value::from_row_values(rows, env)?
            }
            (_, val) => return Err(env.error(format!("Cannot parse {} array", val.type_name()))),
        };
        parsed.meta.set_per_meta(per_meta);
        Ok(parsed)
    }

    fn padded(
        c: char,
        right: bool,
        arr: Array<impl Into<f64> + Copy>,
        base: usize,
        to_string: impl Fn(f64) -> UiuaResult<String>,
        env: &Uiua,
    ) -> UiuaResult<Array<char>> {
        let is_digit = Self::BASE_CHARSET[..base].contains(c.to_ascii_lowercase());

        let mut buf = Vec::new();
        let mut max_whole = 0;
        let mut max_dec = 0;
        for v in &arr.data {
            buf.clear();
            write!(&mut buf, "{}", to_string((*v).into())?).unwrap();
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
        let mut new_shape = arr.shape.clone();
        new_shape.push(max_len);
        let elem_count = validate_size::<char>(new_shape.iter().copied(), env)?;
        let mut new_data = eco_vec![c; elem_count];
        if max_len > 0 {
            for (i, s) in new_data.make_mut().chunks_exact_mut(max_len).enumerate() {
                let n = to_string(arr.data[i].into())?;
                let dot_pos = n.find('.');
                if right {
                    for (s, c) in s.iter_mut().zip(n.chars()) {
                        *s = c;
                    }
                } else {
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
                }
                if dot_pos.is_none() && max_dec > 0 && is_digit {
                    s[max_whole] = '.';
                }
            }
        }
        Ok(Array::new(new_shape, new_data))
    }
    pub(crate) fn unparse(mut self, env: &Uiua) -> UiuaResult<Self> {
        let per_meta = self.meta.take_per_meta();
        let mut unparsed = if self.rank() == 0 {
            match self {
                Value::Box(b) => b.into_scalar().unwrap().0.unparse(env)?,
                value => value.format().into(),
            }
        } else {
            match self {
                Value::Num(arr) => {
                    if let Ok(c) = env.scalar_fill::<char>() {
                        Self::padded(c.value, c.is_right(), arr, 10, |n| Ok(n.to_string()), env)?
                            .into()
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
                        Self::padded(c.value, c.is_right(), arr, 10, |n| Ok(n.to_string()), env)?
                            .into()
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
            }
        };
        unparsed.meta.set_per_meta(per_meta);
        Ok(unparsed)
    }

    const BASE_CHARSET: &str = "0123456789abcdefghijklmnopqrstuvwxyz";
    const BASE64_CHARSET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    /// Attempt to parse strings in some base into numbers
    pub fn parse_base(mut self, base: usize, env: &Uiua) -> UiuaResult<Self> {
        let parse_base_str = |mut s: &str| -> UiuaResult<Value> {
            if base == 64 {
                let mut s = s.to_string();
                s = s.replace("-", "+");
                s = s.replace("_", "/");

                let mut num = 0.0;
                for (place, c) in s.chars().rev().enumerate() {
                    let scale = (base as f64).powi(place as i32);
                    if let Some(value) = Self::BASE64_CHARSET.find(c) {
                        num += value as f64 * scale;
                    } else {
                        return Err(env.error(format!("Cannot parse character {c} in base {base}")));
                    }
                }
                return Ok(num.into());
            }

            let sign = if let Some(pos) = s.strip_prefix("-") {
                s = pos;
                -1.0
            } else {
                1.0
            };

            if base == 1 {
                let not_one = s.chars().find(|&c| c != '1');
                return if let Some(c) = not_one {
                    Err(env.error(format!("Cannot parse character {c} in base {base}")))
                } else {
                    Ok(s.len().into())
                };
            }

            if s.is_empty() {
                return Err(env.error("Number must have digits"));
            }

            let fract = if let Some((num, sfract)) = s.split_once(".") {
                if num.is_empty() && sfract.is_empty() {
                    return Err(env.error("Number must have digits"));
                }
                s = num;
                let mut fract = 0.0;
                for (place, c) in sfract.chars().enumerate() {
                    let scale = (base as f64).powi(-(place as i32 + 1));
                    if let Some(value) =
                        Self::BASE_CHARSET[..base as usize].find(c.to_ascii_lowercase())
                    {
                        fract += value as f64 * scale;
                    } else {
                        return Err(env.error(format!("Cannot parse character {c} in base {base}")));
                    }
                }
                fract
            } else {
                0.0
            };

            let mut num = 0.0;
            for (place, c) in s.chars().rev().enumerate() {
                let scale = (base as f64).powi(place as i32);
                if let Some(value) =
                    Self::BASE_CHARSET[..base as usize].find(c.to_ascii_lowercase())
                {
                    num += value as f64 * scale;
                } else {
                    return Err(env.error(format!("Cannot parse character {c} in base {base}")));
                }
            }

            num += fract;
            num *= sign;

            Ok(num.into())
        };

        let per_meta = self.meta.take_per_meta();
        let mut parsed = match (self.rank(), self) {
            (0 | 1, Value::Char(arr)) => {
                let s = arr.data.iter().copied().collect::<String>();

                parse_base_str(&s)
                    .or_else(|e| env.value_fill().map(|fv| fv.value.clone()).ok_or(e))?
            }
            (0, Value::Box(arr)) => {
                let Boxed(value) = arr.into_scalar().unwrap();
                value.parse_base(base, env)?
            }
            (_, val @ (Value::Char(_) | Value::Box(_))) => {
                let mut rows = Vec::with_capacity(val.row_count());
                for row in val.into_rows() {
                    rows.push(row.parse_base(base, env)?);
                }
                Value::from_row_values(rows, env)?
            }
            (_, val) => return Err(env.error(format!("Cannot parse {} array", val.type_name()))),
        };
        parsed.meta.set_per_meta(per_meta);
        Ok(parsed)
    }
    /// Format numbers into some base
    pub fn unparse_base(mut self, base: usize, env: &Uiua) -> UiuaResult<Self> {
        // This expression expression determines how many digits in some base
        let sigfigs = (40.0 / (base as f64).ln()).ceil() as usize;

        let unparse_base_num = |mut n: f64| match base {
            _ if n.is_infinite() || n.is_nan() => {
                Err(env.error(format!("Cannot unparse {n} to base {base}")))
            }
            1 => {
                if n < 0.0 || n.fract() != 0.0 {
                    return Err(env.error(format!("Cannot unparse {n} to base {base}")));
                }
                let _ = validate_size::<char>([n as usize], env)?;
                Ok(iter::repeat_n('1', n as usize).collect::<String>().into())
            }
            10 => Ok(n.to_string()),
            64 => {
                if n.fract() != 0.0 {
                    return Err(env.error(format!("Cannot unparse {n} to base {base}")));
                }
                if n == 0.0 {
                    return Ok("A".to_string());
                }

                let mut num = Vec::new();
                while n != 0.0 {
                    num.push(
                        Self::BASE64_CHARSET
                            .chars()
                            .nth((n % 64.0) as usize)
                            .unwrap(),
                    );
                    n = (n / 64.0).floor();
                }

                Ok(num.into_iter().rev().collect())
            }
            _ => {
                let sign = if n < 0.0 {
                    "-".to_string()
                } else {
                    String::new()
                };
                n = n.abs();
                let mut fract = n.fract();
                n = n.floor();

                let mut num = Vec::new();
                while n != 0.0 {
                    num.push(
                        Self::BASE_CHARSET[..base]
                            .chars()
                            .nth((n % base as f64) as usize)
                            .unwrap(),
                    );
                    n = (n / base as f64).floor();
                }
                num = num
                    .into_iter()
                    .rev()
                    .enumerate()
                    .map(|(i, c)| if i <= sigfigs { c } else { '0' })
                    .collect();

                let fract = if fract != 0.0 {
                    let mut digits = ".".to_string();
                    for _ in 0..sigfigs - num.len() {
                        fract *= base as f64;
                        digits.push(
                            Self::BASE_CHARSET[..base]
                                .chars()
                                .nth(fract.floor() as usize)
                                .unwrap(),
                        );
                        fract %= 1.0;
                    }
                    let digits = digits.trim_end_matches("0");
                    let digits = digits.trim_end_matches(".");
                    digits.to_string()
                } else {
                    String::new()
                };

                if num.is_empty() {
                    num.push('0');
                }

                let num = num.into_iter().collect::<String>();

                Ok(format!("{sign}{num}{fract}"))
            }
        };

        let per_meta = self.meta.take_per_meta();
        let mut unparsed = if self.rank() == 0 {
            match self {
                Value::Box(b) => b.into_scalar().unwrap().0.unparse_base(base, env)?,
                Value::Byte(n) => unparse_base_num(*n.as_scalar().unwrap() as f64)?.into(),
                Value::Num(n) => unparse_base_num(*n.as_scalar().unwrap())?.into(),
                val => {
                    return Err(env.error(format!(
                        "Cannot unparse {} array in base {base}",
                        val.type_name()
                    )))
                }
            }
        } else {
            match self {
                Value::Num(arr) => {
                    if let Ok(c) = env.scalar_fill::<char>() {
                        Self::padded(c.value, c.is_right(), arr, base, unparse_base_num, env)?
                            .into()
                    } else {
                        let new_data: CowSlice<Boxed> =
                            (arr.data.iter().copied().map(unparse_base_num))
                                .map(|v| v.map(Value::from))
                                .map(|v| v.map(Boxed))
                                .collect::<UiuaResult<_>>()?;
                        Array::new(arr.shape.clone(), new_data).into()
                    }
                }
                Value::Byte(arr) => {
                    if let Ok(c) = env.scalar_fill::<char>() {
                        Self::padded(c.value, c.is_right(), arr, base, unparse_base_num, env)?
                            .into()
                    } else {
                        let new_data: CowSlice<Boxed> =
                            (arr.data.iter().map(|&n| n as f64).map(unparse_base_num))
                                .map(|v| v.map(Value::from))
                                .map(|v| v.map(Boxed))
                                .collect::<UiuaResult<_>>()?;
                        Array::new(arr.shape.clone(), new_data).into()
                    }
                }
                val => return Err(env.error(format!("Cannot unparse {} array", val.type_name()))),
            }
        };
        unparsed.meta.set_per_meta(per_meta);
        Ok(unparsed)
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
            self.meta.take_map_keys();
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
            if let Some(keys) = self.meta.map_keys_mut() {
                keys.fix();
            }
        }
    }
    /// Remove a 1-length dimension from the front of the array's shape
    pub fn unfix(&mut self, env: &Uiua) -> UiuaResult {
        if let Some(keys) = self.meta.map_keys_mut() {
            keys.unfix();
        }
        self.shape.unfix().map_err(|e| env.error(e))?;
        self.validate();
        Ok(())
    }
    /// Collapse the top two dimensions of the array's shape
    pub fn undo_fix(&mut self) {
        if let Some(keys) = self.meta.map_keys_mut() {
            keys.unfix();
        }
        _ = self.shape.unfix();
        self.validate();
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
        let per_meta = self.meta.take_per_meta();
        let new_shape: Shape = self.shape[..depth].into();
        let row_shape: Shape = self.shape[depth..].into();
        let data: EcoVec<Boxed> = self
            .into_row_shaped_slices(row_shape)
            .map(Value::from)
            .map(Boxed)
            .collect();
        let mut arr = Array::new(new_shape, data);
        arr.meta.set_per_meta(per_meta);
        arr
    }
}

impl Value {
    /// Create a `range` array
    pub fn range(&self, env: &Uiua) -> UiuaResult<Self> {
        self.range_start_impl(0, env)
    }
    pub(crate) fn range_start(&self, shape: &Self, env: &Uiua) -> UiuaResult<Self> {
        let start = self.as_num(env, "Range start should be a scalar number")?;
        if start.fract() == 0.0 && (isize::MIN as f64..=isize::MAX as f64).contains(&start) {
            shape.range_start_impl(start as isize, env)
        } else {
            let range = shape.range(env)?;
            self.clone().add(range, env)
        }
    }
    fn range_start_impl(&self, start: isize, env: &Uiua) -> UiuaResult<Self> {
        let ishape = self.as_ints(
            env,
            "Range max should be a single integer \
            or a list of integers",
        )?;
        if self.rank() == 0 {
            let max = ishape[0];
            let mut value: Value = if max >= 0 {
                if start >= 0 && max + start <= 256 {
                    (start..max + start).map(|i| i as u8).collect()
                } else {
                    validate_size::<f64>([max.unsigned_abs()], env)?;
                    (start..max + start).map(|i| i as f64).collect()
                }
            } else {
                validate_size::<f64>([max.unsigned_abs()], env)?;
                (max + start..start).map(|i| i as f64).rev().collect()
            };
            value.meta.mark_sorted_up(max >= 0);
            value.meta.mark_sorted_down(max <= 0);
            value.validate();
            return Ok(value);
        }
        if ishape.is_empty() {
            return Ok(Array::<f64>::new(0, CowSlice::new()).into());
        }
        let mut shape = Shape::from_iter(ishape.iter().map(|d| d.unsigned_abs()));
        shape.push(shape.len());
        let data = range(&ishape, start, env)?;
        let mut value: Value = match data {
            Ok(data) => Array::new(shape, data).into(),
            Err(data) => Array::new(shape, data).into(),
        };
        let first_max = ishape.first().copied().unwrap_or(0);
        value.meta.mark_sorted_up(first_max >= 0);
        value.meta.mark_sorted_down(first_max <= 0);
        value.validate();
        Ok(value)
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
        let first_max = ishape.first().copied().unwrap_or(0);
        for (i, s) in ishape.into_iter().enumerate() {
            if s < 0 {
                arr.reverse_depth(i);
            }
        }
        arr.meta.mark_sorted_up(first_max >= 0);
        arr.meta.mark_sorted_down(first_max <= 0);
        Ok(arr.into())
    }
}

pub(crate) fn range(
    shape: &[isize],
    start: isize,
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
    if start >= 0 && max as isize + start <= 256 && !any_neg {
        validate_size::<u8>([len], env)?;
        let mut data: EcoVec<u8> = eco_vec![0; len];
        let data_slice = data.make_mut();
        let start = start.unsigned_abs();
        for i in 0..elem_count {
            let dim = i % shape.len();
            let index = i / shape.len();
            data_slice[i] = (index / scan[dim] % shape[dim].unsigned_abs() + start) as u8;
        }
        Ok(Err(data.into()))
    } else {
        validate_size::<f64>([len], env)?;
        let mut data: EcoVec<f64> = eco_vec![0.0; len];
        let data_slice = data.make_mut();
        let start = start as f64;
        for i in 0..elem_count {
            let dim = i % shape.len();
            let index = i / shape.len();
            data_slice[i] = (index / scan[dim] % shape[dim].unsigned_abs()) as f64;
            if shape[dim] < 0 {
                data_slice[i] = -1.0 - data_slice[i];
            }
            data_slice[i] += start;
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
                    self.data.extend_repeat(&fill.value, self.row_len());
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
                self.meta.take_map_keys();
                self.meta.take_label();
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
                    self.data.extend_repeat(&fill.value, self.shape.elements());
                    self.validate();
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
                self.validate();
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
                self.validate();
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
                    self.data.extend_repeat(&fill.value, self.row_len());
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
                self.meta.take_map_keys();
                self.meta.take_label();
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
                    self.data.extend_repeat(&fill.value, self.shape.elements());
                    Ok(self)
                }
                Err(e) => Err(env
                    .error(format!("Cannot take last of an empty array{e}"))
                    .fill()),
            },
            [1, ..] => {
                self.shape.remove(depth);
                self.validate();
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
                self.validate();
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

impl<T: ArrayValue> Array<T> {
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
        let sorted_up = self.meta.is_sorted_up();
        let sorted_down = self.meta.is_sorted_down();
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
            if let Some(meta) = self.meta.get_mut() {
                if let Some(keys) = &mut meta.map_keys {
                    keys.reverse();
                }
            }
            self.meta.mark_sorted_up(sorted_down);
            self.meta.mark_sorted_down(sorted_up);
        } else {
            self.meta.take_sorted_flags();
        }
        self.validate();
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
    /// Like transpose, but the axes are reversed instead of rotated
    pub(crate) fn retropose_depth(&mut self, depth: usize) {
        match self {
            Value::Num(n) => n.retropose_depth(depth),
            Value::Byte(b) => b.retropose_depth(depth),
            Value::Complex(c) => c.retropose_depth(depth),
            Value::Char(c) => c.retropose_depth(depth),
            Value::Box(b) => {
                if depth == b.rank() {
                    for b in b.data.as_mut_slice() {
                        b.0.retropose_depth(0);
                    }
                } else {
                    b.retropose_depth(depth);
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
    /// Untranspose the array
    pub fn untranspose(&mut self) {
        self.transpose_depth(0, -1);
    }
    pub(crate) fn transpose_depth(&mut self, mut depth: usize, amnt: i32) {
        crate::profile_function!();
        if depth == 0 && self.is_map() {
            self.meta.take_map_keys();
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
        self.meta.take_sorted_flags();
        let forward = amnt.is_positive();
        // Early return if any dimension is 0, because there are no elements
        if self.shape[depth..].contains(&0) || depth > 0 && self.shape[depth - 1] == 0 {
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
    /// Like transpose, but the axes are reversed instead of rotated
    pub(crate) fn retropose_depth(&mut self, mut depth: usize) {
        crate::profile_function!();
        if depth == 0 && self.is_map() {
            self.meta.take_map_keys();
        }
        if self.rank() == 0 {
            return;
        }
        depth = depth.min(self.rank());
        let trans_rank = self.rank() - depth;
        // Early return if nothing would actually happen
        if trans_rank < 2 {
            return;
        }
        self.meta.take_sorted_flags();
        // Early return if any dimension is 0, because there are no elements
        if self.shape[depth..].contains(&0) || depth > 0 && self.shape[depth - 1] == 0 {
            self.shape[depth..].reverse();
            return;
        }
        let subshape = Shape::from(&self.shape[depth..]);
        let newshape = Shape::from_iter(subshape.iter().rev().copied());
        let chunk_size = subshape.elements();
        let data_slice = self.data.as_mut_slice();
        let mut temp = data_slice[..chunk_size].to_vec();
        let mut dims = vec![0; subshape.len()];
        // Divide the array into chunks at the given depth
        for data in data_slice.chunks_exact_mut(chunk_size) {
            for i in 0..chunk_size {
                subshape.flat_to_dims(i, &mut dims);
                dims.reverse();
                let j = newshape.dims_to_flat(&dims).unwrap();
                temp[j] = data[i].clone();
            }
            data.swap_with_slice(&mut temp);
        }
        self.shape[depth..].reverse();
    }
}

impl Value {
    /// `classify` the rows of the value
    pub fn classify(&self) -> Self {
        if self.rank() == 0 {
            return 0.into();
        }
        let map_keys = self.meta.map_keys.clone();
        let mut val: Value = val_as_arr!(self, Array::classify).into_iter().collect();
        if let Some(map_keys) = map_keys {
            val.meta.map_keys = Some(map_keys);
        }
        val.meta.mark_sorted_up(self.meta.is_sorted_up());
        val.validate();
        val
    }
    pub(crate) fn classify_depth(&self, depth: usize) -> Self {
        let map_keys = self.meta.map_keys.clone();
        let mut val = val_as_arr!(self, |a| a.classify_depth(depth));
        if let Some(map_keys) = map_keys {
            val.meta.map_keys = Some(map_keys);
        }
        val.validate();
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
    /// Count which occurrence of each row that row is
    pub fn occurrences(&self) -> Array<f64> {
        val_as_arr!(self, Array::occurrences)
    }
}

impl<T: ArrayValue> Array<T> {
    /// `classify` the rows of the array
    pub fn classify(&self) -> Vec<usize> {
        let mut classified = Vec::with_capacity(self.row_count());
        if self.meta.is_sorted_up() {
            let mut rows = self.row_slices().map(ArrayCmpSlice);
            if let Some(mut prev) = rows.next() {
                let mut curr_class = 0;
                classified.push(curr_class);
                for row in rows {
                    if row != prev {
                        curr_class += 1;
                        prev = row;
                    }
                    classified.push(curr_class);
                }
            }
            classified
        } else {
            let mut classes = HashMap::new();
            for row in self.row_slices().map(ArrayCmpSlice) {
                let new_class = classes.len();
                let class = *classes.entry(row).or_insert(new_class);
                classified.push(class);
            }
            classified
        }
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
        let val: Value = if row_row_count < 256 {
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
        };
        val.validate();
        val
    }
    /// `deduplicate` the rows of the array
    pub fn deduplicate(&mut self, env: &Uiua) -> UiuaResult {
        if self.rank() == 0 {
            return Ok(());
        }
        let map_keys_unique = self
            .meta
            .take_map_keys()
            .map(|keys| (keys.normalized(), self.unique()));
        let row_count = self.row_count();
        let row_len = self.row_len();
        let mut new_len = 0;
        if self.meta.is_sorted_up() || self.meta.is_sorted_down() {
            if row_count > 0 {
                let slice = self.data.as_mut_slice();
                if row_count > 0 {
                    new_len += 1;
                }
                for i in 1..row_count {
                    let prev_start = (new_len - 1) * row_len;
                    let prev_end = prev_start + row_len;
                    let curr_start = i * row_len;
                    let curr_end = curr_start + row_len;
                    if ArrayCmpSlice(&slice[prev_start..prev_end])
                        != ArrayCmpSlice(&slice[curr_start..curr_end])
                    {
                        for j in 0..row_len {
                            slice[new_len * row_len + j] = slice[curr_start + j].clone();
                        }
                        new_len += 1;
                    }
                }
                self.data.truncate(new_len * row_len);
            }
        } else {
            let mut seen = HashSet::new();
            let mut deduped = CowSlice::new();
            for row in self.row_slices() {
                if seen.insert(ArrayCmpSlice(row)) {
                    deduped.extend_from_slice(row);
                    new_len += 1;
                }
            }
            self.data = deduped;
        }
        self.shape[0] = new_len;
        if let Some((keys, unique)) = map_keys_unique {
            let keys = Value::from(unique).keep(keys, env)?;
            self.map(keys, env)?;
        }
        self.validate();
        Ok(())
    }
    /// Mask the `unique` rows of the array
    pub fn unique(&self) -> Array<u8> {
        if self.rank() == 0 {
            return 1u8.into();
        }
        let map_keys = self.meta.map_keys.clone();
        let mut seen = HashSet::new();
        let mut mask = eco_vec![0u8; self.row_count()];
        let mask_slice = mask.make_mut();
        for (i, row) in self.row_slices().enumerate() {
            if seen.insert(ArrayCmpSlice(row)) {
                mask_slice[i] = 1;
            }
        }
        let mut arr = Array::new([self.row_count()], mask);
        arr.meta.flags.set(ArrayFlags::BOOLEAN, true);
        arr.meta.map_keys = map_keys;
        arr
    }
    /// Count the number of unique rows in the array
    pub fn count_unique(&self) -> usize {
        let mut seen = HashSet::new();
        self.row_slices()
            .filter(|row| seen.insert(ArrayCmpSlice(row)))
            .count()
    }
    /// Count which occurrence of each row that row is
    pub fn occurrences(&self) -> Array<f64> {
        let mut data = eco_vec![0.0; self.row_count()];
        let shape: Shape = self.shape.iter().take(1).copied().collect();
        if self.row_count() == 0 {
            return Array::new(shape, data);
        }
        let slice = data.make_mut();
        if self.meta.is_sorted_up() || self.meta.is_sorted_down() {
            let mut rows = self.row_slices().map(ArrayCmpSlice);
            let mut prev = rows.next().unwrap();
            slice[0] = 0.0;
            let mut next_count = 1.0;
            for (row, count) in rows.zip(slice.iter_mut().skip(1)) {
                if row == prev {
                    *count = next_count;
                    next_count += 1.0;
                } else {
                    *count = 0.0;
                    next_count = 1.0;
                }
                prev = row;
            }
        } else {
            let mut counts = HashMap::new();
            for (row, count) in self.row_slices().zip(slice.iter_mut()) {
                let curr = counts.entry(ArrayCmpSlice(row)).or_insert(0.0);
                *count = *curr;
                *curr += 1.0;
            }
        }
        Array::new(shape, data)
    }
}

impl Value {
    /// Encode the `bits` of the value
    pub fn bits(&self, count: Option<usize>, env: &Uiua) -> UiuaResult<Value> {
        match self {
            Value::Byte(n) => n.bits(count, env),
            Value::Num(n) => n.bits(count, env),
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
            Value::Byte(n) => n.bits_impl(min_bits_len, None, env),
            Value::Num(n) => n.bits_impl(min_bits_len, None, env),
            _ => Err(env.error("Argument to undo un bits must be an array of integers")),
        }
    }
}

impl<T: RealArrayValue> Array<T> {
    /// Encode the `bits` of the array
    pub fn bits(&self, count: Option<usize>, env: &Uiua) -> UiuaResult<Value> {
        self.bits_impl(0, count, env)
    }
    fn bits_impl(
        &self,
        min_bits_len: usize,
        count: Option<usize>,
        env: &Uiua,
    ) -> UiuaResult<Value> {
        let mut nats = Vec::with_capacity(self.data.len());
        let mut negatives = Vec::with_capacity(self.data.len());
        let mut any_neg = false;
        for &n in &self.data {
            if !n.is_int() {
                return Err(env.error(format!(
                    "Array must be a list of integers, but {} is not an integer",
                    n.grid_string(false)
                )));
            }
            let n = n.to_f64();
            if n.abs() > u128::MAX as f64 {
                return Err(env.error(format!(
                    "{} is too large for the {} algorithm",
                    n.grid_string(false),
                    Primitive::Bits.format()
                )));
            }
            let mut nat = n.abs().round() as u128;
            if let Some(count) = count {
                nat &= (1 << count) - 1;
            }
            nats.push(nat);
            negatives.push(n < 0.0);
            any_neg |= n < 0.0;
        }
        let bit_count = if let Some(count) = count {
            count
        } else {
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
            max_bits.max(min_bits_len)
        };
        let mut shape = self.shape.clone();
        shape.push(bit_count);
        let val: Value = if any_neg {
            // If any number is negative, make a f64 array
            let mut new_data = eco_vec![0.0; self.data.len() * bit_count];
            let new_data_slice = new_data.make_mut();
            // LSB first
            for (i, (n, is_neg)) in nats.into_iter().zip(negatives).enumerate() {
                for j in 0..bit_count {
                    let index = i * bit_count + j;
                    new_data_slice[index] = u8::from(n & (1 << j) != 0) as f64;
                    if is_neg {
                        new_data_slice[index] = -new_data_slice[index];
                    }
                }
            }
            Array::new(shape, new_data).into()
        } else {
            // If all numbers are natural, make a u8 array
            let mut new_data = eco_vec![0; self.data.len() * bit_count];
            let new_data_slice = new_data.make_mut();
            // LSB first
            for (i, n) in nats.into_iter().enumerate() {
                for j in 0..bit_count {
                    let index = i * bit_count + j;
                    new_data_slice[index] = u8::from(n & (1 << j) != 0);
                }
            }
            let mut arr = Array::new(shape, new_data);
            arr.meta.flags.set(ArrayFlags::BOOLEAN, true);
            arr.into()
        };
        val.validate();
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
                    "Array must be a list of integers, but {} is not an integer",
                    n.grid_string(false)
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
        let mut val: Value = match self.rank() {
            0 => {
                validate_size::<u8>([total], env)?;
                let data = eco_vec![0u8; total];
                let mut arr = Array::new([total], data);
                arr.meta.mark_sorted_down(true);
                arr.into()
            }
            1 => {
                validate_size::<f64>([total], env)?;
                let mut data = EcoVec::with_capacity(total);
                for (i, &b) in counts.data.iter().enumerate() {
                    let i = i as f64;
                    for _ in 0..b {
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
        };
        val.meta.mark_sorted_up(true);
        val.validate();
        Ok(val)
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
                        .map(|fv| fv.value.into())
                        .map_err(|e| env.error(format!("Cannot take first of an empty array{e}")))
                }
                Value::Byte(bytes) => {
                    for (i, n) in byte_iter(bytes.data.iter().enumerate()) {
                        if *n != 0 {
                            return Ok(Array::scalar(i as f64));
                        }
                    }
                    env.scalar_fill::<f64>()
                        .map(|fv| fv.value.into())
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
                        .map(|fv| fv.value.into())
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
                        .map(|fv| fv.value.into())
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
        Ok(match &*self.shape {
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
        if self.row_count() <= 1
            || self.rank() == 1 && self.meta.is_sorted_up() && self.meta.is_sorted_down()
        {
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
        let fill = env.scalar_fill::<f64>();
        if self.rank() == 0 || self.meta.is_sorted_up() && fill.is_err() {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return fill
                .map(|fv| fv.value)
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
                .map(|fv| fv.value)
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
                .map(|fv| fv.value)
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
        let fill = env.scalar_fill::<f64>();
        if self.rank() == 0 || self.meta.is_sorted_up() && fill.is_err() {
            return Ok(0.0);
        }
        if self.row_count() == 0 {
            return fill
                .map(|fv| fv.value)
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

        if self.shape.elements() == 1 {
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
            shape.prepend(primes.len());
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
        shape.prepend(longest);
        Ok(Array::new(shape, data))
    }
}

impl Value {
    /// Convert a value from RGB to HSV
    pub fn rgb_to_hsv(self, env: &Uiua) -> UiuaResult<Self> {
        match self {
            Value::Num(arr) => arr.rgb_to_hsv(env).map(Into::into),
            Value::Byte(arr) => arr.convert_ref::<f64>().rgb_to_hsv(env).map(Into::into),
            val => Err(env.error(format!("Cannot convert {} to HSV", val.type_name_plural()))),
        }
    }
    /// Convert a value from HSV to RGB
    pub fn hsv_to_rgb(self, env: &Uiua) -> UiuaResult<Self> {
        match self {
            Value::Num(arr) => arr.hsv_to_rgb(env).map(Into::into),
            Value::Byte(arr) => arr.convert_ref::<f64>().hsv_to_rgb(env).map(Into::into),
            val => Err(env.error(format!("Cannot convert {} to RGB", val.type_name_plural()))),
        }
    }
}

impl Array<f64> {
    /// Convert an array from RGB to HSV
    pub fn rgb_to_hsv(mut self, env: &Uiua) -> UiuaResult<Self> {
        if !(self.shape.ends_with(&[3]) || self.shape.ends_with(&[4])) {
            return Err(env.error(format!(
                "Array to convert to HSV must have a shape \
                ending with 3 or 4, but its shape is {}",
                self.shape
            )));
        }
        let channels = *self.shape.last().unwrap();
        for rgb in self.data.as_mut_slice().chunks_exact_mut(channels) {
            let [r, g, b, ..] = *rgb else {
                unreachable!();
            };
            let max = r.max(g).max(b);
            let min = r.min(g).min(b);
            let delta = max - min;
            let recip_delta = if delta != 0.0 { 1.0 / delta } else { 0.0 };
            let h = if delta != 0.0 {
                (TAU * if max == r {
                    ((g - b) * recip_delta).rem_euclid(6.0)
                } else if max == g {
                    (b - r).mul_add(recip_delta, 2.0)
                } else {
                    (r - g).mul_add(recip_delta, 4.0)
                }) / 6.0
            } else {
                0.0
            };
            let s = if max == 0.0 { 0.0 } else { 1.0 - min / max };
            let v = max;
            rgb[0] = h;
            rgb[1] = s;
            rgb[2] = v;
        }
        self.meta.take_sorted_flags();
        self.validate();
        Ok(self)
    }
    /// Convert an array from HSV to RGB
    pub fn hsv_to_rgb(mut self, env: &Uiua) -> UiuaResult<Self> {
        if !(self.shape.ends_with(&[3]) || self.shape.ends_with(&[4])) {
            return Err(env.error(format!(
                "Array to convert to RBG must have a shape \
                ending with 3 or 4, but its shape is {}",
                self.shape
            )));
        }
        let channels = *self.shape.last().unwrap();
        for hsv in self.data.as_mut_slice().chunks_exact_mut(channels) {
            let [h, s, v, ..] = *hsv else {
                unreachable!();
            };
            let [r, g, b] = hsv_to_rgb(h, s, v);
            hsv[0] = r;
            hsv[1] = g;
            hsv[2] = b;
        }
        self.meta.take_sorted_flags();
        self.validate();
        Ok(self)
    }
}

pub(crate) fn hsv_to_rgb(h: f64, s: f64, v: f64) -> [f64; 3] {
    let h = h / TAU * 6.0;
    let i = h.floor() as isize;
    let f = h - i as f64;
    let p = v * (1.0 - s);
    let q = v * (1.0 - f * s);
    let t = v * (1.0 - (1.0 - f) * s);
    match i.rem_euclid(6) {
        0 => [v, t, p],
        1 => [q, v, p],
        2 => [p, v, t],
        3 => [p, q, v],
        4 => [t, p, v],
        _ => [v, p, q],
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
                    let bool_lit = arr.meta.flags.contains(ArrayFlags::BOOLEAN_LITERAL);
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
                    let bool_lit = arr.meta.flags.contains(ArrayFlags::BOOLEAN_LITERAL);
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
        if let Some(map_keys) = &self.meta.map_keys {
            s = format!(
                "map {} {}",
                map_keys.clone().normalized().representation(),
                s
            );
        }
        if let Some(label) = &self.meta.label {
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
            let dur = time::Duration::checked_seconds_f64(n).ok_or_else(|| {
                env.error(format!("{} is not a valid time", n.grid_string(false)))
            })?;
            let dt = if n >= 0.0 {
                OffsetDateTime::UNIX_EPOCH.checked_add(dur)
            } else {
                OffsetDateTime::UNIX_EPOCH.checked_sub(dur)
            }
            .ok_or_else(|| env.error(format!("{} is not a valid time", n.grid_string(false))))?;
            slice[i * 6] = dt.year() as f64;
            slice[i * 6 + 1] = dt.month() as u8 as f64;
            slice[i * 6 + 2] = dt.day() as f64;
            slice[i * 6 + 3] = dt.hour() as f64;
            slice[i * 6 + 4] = dt.minute() as f64;
            slice[i * 6 + 5] = dt.second() as f64;
        }
        arr.data = new_data.into();
        arr.shape.push(6);
        arr.meta.reset_flags();
        arr.validate();
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
        arr.validate();
        Ok(arr)
    }
}
