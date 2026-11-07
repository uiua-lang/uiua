//! Pretty printing Uiua arrays

use std::{
    any::type_name,
    borrow::Cow,
    collections::HashMap,
    f64::consts::{E, PI, SQRT_2, TAU},
    iter::{once, repeat_n},
    mem::take,
};

use ecow::EcoString;
use smallvec::SmallVec;

use crate::{
    ArrayCmp, ArrayMeta, Complex, Primitive, WILDCARD_CHAR, WILDCARD_NAN,
    algorithm::map::{EMPTY_CHAR, EMPTY_NAN, TOMBSTONE_CHAR, TOMBSTONE_NAN},
    array::{Array, ArrayValue},
    boxed::Boxed,
    terminal_size, val_as_arr,
    value::Value,
};

type Grid<T = char> = Vec<Vec<T>>;
type Metagrid = Grid<Grid>;

#[derive(Debug, Clone, Copy, Default)]
pub struct GridFmtParams {
    pub(crate) label: bool,
    pub(crate) depth: usize,
    pub(crate) parent_rank: usize,
    /// The maximum rank among sibling boxes within the parent box array.
    /// 0 if this array is not being formatted as a boxed child.
    pub(crate) max_boxed_rank: usize,
    /// The maximum length among sibling boxes within the parent box array.
    /// 0 if this array is not being formatted as a boxed child.
    pub(crate) max_boxed_len: usize,
    /// Whether this array is being formatted as one "cell" of a "struct of arrays".
    /// This is the case if all sibling boxes have labels, matching lengths, and no map keys.
    /// If so, the array being formatted is not actually the entire array, but one row of it.
    pub(crate) soa_row: bool,
    /// Whether multiline formatting should be used,
    /// even if this array can be formatted as one line.
    /// This is the case if any sibling boxes require multiline formatting.
    pub(crate) multiline: bool,
}

pub trait GridFmt: Sized {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid;
    fn grid_string(&self, label: bool) -> String {
        let mut s: String = self
            .fmt_grid(GridFmtParams {
                label,
                ..Default::default()
            })
            .into_iter()
            .flat_map(|v| v.into_iter().chain(once('\n')))
            .collect();
        s.pop();
        s
    }
    /// Delimiters for formatting
    fn format_delims() -> (&'static str, &'static str) {
        ("[", "]")
    }
    /// Marker for empty lists in grid formatting
    fn empty_list_inner() -> &'static str {
        ""
    }
    /// How much to separate items when formatting
    fn separation() -> usize {
        1
    }
    /// Delimiters for grid formatting
    fn grid_fmt_delims() -> (char, char) {
        ('[', ']')
    }
    /// Whether to compress all items of a list when grid formatting
    fn compress_list_grid() -> bool {
        false
    }
    /// Whether to divide cells with box drawing lines when grid formatting
    fn box_lines() -> bool {
        false
    }
    /// Whether to allow list elements to be shown on the same line
    fn list_same_line() -> bool {
        true
    }
    /// Summarize the elements of an array of this type
    fn summarize(_: &[Self]) -> String {
        String::new()
    }
    /// The minimum number of elements that require a summary
    fn summary_min_elems() -> usize {
        3600
    }
    /// The maximum number of distinct elements that can be summarized using the multiset format
    fn summary_multiset_max_distinct() -> usize {
        5
    }
    /// How to align elements when formatting
    fn alignment() -> ElemAlign {
        ElemAlign::Left
    }
    /// How to determine the maximum width of a formatted column
    fn max_col_width<'a>(rows: impl Iterator<Item = &'a [char]> + Clone) -> usize {
        rows.map(|row| row.len()).max().unwrap_or(0)
    }
    /// Get SoA rows
    fn soa_rows(_arr: &Array<Self>) -> Option<Vec<(&EcoString, &Value)>> {
        None
    }
}

impl GridFmt for u8 {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        vec![self.to_string().chars().collect()]
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        if let Some(s) = multiset_summary(elems) {
            return s;
        }
        let mut min = u8::MAX;
        let mut max = 0;
        for &elem in elems {
            min = min.min(elem);
            max = max.max(elem);
        }
        let sum: u64 = elems.iter().map(|&e| e as u64).sum();
        let mean = (sum as f64) / (elems.len() as f64);
        if min == max {
            format!("all {}", min.grid_string(false))
        } else {
            format!(
                "{}-{} μ{}",
                min.grid_string(false),
                max.grid_string(false),
                round_sig_dec(mean, 4).grid_string(false)
            )
        }
    }
    fn alignment() -> ElemAlign {
        ElemAlign::Right
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        let f = *self;
        let positive = f.abs();
        let is_neg = f.is_sign_negative();
        let minus = if is_neg { "¯" } else { "" };
        let s = if f.to_bits() == EMPTY_NAN.to_bits() {
            "∅".into()
        } else if f.to_bits() == TOMBSTONE_NAN.to_bits() {
            "⊥".into()
        } else if f.to_bits() == WILDCARD_NAN.to_bits() {
            "W".into()
        } else if positive.fract() == 0.0 || positive.is_nan() {
            format!("{minus}{positive}")
        } else if (positive - PI).abs() <= f64::EPSILON {
            format!("{minus}π")
        } else if (positive - TAU).abs() <= f64::EPSILON {
            format!("{minus}τ")
        } else if (positive - PI / 2.0).abs() <= f64::EPSILON {
            format!("{minus}η")
        } else if (positive - E).abs() <= f64::EPSILON {
            format!("{minus}e")
        } else if positive == f64::INFINITY {
            format!("{minus}∞")
        } else if positive.fract() != 0.0
            && let Some((num, denom, approx)) =
                [1u8, 2, 3, 4, 5, 6, 8, 9, 12].iter().find_map(|&denom| {
                    let num = (positive * denom as f64) / TAU;
                    let rounded = num.round();
                    if rounded <= 100.0
                        && (num - rounded).abs() <= 8.0 * f64::EPSILON
                        && rounded != 0.0
                    {
                        Some((rounded, denom, num != rounded))
                    } else {
                        None
                    }
                })
        {
            let prefix = if approx { "~" } else { "" };
            if denom == 1 {
                format!("{prefix}{minus}{num}τ")
            } else if denom == 2 {
                if num == 1.0 {
                    format!("{prefix}{minus}π")
                } else {
                    format!("{prefix}{minus}{num}π")
                }
            } else if denom == 4 {
                if num == 1.0 {
                    format!("{prefix}{minus}η")
                } else {
                    format!("{prefix}{minus}{num}η")
                }
            } else if num == 1.0 {
                format!("{prefix}{minus}τ/{denom}")
            } else {
                format!("{prefix}{minus}{num}τ/{denom}")
            }
        } else if positive <= 2.5
            && positive.fract() != 0.0
            && let Some((.., sqr)) = [
                (SQRT_2, 2.0, "2"),
                (SQRT_2 / 2.0, 0.5, "½"),
                (3f64.sqrt(), 3.0, "3"),
                (3f64.sqrt() / 2.0, 0.75, "¾"),
                (3f64.sqrt() / 3.0, 1.0 / 3.0, "⅓"),
                (5f64.sqrt(), 5.0, "5"),
                (5f64.sqrt() / 5.0, 0.2, "⅕"),
            ]
            .into_iter()
            .find(|(sqrt, ..)| (sqrt - positive).abs() <= f64::EPSILON)
        {
            format!("{minus}√{sqr}")
        } else {
            let mut pos_formatted = positive.to_string();
            if pos_formatted.len() >= 17 {
                let mut consecutive_start = 0;
                let mut consecutive_len = 0;
                let mut hit_decimal = false;
                for (i, c) in pos_formatted.chars().enumerate() {
                    if c == '.' {
                        hit_decimal = true;
                    } else if !hit_decimal {
                        continue;
                    }
                    let local_len = pos_formatted
                        .chars()
                        .skip(i + 1)
                        .take_while(|&d| d == c)
                        .count();
                    if local_len > consecutive_len {
                        consecutive_start = i;
                        consecutive_len = local_len;
                    }
                }
                if consecutive_len >= 5 {
                    if consecutive_start + consecutive_len + 1 == pos_formatted.len() {
                        pos_formatted.replace_range(consecutive_start + 3.., "…")
                    } else {
                        pos_formatted.replace_range(
                            consecutive_start + 2..consecutive_start + consecutive_len,
                            "…",
                        )
                    }
                }
            }
            if is_neg {
                format!("{minus}{pos_formatted}")
            } else {
                pos_formatted
            }
        };
        vec![s.chars().collect()]
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        if let Some(s) = multiset_summary(elems) {
            return s;
        }
        if elems.iter().all(|&n| n.is_nan()) {
            return "all NaN".into();
        }
        let mut min = f64::NAN;
        let mut max = f64::NAN;
        let mut nan_count = elems.iter().take_while(|n| n.is_nan()).count();
        let mut mean = 0.0;
        let mut i = 0;
        let mut inf_balance = 0i64;
        for &elem in &elems[nan_count..] {
            if elem.is_nan() {
                nan_count += 1;
            } else if elem.is_infinite() {
                inf_balance += elem.signum() as i64;
                min = min.min(elem);
                max = max.max(elem);
            } else {
                min = min.min(elem);
                max = max.max(elem);
                mean += (elem - mean) / (i + 1) as f64;
                i += 1;
            }
        }
        if inf_balance != 0 {
            mean = inf_balance.signum() as f64 * f64::INFINITY;
        }
        if min == max {
            if nan_count == 0 {
                format!("all {}", min.grid_string(false))
            } else {
                format!(
                    "{} {}s and {} NaNs",
                    elems.len() - nan_count,
                    min.grid_string(false),
                    nan_count
                )
            }
        } else {
            let mut s = format!(
                "{}-{} μ{}",
                round_sig_dec(min, 4).grid_string(false),
                round_sig_dec(max, 4).grid_string(false),
                round_sig_dec(mean, 4).grid_string(false)
            );
            if nan_count > 0 {
                s.push_str(&format!(
                    " ({nan_count} NaN{})",
                    if nan_count > 1 { "s" } else { "" }
                ));
            }
            s
        }
    }
    fn alignment() -> ElemAlign {
        ElemAlign::DelimOrRight(".")
    }
    fn max_col_width<'a>(rows: impl Iterator<Item = &'a [char]>) -> usize {
        let mut max_whole_len = 0;
        let mut max_dec_len: Option<usize> = None;
        for row in rows {
            if let Some(dot_pos) = row.iter().position(|&c| c == '.') {
                max_whole_len = max_whole_len.max(dot_pos);
                max_dec_len = max_dec_len.max(Some(row.len() - dot_pos - 1));
            } else {
                max_whole_len = max_whole_len.max(row.len());
            }
        }
        if let Some(dec_len) = max_dec_len {
            max_whole_len + dec_len + 1
        } else {
            max_whole_len
        }
    }
}

impl GridFmt for Complex {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        if self.im.abs() == 0.0 {
            self.re.fmt_grid(params)
        } else if self.re.abs() == 0.0 {
            if self.im == 1.0 {
                vec![vec!['i']]
            } else if self.im == -1.0 {
                vec![vec!['¯', 'i']]
            } else {
                let mut grid = self.im.fmt_grid(params);
                grid[0].push('i');
                grid
            }
        } else {
            let mut re = self.re.fmt_grid(params);
            let im = if self.im.abs() == 1.0 {
                String::new()
            } else {
                self.im.abs().grid_string(params.label)
            };
            let sign = if self.im < 0.0 { '-' } else { '+' };
            re[0].push(sign);
            re[0].extend(im.chars());
            re[0].push('i');
            re
        }
    }
    fn empty_list_inner() -> &'static str {
        "ℂ"
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        if let Some(s) = multiset_summary(elems) {
            return s;
        }
        let (mut re_min, mut im_min) = (f64::INFINITY, f64::INFINITY);
        let (mut re_max, mut im_max) = (f64::NEG_INFINITY, f64::NEG_INFINITY);
        let (mut re_mean, mut im_mean) = (0.0, 0.0);
        let (mut re_nan_count, mut im_nan_count) = (0, 0);
        let (mut re_inf_balance, mut im_inf_balance) = (0i64, 0i64);
        let (mut re_i, mut im_i) = (0, 0);
        for &elem in elems {
            for ((elem, i), (min, max, mean), (nan_count, inf_balance)) in [
                (
                    (elem.re, &mut re_i),
                    (&mut re_min, &mut re_max, &mut re_mean),
                    (&mut re_nan_count, &mut re_inf_balance),
                ),
                (
                    (elem.im, &mut im_i),
                    (&mut im_min, &mut im_max, &mut im_mean),
                    (&mut im_nan_count, &mut im_inf_balance),
                ),
            ] {
                if elem.is_nan() {
                    *nan_count += 1;
                } else if elem.is_infinite() {
                    *inf_balance += elem.signum() as i64;
                    *min = min.min(elem);
                    *max = max.max(elem);
                } else {
                    *min = min.min(elem);
                    *max = max.max(elem);
                    *mean += (elem - *mean) / (*i + 1) as f64;
                    *i += 1;
                }
            }
        }
        for (inf_balance, mean) in [
            (re_inf_balance, &mut re_mean),
            (im_inf_balance, &mut im_mean),
        ] {
            if inf_balance != 0 {
                *mean = inf_balance.signum() as f64 * f64::INFINITY;
            }
        }
        if re_min == re_max && im_min == im_max {
            format!("all {}", Complex::new(re_min, im_min).grid_string(false))
        } else {
            let min = Complex::new(round_sig_dec(re_min, 3), round_sig_dec(im_min, 3));
            let max = Complex::new(round_sig_dec(re_max, 3), round_sig_dec(im_max, 3));
            let mean = Complex::new(round_sig_dec(re_mean, 3), round_sig_dec(im_mean, 3));
            format!(
                "{} - {} μ{}",
                min.grid_string(false),
                max.grid_string(false),
                mean.grid_string(false)
            )
        }
    }
}

#[cfg(feature = "ga")]
impl GridFmt for crate::Multivector {
    fn fmt_grid(&self, _: GridFmtParams) -> Grid {
        let dims = self.dims();
        let dim_offset = (self.flavor.metric(0) != 0) as usize;
        let (mask_table, _) = crate::ga::mask_tables(dims);
        let mut s = Vec::new();
        for (i, n) in self.iter().enumerate() {
            if n == 0.0 {
                continue;
            }
            if s.is_empty() {
                if n < 0.0 {
                    s.push('¯');
                }
            } else {
                s.extend(if n > 0.0 { " + " } else { " - " }.chars());
            }
            let mask = mask_table[i];
            if n.abs() != 1.0 || mask == 0 {
                let n_grid = n.abs().fmt_grid(Default::default());
                s.extend(n_grid.into_iter().next().unwrap());
            }
            if mask == 0 {
                continue;
            }
            s.push('e');
            for j in 0..dims {
                if mask & (1 << j) != 0 {
                    s.push(crate::SUBSCRIPT_DIGITS[j as usize + dim_offset]);
                }
            }
            if dims == 3 && mask == 0b101 {
                let (a, b) = (s.len() - 1, s.len() - 2);
                s.swap(a, b);
            }
        }
        if s.is_empty() {
            s = vec!['0'];
        }
        vec![s]
    }
    fn list_same_line() -> bool {
        false
    }
    fn separation() -> usize {
        2
    }
    fn summary_min_elems() -> usize {
        100
    }
    fn summarize(mvs: &[Self]) -> String {
        let mut dims = Vec::new();
        let mut flavors = Vec::new();
        let mut min_mag = 0f64;
        let mut max_mag = 0f64;
        let mut grades = Vec::new();
        for mv in mvs {
            dims.push(mv.dims().saturating_sub(mv.flavor.dim_adjustment()));
            dims.sort();
            dims.dedup();
            flavors.push(mv.flavor);
            flavors.sort();
            flavors.dedup();
            for (g, b) in mv.grades().enumerate() {
                if b.iter().any(|&b| b != 0.0) {
                    grades.push(g);
                }
            }
            grades.sort();
            grades.dedup();
            let mag = mv.magnitude();
            min_mag = min_mag.min(mag);
            max_mag = max_mag.max(mag);
        }
        let dims = match dims.len() {
            1 => {
                format!("{}D", dims[0])
            }
            2.. if dims.windows(2).all(|w| w[1] - w[0] == 1) => {
                format!("{}-{}D", dims[0], dims.last().unwrap())
            }
            _ => {
                let mut s = String::new();
                for (i, d) in dims.iter().enumerate() {
                    if i > 0 {
                        s.push(' ');
                    }
                    s.push_str(&d.to_string());
                    s.push('D');
                }
                s
            }
        };
        let grades = match grades.len() {
            0 => "no grades".into(),
            1 => {
                format!("grade {}", grades[0])
            }
            2.. if grades.windows(2).all(|w| w[1] - w[0] == 1) => {
                format!("grades {}-{}", grades[0], grades.last().unwrap())
            }
            _ => {
                let mut s = "grades".to_string();
                for g in grades {
                    s.push(' ');
                    s.push_str(&g.to_string());
                }
                s
            }
        };
        let flavors = match &*flavors {
            [] => "".into(),
            [fl] => fl.to_string(),
            flavors => {
                let mut s = String::new();
                for (i, fl) in flavors.iter().enumerate() {
                    if i > 0 {
                        s.push('/');
                    }
                    s.push_str(&fl.to_string());
                }
                s
            }
        };
        format!(
            "{dims} {flavors} ({grades}) ⌵{}-{}",
            min_mag.grid_string(false),
            max_mag.grid_string(false)
        )
    }
}

impl GridFmt for Value {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        if params.depth > 100 {
            return vec!["…".to_string().chars().collect()];
        }
        match self {
            Value::Num(n) => n.fmt_grid(params),
            Value::Byte(b) => b.fmt_grid(params),
            Value::Complex(c) => c.fmt_grid(params),
            Value::Char(c) => c.fmt_grid(params),
            #[cfg(feature = "ga")]
            Value::Mv(m) => m.fmt_grid(params),
            Value::Box(arr) => {
                fn val_is_multiline(val: &Value) -> bool {
                    if val.rank() > 1 {
                        return true;
                    }
                    match val {
                        Value::Box(arr) => (arr.data.iter())
                            .any(|Boxed(v)| v.meta.label.is_some() || val_is_multiline(v)),
                        _ => false,
                    }
                }
                let max_boxed_rank = arr.data.iter().map(|Boxed(v)| v.rank()).max().unwrap_or(0);
                arr.fmt_grid(GridFmtParams {
                    depth: params.depth + 1,
                    max_boxed_rank,
                    multiline: val_is_multiline(self),
                    max_boxed_len: (arr.data.iter())
                        .filter(|Boxed(v)| v.rank() == max_boxed_rank)
                        .map(|Boxed(v)| v.row_count())
                        .max()
                        .unwrap_or(0),
                    ..params
                })
            }
        }
    }
}

pub fn format_char_inner_repr(c: char) -> Cow<'static, str> {
    match c {
        char::MAX => return Cow::Borrowed(r"\_"),
        WILDCARD_CHAR => return Cow::Borrowed(r"\W"),
        ' ' => return Cow::Borrowed(r"\s"),
        _ => {}
    }
    let formatted = format!("{c:?}");
    if c == '\'' {
        Cow::Borrowed("'")
    } else {
        Cow::Owned(if formatted.starts_with("'\\u{") {
            let n = c as u32;
            if n < 128 {
                format!("\\x{n:02x}")
            } else if n < 16u32.pow(4) {
                format!("\\u{n:04x}")
            } else {
                format!("\\u{{{n:x}}}")
            }
        } else {
            formatted[1..formatted.len() - 1].to_string()
        })
    }
}

pub fn format_char_inner(c: char) -> Cow<'static, str> {
    match c {
        EMPTY_CHAR => Cow::Borrowed(r"\∅"),
        TOMBSTONE_CHAR => Cow::Borrowed(r"\⊥"),
        ' ' => Cow::Borrowed(" "),
        _ => format_char_inner_repr(c),
    }
}

impl GridFmt for char {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        vec![once('@').chain(format_char_inner(*self).chars()).collect()]
    }
    fn format_delims() -> (&'static str, &'static str) {
        ("", "")
    }
    fn separation() -> usize {
        0
    }
    fn grid_fmt_delims() -> (char, char) {
        ('"', '"')
    }
    fn compress_list_grid() -> bool {
        true
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        if let Some(s) = multiset_summary(elems) {
            return s;
        }
        let mut parts = Vec::new();
        let lowercase = elems.iter().any(|c| c.is_lowercase());
        let uppercase = elems.iter().any(|c| c.is_uppercase());
        let writing = elems
            .iter()
            .any(|c| c.is_alphabetic() && !(c.is_lowercase() || c.is_uppercase()));
        let numeric = elems.iter().any(|c| c.is_numeric() && !c.is_ascii_digit());
        let digit = elems.iter().any(|c| c.is_ascii_digit());
        let punct = elems.iter().any(|c| c.is_ascii_punctuation());
        let whitespace = elems.iter().any(|c| c.is_whitespace());
        let control = elems.iter().any(|c| c.is_control());
        let other = (elems.iter()).any(|c| {
            !(c.is_lowercase()
                || c.is_uppercase()
                || c.is_alphabetic()
                || c.is_numeric()
                || c.is_ascii_punctuation()
                || c.is_whitespace()
                || c.is_control())
        });
        if writing {
            parts.push("writing");
        } else if lowercase && uppercase {
            parts.push("letters");
        } else if lowercase {
            parts.push("lower");
        } else if uppercase {
            parts.push("upper");
        }
        if numeric {
            parts.push("nums");
        }
        if digit {
            parts.push("digits");
        }
        if punct {
            parts.push("punct");
        }
        if whitespace {
            parts.push("whitespace");
        }
        if control {
            parts.push("control");
        }
        if other {
            parts.push("other");
        }
        match parts.len() {
            0 => String::new(),
            1 => parts[0].to_string(),
            2 => format!("{} and {}", parts[0], parts[1]),
            _ => {
                let mut s = String::new();
                for (i, &part) in parts.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(part);
                }
                s
            }
        }
    }
}

impl GridFmt for Boxed {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        let mut grid = self.0.fmt_grid(params);
        // println!(
        //     "  shape: {}, parent rank: {}, is box: {}",
        //     self.0.shape,
        //     params.parent_rank,
        //     matches!(self.0, Value::Box(_))
        // );
        if (params.soa_row || params.multiline || params.parent_rank == 0)
            && (params.parent_rank == 0
                || self.0.shape.is_empty() && !(matches!(self.0, Value::Box(_) | Value::Char(_))))
        {
            let symbol = if params.parent_rank == 0 {
                Primitive::Box.glyph().unwrap()
            } else {
                '∙'
            };
            match grid.len() {
                0 => {}
                1 => grid = vec![once(symbol).chain(grid.into_iter().flatten()).collect()],
                height => {
                    for row in &mut grid {
                        row.insert(0, ' ');
                    }
                    grid[height / 2][0] = symbol;
                }
            }
        }
        grid
    }
    fn empty_list_inner() -> &'static str {
        "□"
    }
    fn box_lines() -> bool {
        true
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        let smallest_rank = elems.iter().map(|e| e.0.rank()).min().unwrap();
        let largest_rank = elems.iter().map(|e| e.0.rank()).max().unwrap();
        let smallest_shape = (elems.iter().map(|e| &e.0.shape))
            .min_by_key(|s| s.elements())
            .unwrap();
        let largest_shape = (elems.iter().map(|e| &e.0.shape))
            .max_by_key(|s| s.elements())
            .unwrap();
        let rank_summary = if smallest_rank == largest_rank {
            format!("all rank {smallest_rank}")
        } else {
            format!("ranks {smallest_rank}-{largest_rank}")
        };
        let shape_summary = if smallest_shape == largest_shape {
            format!("all shape {smallest_shape}")
        } else {
            format!("shapes {smallest_shape}-{largest_shape}")
        };
        format!("{rank_summary}, {shape_summary}")
    }
    fn summary_min_elems() -> usize {
        1000
    }
    fn alignment() -> ElemAlign {
        ElemAlign::DelimOrLeft(": ")
    }
    fn max_col_width<'a>(rows: impl Iterator<Item = &'a [char]>) -> usize {
        let mut max_labelled_len = 0;
        let mut max_unlabelled_len = 0;
        let mut max_label_len: Option<usize> = None;
        for row in rows {
            if let Some(delim_pos) = (0..row.len()).find(|&i| row[i..].starts_with(&[':', ' '])) {
                max_labelled_len = max_labelled_len.max(row.len() - delim_pos - 2);
                max_label_len = max_label_len.max(Some(delim_pos));
            }
            max_unlabelled_len = max_unlabelled_len.max(row.len());
        }
        if let Some(label_len) = max_label_len {
            (max_labelled_len + label_len + 2).max(max_unlabelled_len)
        } else {
            max_unlabelled_len
        }
    }
    fn soa_rows(arr: &Array<Self>) -> Option<Vec<(&EcoString, &Value)>> {
        if arr.rank() != 1 {
            return None;
        }
        let mut rows = Vec::new();
        let mut len = None;
        for Boxed(val) in &arr.data {
            if val.rank() == 0
                || val.rank() == 1 && matches!(val, Value::Char(_))
                || value_requires_summary(val)
            {
                return None;
            }
            let label = val.meta.label.as_ref()?;
            if *len.get_or_insert(val.row_count()) != val.row_count() {
                return None;
            }
            rows.push((label, val))
        }
        if rows.is_empty() {
            return None;
        }
        Some(rows)
    }
    fn grid_fmt_delims() -> (char, char) {
        ('{', '}')
    }
}

impl<T: GridFmt + ArrayValue> GridFmt for Array<T> {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        let mut metagrid: Option<Metagrid> = None;
        let mut map_key_align: Option<ElemAlign> = None;
        // println!(
        //     "shape: {}, box lines: {}, parent rank: {}, max boxed rank: {}, max boxed len: {}",
        //     self.shape,
        //     T::box_lines(),
        //     params.parent_rank,
        //     params.max_boxed_rank,
        //     params.max_boxed_len
        // );
        let mut outlined = false;
        let mut grid = if let Some(pointer) = self.meta.pointer.as_ref() {
            let mut ffi_type = pointer.ty.to_string();
            if ffi_type.len() > 20 {
                ffi_type = "{…}".to_string();
            }
            vec![
                format!("0x{:x}: {}", pointer.ptr, ffi_type)
                    .chars()
                    .collect(),
            ]
        } else if self.rank() == 0 && !self.is_map() {
            // Scalar
            let params = GridFmtParams {
                parent_rank: self.rank(),
                ..params
            };
            self.data[0].fmt_grid(params)
        } else if self.shape == [0] && !self.is_map() {
            // Empty list
            let (left, right) = T::grid_fmt_delims();
            let inner = T::empty_list_inner();
            let mut row = vec![left];
            row.extend(inner.chars());
            row.push(right);
            vec![row]
        } else {
            // Hashmap
            if let Some(keys) = &self.meta.map_keys {
                map_key_align = Some(match &keys.keys {
                    Value::Num(_) => f64::alignment(),
                    Value::Byte(_) => u8::alignment(),
                    Value::Complex(_) => Complex::alignment(),
                    Value::Char(_) => char::alignment(),
                    Value::Box(_) => Boxed::alignment(),
                    #[cfg(feature = "ga")]
                    Value::Mv(_) => crate::Multivector::alignment(),
                });
                let metagrid = metagrid.get_or_insert_with(Metagrid::new);
                for (key, value) in self.map_kv() {
                    let key = key.fmt_grid(params);
                    let value = value.fmt_grid(params);
                    metagrid.push(vec![key, vec![" → ".chars().collect()], value]);
                }
                if metagrid.is_empty() {
                    let mut keys_row_shape = keys.keys.shape.clone();
                    keys_row_shape.make_row();
                    let mut row = shape_row(&keys_row_shape, keys.keys.domain_symbol());
                    row.extend([' ', '→', ' ']);
                    let mut value_row_shape = self.shape.clone();
                    value_row_shape.make_row();
                    row.extend(shape_row(&value_row_shape, self.domain_symbol()));
                    metagrid.push(vec![vec![row]]);
                }
            }

            // SoA (struct-of-arrays)
            let mut is_soa = false;
            if let Some(rows) = T::soa_rows(self)
                && !rows.iter().any(|(_, val)| val.is_map())
            {
                is_soa = true;
                let metagrid = metagrid.get_or_insert_with(Metagrid::new);
                let mut labels_row = Vec::with_capacity(rows.len());
                for (label, _) in &rows {
                    labels_row.push(vec![label.chars().collect()]);
                }
                metagrid.push(labels_row);
                let row_params = GridFmtParams {
                    label: true,
                    soa_row: true,
                    ..Default::default()
                };
                for i in 0..rows[0].1.row_count() {
                    let mut metarow = Vec::with_capacity(rows.len());
                    for (_, val) in &rows {
                        metarow.push(val.row(i).fmt_grid(row_params));
                    }
                    metagrid.push(metarow);
                }
            }

            // Default array formatting
            let metagrid = metagrid.unwrap_or_else(|| {
                let mut metagrid = Metagrid::new();
                let params = GridFmtParams {
                    parent_rank: self.rank(),
                    ..params
                };
                fmt_array(self.rank(), &self.shape, &self.data, params, &mut metagrid);
                metagrid
            });

            let is_summary = requires_summary::<T>(&self.shape);

            let main_align = if is_summary {
                ElemAlign::None
            } else {
                T::alignment()
            };

            let dimensions = MetagridDimensions::of::<T>(&metagrid, is_summary);

            let mut grid = synthesize_grid(
                metagrid,
                dimensions,
                &self.shape,
                map_key_align.unwrap_or(main_align),
                main_align,
                self.is_map(),
                is_soa,
                T::box_lines(),
                params.multiline,
            );

            let is_mapling = self.is_map() && self.rank() == 0;
            if !is_mapling {
                if should_outline::<T>(&params, self.rank()) {
                    outlined = true;
                    let list_brackets = if is_summary || self.is_map() {
                        ('[', ']')
                    } else {
                        T::grid_fmt_delims()
                    };
                    outline_grid(
                        &mut grid,
                        &self.shape,
                        &params,
                        is_summary,
                        T::box_lines(),
                        list_brackets,
                    );
                } else if !T::box_lines() && !is_summary {
                    disambiguate_fixed(&mut grid, &self.shape, &params, T::compress_list_grid())
                }
            }
            grid
        };

        // Non-generic function to reduce size of monomophized function
        fn extras(
            grid: &mut Grid,
            shape: &[usize],
            meta: &ArrayMeta,
            params: GridFmtParams,
            outlined: bool,
            tname: &'static str,
        ) {
            // Add handle kind
            if let Some(kind) = &meta.handle_kind
                && grid.len() == 1
            {
                grid[0] = (kind.to_string().chars().chain(['(']))
                    .chain(take(&mut grid[0]))
                    .chain([')'])
                    .collect();
            }

            // Add complex marker
            if tname == type_name::<Complex>()
                && !grid.iter().flatten().any(|&c| c == 'ℂ' || c == 'i')
            {
                if shape.is_empty() {
                    grid[0].push('ℂ');
                } else if grid.len() == 1 {
                    let offset = outlined as usize;
                    grid[0].insert(offset, 'ℂ');
                    grid[0].insert(offset + 1, ' ');
                } else if outlined {
                    grid[0][2] = 'ℂ';
                } else {
                    for row in &mut *grid {
                        row.push(' ');
                        row.push(' ');
                        row.rotate_right(2);
                    }
                    grid[0][0] = 'ℂ';
                }
            }
            // Add multivector marker
            #[cfg(feature = "ga")]
            if tname == type_name::<crate::Multivector>()
                && !(grid.iter().flatten())
                    .any(|c| *c == '𝕍' || crate::SUBSCRIPT_DIGITS.contains(c))
            {
                if shape.is_empty() {
                    grid[0].push('𝕍');
                } else if grid.len() == 1 {
                    let offset = outlined as usize;
                    grid[0].insert(offset, '𝕍');
                    grid[0].insert(offset + 1, ' ');
                } else if outlined {
                    grid[0][2] = '𝕍';
                } else {
                    for row in &mut *grid {
                        row.push(' ');
                        row.push(' ');
                        row.rotate_right(2);
                    }
                    grid[0][0] = '𝕍';
                }
            }

            // Add label
            if params.label
                && let Some(label) = &meta.label
            {
                if grid.len() == 1 && params.parent_rank == 0 {
                    grid[0] = (label.chars().chain([':', ' ']))
                        .chain(take(&mut grid[0]))
                        .collect();
                } else {
                    if grid[0].len() >= 2 && "┌╭".contains(grid[0][1]) {
                        grid[0] = label
                            .chars()
                            .chain(take(&mut grid[0]).into_iter().skip(1))
                            .collect();
                        for row in grid.iter_mut().skip(1) {
                            let ext = label.chars().count() - 1;
                            row.extend(repeat_n(' ', ext));
                            row.rotate_right(ext);
                        }
                    } else {
                        if "┌╭".contains(grid[0][0]) {
                            let trunc = if grid[0][2] == 'ℂ' { 3 } else { 2 };
                            grid[0].truncate(trunc);
                            grid[0].push(' ');
                        } else {
                            grid.insert(0, Vec::new());
                        }
                        grid[0].extend(label.chars());
                    }
                    // Normalize lengths
                    while grid[0].len() < grid[1].len() {
                        grid[0].push(' ');
                    }
                    for i in 1..grid.len() {
                        while grid[i].len() < grid[0].len() {
                            grid[i].push(' ');
                        }
                    }
                }
            }

            // Handle really big grid
            if shape.len() > 1 {
                let max_width = terminal_size().map_or(1000, |(w, _)| w.saturating_sub(4).max(4));
                for row in grid.iter_mut() {
                    if row.len() > max_width {
                        let diff = row.len() - max_width;
                        row.truncate(max_width);
                        if !(row[max_width - 1].is_whitespace() && diff == 1)
                            && (2..4).any(|i| !row[max_width - i].is_whitespace())
                        {
                            row[max_width - 1] = '…';
                        }
                    }
                }
            }
        }
        extras(
            &mut grid,
            &self.shape,
            &self.meta,
            params,
            outlined,
            type_name::<T>(),
        );

        grid
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get a string representation of the shape of the array
    pub fn shape_string(&self) -> String {
        let base = shape_row(&self.shape, self.domain_symbol());

        let Some(keys) = &self.meta.map_keys else {
            return String::from_iter(base);
        };

        let mut keys_shape = keys.keys.shape.clone();
        if let Some(len) = keys_shape.first_mut() {
            *len = self.row_count();
        }

        shape_row(&keys_shape, keys.keys.domain_symbol())
            .into_iter()
            .chain([' ', '→', ' '])
            .chain(base)
            .collect()
    }

    /// Get a glyph indicating the domain of the array's elements
    pub fn domain_symbol(&self) -> char {
        T::domain_symbol(&self.data)
    }
}

impl Value {
    /// Get a string representation of the shape of the value
    pub fn shape_string(&self) -> String {
        val_as_arr!(self, Array::shape_string)
    }

    /// Get a glyph indicating the domain of the value's array elements
    pub fn domain_symbol(&self) -> char {
        val_as_arr!(self, Array::domain_symbol)
    }
}

fn shape_row(shape: &[usize], symbol: char) -> Vec<char> {
    let mut shape_row = Vec::new();
    const MAX_RENDERED_RANK: usize = 40;
    if shape.len() <= MAX_RENDERED_RANK {
        for (i, dim) in shape.iter().enumerate() {
            if i > 0 {
                shape_row.extend("×".chars());
            }
            shape_row.extend(dim.to_string().chars());
        }
    } else {
        for dim in shape.iter().take(MAX_RENDERED_RANK / 2) {
            shape_row.extend(dim.to_string().chars());
            shape_row.extend("×".chars());
        }
        shape_row.extend([' ', '…', ' ']);
        for dim in shape.iter().rev().take(MAX_RENDERED_RANK / 2).rev() {
            shape_row.extend("×".chars());
            shape_row.extend(dim.to_string().chars());
        }
    }
    if !shape.is_empty() {
        shape_row.push(' ');
    }
    shape_row.push(symbol);
    shape_row
}

const MAX_RANK: usize = 8;

fn requires_summary<T: ArrayValue>(shape: &[usize]) -> bool {
    shape.iter().product::<usize>() > T::summary_min_elems() || shape.len() > MAX_RANK
}

fn value_requires_summary(val: &Value) -> bool {
    match val {
        Value::Byte(a) => requires_summary::<f64>(&a.shape),
        Value::Num(a) => requires_summary::<u8>(&a.shape),
        Value::Complex(a) => requires_summary::<Complex>(&a.shape),
        Value::Char(a) => requires_summary::<char>(&a.shape),
        Value::Box(a) => requires_summary::<Boxed>(&a.shape),
        #[cfg(feature = "ga")]
        Value::Mv(a) => requires_summary::<crate::Multivector>(&a.shape),
    }
}

fn fmt_array<T: GridFmt + ArrayValue>(
    max_rank: usize,
    shape: &[usize],
    data: &[T],
    params: GridFmtParams,
    metagrid: &mut Metagrid,
) {
    if data.is_empty() {
        metagrid.push(vec![vec![shape_row(shape, T::SYMBOL)]]);
        return;
    }
    if requires_summary::<T>(shape) {
        let summary = T::summarize(data);
        if !summary.is_empty() {
            let mut first_row = shape_row(shape, T::domain_symbol(data));
            let grid: Grid = if shape.len() == 1 {
                first_row.extend(": ".chars());
                first_row.extend(summary.chars());
                vec![first_row]
            } else {
                vec![first_row, summary.chars().collect()]
            };
            metagrid.push(vec![grid]);
            return;
        }
    }
    let rank = shape.len();
    if rank == 0 {
        metagrid.push(vec![data[0].fmt_grid(params)]);
        return;
    }
    let vertical_list = max_rank == 1 && !T::list_same_line();
    if rank == 1 && !vertical_list {
        let mut row = Vec::with_capacity(shape[0]);
        if T::compress_list_grid() {
            let s: String = data
                .iter()
                .map(|c| c.to_string())
                .collect::<String>()
                .chars()
                .map(format_char_inner)
                .collect();
            row.push(vec![s.chars().collect()]);
        } else {
            for (i, val) in data.iter().enumerate() {
                let mut grid = val.fmt_grid(params);
                if i > 0 && !T::box_lines() {
                    pad_grid_min(grid[0].len() + T::separation(), grid.len(), &mut grid);
                }
                row.push(grid);
            }
        }
        metagrid.push(row);
        return;
    }
    let cell_count = shape[0];
    if cell_count == 0 {
        metagrid.push(vec![vec![vec![' ']]]);
        return;
    }
    let row_shape = &shape[1..];
    let cell_size = data.len() / cell_count;
    let start_len = metagrid.len();
    let adjusted_rank = rank + vertical_list as usize;
    for (i, cell) in data.chunks(cell_size).enumerate() {
        if i > 0 && adjusted_rank > 2 && !T::box_lines() && adjusted_rank.is_multiple_of(2) {
            for _ in 0..(adjusted_rank - 2) / 2 {
                metagrid.push(vec![vec![vec![' ']]; metagrid.last().unwrap().len()]);
            }
        }
        let len_before = metagrid.len();
        fmt_array(max_rank, row_shape, cell, params, metagrid);
        if T::compress_list_grid() && adjusted_rank == 2 {
            let (left, right) = T::grid_fmt_delims();
            for grid in metagrid.last_mut().unwrap() {
                for row in grid.iter_mut() {
                    row.insert(0, left);
                    row.push(right);
                }
            }
        }
        if i > 0 && (adjusted_rank > 2 || T::box_lines()) && adjusted_rank % 2 == 1 {
            let elem_rows = metagrid.iter().flatten().flatten();
            let max_width = elem_rows.map(|r| r.len()).max().unwrap_or(0);
            let div = if max_width > 10 { 1 } else { 2 };
            let rows = metagrid.split_off(len_before);
            for (mrow, row) in metagrid.iter_mut().skip(start_len).zip(rows) {
                if !T::box_lines() {
                    mrow.push(vec![vec![' '; adjusted_rank.div_ceil(div)]]);
                }
                mrow.extend(row);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ElemAlign {
    None,
    Left,
    Right,
    DelimOrRight(&'static str),
    DelimOrLeft(&'static str),
}

fn pad_grid_center(
    width: usize,
    height: usize,
    horiz_align: ElemAlign,
    center_vert: bool,
    lr_lens: Option<(usize, usize)>,
    grid: &mut Grid,
) {
    grid.truncate(height);
    if grid.len() < height {
        let diff = height - grid.len();
        let post_pad = if center_vert { diff / 2 } else { diff };
        let pre_pad = diff - post_pad;
        for _ in 0..pre_pad {
            grid.insert(0, vec![' '; width]);
        }
        for _ in 0..post_pad {
            grid.push(vec![' '; width]);
        }
    }
    for row in grid.iter_mut() {
        if row.len() < width {
            let diff = width - row.len();
            let (pre, post) = match horiz_align {
                ElemAlign::Left => (0, diff),
                ElemAlign::Right => (diff, 0),
                ElemAlign::None => {
                    let post = diff.div_ceil(2);
                    let pre = diff - post;
                    (pre, post)
                }
                ElemAlign::DelimOrRight(s) => {
                    let (left, right) = lr_lens.unwrap();
                    if let Some(pos) = (0..row.len())
                        .find(|i| row[*i..].iter().zip(s.chars()).all(|(&a, b)| a == b))
                    {
                        let pre = left - pos;
                        let post = diff - pre;
                        (pre, post)
                    } else if right > 0 {
                        let pre = left.saturating_sub(row.len());
                        let post = diff - pre;
                        (pre, post)
                    } else {
                        (diff, 0)
                    }
                }
                ElemAlign::DelimOrLeft(s) => {
                    let (left, right) = lr_lens.unwrap();
                    if let Some(pos) = (0..row.len().saturating_sub(s.chars().count()))
                        .find(|i| row[*i..].iter().zip(s.chars()).all(|(&a, b)| a == b))
                    {
                        let pre = left - pos;
                        let post = diff - pre;
                        (pre, post)
                    } else if left > 0 {
                        let post = right.saturating_sub(row.len());
                        let pre = diff - post;
                        (pre, post)
                    } else {
                        (0, diff)
                    }
                }
            };
            for _ in 0..pre {
                row.insert(0, ' ');
            }
            for _ in 0..post {
                row.push(' ');
            }
        } else {
            row.truncate(width);
        }
    }
}

fn pad_grid_min(width: usize, height: usize, grid: &mut Grid) {
    grid.truncate(height);
    while grid.len() < height {
        grid.insert(0, vec![' '; width]);
    }
    for row in grid.iter_mut() {
        row.truncate(width);
        while row.len() < width {
            row.insert(0, ' ');
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Line {
    Single,
    Double,
    OpenSingle,
    CloseSingle,
    OpenDouble,
    CloseDouble,
    ForceSingle,
    ForceDouble,
}
impl Line {
    fn set(n: usize) -> &'static [Line] {
        match n {
            0 => &[Line::Single],
            1 => &[Line::Double],
            2 => &[Line::OpenSingle, Line::CloseSingle],
            _ => &[Line::OpenDouble, Line::CloseDouble],
        }
    }
    fn vert(&self) -> char {
        match self {
            Line::Single | Line::OpenSingle | Line::CloseSingle | Line::ForceSingle => '│',
            Line::Double | Line::OpenDouble | Line::CloseDouble | Line::ForceDouble => '║',
        }
    }
    fn horiz(&self) -> char {
        match self {
            Line::Single | Line::OpenSingle | Line::CloseSingle | Line::ForceSingle => '─',
            Line::Double | Line::OpenDouble | Line::CloseDouble | Line::ForceDouble => '═',
        }
    }
    fn singleton(&self) -> &Self {
        match self {
            Line::Single => &Line::OpenSingle,
            Line::CloseSingle => &Line::ForceSingle,
            Line::Double => &Line::OpenDouble,
            Line::CloseDouble => &Line::ForceDouble,
            line => line,
        }
    }
    fn intersect(&self, column: Self) -> char {
        use Line::*;
        match (self, column) {
            (Single, Single) | (ForceSingle, ForceSingle) => '┼',
            (Single, Double) | (ForceSingle, ForceDouble) => '╫',
            (Single, OpenSingle) => '┤',
            (Single, CloseSingle) => '├',
            (Single, OpenDouble) => '╢',
            (Single, CloseDouble) => '╟',
            (Double, Single) | (ForceDouble, ForceSingle) => '╪',
            (Double, Double) | (ForceDouble, ForceDouble) => '╬',
            (Double, OpenSingle) => '╡',
            (Double, CloseSingle) => '╞',
            (Double, OpenDouble) => '╣',
            (Double, CloseDouble) => '╠',
            (OpenSingle, Single) => '┴',
            (OpenSingle, Double) => '╨',
            (OpenSingle, OpenSingle) => '┘',
            (OpenSingle, CloseSingle) => '└',
            (OpenSingle, OpenDouble) => '╜',
            (OpenSingle, CloseDouble) => '╙',
            (CloseSingle, Single) => '┬',
            (CloseSingle, Double) => '╥',
            (CloseSingle, OpenSingle) => '┐',
            (CloseSingle, CloseSingle) => '┌',
            (CloseSingle, OpenDouble) => '╖',
            (CloseSingle, CloseDouble) => '╓',
            (OpenDouble, Single) => '╧',
            (OpenDouble, Double) => '╩',
            (OpenDouble, OpenSingle) => '╛',
            (OpenDouble, CloseSingle) => '╘',
            (OpenDouble, OpenDouble) => '╝',
            (OpenDouble, CloseDouble) => '╚',
            (CloseDouble, Single) => '╤',
            (CloseDouble, Double) => '╦',
            (CloseDouble, OpenSingle) => '╕',
            (CloseDouble, CloseSingle) => '╒',
            (CloseDouble, OpenDouble) => '╗',
            (CloseDouble, CloseDouble) => '╔',
            (ForceSingle, _) => '─',
            (ForceDouble, _) => '═',
            (_, ForceSingle) => '│',
            (_, ForceDouble) => '║',
        }
    }
}

/// Round to a number of significant decimal places
fn round_sig_dec(f: f64, n: i32) -> f64 {
    if f.fract() == 0.0 || f.is_infinite() || [PI / 2.0, PI, TAU].contains(&f) {
        return f;
    }
    let mul = 10f64.powf(n as f64 - f.fract().abs().log10().ceil());
    (f * mul).round() / mul
}

fn multiset_summary<T: ArrayCmp + GridFmt + Clone>(elems: &[T]) -> Option<String> {
    if elems.is_empty() {
        return None;
    }

    // Typically, one would use a BTreeMap here.
    // However, this function bails at a size limit that is expected to be very small.
    // So small, in fact, that a BTreeMap would only have one node anyway. (B = 6 -> node capacity = 11)
    // In addition, the usage of non-Ord keys (f64, Complex) would require a newtype.
    // This is plenty doable, but gets more annoying in a generic context.
    let max_distinct = T::summary_multiset_max_distinct();
    let mut pairs = SmallVec::<[(T, usize); 8]>::with_capacity(max_distinct);
    for elem in elems.iter().cloned() {
        match pairs.binary_search_by(|p| T::array_cmp(&p.0, &elem)) {
            Ok(i) => pairs[i].1 += 1,
            Err(_) if pairs.len() >= max_distinct => return None,
            Err(i) => pairs.insert(i, (elem, 1)),
        }
    }

    if let [(val, _)] = &pairs[..] {
        return Some(format!("all {}", val.grid_string(false)));
    }

    let mut s = String::from("◴");
    for (val, _) in &pairs {
        s += " ";
        s += &val.grid_string(false);
    }
    s += " ⧆";
    for (_, count) in &pairs {
        s += " ";
        s += &count.to_string();
    }
    Some(s)
}

fn max_len<T>(a: &Vec<Vec<T>>, fill: usize) -> usize {
    a.iter().map(|r| r.len()).max().unwrap_or(fill)
}

fn metacolumn_subrows(metagrid: &Metagrid, meta_x: usize) -> impl Iterator<Item = &[char]> + Clone {
    metagrid
        .iter()
        .filter_map(move |metarow| metarow.get(meta_x))
        .flatten()
        .map(Vec::as_slice)
}

struct MetagridDimensions {
    metagrid_height: usize,
    metagrid_width: usize,
    row_heights: Vec<usize>,
    column_widths: Vec<usize>,
}

impl MetagridDimensions {
    fn of<T: ArrayValue>(metagrid: &Metagrid, is_summary: bool) -> Self {
        let metagrid_height = metagrid.len();
        let metagrid_width = max_len(&metagrid, 0);

        let row_heights: Vec<usize> = metagrid.iter().map(|metarow| max_len(metarow, 1)).collect();

        // A rank-2 iterator over the non-meta rows within each meta-column.
        let metacolumn_rows =
            (0..metagrid_width).map(|meta_col_idx| metacolumn_subrows(&metagrid, meta_col_idx));

        let column_widths: Vec<usize> = if is_summary {
            metacolumn_rows
                .map(|rows| rows.map(|row| row.len()).max().unwrap_or(0))
                .collect()
        } else {
            metacolumn_rows.map(T::max_col_width).collect()
        };

        Self {
            metagrid_height,
            metagrid_width,
            row_heights,
            column_widths,
        }
    }
}

fn synthesize_grid(
    mut metagrid: Metagrid,
    dimensions: MetagridDimensions,
    shape: &[usize],
    first_align: ElemAlign,
    main_align: ElemAlign,
    is_map: bool,
    is_soa: bool,
    box_lines: bool,
    multiline: bool,
) -> Grid {
    let mut output_grid = Grid::new();

    let MetagridDimensions {
        metagrid_height,
        metagrid_width,
        row_heights,
        column_widths,
    } = dimensions;

    let max_lr_lengths: Vec<(usize, usize)> = (0..metagrid_width)
        .map(|meta_x| {
            let alignment = if meta_x == 0 { first_align } else { main_align };

            let (delimiter, is_right_aligned) = match alignment {
                ElemAlign::DelimOrLeft(delim) => (delim, false),
                ElemAlign::DelimOrRight(delim) => (delim, true),
                _ => return (0, 0),
            };

            let delim_len = delimiter.chars().count();
            let find_delim = |row: &[char]| {
                row.windows(delim_len)
                    .position(|w| w.iter().copied().eq(delimiter.chars()))
            };

            let left = metacolumn_subrows(&metagrid, meta_x).map(|row| match find_delim(row) {
                Some(pos) => pos,
                None if is_right_aligned => row.len(),
                None => 0,
            });
            let right = metacolumn_subrows(&metagrid, meta_x).map(|row| match find_delim(row) {
                Some(pos) => row.len() - (pos + delim_len),
                None if is_right_aligned => 0,
                None => row.len(),
            });
            (left.max().unwrap_or(0), right.max().unwrap_or(0))
        })
        .collect();

    // Pad each metagrid cell to its row's max height and column's max width
    for (meta_y, metarow) in metagrid.iter_mut().enumerate() {
        let row_height = row_heights[meta_y];

        for (meta_x, meta_cell) in metarow.iter_mut().enumerate() {
            let alignment = if meta_x == 0 { first_align } else { main_align };

            pad_grid_center(
                column_widths[meta_x],
                row_height,
                alignment,
                false,
                Some(max_lr_lengths[meta_x]),
                meta_cell,
            );
        }
    }

    // Actually combine the cells and insert dividers
    for (meta_y, metarow) in metagrid.into_iter().enumerate() {
        let row_height = row_heights[meta_y];
        // The rows of the output grid that we're currently working on
        let mut subrows: Grid = vec![vec![]; row_height];
        // The horizontal positions of vertical gridlines
        let mut divider_intersections = HashMap::<usize, Line>::new();

        let vertical_offset = count_dividers(meta_y, shape, 1);

        for (meta_x, meta_cell) in metarow.into_iter().enumerate() {
            let horizontal_offset = count_dividers(meta_x, shape, 0);

            for (subrow, inner_row) in subrows.iter_mut().zip(meta_cell) {
                subrow.extend(inner_row);

                let add_column_divider = box_lines
                    && !is_map
                    && (meta_x + 1 < metagrid_width
                        || shape[shape.len() - 1 - horizontal_offset * 2] == 1);

                if add_column_divider {
                    if !is_soa && shape.len() == 1 && row_height == 1 && !multiline {
                        if shape[0] > 1 {
                            subrow.push(' ');
                        }
                    } else {
                        for mut line in Line::set(horizontal_offset) {
                            if meta_x + 1 == metagrid_width {
                                line = line.singleton();
                            }
                            // Mark the current horizontal position as a grid line intersection.
                            divider_intersections.insert(subrow.len(), *line);
                            subrow.push(line.vert());
                        }
                    }
                }
            }
        }

        let len = subrows.last().unwrap().len();
        output_grid.extend(subrows);

        let vert_offset = 2 * (1 + vertical_offset);
        let add_row_divider = (meta_y == 0 || !is_soa)
            && box_lines
            && !is_map
            && (meta_y + 1 < metagrid_height
                || shape.len() >= vert_offset && shape[shape.len() - vert_offset] == 1);

        if add_row_divider {
            for mut row_line in Line::set(vertical_offset) {
                if meta_y + 1 == metagrid_height {
                    row_line = row_line.singleton();
                }

                let row_ch = row_line.horiz();
                let mut divider_row = vec![row_ch; len];
                for (&x, &col_line) in &divider_intersections {
                    divider_row[x] = row_line.intersect(col_line);
                }
                output_grid.push(divider_row);
            }
        }
    }

    output_grid
}

fn should_outline<T: ArrayValue>(params: &GridFmtParams, rank: usize) -> bool {
    if rank <= 1 && T::compress_list_grid() {
        // This is a string (or something with similar semantics). Strings always get quotes.
        true
    } else if params.soa_row {
        // If an SoA label row is to be printed, normal borders are not.
        false
    } else if params.parent_rank == 0 {
        // This array is not a child of some parent box array. It's being formatted normally.
        true
    } else if rank <= 1 && params.parent_rank == 1 && !params.multiline {
        // This is a scalar or list, and its parent is a list.
        true
    } else {
        if T::box_lines() {
            rank <= 1 && params.parent_rank <= 1
        } else {
            rank < params.max_boxed_rank
        }
    }
}

fn outline_grid(
    grid: &mut Grid,
    shape: &[usize],
    params: &GridFmtParams,
    is_summary: bool,
    box_lines: bool,
    list_brackets: (char, char),
) {
    let rank = shape.len();
    let grid_row_count = grid.len();

    if grid_row_count == 1 && rank == 1 && !params.multiline {
        grid[0].insert(0, list_brackets.0);
        grid[0].push(list_brackets.1);
        if box_lines && shape[0] == 0 && params.parent_rank == 0 {
            grid[0].insert(1, Primitive::Box.glyph().unwrap());
        }
    } else {
        let apparent_rank = if is_summary { 1 } else { rank };
        // Add corners to non-vectors
        let width = grid[0].len();
        let height = grid.len();
        pad_grid_center(
            width + 4,
            (height + 2).max(apparent_rank + 1),
            ElemAlign::None,
            true,
            None,
            grid,
        );
        grid[0][0] = if box_lines { '┌' } else { '╭' };
        grid[0][1] = '─';
        for i in 0..apparent_rank.saturating_sub(1) {
            grid[i + 1][0] = '╷';
        }
        *grid.last_mut().unwrap().last_mut().unwrap() = if box_lines { '┘' } else { '╯' };
    }
}

fn disambiguate_fixed(
    grid: &mut Grid,
    shape: &[usize],
    params: &GridFmtParams,
    compress_list_grid: bool,
) {
    let mut fix_amount = shape.iter().take_while(|d| **d == 1).count();
    if fix_amount == 0 {
        return;
    }

    let rank = shape.len();

    let should_disambiguate = params.soa_row
        || (rank < params.max_boxed_rank || params.max_boxed_len == 1 || compress_list_grid)
            && params.max_boxed_rank != 1;

    if !should_disambiguate {
        return;
    }

    if compress_list_grid && fix_amount == rank {
        // Prefer e.g. "x" over ¤@x, ¤"x" over ¤¤@x, etc.
        fix_amount = fix_amount.saturating_sub(1);
    }

    for row in grid.iter_mut() {
        row.extend(repeat_n(' ', fix_amount));
        row.rotate_right(fix_amount);
    }
    for i in 0..fix_amount {
        grid[0][i] = Primitive::Fix.glyph().unwrap();
    }
}

/// Count how many dimensions the current meta-row or meta-column needs a divider for.
fn count_dividers(position: usize, shape: &[usize], parity: usize) -> usize {
    if shape.contains(&0) {
        return 0;
    }

    let dimensions = shape.iter().rev().skip(parity).step_by(2);

    let mut dividers: usize = 0;
    let mut product: usize = 1;
    for dimension in dimensions {
        if (position + 1) % product.max(1) == 0 {
            dividers += 1;
        }
        product *= dimension;
    }
    dividers.saturating_sub(1)
}
