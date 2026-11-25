//! GridFmt trait implementations for scalar types, *i.e.*, pretty-printing individual values.
//!
//! (Code for summaries also lives here, but that still doesn't operate on shaped Uiua arrays.)

use super::{ElemAlign, Grid, GridFmt, GridFmtParams};
use crate::{
    ArrayCmp, Complex, WILDCARD_CHAR, WILDCARD_NAN,
    algorithm::map::{EMPTY_CHAR, EMPTY_NAN, TOMBSTONE_CHAR, TOMBSTONE_NAN},
};
use smallvec::SmallVec;
use std::borrow::Cow;
use std::f64::consts::{E, PI, SQRT_2, TAU};
use std::fmt::Write;

impl GridFmt for u8 {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        Grid::from_utf8(&self.to_string())
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
        let positive = self.abs();
        let minus = if self.is_sign_negative() { "¯" } else { "" };
        let s = if let Ok(math_const) = MathConstant::try_from(*self) {
            math_const.to_string()
        } else if positive.is_nan() {
            format!("{minus}nan")
        } else if positive.fract() == 0.0 {
            format!("{minus}{positive}")
        } else {
            let mut pos_formatted = positive.to_string();
            if pos_formatted.len() >= 17 {
                abbreviate_consecutive_fract_digits(&mut pos_formatted);
            }
            pos_formatted.insert_str(0, minus);
            pos_formatted
        };
        Grid::from_utf8(&s)
    }
    fn summarize(elems: &[Self]) -> String {
        if elems.is_empty() {
            return String::new();
        }
        if let Some(s) = multiset_summary(elems) {
            return s;
        }
        if elems.iter().all(|&n| n.is_nan()) {
            return "all nan".into();
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
                    "{} {}s and {} nans",
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
                    " ({nan_count} nan{})",
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

struct MathConstant {
    glyph: char,
    negative: bool,
    approximate: bool,
    square_root: bool,
    numerator: u8,
    denominator: u8,
}

impl std::fmt::Display for MathConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (flag, ch) in [
            (self.approximate, '~'),
            (self.negative, '¯'),
            (self.square_root, '√'),
        ] {
            if flag {
                f.write_char(ch)?;
            }
        }
        if self.numerator >= 2 {
            assert!(!self.square_root);
            write!(f, "{}", self.numerator)?;
        }
        f.write_char(self.glyph)?;
        if self.denominator >= 2 {
            assert!(!self.square_root);
            write!(f, "/{}", self.denominator)?;
        }
        Ok(())
    }
}

impl TryFrom<f64> for MathConstant {
    type Error = ();
    fn try_from(f: f64) -> Result<Self, Self::Error> {
        if f.is_finite() && f.fract() == 0.0 {
            // None of the values being checked for are integers.
            return Err(());
        }

        let mut math = Self {
            glyph: '\0',
            negative: false,
            approximate: false,
            square_root: false,
            numerator: 1,
            denominator: 1,
        };

        let bit_eq = |n: f64| f.to_bits() == n.to_bits();

        // Special NaN values
        if f.is_nan() {
            math.glyph = if bit_eq(EMPTY_NAN) {
                '∅'
            } else if bit_eq(TOMBSTONE_NAN) {
                '⊥'
            } else if bit_eq(WILDCARD_NAN) {
                'W'
            } else {
                // Regular NaN can't be handled as a MathConstant,
                // because it gets formatted as multiple characters.
                return Err(());
            };
            return Ok(math);
        }

        let positive = f.abs();
        math.negative = f.is_sign_negative();
        let eps_eq = |n: f64| (positive - n).abs() <= f64::EPSILON;

        // Basic constants
        math.glyph = if eps_eq(PI) {
            'π'
        } else if eps_eq(TAU) {
            'τ'
        } else if eps_eq(PI / 2.0) {
            'η'
        } else if eps_eq(E) {
            'e'
        } else if positive.is_infinite() {
            '∞'
        } else {
            '\0'
        };
        if math.glyph != '\0' {
            return Ok(math);
        }

        // Simple rational multiples of tau
        for denom in [1u8, 2, 3, 4, 5, 6, 8, 9, 12] {
            let num = (positive * denom as f64) / TAU;
            let rounded = num.round();
            if rounded == 0.0 || rounded > 100.0 || (num - rounded).abs() > 8.0 * f64::EPSILON {
                continue;
            }

            math.numerator = rounded as u8;
            math.approximate = rounded != num;
            // Should multiples of 4/2 also use η/π?
            if denom == 4 {
                math.glyph = 'η';
            } else if denom == 2 {
                math.glyph = 'π';
            } else {
                math.glyph = 'τ';
                math.denominator = denom;
            }

            return Ok(math);
        }

        // Common square roots
        let sqrt_3 = 3.0_f64.sqrt();
        let sqrt_5 = 5.0_f64.sqrt();
        for (sqrt, sqr) in [
            (SQRT_2, '2'),
            (SQRT_2 / 2.0, '½'),
            (sqrt_3, '3'),
            (sqrt_3 / 2.0, '¾'),
            (sqrt_3 / 3.0, '⅓'),
            (sqrt_5, '5'),
            (sqrt_5 / 5.0, '⅕'),
        ] {
            if eps_eq(sqrt) {
                math.glyph = sqr;
                math.square_root = true;
                return Ok(math);
            }
        }

        Err(())
    }
}

fn abbreviate_consecutive_fract_digits(s: &mut String) {
    let Some(decimal_pos) = s.find('.').map(|i| i + 1) else {
        return;
    };

    debug_assert!(s.is_ascii());

    let mut consecutive_start = 0;
    let mut consecutive_len = 0;
    for (i, c) in s[decimal_pos..].char_indices() {
        let j = decimal_pos + i;
        let local_len = s[j..].chars().take_while(|&d| d == c).count();
        if local_len > consecutive_len {
            consecutive_start = j;
            consecutive_len = local_len;
        }
    }

    if consecutive_len >= 6 {
        let start = consecutive_start;
        let end = start + consecutive_len;
        let range = if end == s.len() {
            // Remove digits from the end, keeping 3 before
            (start + 3)..end
        } else {
            // Remove digits from the middle, keeping 2 before and 1 after
            (start + 2)..(end - 1)
        };
        s.replace_range(range, "…");
    }
}

impl GridFmt for Complex {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        if self.im.abs() == 0.0 {
            self.re.fmt_grid(params)
        } else if self.re.abs() == 0.0 {
            if self.im == 1.0 {
                Grid::from_scalar('i')
            } else if self.im == -1.0 {
                Grid::from_utf8("¯i")
            } else {
                let mut grid = self.im.fmt_grid(params);
                grid.0[0].push('i');
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
            re.0[0].push(sign);
            re.0[0].extend(im.chars());
            re.0[0].push('i');
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
                s.extend(n_grid.0.into_iter().next().unwrap());
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
        Grid::from_utf32(s)
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

impl GridFmt for char {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        let mut s = format_char_inner_pretty(*self).chars().collect::<Vec<_>>();
        s.insert(0, '@');
        Grid::from_utf32(s)
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

/// Round to a number of significant decimal places
fn round_sig_dec(f: f64, n: i32) -> f64 {
    if f.fract() == 0.0 || f.is_infinite() || [PI / 2.0, PI, TAU].contains(&f) {
        return f;
    }
    let mul = 10f64.powf(n as f64 - f.fract().abs().log10().ceil());
    (f * mul).round() / mul
}

pub fn format_char_inner_repr(c: char) -> Cow<'static, str> {
    format_char_inner(c, false)
}

pub fn format_char_inner_pretty(c: char) -> Cow<'static, str> {
    format_char_inner(c, true)
}

pub fn format_char_inner(c: char, pretty: bool) -> Cow<'static, str> {
    let special = match c {
        char::MAX => r"\_",
        WILDCARD_CHAR => r"\W",
        '\'' => "'",

        EMPTY_CHAR if pretty => r"\∅",
        TOMBSTONE_CHAR if pretty => r"\⊥",

        ' ' if pretty => " ",
        ' ' if !pretty => r"\s",

        _ => "",
    };
    if !special.is_empty() {
        return Cow::Borrowed(special);
    }

    let mut formatted = format!("{c:?}");
    formatted.pop();
    formatted.remove(0);
    if formatted.starts_with(r"\u{") {
        formatted = match c as u32 {
            n @ 0x00..=0xFF => format!(r"\x{n:02x}"),
            n @ 0x0100..=0xFFFF => format!(r"\u{n:04x}"),
            _ => formatted, // Rust already gives us \u{n}
        };
    }
    Cow::Owned(formatted)
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
    let mut pairs = SmallVec::<[(T, usize);
    5 // can be replaced with T::SUMMARY_MULTISET_MAX_DISTINCT on nightly compiler
      // but this is just a lower bound so it's ok
    ]>::new();
    for elem in elems.iter().cloned() {
        match pairs
            .iter_mut()
            .position(|p| T::array_cmp(&p.0, &elem).is_eq())
        {
            Some(i) => pairs[i].1 += 1,
            None if pairs.len() >= T::SUMMARY_MULTISET_MAX_DISTINCT => return None,
            None => pairs.push((elem, 1)),
        }
    }
    pairs.sort_unstable_by(|a, b| T::array_cmp(&a.0, &b.0));

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
