//! Pretty printing Uiua arrays

use std::{
    collections::HashMap,
    f64::consts::{PI, TAU},
    iter::{once, repeat},
    mem::take,
};

use crate::{
    algorithm::map::{EMPTY_CHAR, EMPTY_NAN, TOMBSTONE_CHAR, TOMBSTONE_NAN},
    array::{Array, ArrayValue},
    boxed::Boxed,
    terminal_size, val_as_arr,
    value::Value,
    Complex, Primitive, WILDCARD_CHAR, WILDCARD_NAN,
};

type Grid<T = char> = Vec<Vec<T>>;
type Metagrid = Grid<Grid>;

#[derive(Debug, Clone, Copy, Default)]
pub struct GridFmtParams {
    label: bool,
    depth: usize,
    parent_rank: usize,
    max_boxed_rank: usize,
    max_boxed_len: usize,
}

pub trait GridFmt {
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
}

impl GridFmt for u8 {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        vec![self.to_string().chars().collect()]
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        let f = *self;
        let positive = f.abs();
        let is_neg = f.is_sign_negative();
        let minus = if is_neg { "¯" } else { "" };
        let s = if (positive - PI).abs() <= f64::EPSILON {
            format!("{minus}π")
        } else if (positive - TAU).abs() <= f64::EPSILON {
            format!("{minus}τ")
        } else if (positive - PI / 2.0).abs() <= f64::EPSILON {
            format!("{minus}η")
        } else if positive == f64::INFINITY {
            format!("{minus}∞")
        } else if f.to_bits() == EMPTY_NAN.to_bits() {
            "_".into()
        } else if f.to_bits() == TOMBSTONE_NAN.to_bits() {
            "⊥".into()
        } else if f.to_bits() == WILDCARD_NAN.to_bits() {
            "W".into()
        } else if positive.fract() == 0.0 || positive.is_nan() {
            format!("{minus}{positive}")
        } else if let Some((num, denom, approx)) =
            [1u8, 2, 3, 4, 5, 6, 8, 9, 12].iter().find_map(|&denom| {
                let num = (positive * denom as f64) / TAU;
                let rounded = num.round();
                if rounded <= 100.0 && (num - rounded).abs() <= f64::EPSILON && rounded != 0.0 {
                    Some((rounded, denom, num != rounded))
                } else {
                    None
                }
            })
        {
            let prefix = if approx { "~" } else { "" };
            if denom == 1 {
                format!("{prefix}{minus}{num}τ")
            } else if num == 1.0 {
                format!("{prefix}{minus}τ/{denom}")
            } else {
                format!("{prefix}{minus}{num}τ/{denom}")
            }
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
            Value::Box(v) => {
                let max_boxed_rank = v.data.iter().map(|Boxed(v)| v.rank()).max().unwrap_or(0);
                v.fmt_grid(GridFmtParams {
                    depth: params.depth + 1,
                    max_boxed_rank,
                    max_boxed_len: (v.data.iter())
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

pub fn format_char_inner(c: char) -> String {
    match c {
        char::MAX => return "\\x¯01".into(),
        WILDCARD_CHAR => return '�'.to_string(),
        EMPTY_CHAR => return '_'.to_string(),
        TOMBSTONE_CHAR => return '⊥'.to_string(),
        _ => {}
    }
    let formatted = format!("{c:?}");
    if c == '\'' {
        "'".to_string()
    } else if formatted.starts_with("'\\u{") {
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
    }
}

impl GridFmt for char {
    fn fmt_grid(&self, _params: GridFmtParams) -> Grid {
        vec![once('@').chain(format_char_inner(*self).chars()).collect()]
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
        if params.parent_rank == 0
            || self.0.shape.is_empty()
                && !(params.parent_rank == 1 && matches!(self.0, Value::Box(_)))
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
                    grid[(height - 1) / 2][0] = symbol;
                }
            }
        }
        grid
    }
}

impl<T: GridFmt + ArrayValue> GridFmt for Array<T> {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        let mut metagrid: Option<Metagrid> = None;
        let mut first_align: Option<ElemAlign> = None;
        // println!(
        //     "shape: {}, box lines: {}, parent rank: {}, max boxed rank: {}, max boxed len: {}",
        //     self.shape,
        //     T::box_lines(),
        //     params.parent_rank,
        //     params.max_boxed_rank,
        //     params.max_boxed_len
        // );
        let mut outlined = false;
        let mut grid = if let Some(pointer) = self.meta.pointer.filter(|p| p.raw) {
            vec![format!("0x{:x}", pointer.ptr).chars().collect()]
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
                first_align = Some(match &keys.keys {
                    Value::Num(_) => f64::alignment(),
                    Value::Byte(_) => u8::alignment(),
                    Value::Complex(_) => Complex::alignment(),
                    Value::Char(_) => char::alignment(),
                    Value::Box(_) => Boxed::alignment(),
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
                    let mut row = match &keys.keys {
                        Value::Num(_) => shape_row::<f64>(&keys_row_shape),
                        Value::Byte(_) => shape_row::<u8>(&keys_row_shape),
                        Value::Complex(_) => shape_row::<Complex>(&keys_row_shape),
                        Value::Char(_) => shape_row::<char>(&keys_row_shape),
                        Value::Box(_) => shape_row::<Boxed>(&keys_row_shape),
                    };
                    row.extend([' ', '→', ' ']);
                    let mut value_row_shape = self.shape.clone();
                    value_row_shape.make_row();
                    row.extend(shape_row::<T>(&value_row_shape));
                    metagrid.push(vec![vec![row]]);
                }
            }

            // Default array formatting
            let mut metagrid = metagrid.unwrap_or_else(|| {
                let mut metagrid = Metagrid::new();
                let params = GridFmtParams {
                    parent_rank: self.rank(),
                    ..params
                };
                fmt_array(&self.shape, &self.data, params, &mut metagrid);
                metagrid
            });

            // Synthesize a grid from the metagrid
            let mut grid: Grid = Grid::new();

            // Determine max row heights and column widths
            let metagrid_width = metagrid.iter().map(|row| row.len()).max().unwrap();
            let metagrid_height = metagrid.len();
            let mut row_heights = vec![0; metagrid_height];
            let mut column_widths = vec![0; metagrid_width];
            let mut max_lr_lens = vec![(0, 0); metagrid_width];
            for row in 0..metagrid_height {
                let max_row_height = metagrid[row]
                    .iter()
                    .map(|cell| cell.len())
                    .max()
                    .unwrap_or(1);
                row_heights[row] = max_row_height;
            }
            let requires_summary = requires_summary::<T>(&self.shape);
            for col in 0..metagrid_width {
                let max_col_width = if requires_summary {
                    metagrid
                        .iter()
                        .filter_map(|row| row.get(col))
                        .flatten()
                        .map(|row| row.len())
                        .max()
                        .unwrap_or(0)
                } else {
                    T::max_col_width(
                        metagrid
                            .iter()
                            .filter_map(|row| row.get(col))
                            .flatten()
                            .map(Vec::as_slice),
                    )
                };
                column_widths[col] = max_col_width;
                let align = first_align.filter(|_| col == 0).unwrap_or(T::alignment());
                match align {
                    ElemAlign::DelimOrRight(s) | ElemAlign::DelimOrLeft(s) => {
                        for row in metagrid.iter().filter_map(|row| row.get(col)).flatten() {
                            if let Some(pos) = (0..row.len().saturating_sub(s.chars().count()))
                                .find(|&i| row[i..].iter().zip(s.chars()).all(|(&a, b)| a == b))
                            {
                                let right_len = row.len() - pos - s.chars().count();
                                max_lr_lens[col].0 = max_lr_lens[col].0.max(pos);
                                max_lr_lens[col].1 = max_lr_lens[col].1.max(right_len);
                            } else if let ElemAlign::DelimOrRight(_) = align {
                                max_lr_lens[col].0 = max_lr_lens[col].0.max(row.len());
                            } else {
                                max_lr_lens[col].1 = max_lr_lens[col].1.max(row.len());
                            }
                        }
                    }
                    _ => {}
                }
            }
            // Pad each metagrid cell to its row's max height and column's max width
            let horiz_at = |i: usize| {
                (0..self.rank())
                    .rev()
                    .step_by(2)
                    .scan(1, |prod, d| {
                        let is_mul = (i + 1) % *prod == 0;
                        *prod *= self.shape[d];
                        Some(is_mul)
                    })
                    .filter(|&is_mul| is_mul)
                    .count()
                    .saturating_sub(1)
            };
            let vert_at = |i: usize| {
                (0..self.rank().saturating_sub(1))
                    .rev()
                    .step_by(2)
                    .scan(1, |prod, d| {
                        let is_mul = (i + 1) % *prod == 0;
                        *prod *= self.shape[d];
                        Some(is_mul)
                    })
                    .filter(|&is_mul| is_mul)
                    .count()
                    .saturating_sub(1)
            };

            for i in 0..metagrid_height {
                let row_height = row_heights[i];
                let mut subrows = vec![vec![]; row_height];
                let mut div_pos = HashMap::new();
                for (j, ((col_width, max_lr_lens), cell)) in (column_widths.iter())
                    .zip(&max_lr_lens)
                    .zip(&mut metagrid[i])
                    .enumerate()
                {
                    let align = first_align
                        .filter(|_| j == 0)
                        .or_else(|| requires_summary.then_some(ElemAlign::None))
                        .unwrap_or(T::alignment());
                    pad_grid_center(
                        *col_width,
                        row_height,
                        align,
                        false,
                        Some(*max_lr_lens),
                        cell,
                    );
                    for (subrow, cell_row) in subrows.iter_mut().zip(take(cell)) {
                        subrow.extend(cell_row);
                        let horiz = horiz_at(j);
                        // Add column dividers
                        if T::box_lines()
                            && !self.is_map()
                            && (j + 1 < metagrid_width
                                || self.shape[self.rank() - 1 - horiz * 2] == 1)
                        {
                            for mut line in Line::set(horiz) {
                                if j + 1 == metagrid_width {
                                    line = line.singleton();
                                }
                                div_pos.insert(subrow.len(), *line);
                                subrow.push(line.vert());
                            }
                        }
                    }
                }
                let len = subrows.last().unwrap().len();
                grid.extend(subrows);
                // Add row dividers
                let vert = vert_at(i);
                let vert_offset = 2 + vert * 2;
                if T::box_lines()
                    && !self.is_map()
                    && (i + 1 < metagrid_height
                        || self.rank() >= vert_offset && self.shape[self.rank() - vert_offset] == 1)
                {
                    for mut row_line in Line::set(vert) {
                        if i + 1 == metagrid_height {
                            row_line = row_line.singleton();
                        }
                        let row_c = row_line.horiz();
                        let mut row = Vec::with_capacity(len);
                        for k in 0..len {
                            let c = if let Some(col_line) = div_pos.get(&k).copied() {
                                row_line.intersect(col_line)
                            } else {
                                row_c
                            };
                            row.push(c);
                        }
                        grid.push(row);
                    }
                }
            }
            // Outline the grid
            let grid_row_count = grid.len();
            if self.rank() == 0 && self.is_map() {
                // Don't surrond maplings
            } else if !(params.parent_rank == 0
                || T::box_lines() && self.rank() <= 1
                || !T::box_lines() && self.rank() < params.max_boxed_rank
                || T::compress_list_grid() && self.rank() <= 1)
            {
                // Don't surround if going to draw box separators later
                if !T::box_lines()
                    && self.row_count() == 1
                    && (params.max_boxed_len == 1 || T::compress_list_grid())
                {
                    // Disambiguate fixed arrays
                    let fix_amnt = self.shape.iter().take_while(|&&d| d == 1).count();
                    for row in &mut grid {
                        row.extend(repeat(' ').take(fix_amnt));
                        row.rotate_right(fix_amnt);
                    }
                    for i in 0..fix_amnt {
                        grid[0][i] = Primitive::Fix.glyph().unwrap();
                    }
                }
            } else if grid_row_count == 1 && self.rank() == 1 {
                outlined = true;
                // Add brackets to lists
                let (left, right) = if requires_summary || self.is_map() {
                    ('[', ']')
                } else {
                    T::grid_fmt_delims()
                };
                grid[0].insert(0, left);
                grid[0].push(right);
                if T::box_lines() && self.row_count() == 0 && params.parent_rank == 0 {
                    grid[0].insert(1, Primitive::Box.glyph().unwrap());
                }
            } else {
                outlined = true;
                let apparent_rank = if requires_summary { 1 } else { self.rank() };
                // Add corners to non-vectors
                let width = grid[0].len();
                let height = grid.len();
                pad_grid_center(
                    width + 4,
                    (height + 2).max(apparent_rank + 1),
                    ElemAlign::None,
                    true,
                    None,
                    &mut grid,
                );
                grid[0][0] = if T::box_lines() { '┌' } else { '╭' };
                grid[0][1] = '─';
                for i in 0..apparent_rank.saturating_sub(1) {
                    grid[i + 1][0] = '╷';
                }
                *grid.last_mut().unwrap().last_mut().unwrap() =
                    if T::box_lines() { '┘' } else { '╯' };
            }
            grid
        };

        // Add handle kind
        if let Some(kind) = &self.meta.handle_kind {
            if grid.len() == 1 {
                grid[0] = (kind.to_string().chars().chain(['(']))
                    .chain(take(&mut grid[0]))
                    .chain([')'])
                    .collect();
            }
        }

        // Add complex marker
        if T::TYPE_ID == Complex::TYPE_ID && !grid.iter().flatten().any(|&c| c == 'ℂ' || c == 'i')
        {
            if self.shape.is_empty() {
                grid[0].push('ℂ');
            } else if grid.len() == 1 {
                let offset = outlined as usize;
                grid[0].insert(offset, 'ℂ');
                grid[0].insert(offset + 1, ' ');
            } else if outlined {
                grid[0][2] = 'ℂ';
            } else {
                for row in &mut grid {
                    row.push(' ');
                    row.push(' ');
                    row.rotate_right(2);
                }
                grid[0][0] = 'ℂ';
            }
        }

        // Add label
        if params.label {
            if let Some(label) = &self.meta.label {
                if grid.len() == 1 && params.parent_rank == 0 {
                    grid[0] = (label.chars().chain([':', ' ']))
                        .chain(take(&mut grid[0]))
                        .collect();
                } else if grid[0].len() >= 2 && "┌╭".contains(grid[0][1]) {
                    grid[0] = label
                        .chars()
                        .chain(take(&mut grid[0]).into_iter().skip(1))
                        .collect();
                    for row in grid.iter_mut().skip(1) {
                        let ext = label.chars().count() - 1;
                        row.extend(repeat(' ').take(ext));
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
                    while grid[0].len() < grid[1].len() {
                        grid[0].push(' ');
                    }
                }
            }
        }

        // Add pointer
        if let Some(pointer) = self.meta.pointer.filter(|p| !p.raw) {
            if grid.len() == 1 {
                grid[0].extend(format!("(0x{:x})", pointer.ptr).chars());
            }
        }

        // Handle really big grid
        if self.rank() > 1 {
            let max_width = terminal_size().map_or(1000, |(w, _)| w);
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

        grid
    }
}

impl<T: ArrayValue> Array<T> {
    /// Get a string representation of the shape of the array
    pub fn shape_string(&self) -> String {
        let base: String = shape_row::<T>(&self.shape).into_iter().collect();
        if let Some(keys) = &self.meta.map_keys {
            let mut keys_shape = keys.keys.shape.clone();
            keys_shape[0] = self.row_count();
            let mut s: String = match keys.keys {
                Value::Num(_) => shape_row::<f64>(&keys_shape),
                Value::Byte(_) => shape_row::<u8>(&keys_shape),
                Value::Complex(_) => shape_row::<Complex>(&keys_shape),
                Value::Char(_) => shape_row::<char>(&keys_shape),
                Value::Box(_) => shape_row::<Boxed>(&keys_shape),
            }
            .into_iter()
            .collect();
            s.push_str(" → ");
            s.push_str(&base);
            s
        } else {
            base
        }
    }
}

impl Value {
    /// Get a string representation of the shape of the value
    pub fn shape_string(&self) -> String {
        val_as_arr!(self, Array::shape_string)
    }
}

fn shape_row<T: ArrayValue>(shape: &[usize]) -> Vec<char> {
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
    shape_row.push(T::SYMBOL);
    shape_row
}

const MAX_RANK: usize = 8;

fn requires_summary<T: ArrayValue>(shape: &[usize]) -> bool {
    shape.iter().product::<usize>() > T::summary_min_elems() || shape.len() > MAX_RANK
}

fn fmt_array<T: GridFmt + ArrayValue>(
    shape: &[usize],
    data: &[T],
    params: GridFmtParams,
    metagrid: &mut Metagrid,
) {
    if data.is_empty() {
        metagrid.push(vec![vec![shape_row::<T>(shape)]]);
        return;
    }
    if requires_summary::<T>(shape) {
        let summary = T::summarize(data);
        if !summary.is_empty() {
            metagrid.push(vec![if shape.len() == 1 {
                let mut chars = shape_row::<T>(shape);
                chars.extend(": ".chars());
                chars.extend(summary.chars());
                vec![chars]
            } else {
                vec![shape_row::<T>(shape), summary.chars().collect()]
            }]);
            return;
        }
    }
    let rank = shape.len();
    if rank == 0 {
        metagrid.push(vec![data[0].fmt_grid(params)]);
        return;
    }
    if rank == 1 {
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
                    pad_grid_min(grid[0].len() + 1, grid.len(), &mut grid);
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
    for (i, cell) in data.chunks(cell_size).enumerate() {
        if i > 0 && rank > 2 && !T::box_lines() && rank % 2 == 0 {
            for _ in 0..(rank - 2) / 2 {
                metagrid.push(vec![vec![vec![' ']]; metagrid.last().unwrap().len()]);
            }
        }
        let len_before = metagrid.len();
        fmt_array(row_shape, cell, params, metagrid);
        if T::compress_list_grid() && rank == 2 {
            let (left, right) = T::grid_fmt_delims();
            for grid in metagrid.last_mut().unwrap() {
                for row in grid.iter_mut() {
                    row.insert(0, left);
                    row.push(right);
                }
            }
        }
        if i > 0 && (rank > 2 || T::box_lines()) && rank % 2 == 1 {
            let rows = metagrid.split_off(len_before);
            for (mrow, row) in metagrid.iter_mut().skip(start_len).zip(rows) {
                if !T::box_lines() {
                    mrow.push(vec![vec![' '; (rank + 1) / 2]])
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
                    let post = (diff + 1) / 2;
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
