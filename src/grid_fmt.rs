//! Pretty printing Uiua arrays

use std::{
    any::type_name,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    iter::once,
    mem::take,
};

use crate::{
    array::{Array, ArrayValue},
    boxed::Boxed,
    value::Value,
    Complex, Primitive,
};

type Grid<T = char> = Vec<Vec<T>>;
type Metagrid = Grid<Grid>;

pub trait GridFmt {
    fn fmt_grid(&self, boxed: bool) -> Grid;
    fn grid_string(&self) -> String {
        let mut s: String = self
            .fmt_grid(false)
            .into_iter()
            .flat_map(|v| v.into_iter().chain(once('\n')))
            .collect();
        s.pop();
        s
    }
}

fn boxed_scalar(boxed: bool) -> impl Iterator<Item = char> {
    boxed.then_some(Primitive::Box.glyph().unwrap()).into_iter()
}

impl GridFmt for u8 {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        vec![boxed_scalar(boxed)
            .chain(self.to_string().chars())
            .collect()]
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        let positive = self.abs();
        let minus = if *self < -0.0 { "¯" } else { "" };
        let s = if (positive - PI).abs() < f64::EPSILON {
            format!("{minus}π")
        } else if (positive - TAU).abs() < f64::EPSILON {
            format!("{minus}τ")
        } else if (positive - PI / 2.0).abs() < f64::EPSILON {
            format!("{minus}η")
        } else if positive == INFINITY {
            format!("{minus}∞")
        } else {
            format!("{minus}{positive}")
        };
        vec![boxed_scalar(boxed).chain(s.chars()).collect()]
    }
}

impl GridFmt for Complex {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        if self.im == 0.0 {
            self.re.fmt_grid(boxed)
        } else if self.re == 0.0 {
            let mut grid = self.im.fmt_grid(boxed);
            grid[0].push('i');
            grid
        } else {
            let mut re = self.re.fmt_grid(boxed);
            let im = if self.im.abs() == 1.0 {
                String::new()
            } else {
                self.im.abs().grid_string()
            };
            let sign = if self.im < 0.0 { '-' } else { '+' };
            re[0].push(sign);
            re[0].extend(im.chars());
            re[0].push('i');
            re
        }
    }
}

pub fn format_char_inner(c: char) -> String {
    if c == char::MAX {
        return '_'.to_string();
    }
    let formatted = format!("{c:?}");
    if c == '\'' {
        "'".to_string()
    } else if formatted.starts_with("'\\u{") {
        let n = c as u32;
        if n < 128 {
            format!("\\x{:02x}", n)
        } else {
            format!("\\u{:04x}", n)
        }
    } else {
        formatted[1..formatted.len() - 1].to_string()
    }
}

impl GridFmt for char {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        vec![once(if boxed { '⌞' } else { '@' })
            .chain(format_char_inner(*self).chars())
            .collect()]
    }
}

impl GridFmt for Boxed {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        let mut grid = match self.as_value() {
            Value::Num(array) => array.fmt_grid(true),
            #[cfg(feature = "bytes")]
            Value::Byte(array) => array.fmt_grid(true),

            Value::Complex(array) => array.fmt_grid(true),
            Value::Char(array) => array.fmt_grid(true),
            Value::Box(array) => array.fmt_grid(true),
        };
        if boxed && grid.len() == 1 {
            grid = vec![boxed_scalar(true)
                .chain(grid.into_iter().flatten())
                .collect()];
        }
        grid
    }
}

impl<T: GridFmt + ArrayValue> GridFmt for Array<T> {
    fn fmt_grid(&self, boxed: bool) -> Grid {
        if self.shape.is_empty() {
            return self.data[0].fmt_grid(boxed);
        }
        let stringy = type_name::<T>() == type_name::<char>();
        let boxy = type_name::<T>() == type_name::<Boxed>();
        let complexy = type_name::<T>() == type_name::<Complex>();
        if *self.shape == [0] {
            return if stringy {
                if boxed {
                    vec![vec!['⌜', '⌟']]
                } else {
                    vec![vec!['"', '"']]
                }
            } else {
                let (left, right) = if boxed { ('⟦', '⟧') } else { ('[', ']') };
                if boxy {
                    vec![vec![left, '□', right]]
                } else if complexy {
                    vec![vec![left, 'ℂ', right]]
                } else {
                    vec![vec![left, right]]
                }
            };
        }

        // Fill the metagrid
        let mut metagrid = Metagrid::new();

        let mut grid: Grid = Grid::new();

        fmt_array(&self.shape, &self.data, stringy, boxed, &mut metagrid);
        // Determine max row heights and column widths
        let metagrid_width = metagrid.iter().map(|row| row.len()).max().unwrap();
        let metagrid_height = metagrid.len();
        let mut column_widths = vec![0; metagrid_width];
        let mut row_heights = vec![0; metagrid_height];
        for row in 0..metagrid_height {
            let max_row_height = metagrid[row]
                .iter()
                .map(|cell| cell.len())
                .max()
                .unwrap_or(1);
            row_heights[row] = max_row_height;
        }
        for col in 0..metagrid_width {
            let max_col_width = metagrid
                .iter_mut()
                .flat_map(|row| row.get(col)?.iter().map(|cell| cell.len()).max())
                .max()
                .unwrap();
            column_widths[col] = max_col_width;
        }
        // Pad each metagrid cell to its row's max height and column's max width
        for row in 0..metagrid_height {
            let row_height = row_heights[row];
            let mut subrows = vec![vec![]; row_height];
            for (col_width, cell) in column_widths.iter().zip(&mut metagrid[row]) {
                pad_grid_center(*col_width, row_height, true, cell);
                for (subrow, cell_row) in subrows.iter_mut().zip(take(cell)) {
                    subrow.extend(cell_row);
                }
            }
            grid.extend(subrows);
        }
        // Outline the grid
        let row_count = grid.len();
        if row_count == 1 && self.rank() == 1 {
            // Add brackets to vectors
            if !stringy {
                let (left, right) = if boxed { ('⟦', '⟧') } else { ('[', ']') };
                grid[0].insert(0, left);
                grid[0].push(right);
            }
        } else {
            // Add corners to non-vectors
            let width = grid[0].len();
            let height = grid.len();
            pad_grid_center(
                width + 4,
                (height + 2).max(self.rank() + 1),
                false,
                &mut grid,
            );
            grid[0][0] = if boxed { '╓' } else { '╭' };
            grid[0][1] = '─';
            for i in 0..self.rank().saturating_sub(1) {
                grid[i + 1][0] = if boxed { '╟' } else { '╷' };
            }
            *grid.last_mut().unwrap().last_mut().unwrap() = if boxed { '╜' } else { '╯' };
            // Handle really big grid
            let max_width = term_size::dimensions().map_or(54, |(w, _)| w);
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

fn fmt_array<T: GridFmt + ArrayValue>(
    shape: &[usize],
    data: &[T],
    stringy: bool,
    boxed: bool,
    metagrid: &mut Metagrid,
) {
    if data.is_empty() {
        let mut shape_row = Vec::new();
        for (i, dim) in shape.iter().enumerate() {
            if i > 0 {
                shape_row.extend("×".chars());
            }
            shape_row.extend(dim.to_string().chars());
        }
        shape_row.push(' ');
        shape_row.push(T::SYMBOL);
        metagrid.push(vec![vec![shape_row]]);
        return;
    }
    let rank = shape.len();
    if rank == 0 {
        metagrid.push(vec![data[0].fmt_grid(false)]);
        return;
    }
    if rank == 1 {
        let mut row = Vec::with_capacity(shape[0]);
        if stringy {
            let mut s = String::new();
            s.extend(data.iter().map(|c| c.to_string()));
            let mut s: String = s.chars().map(format_char_inner).collect();
            if boxed {
                s.insert(0, '⌜');
                s.push('⌟');
            } else {
                s.insert(0, '"');
                s.push('"');
            }
            row.push(vec![s.chars().collect()]);
        } else {
            for (i, val) in data.iter().enumerate() {
                let mut grid = val.fmt_grid(false);
                if i > 0 {
                    pad_grid_min(grid[0].len() + 1, grid.len(), &mut grid)
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
    let shape = &shape[1..];
    let cell_size = data.len() / cell_count;
    for (i, cell) in data.chunks(cell_size).enumerate() {
        if i > 0 && rank > 2 {
            for _ in 0..rank - 2 {
                metagrid.push(vec![vec![vec![' ']]; metagrid.last().unwrap().len()]);
            }
        }
        fmt_array(shape, cell, stringy, false, metagrid);
        if i * cell_size > 1000 {
            let mut elipses_row = Vec::new();
            for prev_grid in metagrid.last().unwrap() {
                let prev_row = &prev_grid[0];
                let mut new_row = Vec::with_capacity(prev_row.len());
                for c in prev_row {
                    new_row.push(if c.is_whitespace() { ' ' } else { '⋮' });
                }
                elipses_row.push(vec![new_row]);
            }
            metagrid.push(elipses_row);
            break;
        }
    }
}

fn pad_grid_center(width: usize, height: usize, align_numbers: bool, grid: &mut Grid) {
    grid.truncate(height);
    if grid.len() < height {
        let diff = height - grid.len();
        let post_pad = diff / 2;
        let pre_pad = diff - post_pad;
        for _ in 0..pre_pad {
            grid.insert(0, vec![' '; width]);
        }
        for _ in 0..post_pad {
            grid.push(vec![' '; width]);
        }
    }
    for row in grid.iter_mut() {
        row.truncate(width);
        if row.len() < width {
            let diff = width - row.len();
            let post_pad = if align_numbers && row.last().map_or(false, char::is_ascii_digit) {
                0
            } else {
                diff / 2
            };
            let pre_pad = diff - post_pad;
            for _ in 0..pre_pad {
                row.insert(0, ' ');
            }
            for _ in 0..post_pad {
                row.push(' ');
            }
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
