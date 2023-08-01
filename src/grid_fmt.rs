//! Pretty printing Uiua arrays

use std::{
    any::type_name,
    f64::{
        consts::{PI, TAU},
        INFINITY,
    },
    iter::once,
    mem::take,
    rc::Rc,
};

use crate::{
    array::{Array, ArrayValue},
    function::Function,
    value::Value,
    Byte,
};

type Grid<T = char> = Vec<Vec<T>>;
type Metagrid = Grid<Grid>;

pub trait GridFmt {
    fn fmt_grid(&self) -> Grid;
    fn grid_string(&self) -> String {
        let mut s: String = self
            .fmt_grid()
            .into_iter()
            .flat_map(|v| v.into_iter().chain(once('\n')))
            .collect();
        s.pop();
        s
    }
}

impl GridFmt for Byte {
    fn fmt_grid(&self) -> Grid {
        if self.is_fill_value() {
            vec![vec!['_']]
        } else {
            vec![self.to_string().chars().collect()]
        }
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self) -> Grid {
        if self.is_fill_value() {
            vec![vec!['_']]
        } else {
            let positive = (self.abs() * 1e12).round() / 1e12;
            let minus = if *self < -0.0 { "¯" } else { "" };
            let s = if (positive - PI).abs() < 1e-12 {
                format!("{minus}π")
            } else if (positive - TAU).abs() < 1e-12 {
                format!("{minus}τ")
            } else if positive == INFINITY {
                format!("{minus}∞")
            } else {
                format!("{minus}{positive}")
            };
            vec![s.chars().collect()]
        }
    }
}

impl GridFmt for char {
    fn fmt_grid(&self) -> Grid {
        if self.is_fill_value() {
            vec![vec![' ']]
        } else {
            vec![format!("{self:?}").chars().collect()]
        }
    }
}

impl GridFmt for Rc<Function> {
    fn fmt_grid(&self) -> Grid {
        vec![self.to_string().chars().collect()]
    }
}

impl GridFmt for Value {
    fn fmt_grid(&self) -> Grid {
        match self {
            Value::Num(array) => array.fmt_grid(),
            Value::Byte(array) => array.fmt_grid(),
            Value::Char(array) => array.fmt_grid(),
            Value::Func(array) => array.fmt_grid(),
        }
    }
}

impl<T: GridFmt + ArrayValue> GridFmt for Array<T> {
    fn fmt_grid(&self) -> Grid {
        if self.shape.is_empty() {
            return self.data[0].fmt_grid();
        }
        // Fill the metagrid
        let mut metagrid = Metagrid::new();

        // Handle really big arrays
        let mut just_dims = false;
        if self.shape.len() > 1 {
            let columns = *self.shape.last().unwrap();
            if let Some((w, _)) = term_size::dimensions() {
                if columns > w / 2 - 1 {
                    just_dims = true;
                }
            } else if columns > 40 {
                just_dims = true;
            } else {
                let rows = self.shape.iter().rev().skip(1).product::<usize>();
                if rows > 100 {
                    just_dims = true;
                }
            }
        }

        let mut grid: Grid = Grid::new();

        if !just_dims {
            let stringy = type_name::<T>() == type_name::<char>();
            fmt_array(&self.shape, &self.data, stringy, &mut metagrid);
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
                    .iter()
                    .map(|row| row[col].iter().map(|cell| cell.len()).max().unwrap())
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
                    grid[0].insert(0, '[');
                    grid[0].push(']');
                }
            } else {
                // Add corners to non-vectors
                let width = grid[0].len();
                let height = grid.len();
                pad_grid_center(
                    width + 4,
                    (height + 2).max(self.rank() + 2),
                    false,
                    &mut grid,
                );
                grid[0][0] = '┌';
                grid[0][1] = '─';
                for i in 0..self.rank() {
                    grid[i + 1][0] = '·';
                }
                *grid.last_mut().unwrap().last_mut().unwrap() = '┘';
                // Handle really big grid
                if let Some((w, _)) = term_size::dimensions() {
                    for row in grid.iter_mut() {
                        if row.len() > w {
                            let diff = row.len() - w;
                            row.truncate(w);
                            if !(row[w - 1].is_whitespace() && diff == 1)
                                && (2..4).any(|i| !row[w - i].is_whitespace())
                            {
                                row[w - 1] = '…';
                            }
                        }
                    }
                }
            }
        }

        if just_dims {
            let mut s = String::from('[');
            for (i, d) in self.shape.iter().enumerate() {
                if i > 0 {
                    s.push_str(" × ");
                }
                s.push_str(&d.to_string());
            }
            s.push(' ');
            s.push_str(T::NAME);
            s.push(']');
            return vec![s.chars().collect()];
        }
        grid
    }
}

fn fmt_array<T: GridFmt + ArrayValue>(
    shape: &[usize],
    data: &[T],
    stringy: bool,
    metagrid: &mut Metagrid,
) {
    if data.is_empty() {
        metagrid.push(vec![vec![vec![' ']]]);
        return;
    }
    let rank = shape.len();
    if rank == 0 {
        metagrid.push(vec![data[0].fmt_grid()]);
        return;
    }
    if rank == 1 {
        let mut row = Vec::with_capacity(shape[0]);
        if stringy {
            let mut s = String::new();
            s.extend(data.iter().map(|c| {
                if c.is_fill_value() {
                    " ".into()
                } else {
                    c.to_string()
                }
            }));
            row.push(vec![format!("{s:?}").chars().collect()]);
        } else {
            for (i, val) in data.iter().enumerate() {
                let mut grid = val.fmt_grid();
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
        fmt_array(shape, cell, stringy, metagrid);
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
