use std::{
    f64::{consts::PI, INFINITY},
    iter::once,
    mem::take,
};

use crate::{
    array::{Array, ArrayType},
    function::Function,
    value::{Type, Value},
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

impl GridFmt for u8 {
    fn fmt_grid(&self) -> Grid {
        vec![format!("{self:02X}").chars().collect()]
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self) -> Grid {
        let positive = self.abs();
        let minus = if *self < -0.0 { "¯" } else { "" };
        let s = if positive == PI {
            format!("{minus}π")
        } else if positive == INFINITY {
            format!("{minus}∞")
        } else {
            format!("{minus}{positive}")
        };
        vec![s.chars().collect()]
    }
}

impl GridFmt for char {
    fn fmt_grid(&self) -> Grid {
        vec![format!("{self:?}").chars().collect()]
    }
}

impl GridFmt for Value {
    fn fmt_grid(&self) -> Grid {
        match self.ty() {
            Type::Num => self.number().fmt_grid(),
            Type::Byte => self.byte().fmt_grid(),
            Type::Char => self.char().fmt_grid(),
            Type::Function => self.function().fmt_grid(),
            Type::Array => self.array().fmt_grid(),
        }
    }
}

impl GridFmt for Array {
    fn fmt_grid(&self) -> Grid {
        // Fill the metagrid
        let mut metagrid = Metagrid::new();
        let shape = self.shape();

        // Handle really big arrays
        if shape.iter().product::<usize>() > 1000 {
            let mut s = String::from('[');
            for (i, d) in shape.iter().enumerate() {
                if i > 0 {
                    s.push_str(" × ");
                }
                s.push_str(&d.to_string());
            }
            match self.ty() {
                ArrayType::Num => s.push_str(" numbers"),
                ArrayType::Byte => s.push_str(" bytes"),
                ArrayType::Char => s.push_str(" chars"),
                ArrayType::Value => s.push_str(" array"),
            }
            s.push(']');
            return vec![s.chars().collect()];
        }

        match self.ty() {
            ArrayType::Num => fmt_array(shape, self.numbers(), false, &mut metagrid),
            ArrayType::Byte => fmt_array(shape, self.bytes(), false, &mut metagrid),
            ArrayType::Char => fmt_array(shape, self.chars(), true, &mut metagrid),
            ArrayType::Value => fmt_array(shape, self.values(), false, &mut metagrid),
        }
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
        let mut grid: Grid = Grid::new();
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
            if !self.is_chars() {
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
        }
        grid
    }
}

fn fmt_array<T: GridFmt + std::fmt::Display>(
    shape: &[usize],
    data: &[T],
    stringy: bool,
    metagrid: &mut Metagrid,
) {
    let rank = shape.len();
    if rank == 0 {
        metagrid.push(vec![data[0].fmt_grid()]);
        return;
    }
    if rank == 1 {
        let mut row = Vec::with_capacity(shape[0]);
        if stringy {
            let mut s = String::new();
            s.extend(data.iter().map(|c| c.to_string()));
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

impl GridFmt for Function {
    fn fmt_grid(&self) -> Grid {
        vec![self.to_string().chars().collect()]
    }
}
