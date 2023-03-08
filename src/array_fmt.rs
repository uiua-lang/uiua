use std::iter::once;

use crate::{
    array::{Array, ArrayType},
    value::{RawType, Value},
};

pub trait GridFmt {
    fn fmt_grid(&self) -> Vec<Vec<char>>;
    fn grid_string(&self) -> String {
        self.fmt_grid()
            .into_iter()
            .flat_map(|v| v.into_iter().chain(once('\n')))
            .collect()
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self) -> Vec<Vec<char>> {
        vec![self.to_string().chars().collect()]
    }
}

impl GridFmt for char {
    fn fmt_grid(&self) -> Vec<Vec<char>> {
        vec![format!("{self:?}").chars().collect()]
    }
}

impl GridFmt for Value {
    fn fmt_grid(&self) -> Vec<Vec<char>> {
        match self.raw_ty() {
            RawType::Num => self.number().fmt_grid(),
            RawType::Char => self.char().fmt_grid(),
            RawType::Function => vec![self.function().to_string().chars().collect()],
            RawType::Partial => vec![self.partial().to_string().chars().collect()],
            RawType::Array => self.array().fmt_grid(),
        }
    }
}

impl GridFmt for Array {
    fn fmt_grid(&self) -> Vec<Vec<char>> {
        let shape = self.shape();
        let mut grid = match self.ty() {
            ArrayType::Num => fmt_array(shape, self.numbers(), false),
            ArrayType::Char => fmt_array(shape, self.chars(), true),
            ArrayType::Value => fmt_array(shape, self.values(), false),
        };
        let row_count = grid.len();
        if row_count == 1 && self.rank() <= 1 {
            if !self.is_chars() {
                grid[0].insert(0, '[');
                grid[0].push(']');
            }
        } else {
            let width = grid[0].len();
            let height = grid.len();
            pad_grid_min(&mut grid, width + 2, (height + 1).max(self.rank()));
            pad_grid_max(&mut grid, width + 4, (height + 2).max(self.rank() + 1));
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
) -> Vec<Vec<char>> {
    if shape.is_empty() {
        return if stringy {
            vec![data[0].to_string().chars().collect()]
        } else {
            data[0].fmt_grid()
        };
    }
    let chunk_size: usize = shape[1..].iter().product();
    let mut cell_grids: Vec<Vec<Vec<char>>> = data
        .chunks(chunk_size)
        .map(|chunk| fmt_array(&shape[1..], chunk, stringy))
        .collect();
    let max_width = cell_grids
        .iter()
        .map(|grid| grid[0].len())
        .max()
        .unwrap_or(1);
    let max_height = cell_grids.iter().map(|grid| grid.len()).max().unwrap_or(1);
    if shape.len() == 1 {
        for grid in &mut cell_grids {
            pad_grid_min(grid, grid[0].len(), max_height);
        }
        let init = if stringy { vec!['"'] } else { Vec::new() };
        let mut rows = vec![init; max_height];
        for (i, grid) in cell_grids.into_iter().enumerate() {
            for (j, mut row) in grid.into_iter().enumerate() {
                if i > 0 && !stringy {
                    rows[j].push(' ');
                }
                rows[j].append(&mut row);
            }
        }
        for row in &mut rows {
            if stringy {
                row.push('"');
            }
        }
        rows
    } else {
        for grid in &mut cell_grids {
            pad_grid_min(grid, max_width, grid.len());
        }
        let mut rows = Vec::new();
        for (i, grid) in cell_grids.into_iter().enumerate() {
            if i > 0 {
                for _ in 0..shape.len() - 2 {
                    rows.push(vec![' '; max_width]);
                }
            }
            for row in grid {
                rows.push(row);
            }
        }
        rows
    }
}

fn pad_grid_min(grid: &mut Vec<Vec<char>>, width: usize, height: usize) {
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

fn pad_grid_max(grid: &mut Vec<Vec<char>>, width: usize, height: usize) {
    for row in grid.iter_mut() {
        row.resize(width, ' ');
    }
    grid.resize(height, vec![' '; width]);
}
