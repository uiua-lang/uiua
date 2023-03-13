use std::{iter::once, mem::take};

use crate::{
    array::{Array, ArrayType},
    function::{Function, Partial, PrimitiveId},
    ops::HigherOp,
    value::{RawType, Value},
};

type Grid<T = char> = Vec<Vec<T>>;
type Metagrid = Grid<Grid>;

pub trait GridFmt {
    fn fmt_grid(&self) -> Grid;
    fn grid_string(&self) -> String {
        self.fmt_grid()
            .into_iter()
            .flat_map(|v| v.into_iter().chain(once('\n')))
            .collect()
    }
}

impl GridFmt for f64 {
    fn fmt_grid(&self) -> Grid {
        vec![self.to_string().chars().collect()]
    }
}

impl GridFmt for char {
    fn fmt_grid(&self) -> Grid {
        vec![format!("{self:?}").chars().collect()]
    }
}

impl GridFmt for Value {
    fn fmt_grid(&self) -> Grid {
        match self.raw_ty() {
            RawType::Num => self.number().fmt_grid(),
            RawType::Char => self.char().fmt_grid(),
            RawType::Function => self.function().fmt_grid(),
            RawType::Partial => self.partial().fmt_grid(),
            RawType::Array => self.array().fmt_grid(),
        }
    }
}

impl GridFmt for Array {
    fn fmt_grid(&self) -> Grid {
        // Fill the metagrid
        let mut metagrid = Metagrid::new();
        let shape = self.shape();
        match self.ty() {
            ArrayType::Num => fmt_array(shape, self.numbers(), false, &mut metagrid),
            ArrayType::Char => fmt_array(shape, self.chars(), true, &mut metagrid),
            ArrayType::Value => fmt_array(shape, self.values(), false, &mut metagrid),
        }
        // Determine max row heights and column widths
        let metagrid_width = metagrid.iter().map(|row| row.len()).max().unwrap();
        let metagrid_height = metagrid.len();
        let mut column_widths = vec![0; metagrid_width];
        let mut row_heights = vec![0; metagrid_height];
        for row in 0..metagrid_height {
            let max_row_height = metagrid[row].iter().map(|cell| cell.len()).max().unwrap();
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
            metagrid.push(vec![empty_grid(); metagrid.last().unwrap().len()]);
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

fn pad_grid_max(width: usize, height: usize, grid: &mut Grid) {
    for row in grid.iter_mut() {
        row.resize(width, ' ');
    }
    grid.resize(height, vec![' '; width]);
}

impl GridFmt for Function {
    fn fmt_grid(&self) -> Grid {
        vec![self.to_string().chars().collect()]
    }
}

#[allow(clippy::needless_range_loop)]
fn draw_grid_line(ax: usize, ay: usize, bx: usize, by: usize, grid: &mut Grid) {
    let dx = bx as f64 - ax as f64;
    let dy = by as f64 - ay as f64;
    let slope = dy / dx;
    let fill_char = if slope > 0.0 {
        if slope < 0.5 {
            '⟍'
        } else if slope < 1.5 {
            '╲'
        } else {
            '│'
        }
    } else if slope > -0.5 {
        '⟋'
    } else if slope > -1.5 {
        '╱'
    } else {
        '│'
    };
    let min_x = ax.min(bx);
    let max_x = ax.max(bx);
    let min_y = ay.min(by);
    let max_y = ay.max(by);
    for y in min_y..=max_y {
        let x = ((y as f64 - ay as f64) / slope + ax as f64).round() as usize;
        if x >= min_x && x <= max_x {
            grid[y][x] = fill_char;
        }
    }
}

fn empty_grid() -> Grid {
    vec![vec![' ']]
}

impl GridFmt for Partial {
    fn fmt_grid(&self) -> Grid {
        let args: Vec<_> = self.args.iter().collect();
        let mut arg_grids: Vec<Grid> = args.iter().map(|arg| arg.fmt_grid()).collect();
        let mut grid = self.function.fmt_grid();
        let tree = if let Some(PrimitiveId::HigherOp(hop)) = self.function.primitive {
            match hop {
                HigherOp::Compose => {
                    if let Some(f) = arg_grids.pop() {
                        grid = f;
                        if arg_grids.is_empty() {
                            arg_grids.push(empty_grid());
                        }
                    }
                }
                HigherOp::BlackBird => {
                    if let Some(f) = arg_grids.pop() {
                        grid = f;
                        if let Some(g) = arg_grids.pop() {
                            if let Some(a) = arg_grids.pop() {
                                arg_grids.push(layout_function_grid(
                                    g,
                                    vec![empty_grid(), a],
                                    true,
                                ));
                            } else {
                                arg_grids.push(empty_grid());
                            };
                        } else {
                            arg_grids.push(empty_grid());
                        }
                    }
                }
                HigherOp::Flip => {
                    if let Some(f) = arg_grids.pop() {
                        grid = f;
                        if let Some(a) = arg_grids.pop() {
                            arg_grids.push(a);
                            arg_grids.push(empty_grid());
                        } else {
                            arg_grids = vec![empty_grid(); 2];
                        }
                    }
                }
                HigherOp::LeftLeaf => {
                    if arg_grids.len() >= 2 {
                        let g = arg_grids.pop().unwrap();
                        let f = arg_grids.pop().unwrap();
                        grid = f;
                        arg_grids.push(empty_grid());
                        arg_grids.push(g);
                    }
                }
                HigherOp::RightLeaf => {
                    if arg_grids.len() >= 2 {
                        let f = arg_grids.pop().unwrap();
                        let g = arg_grids.pop().unwrap();
                        grid = f;
                        arg_grids.push(g);
                        arg_grids.push(empty_grid());
                    }
                }
                _ => {}
            }
            true
        } else {
            false
        };
        layout_function_grid(grid, arg_grids, tree)
    }
}

fn inlay_grid(left: usize, top: usize, parent: &mut Grid, child: &Grid) {
    for (r, row) in child.iter().enumerate() {
        for (c, cell) in row.iter().enumerate() {
            parent[top + r][left + c] = *cell;
        }
    }
}

fn layout_function_grid(mut grid: Grid, mut arg_grids: Vec<Grid>, tree: bool) -> Grid {
    arg_grids.reverse();
    let max_arg_height = arg_grids.iter().map(|grid| grid.len()).max().unwrap();
    for grid in &mut arg_grids {
        pad_grid_max(grid[0].len(), max_arg_height, grid);
    }
    let total_args_width =
        arg_grids.iter().map(|grid| grid[0].len()).sum::<usize>() + 2 * (arg_grids.len() - 1);
    let mut attach_xs = Vec::with_capacity(arg_grids.len());
    let mut curr_x = 0;
    for grid in &arg_grids {
        attach_xs.push(curr_x + grid[0].len() / 2);
        curr_x += grid[0].len() + 2;
    }
    let attach_range = attach_xs.last().unwrap() - attach_xs[0];
    if tree {
        let parent_width = grid[0].len();
        pad_grid_center(
            grid[0].len().max(total_args_width),
            grid.len(),
            false,
            &mut grid,
        );
        let center_x = grid[0].len() as f32 / 2.0;
        let parent_bottom = grid.len();
        let args_y = parent_bottom + (attach_range / 4).max(1);
        pad_grid_max(grid[0].len(), args_y + max_arg_height, &mut grid);
        let mut curr_x = 0;
        let arg_count = arg_grids.len();
        for (i, (arg_grid, attach_x)) in arg_grids.into_iter().zip(attach_xs).enumerate() {
            let ax = (center_x - parent_width as f32 / 2.0
                + parent_width as f32 * (i as f32 + 0.4) / arg_count as f32)
                .round() as usize;
            draw_grid_line(ax, parent_bottom, attach_x, args_y, &mut grid);
            inlay_grid(curr_x, args_y, &mut grid, &arg_grid);
            curr_x += arg_grid[0].len() + 2;
        }
    } else {
        pad_grid_center(grid[0].len(), max_arg_height, false, &mut grid);
        for (r, row) in grid.iter_mut().enumerate() {
            for arg_grid in &mut arg_grids {
                row.push(' ');
                row.extend(arg_grid[r].iter().copied());
            }
        }
    }
    grid
}
