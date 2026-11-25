//! Pretty printing Uiua arrays

use std::{
    any::type_name,
    collections::HashMap,
    iter::{once, repeat_n},
    mem::take,
};

use ecow::EcoString;

use crate::{
    ArrayMeta, Complex, Primitive,
    array::{Array, ArrayValue},
    boxed::Boxed,
    terminal_size, val_as_arr,
    value::Value,
};

mod metagrid;
mod scalar;
use metagrid::{Grid, Metagrid};

pub use scalar::{format_char_inner_pretty, format_char_inner_repr};

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
            .rows()
            .flat_map(|row| row.iter().copied().chain(once('\n')))
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
    /// How to align elements when formatting
    fn alignment() -> ElemAlign {
        ElemAlign::Left
    }
    /// Get SoA rows
    fn soa_rows(_arr: &Array<Self>) -> Option<Vec<(&EcoString, &Value)>> {
        None
    }
    /// The maximum number of distinct elements that can be summarized using the multiset format
    const SUMMARY_MULTISET_MAX_DISTINCT: usize = 5;
}

impl GridFmt for Value {
    fn fmt_grid(&self, params: GridFmtParams) -> Grid {
        if params.depth > 100 {
            return Grid::from_scalar('…');
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
            match grid.len_0() {
                0 => {}
                1 => grid.0[0].insert(0, symbol),
                height => {
                    for (i, row) in grid.rows_mut().enumerate() {
                        let ch = if i == height / 2 { symbol } else { ' ' };
                        row.insert(0, ch);
                    }
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
            let s = format!("0x{:x}: {}", pointer.ptr, ffi_type);
            Grid::from_utf8(&s)
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
            Grid::from_utf32(row)
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
                    let arrow = Grid::from_utf8(" → ");
                    let value = value.fmt_grid(params);
                    metagrid.push(vec![key, arrow, value]);
                }
                if metagrid.is_empty() {
                    let mut keys_row_shape = keys.keys.shape.clone();
                    keys_row_shape.make_row();
                    let mut row = shape_row(&keys_row_shape, keys.keys.domain_symbol());
                    row.extend([' ', '→', ' ']);
                    let mut value_row_shape = self.shape.clone();
                    value_row_shape.make_row();
                    row.extend(shape_row(&value_row_shape, self.domain_symbol()));
                    *metagrid = Metagrid::from_scalar(Grid::from_utf32(row));
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
                    labels_row.push(Grid::from_utf8(label));
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
                let params = GridFmtParams {
                    parent_rank: self.rank(),
                    ..params
                };
                let mut metagrid = fmt_array(&self.data, &self.shape, params);
                if !T::box_lines() {
                    add_spacing(&mut metagrid, &self.shape);
                }
                metagrid
            });

            let is_summary = requires_summary::<T>(&self.shape);

            let main_align = if is_summary {
                ElemAlign::None
            } else {
                T::alignment()
            };

            let dimensions = MetagridDimensions::of(&metagrid);

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
                        T::compress_list_grid(),
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
            let grid: &mut Vec<Vec<char>> = &mut grid.0;

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

/// Transform the data and shape of a Uiua array into a 2x2-dimensional Metagrid of text.
/// For the standard formatting style, this function is responsible for:
///  - deciding whether to use a summary
///  - iterating over the array's contents and formatting each scalar into a Grid
///  - reshaping the N-dimensional collection of formatted values into a 2-dimensional (meta)grid
fn fmt_array<T: GridFmt + ArrayValue>(
    data: &[T],
    shape: &[usize],
    params: GridFmtParams,
) -> Metagrid {
    assert_eq!(data.len(), shape.iter().product::<usize>());

    if data.is_empty() {
        return Metagrid::from_scalar(Grid::from_utf32(shape_row(shape, T::SYMBOL)));
    }

    let rank = shape.len();

    if requires_summary::<T>(shape) {
        let summary = T::summarize(data);
        if !summary.is_empty() {
            // This is going to need some changes for multiline summaries.
            let mut first_row = shape_row(shape, T::domain_symbol(data));
            let grid = if rank == 1 {
                first_row.extend(": ".chars());
                first_row.extend(summary.chars());
                Grid::from_row(first_row)
            } else {
                Grid(vec![first_row, summary.chars().collect()])
            };
            return Metagrid::from_scalar(grid);
        }
    }

    if rank == 0 {
        return Metagrid::from_scalar(data[0].fmt_grid(params));
    }

    if rank == 1 {
        if T::compress_list_grid() {
            let mut s: String = data
                .iter()
                .map(|c| c.to_string())
                .collect::<String>()
                .chars()
                .map(format_char_inner_pretty)
                .collect();

            let (left, right) = T::grid_fmt_delims();
            s.insert(0, left);
            s.push(right);

            return Metagrid::from_scalar(Grid::from_utf8(&s));
        }

        let mut meta_row = vec![];
        for v in data {
            meta_row.push(v.fmt_grid(params));
        }
        return Metagrid::from_row(meta_row);
    }

    let mut metagrid = Metagrid::new();

    assert!(data.len().is_multiple_of(shape[0]));
    let chunk_size = data.len() / shape[0];

    for row in data.chunks(chunk_size) {
        let new_lane = fmt_array(row, &shape[1..], params);
        if shape.len().is_multiple_of(2) {
            metagrid.append_0(new_lane);
        } else {
            if metagrid.is_empty() {
                metagrid.0.resize(new_lane.len_0(), vec![]);
            }
            metagrid.append_1(new_lane);
        }
    }

    metagrid
}

// TODO: This logic ought to be much closer to the code for adding grid lines to arrays of boxes.
fn add_spacing(metagrid: &mut Metagrid, shape: &[usize]) {
    for x in (1..metagrid.len_1()).rev() {
        let spacing = 1 + count_dividers(x - 1, shape, 1);
        let g = Grid::from_scalar_shape(' ', (1, spacing));
        for meta_row in metagrid.rows_mut() {
            meta_row.insert(x, g.clone());
        }
    }
    for y in (1..metagrid.len_0()).rev() {
        let spacing = count_dividers(y - 1, shape, 0);
        if spacing == 0 {
            continue;
        }
        let g = Grid::from_scalar_shape(' ', (spacing, 1));
        metagrid.0.insert(y, vec![g; metagrid.len_1()]);
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

fn max_of_lengths<T>(rows: impl IntoIterator<Item: AsRef<[T]>>, fill: usize) -> usize {
    rows.into_iter()
        .map(|row| row.as_ref().len())
        .max()
        .unwrap_or(fill)
}

// This entire struct's days are numbered.
struct MetagridDimensions {
    metagrid_height: usize,
    metagrid_width: usize,
    row_heights: Vec<usize>,
}

impl MetagridDimensions {
    fn of(metagrid: &Metagrid) -> Self {
        let metagrid_height = metagrid.len_0();
        let metagrid_width = metagrid.len_1();

        let row_heights: Vec<usize> = metagrid
            .rows()
            .map(|metarow| max_of_lengths(metarow.iter().map(|g| &g.0), 1))
            .collect();

        Self {
            metagrid_height,
            metagrid_width,
            row_heights,
        }
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "this code was extracted from <Array<T> as GridFmt>::grid_fmt largely to avoid monomorphization, \
              and several of the arguments would otherwise be available as trait methods on T"
)]
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

    assert!(metagrid.is_rectangular());

    let MetagridDimensions {
        metagrid_height,
        metagrid_width,
        row_heights,
    } = dimensions;

    // Pad each metagrid cell to its row's max height and column's max width
    for y in 0..metagrid.len_0() {
        metagrid.align_row(y);
    }
    for x in 0..metagrid.len_1() {
        let alignment = if x == 0 { first_align } else { main_align };
        metagrid.align_column(x, alignment);
    }
    assert!(metagrid.is_meta_rectangular());

    // Actually combine the cells and insert dividers
    for (meta_y, metarow) in metagrid.0.into_iter().enumerate() {
        let row_height = row_heights[meta_y];
        // The rows of the output grid that we're currently working on
        let mut subrows = Grid(vec![vec![]; row_height]);
        // The horizontal positions of vertical gridlines
        let mut divider_intersections = HashMap::<usize, Line>::new();

        let vertical_offset = count_dividers(meta_y, shape, 1);

        for (meta_x, meta_cell) in metarow.into_iter().enumerate() {
            let horizontal_offset = count_dividers(meta_x, shape, 0);

            for (subrow, inner_row) in subrows.rows_mut().zip(meta_cell.rows()) {
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

        let len = subrows.len_1();
        output_grid.append_0(subrows);

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
    compress_list_grid: bool,
    list_brackets: (char, char),
) {
    let rank = shape.len();
    let grid_row_count = grid.len_0();

    let force_multiline = params.multiline && !compress_list_grid;

    if grid_row_count == 1 && rank == 1 && !force_multiline {
        let grid = &mut grid.0;
        // TODO: this is a hack until responsibility for string quotes is properly figured out
        if list_brackets != ('"', '"') {
            grid[0].insert(0, list_brackets.0);
            grid[0].push(list_brackets.1);
        }
        if box_lines && shape[0] == 0 && params.parent_rank == 0 {
            grid[0].insert(1, Primitive::Box.glyph().unwrap());
        }
    } else {
        let apparent_rank = if is_summary { 1 } else { rank };
        // Add corners to non-vectors
        grid.ensure_height((grid.len_0() + 2).max(apparent_rank + 1), true);
        grid.ensure_width(grid.len_1() + 4, true);
        let grid = &mut grid.0;
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

    for row in grid.rows_mut() {
        row.extend(repeat_n(' ', fix_amount));
        row.rotate_right(fix_amount);
    }
    for i in 0..fix_amount {
        grid.0[0][i] = Primitive::Fix.glyph().unwrap();
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
        if (position + 1).is_multiple_of(product) {
            dividers += 1;
        }
        product *= dimension;
    }
    dividers.saturating_sub(1)
}
