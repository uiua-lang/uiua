#![allow(dead_code)]

use super::ElemAlign;

// TODO: explore smallvecs here.
// It is extremely common for the outer vec (of both grids and metagrids) to have length 1.
// It is also very common for the innermost vec to be pretty short.
#[repr(transparent)]
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Grid<T = char>(pub Vec<Vec<T>>);
pub type Metagrid<T = char> = Grid<Grid<T>>;

impl<T> Grid<T> {
    pub fn new() -> Self {
        Self(vec![])
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    /// ¤¤
    pub fn from_scalar(val: T) -> Self {
        Self(vec![vec![val]])
    }
    /// ˜↯
    pub fn from_scalar_shape(val: T, (rows, cols): (usize, usize)) -> Self
    where
        T: Clone,
    {
        Self(vec![vec![val; cols]; rows])
    }
    /// ¤
    pub fn from_row(row: Vec<T>) -> Self {
        Self(vec![row])
    }
    pub fn len_0(&self) -> usize {
        self.0.len()
    }
    pub fn len_1(&self) -> usize {
        self.0.iter().map(|row| row.len()).max().unwrap_or(0)
    }
    pub fn cell(&self, i: (usize, usize)) -> Option<&T> {
        self.0.get(i.0)?.get(i.1)
    }
    pub fn cell_mut(&mut self, i: (usize, usize)) -> Option<&mut T> {
        self.0.get_mut(i.0)?.get_mut(i.1)
    }
    pub fn row(&self, i0: usize) -> Option<&[T]> {
        self.0.get(i0).map(|r| &**r)
    }
    pub fn rows(&self) -> impl Iterator<Item = &[T]> + Clone {
        self.0.iter().map(|r| &**r)
    }
    pub fn rows_mut(&mut self) -> impl Iterator<Item = &mut Vec<T>> {
        self.0.iter_mut()
    }
    pub fn cells(&self) -> impl Iterator<Item = &T> + Clone {
        self.0.iter().flatten()
    }
    /// ▽≡◇⊃(>⊙⧻|⬚@.⊡)
    pub fn column(&self, i1: usize) -> impl Iterator<Item = &T> + Clone {
        self.rows().filter_map(move |row| row.get(i1))
    }
    /// ▽≡◇⊃(>⊙⧻|⬚@.⊡)
    pub fn column_mut(&mut self, i1: usize) -> impl Iterator<Item = &mut T> {
        self.rows_mut().filter_map(move |row| row.get_mut(i1))
    }
    /// ⧋°⊟ ⇡⊸△₂
    pub fn enumerate_cells(&self) -> impl Iterator<Item = (usize, usize, &T)> + Clone {
        self.0
            .iter()
            .enumerate()
            .flat_map(|(i0, r)| std::iter::repeat(i0).zip(r).enumerate())
            .map(|(i0, (i1, r))| (i0, i1, r))
    }
    /// ⊂
    pub fn append_0(&mut self, rhs: Self) {
        self.0.extend(rhs.0);
    }
    /// ≡⊂
    pub fn append_1(&mut self, rhs: Self) {
        for (self_row, rhs_row) in self.0.iter_mut().zip(rhs.0) {
            self_row.extend(rhs_row);
        }
    }
    pub fn push(&mut self, new_row: Vec<T>) {
        self.0.push(new_row);
    }
    pub fn is_rectangular(&self) -> bool {
        all_same(self.rows().map(|row| row.len()))
    }
}

impl Grid<char> {
    pub fn from_utf32(row: Vec<char>) -> Self {
        Self::from_row(row)
    }
    pub fn from_utf8(s: &str) -> Self {
        Self::from_utf32(s.chars().collect())
    }

    fn align<F: Fn(&[char]) -> usize>(&mut self, width: usize, f: F) {
        for row in self.rows_mut() {
            assert!(row.len() <= width);
            let rotation = f(row);
            row.resize(width, ' ');
            row.rotate_right(rotation);
        }
    }
    pub fn align_right(&mut self, width: usize) {
        self.align(width, |_| 0);
    }
    pub fn align_left(&mut self, width: usize) {
        self.align(width, |row| width - row.len());
    }
    pub fn align_center(&mut self, width: usize) {
        self.align(width, |row| (width - row.len()) / 2);
    }
    /// Align the grid such that instances of the delimiter line up.
    /// If a row has no delimiter, then its text is RIGHT-aligned on the PRE-delimiter side.
    /// This is used for decimal values.
    /// For example: `⍉¤[1.2345 12.345 123.45 1234.5 12345 1 12]`:
    /// ```
    /// ╭─            
    /// ╷     1.2345  
    ///      12.345   
    ///     123.45    
    ///    1234.5     
    ///   12345       
    ///       1       
    ///      12       
    ///              ╯
    /// ```
    pub fn align_delimited_or_right(
        &mut self,
        delim: &[char],
        width_before: usize,
        width_after: usize,
    ) {
        let total_width = width_before + delim.len() + width_after;
        self.align(total_width, |row| match split_once(row, delim) {
            Some((left, _)) => width_before - left.len(),
            None => width_before - row.len(),
        });
    }
    /// Align the grid such that instances of the delimiter line up.
    /// If a row has no delimiter, then its text is LEFT-aligned on the POST-delimiter side.
    /// This is used for arrays of boxes, specifically when values have inline labels.
    /// For example: `⍉¤{□$ab 1 □$cdefg 2_3_4 5678}`:
    /// ```
    /// ┌─                   
    /// ╷    ∙□ab: 1      │  
    ///   ────────────────┤  
    ///   ∙□cdefg: [2 3 4]│  
    ///   ────────────────┤  
    ///            ∙5678  │  
    ///                     ┘
    /// ```
    pub fn align_delimited_or_left(
        &mut self,
        delim: &[char],
        width_before: usize,
        width_after: usize,
    ) {
        let total_width = width_before + delim.len() + width_after;
        self.align(total_width, |row| match split_once(row, delim) {
            Some((left, _)) => width_before - left.len(),
            None => width_before + delim.len(),
        });
    }
}

fn all_same<T: PartialEq>(mut iter: impl Iterator<Item = T>) -> bool {
    let Some(first) = iter.next() else {
        return true;
    };
    return iter.all(|val| val == first);
}

impl Metagrid<char> {
    pub fn is_meta_rectangular(&self) -> bool {
        if !self.is_rectangular() {
            return false; // the meta-rows have jagged width
        }
        for meta_row in self.rows() {
            if !all_same(meta_row.iter().map(Grid::len_0)) {
                return false; // the sub-grids in this meta-row have jagged height
            }
        }
        for i1 in 0..self.len_1() {
            let meta_col = self.column(i1);
            if !all_same(meta_col.flat_map(Grid::rows).map(|sub_row| sub_row.len())) {
                return false; // the sub-rows in this meta-column have jagged width
            }
        }
        true
    }

    pub fn align_row(&mut self, i0: usize) {
        let meta_row = &mut self.0[i0];
        let height = meta_row.iter().map(Grid::len_0).max().unwrap_or(0);
        for sub_grid in meta_row {
            assert!(sub_grid.is_rectangular());
            sub_grid.0.resize(height, vec![' '; sub_grid.len_1()]);
        }
    }

    pub fn align_column(&mut self, i1: usize, align: ElemAlign) {
        let rows = self.column(i1).flat_map(|grid| grid.rows());
        match align {
            ElemAlign::Left | ElemAlign::Right | ElemAlign::None => {
                let width = rows.map(|row| row.len()).max().unwrap_or(0);
                let grids = self.column_mut(i1);
                match align {
                    ElemAlign::Left => grids.for_each(|grid| grid.align_left(width)),
                    ElemAlign::Right => grids.for_each(|grid| grid.align_right(width)),
                    ElemAlign::None => grids.for_each(|grid| grid.align_center(width)),
                    _ => unreachable!(),
                }
            }
            ElemAlign::DelimOrRight(delim) | ElemAlign::DelimOrLeft(delim) => {
                let is_right = matches!(align, ElemAlign::DelimOrRight(_));

                let mut width_before = 0;
                let mut width_after = 0;

                // TODO: change the enum to use a char slice
                let delim = delim.chars().collect::<Vec<_>>();
                let delim = &delim;

                for row in rows {
                    if let Some((left, right)) = split_once(row, delim) {
                        width_before = width_before.max(left.len());
                        width_after = width_after.max(right.len());
                    } else if is_right {
                        width_before = width_before.max(row.len());
                    } else {
                        width_after = width_after.max(row.len());
                    }
                }

                debug_assert_eq!(
                    width_before + delim.len() + width_after,
                    self.column(i1)
                        .flat_map(|grid| grid.rows())
                        .map(|row| row.len())
                        .max()
                        .unwrap_or(0)
                );

                for grid in self.column_mut(i1) {
                    if is_right {
                        grid.align_delimited_or_right(delim, width_before, width_after);
                    } else {
                        grid.align_delimited_or_left(delim, width_before, width_after);
                    }
                }
            }
        }
    }
}

fn split_once<'a, T: PartialEq>(data: &'a [T], delim: &[T]) -> Option<(&'a [T], &'a [T])> {
    for i in 0..data.len() {
        let (left, tail) = data.split_at(i);
        if let Some(right) = tail.strip_prefix(delim) {
            return Some((left, right));
        }
    }
    None
}

#[cfg(test)]
mod test {
    use super::*;

    fn grid_of(v: &[&str]) -> Grid {
        Grid(v.iter().map(|s| s.chars().collect()).collect())
    }

    #[test]
    fn align_floats() {
        let mut g = grid_of(&["1.2345", "12.345", "123.45", "1234.5", "12345", "1", "12"]);
        g.align_delimited_or_right(&['.'], 5, 4);
        let expected = grid_of(&[
            "    1.2345",
            "   12.345 ",
            "  123.45  ",
            " 1234.5   ",
            "12345     ",
            "    1     ",
            "   12     ",
        ]);
        if g != expected {
            for (a, b) in g.rows().zip(expected.rows()) {
                assert_eq!(a, b);
            }
        }
        assert_eq!(g, expected);
    }

    #[test]
    fn align_labeled_boxes() {
        let mut g = grid_of(&["∙□ab: 1", "∙□cdefg: [2 3 4 5 6]", "∙7890", "∙□hijklmn: @x"]);
        g.align_delimited_or_left(&[':', ' '], 9, 11);
        let expected = grid_of(&[
            "     ∙□ab: 1          ",
            "  ∙□cdefg: [2 3 4 5 6]",
            "           ∙7890      ",
            "∙□hijklmn: @x         ",
        ]);
        if g != expected {
            for (a, b) in g.rows().zip(expected.rows()) {
                assert_eq!(a, b);
            }
        }
        assert_eq!(g, expected);
    }
}
