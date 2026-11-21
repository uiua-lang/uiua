#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Line {
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
    pub fn set(n: usize) -> &'static [Line] {
        match n {
            0 => &[Line::Single],
            1 => &[Line::Double],
            2 => &[Line::OpenSingle, Line::CloseSingle],
            _ => &[Line::OpenDouble, Line::CloseDouble],
        }
    }
    pub fn vert(&self) -> char {
        match self {
            Line::Single | Line::OpenSingle | Line::CloseSingle | Line::ForceSingle => '│',
            Line::Double | Line::OpenDouble | Line::CloseDouble | Line::ForceDouble => '║',
        }
    }
    pub fn horiz(&self) -> char {
        match self {
            Line::Single | Line::OpenSingle | Line::CloseSingle | Line::ForceSingle => '─',
            Line::Double | Line::OpenDouble | Line::CloseDouble | Line::ForceDouble => '═',
        }
    }
    pub fn singleton(&self) -> &Self {
        match self {
            Line::Single => &Line::OpenSingle,
            Line::CloseSingle => &Line::ForceSingle,
            Line::Double => &Line::OpenDouble,
            Line::CloseDouble => &Line::ForceDouble,
            line => line,
        }
    }
    pub fn intersect(&self, column: Self) -> char {
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
