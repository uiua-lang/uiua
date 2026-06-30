use super::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct DynShape {
    pub dims: Vec<Dim>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub suffix: Option<Vec<Dim>>,
}

impl From<&Shape> for DynShape {
    fn from(shape: &Shape) -> Self {
        DynShape {
            dims: shape.iter().map(|&d| Dim::Static(d)).collect(),
            suffix: None,
        }
    }
}

impl From<Dim> for DynShape {
    fn from(dim: Dim) -> Self {
        DynShape {
            dims: vec![dim],
            suffix: None,
        }
    }
}

impl<const N: usize> From<[Dim; N]> for DynShape {
    fn from(dims: [Dim; N]) -> Self {
        DynShape {
            dims: dims.into(),
            suffix: None,
        }
    }
}

impl From<Vec<Dim>> for DynShape {
    fn from(dims: Vec<Dim>) -> Self {
        DynShape { dims, suffix: None }
    }
}

impl<const N: usize> From<[usize; N]> for DynShape {
    fn from(dims: [usize; N]) -> Self {
        dims.map(Dim::Static).into()
    }
}

impl From<usize> for DynShape {
    fn from(n: usize) -> Self {
        Dim::Static(n).into()
    }
}

impl DynShape {
    pub const SCALAR: Self = DynShape {
        dims: Vec::new(),
        suffix: None,
    };
    pub const ANY: Self = DynShape {
        dims: Vec::new(),
        suffix: Some(Vec::new()),
    };
    pub fn prefix(prefix: impl Into<Vec<Dim>>) -> Self {
        DynShape {
            dims: prefix.into(),
            suffix: Some(Vec::new()),
        }
    }
    pub fn with_scalar(self, scalar: Scalar) -> Type {
        Type {
            scalar,
            shape: self,
        }
    }
    pub fn row_count(&self) -> Dim {
        self.dims.first().copied().unwrap_or_else(|| {
            if self.suffix.is_some() {
                Dim::Dyn
            } else {
                Dim::Static(1)
            }
        })
    }
    pub fn rank(&self) -> usize {
        self.dims.len() + self.suffix.as_ref().map_or(0, |s| s.len())
    }
    pub fn is_any(&self) -> bool {
        self.dims.is_empty() && self.suffix.as_ref().is_some_and(|s| s.is_empty())
    }
    pub fn is_scalar(&self) -> bool {
        self.dims.is_empty() && self.suffix.is_none()
    }
    pub fn into_row(self) -> DynShape {
        let mut row = self;
        if row.dims.is_empty() {
            if let Some(suff) = &mut row.suffix
                && !suff.is_empty()
            {
                suff.remove(0);
            }
        } else {
            row.dims.remove(0);
        }
        row
    }
    pub fn compatible_with(&self, other: &Self) -> bool {
        self.dims.len() == other.dims.len()
            && (self.dims.iter())
                .zip(&other.dims)
                .all(|(a, b)| Dim::row_compatible(*a, *b))
    }
    pub fn merge_from(&mut self, other: Self) {
        fn merge(a: &mut Vec<Dim>, mut b: Vec<Dim>) {
            match (a.is_empty(), b.is_empty()) {
                (false, false) => {
                    if b.len() > a.len() {
                        swap(a, &mut b);
                    }
                    for (i, a) in a.iter_mut().enumerate() {
                        if let Some(b) = b.get(i).copied() {
                            *a = (*a).max(b);
                        }
                    }
                }
                (true, _) => *a = b,
                (_, true) => {}
            }
        }
        merge(&mut self.dims, other.dims);
        match (&mut self.suffix, other.suffix) {
            (None, None) => {}
            (Some(suf), None) if suf.is_empty() => self.suffix = None,
            (Some(_), None) => {}
            (None, Some(b)) => self.suffix = Some(b),
            (Some(a), Some(b)) => merge(a, b),
        }
    }
}

impl Default for DynShape {
    fn default() -> Self {
        DynShape::ANY
    }
}

impl FromIterator<Dim> for DynShape {
    fn from_iter<T: IntoIterator<Item = Dim>>(iter: T) -> Self {
        iter.into_iter().collect::<Vec<_>>().into()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dim {
    Static(usize),
    Dyn,
}

impl From<usize> for Dim {
    fn from(n: usize) -> Self {
        Dim::Static(n)
    }
}

impl From<f64> for Dim {
    fn from(n: f64) -> Self {
        if n == f64::INFINITY {
            Dim::Dyn
        } else {
            Dim::Static(n as usize)
        }
    }
}

impl Mul for Dim {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Dim::Static(a), Dim::Static(b)) => Dim::Static(a * b),
            _ => Dim::Dyn,
        }
    }
}

impl Div for Dim {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Dim::Static(_), Dim::Static(0)) => Dim::Dyn,
            (Dim::Static(a), Dim::Static(b)) => Dim::Static(a / b),
            _ => Dim::Dyn,
        }
    }
}

impl Dim {
    pub const MIN: Self = Dim::Static(1);
    pub fn compatible(self, other: Self) -> bool {
        match (self, other) {
            (Dim::Dyn, _) | (_, Dim::Dyn) => true,
            (Dim::Static(a), Dim::Static(b)) => a == b,
        }
    }
    pub fn row_compatible(self, other: Self) -> bool {
        match (self, other) {
            (Dim::Dyn | Dim::Static(1), _) | (_, Dim::Dyn | Dim::Static(1)) => true,
            (Dim::Static(a), Dim::Static(b)) => a == b,
        }
    }
    pub fn cmp_defined(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Dim::Static(_), Dim::Dyn) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(_)) => Ordering::Less,
            (Dim::Dyn, Dim::Dyn) => Ordering::Equal,
            (Dim::Static(a), Dim::Static(b)) => a.cmp(b),
        }
    }
}

impl Ord for Dim {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Dim::Static(1), Dim::Dyn) => Ordering::Less,
            (Dim::Static(_), Dim::Dyn) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(1)) => Ordering::Greater,
            (Dim::Dyn, Dim::Static(_)) => Ordering::Less,
            (Dim::Dyn, Dim::Dyn) => Ordering::Equal,
            (Dim::Static(a), Dim::Static(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Dim {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq<usize> for Dim {
    fn eq(&self, other: &usize) -> bool {
        *self == Dim::Static(*other)
    }
}

impl fmt::Display for Dim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Dim::Static(n) => write!(f, "{n}"),
            Dim::Dyn => write!(f, "_"),
        }
    }
}

impl fmt::Debug for DynShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{self}]")
    }
}

impl fmt::Display for DynShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_any() {
            return write!(f, "…");
        }
        for (i, dim) in self.dims.iter().enumerate() {
            if i > 0 {
                write!(f, "×")?;
            }
            write!(f, "{dim}")?;
        }
        if let Some(suffix) = &self.suffix {
            if self.dims.is_empty() {
                write!(f, "…")?;
            } else {
                write!(f, "×…")?;
            }
            for dim in suffix {
                write!(f, "×")?;
                write!(f, "{dim}")?;
            }
        }
        Ok(())
    }
}

impl BitOr for DynShape {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use Dim::*;
        let dim_count = self.dims.len().min(rhs.dims.len());
        let mut dims = Vec::with_capacity(dim_count);
        for i in 0..dim_count {
            let a = self.dims.get(i).unwrap_or(&Dyn);
            let b = rhs.dims.get(i).unwrap_or(&Dyn);
            dims.push(match (a, b) {
                (Static(a), Static(b)) if a == b => Static(*a),
                _ => Dyn,
            });
        }
        let suffix = if let Some((a, b)) = self.suffix.zip(rhs.suffix) {
            let dim_count = a.len().max(b.len());
            let mut dims = Vec::with_capacity(dim_count);
            for i in 0..dim_count {
                let a = (a.len().checked_sub(i))
                    .and_then(|i| a.get(i))
                    .unwrap_or(&Dyn);
                let b = (b.len().checked_sub(i))
                    .and_then(|i| b.get(i))
                    .unwrap_or(&Dyn);
                dims.push(match (a, b) {
                    (Static(a), Static(b)) if a == b => Static(*a),
                    _ => Dyn,
                });
            }
            Some(dims)
        } else if self.dims.len() < dim_count || rhs.dims.len() < dim_count {
            Some(Vec::new())
        } else {
            None
        };
        DynShape { dims, suffix }
    }
}

impl BitOr for Dim {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        use Dim::*;
        match (self, rhs) {
            (Dyn, _) | (_, Dyn) => Dyn,
            (Static(a), Static(b)) if a == b => Static(a),
            (Static(_), Static(_)) => Dyn,
        }
    }
}

impl BitOr for TypeVal {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        if self == rhs {
            return self;
        }
        TypeVal::Type(self.ty() | rhs.ty())
    }
}
