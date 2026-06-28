use super::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Serialize, Deserialize)]
pub struct Type {
    pub scalar: Scalar,
    pub shape: DynShape,
}

impl Type {
    pub fn new(scalar: impl Into<Scalar>, shape: impl Into<DynShape>) -> Self {
        Type {
            scalar: scalar.into(),
            shape: shape.into(),
        }
    }
    pub fn listy() -> Self {
        DynShape::prefix([Dim::Dyn]).with_scalar(Scalar::Any)
    }
    pub fn list() -> Self {
        DynShape::from(Dim::Dyn).with_scalar(Scalar::Any)
    }
    pub fn string() -> Self {
        Scalar::Char.shaped(Dim::Dyn)
    }
    pub fn of_val(val: &Value) -> Self {
        let scalar = Scalar::of_val(val);
        let shape = DynShape::from(&val.shape);
        Type { scalar, shape }
    }
    pub fn from_spec(val: &Value) -> Option<Self> {
        let ty = if let Value::Box(arr) = val
            && arr.rank() == 1
            && let Some((first, rest)) = arr.data.split_first()
            && let Some(mut scalar) = value_as_scalar_spec(&first.0)
        {
            if let [Boxed(arr)] = rest
                && arr.type_id() == f64::TYPE_ID
                && arr.shape == [0]
            {
                if !matches!(scalar, Scalar::Box(_)) {
                    scalar = Scalar::Box(ScalarBox::All(scalar.scalar_type().into()));
                }
                scalar.scalar_type()
            } else if let Some(dims) = (rest.iter())
                .map(|Boxed(val)| value_as_dim(val))
                .collect::<Option<Vec<_>>>()
            {
                let suffix = dims.is_empty().then(Vec::new);
                scalar.shaped(DynShape { suffix, dims })
            } else {
                let shape = DynShape::from(arr.row_count());
                let fields = (arr.data.iter())
                    .map(|Boxed(val)| Type::from_spec(val))
                    .collect::<Option<Vec<_>>>()?;
                let scalar = Scalar::Box(ScalarBox::Def(val.meta.label.clone(), fields));
                Type { scalar, shape }
            }
        } else {
            if val.rank() <= 1 {
                match val {
                    Value::Byte(arr) => {
                        return Some(
                            DynShape::from_iter(arr.data.iter().map(|&d| Dim::Static(d as usize)))
                                .with_scalar(Scalar::Any),
                        );
                    }
                    Value::Num(arr) => {
                        if let Some(shape) =
                            Option::<DynShape>::from_iter(arr.data.iter().copied().map(num_as_dim))
                        {
                            return Some(shape.with_scalar(Scalar::Any));
                        }
                    }
                    _ => {}
                }
            }
            DynShape::SCALAR.with_scalar(value_as_scalar_spec(val)?)
        };
        Some(ty)
    }
    pub fn is_any(&self) -> bool {
        self.scalar.is_any() && self.shape.is_any()
    }
    pub fn is_boxed(&self) -> bool {
        matches!(self.scalar, Scalar::Box(_)) && self.shape.is_scalar()
    }
    pub fn unboxed(self) -> Self {
        if self.shape.is_scalar()
            && let Scalar::Box(sb) = self.scalar
        {
            sb.into_inner().map_or_else(Type::default, Into::into)
        } else {
            self
        }
    }
    pub fn into_row(self) -> Self {
        if self.shape.rank() == 1
            && let Scalar::Box(ScalarBox::Def(_, fields)) = self.scalar
        {
            Scalar::Box(ScalarBox::All(
                fields.into_iter().next().unwrap_or_default().into(),
            ))
        } else {
            self.scalar
        }
        .shaped(self.shape.into_row())
    }
    pub fn into_nth_row(self, n: usize) -> Self {
        if self.shape.rank() == 1
            && let Scalar::Box(ScalarBox::Def(_, mut fields)) = self.scalar
        {
            Scalar::Box(ScalarBox::All(
                if n < fields.len() {
                    fields.remove(n)
                } else {
                    fields.into_iter().next().unwrap_or_default()
                }
                .into(),
            ))
        } else {
            self.scalar
        }
        .shaped(self.shape.into_row())
    }
    pub fn into_first_row(self) -> Self {
        if self.shape.rank() == 1
            && let Scalar::Box(ScalarBox::Def(_, fields)) = self.scalar
        {
            Scalar::Box(ScalarBox::All(
                fields.into_iter().next().unwrap_or_default().into(),
            ))
        } else {
            self.scalar
        }
        .shaped(self.shape.into_row())
    }
    pub fn into_last_row(self) -> Self {
        if self.shape.rank() == 1
            && let Scalar::Box(ScalarBox::Def(_, fields)) = self.scalar
        {
            Scalar::Box(ScalarBox::All(
                fields.into_iter().next_back().unwrap_or_default().into(),
            ))
        } else {
            self.scalar
        }
        .shaped(self.shape.into_row())
    }
    pub fn is_string(&self) -> bool {
        self.shape.rank() <= 1 && Scalar::Char.superset_of(&self.scalar)
            || self.shape.rank() == 0 && self.scalar.compatible_with(&Scalar::Box(ScalarBox::Any))
    }
    pub fn boxed(self) -> Self {
        Scalar::Box(ScalarBox::All(self.into())).scalar_type()
    }
    pub fn box_list(self) -> Self {
        Scalar::Box(ScalarBox::All(self.into())).shaped(Dim::Dyn)
    }
    pub fn as_boxes(&self) -> Option<(Option<EcoString>, Vec<Self>)> {
        let Scalar::Box(sb) = &self.scalar else {
            return None;
        };
        let ty = match sb {
            ScalarBox::Def(name, fields) => {
                return Some((name.clone(), fields.clone()));
            }
            ScalarBox::Any => Type::default(),
            ScalarBox::All(ty) => (**ty).clone(),
        };
        let n = match self.shape.row_count() {
            Dim::Static(n) => n,
            Dim::Dyn => 1,
        };
        Some((None, vec![ty; n]))
    }
    // TODO: Rework this to set only
    pub fn as_mut_fields(
        &mut self,
        name_hint: Option<&str>,
        mut len_hint: usize,
    ) -> Option<(&mut Option<EcoString>, &mut [Self])> {
        if let Scalar::Any = self.scalar {
            self.scalar = Scalar::Box(ScalarBox::Any);
        }
        if self.shape.suffix.is_none()
            && let &[Dim::Static(len)] = self.shape.dims.as_slice()
        {
            len_hint = len;
        }
        let Scalar::Box(sb) = &self.scalar else {
            return None;
        };
        match sb {
            ScalarBox::Any => {
                self.scalar = Scalar::Box(ScalarBox::Def(
                    name_hint.map(Into::into),
                    vec![Type::default(); len_hint],
                ));
                let Scalar::Box(ScalarBox::Def(name, fields)) = &mut self.scalar else {
                    unreachable!()
                };
                Some((name, fields))
            }
            ScalarBox::All(ty) => {
                self.scalar = Scalar::Box(ScalarBox::Def(
                    name_hint.map(Into::into),
                    vec![(**ty).clone(); len_hint],
                ));
                let Scalar::Box(ScalarBox::Def(name, fields)) = &mut self.scalar else {
                    unreachable!()
                };
                Some((name, fields))
            }
            ScalarBox::Def(..) => {
                let Scalar::Box(ScalarBox::Def(name, fields)) = &mut self.scalar else {
                    unreachable!()
                };
                if name.is_none() {
                    *name = name_hint.map(Into::into);
                }
                Some((name, fields))
            }
        }
    }
    pub fn spec_val(self) -> Value {
        fn scalar_to_val(scalar: Scalar) -> Value {
            match scalar {
                Scalar::Any => '*'.into(),
                Scalar::Num => 'ℝ'.into(),
                Scalar::Int => 'ℤ'.into(),
                Scalar::Nat => 'ℕ'.into(),
                Scalar::Bool => '𝔹'.into(),
                Scalar::Ascii => '@'.into(),
                Scalar::Char => '𝕌'.into(),
                Scalar::Complex => 'ℂ'.into(),
                Scalar::Stream => 'ℝ'.into(),
                #[cfg(feature = "ga")]
                Scalar::Multivector => '𝕍'.into(),
                Scalar::Box(ScalarBox::All(ty)) => [Boxed((*ty).spec_val())].into(),
                Scalar::Box(ScalarBox::Any) => '□'.into(),
                Scalar::Box(ScalarBox::Def(..)) => unreachable!(),
            }
        }
        if let Scalar::Box(ScalarBox::Def(name, fields)) = self.scalar {
            let mut val: Value = fields.into_iter().map(Type::spec_val).map(Boxed).collect();
            if let Some(name) = name {
                val.meta.label = Some(name);
            }
            val
        } else if self.shape.is_scalar() {
            scalar_to_val(self.scalar)
        } else if self.shape.is_any() {
            [Boxed(scalar_to_val(self.scalar))].into()
        } else {
            let mut items = eco_vec![Boxed(scalar_to_val(self.scalar))];
            for dim in self.shape.dims {
                items.push(Boxed(match dim {
                    Dim::Static(d) => d.into(),
                    Dim::Dyn => f64::INFINITY.into(),
                }));
            }
            items.into()
        }
    }
}

impl From<Scalar> for Type {
    fn from(scalar: Scalar) -> Self {
        Type {
            scalar,
            shape: DynShape::SCALAR,
        }
    }
}

impl BitOr for Type {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        let scalar = self.scalar.union(rhs.scalar);
        let shape = self.shape | rhs.shape;
        Type { scalar, shape }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_any() {
            write!(f, "…")
        } else if self.scalar.is_any() {
            if self.shape.is_scalar() {
                write!(f, "*")
            } else {
                write!(f, "{:?}", self.shape)
            }
        } else if self.shape.is_scalar() {
            write!(f, "{}", self.scalar)
        } else if self.scalar == Scalar::Char
            && self.shape.dims == [Dim::Dyn]
            && self.shape.suffix.is_none()
        {
            write!(f, "str")
        } else if let Scalar::Box(ScalarBox::Def(name, fields)) = &self.scalar
            && self.shape.rank() > 0
        {
            let mut shape = Cow::Borrowed(&self.shape);
            if self.shape.rank() > 1 {
                if let Some(suf) = &mut shape.to_mut().suffix {
                    suf.pop();
                } else {
                    shape.to_mut().dims.pop();
                }
                write!(f, "{shape}")?;
            }

            if let Some(name) = name {
                write!(f, "{name}")?;
            } else {
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{field}")?;
                }
                write!(f, "}}")?;
            }
            if self.shape.rank() > 1 {
                write!(f, "]")?;
            }
            Ok(())
        } else {
            write!(f, "{}{}", self.shape, self.scalar)
        }
    }
}
