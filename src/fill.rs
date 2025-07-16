use crate::{Array, Boxed, Complex, SubSide, Uiua, Value};

pub struct Fill<'a> {
    env: &'a Uiua,
    value: fn(env: &'a Uiua) -> Option<&'a FillValue>,
    other_value: fn(env: &'a Uiua) -> Option<&'a FillValue>,
    other_error: &'static str,
}

#[derive(Debug, Clone, Copy)]
pub struct FillValue<T = Value> {
    pub value: T,
    pub side: Option<SubSide>,
}

impl<T> FillValue<T> {
    pub fn new(val: impl Into<T>, side: impl Into<Option<SubSide>>) -> Self {
        Self {
            value: val.into(),
            side: side.into(),
        }
    }
    pub fn try_map<U, E>(&self, f: impl FnOnce(&T) -> Result<U, E>) -> Result<FillValue<U>, E> {
        Ok(FillValue {
            value: f(&self.value)?,
            side: self.side,
        })
    }
    pub fn map_ref<'a, U>(&'a self, f: impl FnOnce(&'a T) -> U) -> FillValue<U> {
        FillValue {
            value: f(&self.value),
            side: self.side,
        }
    }
    pub fn is_left(&self) -> bool {
        self.side == Some(SubSide::Left)
    }
    pub fn is_right(&self) -> bool {
        self.side == Some(SubSide::Right)
    }
}

impl<'a> Fill<'a> {
    pub fn new(env: &'a Uiua) -> Self {
        Self {
            env,
            value: Uiua::value_fill,
            other_value: Uiua::value_unfill,
            other_error: ". An unfill is set, but not a normal fill.",
        }
    }
    pub fn new_un(env: &'a Uiua) -> Self {
        Self {
            env,
            value: Uiua::value_unfill,
            other_value: Uiua::value_fill,
            other_error: ". A normal fill is set, but not an unfill.",
        }
    }
    pub fn value(&self) -> Option<&FillValue> {
        (self.value)(self.env)
    }
    fn value_map<U>(
        &self,
        f: impl FnOnce(&Value) -> Result<U, &'static str>,
    ) -> Result<FillValue<U>, &'static str> {
        match self.value() {
            Some(val) => val.try_map(f),
            None => Err(self.error(false)),
        }
    }
    pub(crate) fn num_scalar(&self) -> Result<FillValue<f64>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) if n.rank() == 0 => Ok(n.data[0]),
            Value::Num(_) => Err(self.error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(n.data[0] as f64),
            Value::Byte(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn num_array(&self) -> Result<FillValue<Array<f64>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.clone()),
            Value::Byte(n) => Ok(n.convert_ref()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn byte_scalar(&self) -> Result<FillValue<u8>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n)
                if n.rank() == 0
                    && n.data[0].fract() == 0.0
                    && (0.0..=255.0).contains(&n.data[0]) =>
            {
                Ok(n.data[0] as u8)
            }
            Value::Num(n) if n.rank() == 0 => Err(self.error(false)),
            Value::Num(_) => Err(self.error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(n.data[0]),
            Value::Byte(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn byte_array(&self) -> Result<FillValue<Array<u8>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n)
                if (n.data.iter()).all(|&n| n.fract() == 0.0 && (0.0..=255.0).contains(&n)) =>
            {
                Ok(n.convert_ref_with(|n| n as u8))
            }
            Value::Byte(n) => Ok(n.clone()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn char_scalar(&self) -> Result<FillValue<char>, &'static str> {
        self.value_map(|val| match val {
            Value::Char(c) if c.rank() == 0 => Ok(c.data[0]),
            Value::Char(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn char_array(&self) -> Result<FillValue<Array<char>>, &'static str> {
        self.value_map(|val| match val {
            Value::Char(c) => Ok(c.clone()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn box_scalar(&self) -> Result<FillValue<Boxed>, &'static str> {
        self.value_map(|val| match val {
            Value::Box(b) if b.rank() == 0 => Ok(b.data[0].clone()),
            Value::Box(_) => Err(self.error(true)),
            val => Ok(Boxed(val.clone())),
        })
    }
    pub(crate) fn box_array(&self) -> Result<FillValue<Array<Boxed>>, &'static str> {
        self.value_map(|val| {
            Ok(match val {
                Value::Box(b) => b.clone(),
                val => Array::new([], [Boxed(val.clone())]),
            })
        })
    }
    pub(crate) fn complex_scalar(&self) -> Result<FillValue<Complex>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) if n.rank() == 0 => Ok(Complex::new(n.data[0], 0.0)),
            Value::Num(_) => Err(self.error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(Complex::new(n.data[0] as f64, 0.0)),
            Value::Byte(_) => Err(self.error(true)),
            Value::Complex(c) if c.rank() == 0 => Ok(c.data[0]),
            Value::Complex(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn complex_array(&self) -> Result<FillValue<Array<Complex>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.convert_ref()),
            Value::Byte(n) => Ok(n.convert_ref()),
            Value::Complex(c) => Ok(c.clone()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn value_for(&self, val: &Value) -> Option<&FillValue> {
        let fill = &self.value()?;
        match (val, &fill.value) {
            (Value::Num(_) | Value::Byte(_), Value::Num(_) | Value::Byte(_))
            | (Value::Char(_), Value::Char(_))
            | (Value::Complex(_), Value::Complex(_))
            | (Value::Box(_), Value::Box(_)) => Some(fill),
            _ => None,
        }
    }
    fn error(&self, scalar: bool) -> &'static str {
        if scalar {
            match self.value().map(|fv| &fv.value) {
                Some(Value::Num(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Byte(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Char(_)) => ". A character fill is set, but it is not a scalar.",
                Some(Value::Complex(_)) => ". A complex fill is set, but it is not a scalar.",
                Some(Value::Box(_)) => ". A box fill is set, but it is not a scalar.",
                None => {
                    if (self.other_value)(self.env).is_some() {
                        self.other_error
                    } else {
                        ""
                    }
                }
            }
        } else {
            match self.value().map(|fv| &fv.value) {
                Some(Value::Num(_)) => ". A number fill is set, but the array is not numbers.",
                Some(Value::Byte(_)) => ". A number fill is set, but the array is not numbers.",
                Some(Value::Char(_)) => {
                    ". A character fill is set, but the array is not characters."
                }
                Some(Value::Complex(_)) => {
                    ". A complex fill is set, but the array is not complex numbers."
                }
                Some(Value::Box(_)) => ". A box fill is set, but the array is not boxed values.",
                None => {
                    if (self.other_value)(self.env).is_some() {
                        self.other_error
                    } else {
                        ""
                    }
                }
            }
        }
    }
}
