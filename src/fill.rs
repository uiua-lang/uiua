use crate::{Array, Boxed, Complex, Uiua, Value};

pub struct Fill<'a> {
    env: &'a Uiua,
    value_fill: fn(env: &'a Uiua) -> Option<&Value>,
    other_value_fill: fn(env: &'a Uiua) -> Option<&Value>,
    other_error: &'static str,
}

impl<'a> Fill<'a> {
    pub fn new(env: &'a Uiua) -> Self {
        Self {
            env,
            value_fill: Uiua::value_fill,
            other_value_fill: Uiua::value_unfill,
            other_error: ". An unfill is set, but not a normal fill.",
        }
    }
    pub fn new_un(env: &'a Uiua) -> Self {
        Self {
            env,
            value_fill: Uiua::value_unfill,
            other_value_fill: Uiua::value_fill,
            other_error: ". A normal fill is set, but not an unfill.",
        }
    }
    pub fn value(&self) -> Option<&Value> {
        (self.value_fill)(self.env)
    }
    pub(crate) fn num_scalar(&self) -> Result<f64, &'static str> {
        match self.value() {
            Some(Value::Num(n)) if n.rank() == 0 => Ok(n.data[0]),
            Some(Value::Num(_)) => Err(self.error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(n.data[0] as f64),
            Some(Value::Byte(_)) => Err(self.error(true)),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn num_array(&self) -> Result<Array<f64>, &'static str> {
        match self.value() {
            Some(Value::Num(n)) => Ok(n.clone()),
            Some(Value::Byte(n)) => Ok(n.convert_ref()),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn byte_scalar(&self) -> Result<u8, &'static str> {
        match self.value() {
            Some(Value::Num(n))
                if n.rank() == 0
                    && n.data[0].fract() == 0.0
                    && (0.0..=255.0).contains(&n.data[0]) =>
            {
                Ok(n.data[0] as u8)
            }
            Some(Value::Num(n)) if n.rank() == 0 => Err(self.error(false)),
            Some(Value::Num(_)) => Err(self.error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(n.data[0]),
            Some(Value::Byte(_)) => Err(self.error(true)),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn byte_array(&self) -> Result<Array<u8>, &'static str> {
        match self.value() {
            Some(Value::Num(n))
                if (n.data.iter()).all(|&n| n.fract() == 0.0 && (0.0..=255.0).contains(&n)) =>
            {
                Ok(n.convert_ref_with(|n| n as u8))
            }
            Some(Value::Byte(n)) => Ok(n.clone()),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn char_scalar(&self) -> Result<char, &'static str> {
        match self.value() {
            Some(Value::Char(c)) if c.rank() == 0 => Ok(c.data[0]),
            Some(Value::Char(_)) => Err(self.error(true)),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn char_array(&self) -> Result<Array<char>, &'static str> {
        match self.value() {
            Some(Value::Char(c)) => Ok(c.clone()),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn box_scalar(&self) -> Result<Boxed, &'static str> {
        match self.value() {
            Some(Value::Box(b)) if b.rank() == 0 => Ok(b.data[0].clone()),
            Some(Value::Box(_)) => Err(self.error(true)),
            Some(val) => Ok(Boxed(val.clone())),
            None => Err(self.error(false)),
        }
    }
    pub(crate) fn box_array(&self) -> Result<Array<Boxed>, &'static str> {
        match self.value() {
            Some(Value::Box(b)) => Ok(b.clone()),
            Some(val) => Ok(Array::new([], [Boxed(val.clone())])),
            None => Err(self.error(false)),
        }
    }
    pub(crate) fn complex_scalar(&self) -> Result<Complex, &'static str> {
        match self.value() {
            Some(Value::Num(n)) if n.rank() == 0 => Ok(Complex::new(n.data[0], 0.0)),
            Some(Value::Num(_)) => Err(self.error(true)),
            Some(Value::Byte(n)) if n.rank() == 0 => Ok(Complex::new(n.data[0] as f64, 0.0)),
            Some(Value::Byte(_)) => Err(self.error(true)),
            Some(Value::Complex(c)) if c.rank() == 0 => Ok(c.data[0]),
            Some(Value::Complex(_)) => Err(self.error(true)),
            _ => Err(self.error(false)),
        }
    }
    pub(crate) fn complex_array(&self) -> Result<Array<Complex>, &'static str> {
        match self.value() {
            Some(Value::Num(n)) => Ok(n.convert_ref()),
            Some(Value::Byte(n)) => Ok(n.convert_ref()),
            Some(Value::Complex(c)) => Ok(c.clone()),
            _ => Err(self.error(false)),
        }
    }
    fn error(&self, scalar: bool) -> &'static str {
        if scalar {
            match self.value() {
                Some(Value::Num(_)) => ". A number fill is set, but is is not a scalar.",
                Some(Value::Byte(_)) => ". A number fill is set, but is is not a scalar.",
                Some(Value::Char(_)) => ". A character fill is set, but is is not a scalar.",
                Some(Value::Complex(_)) => ". A complex fill is set, but is is not a scalar.",
                Some(Value::Box(_)) => ". A box fill is set, but is is not a scalar.",
                None => {
                    if (self.other_value_fill)(self.env).is_some() {
                        self.other_error
                    } else {
                        ""
                    }
                }
            }
        } else {
            match self.value() {
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
                    if (self.other_value_fill)(self.env).is_some() {
                        self.other_error
                    } else {
                        ""
                    }
                }
            }
        }
    }
}
