use smallvec::SmallVec;

use crate::{Array, Boxed, Multivector, SubSide, Uiua, Value};

pub struct Fill<'a> {
    env: &'a Uiua,
    value_fill: fn(env: &'a Uiua) -> Option<&'a FillFrame>,
    other_value_fill: fn(env: &'a Uiua) -> Option<&'a FillFrame>,
    other_error: &'static str,
}

#[derive(Debug, Clone, Default)]
pub struct FillFrame {
    pub values: SmallVec<[Value; 1]>,
    pub side: Option<SubSide>,
}

impl From<Value> for FillFrame {
    fn from(value: Value) -> Self {
        (value, None).into()
    }
}

impl From<(Value, Option<SubSide>)> for FillFrame {
    fn from((value, side): (Value, Option<SubSide>)) -> Self {
        FillFrame {
            values: [value].into(),
            side,
        }
    }
}

impl From<FillValue> for FillFrame {
    fn from(fv: FillValue) -> Self {
        (fv.value, fv.side).into()
    }
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

impl<T: Clone> FillValue<&T> {
    pub fn cloned(self) -> FillValue<T> {
        FillValue {
            value: self.value.clone(),
            side: self.side,
        }
    }
}

impl<'a> Fill<'a> {
    pub fn new(env: &'a Uiua) -> Self {
        Self {
            env,
            value_fill: Uiua::fill_frame,
            other_value_fill: Uiua::unfill_frame,
            other_error: ". An unfill is set, but not a normal fill.",
        }
    }
    pub fn new_un(env: &'a Uiua) -> Self {
        Self {
            env,
            value_fill: Uiua::unfill_frame,
            other_value_fill: Uiua::fill_frame,
            other_error: ". A normal fill is set, but not an unfill.",
        }
    }
    pub fn frame(&self) -> Option<&FillFrame> {
        (self.value_fill)(self.env)
    }
    fn value_map<U>(
        &self,
        f: impl Fn(&Value) -> Result<U, &'static str>,
    ) -> Result<FillValue<U>, &'static str> {
        match self.frame() {
            Some(frame) => {
                let mut error = None;
                for val in &frame.values {
                    match f(val) {
                        Ok(val) => return Ok(FillValue::new(val, frame.side)),
                        Err(e) => {
                            error.get_or_insert(e);
                        }
                    }
                }
                Err(error.unwrap_or_else(|| self.error(false)))
            }
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
    pub(crate) fn complex_scalar(&self) -> Result<FillValue<Multivector>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) if n.rank() == 0 => Ok(Multivector::new_complex(n.data[0], 0.0)),
            Value::Num(_) => Err(self.error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(Multivector::new_complex(n.data[0] as f64, 0.0)),
            Value::Byte(_) => Err(self.error(true)),
            Value::Complex(c) if c.rank() == 0 => Ok(c.data[0].clone()),
            Value::Complex(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn complex_array(&self) -> Result<FillValue<Array<Multivector>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.convert_ref()),
            Value::Byte(n) => Ok(n.convert_ref()),
            Value::Complex(c) => Ok(c.clone()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn value_for(&self, val: &Value) -> Option<FillValue<&Value>> {
        let frame = self.frame()?;
        (frame.values.iter())
            .find(|v| val.type_id() == v.type_id())
            .map(|value| FillValue::new(value, frame.side))
    }
    fn error(&self, scalar: bool) -> &'static str {
        if scalar {
            match self.frame().and_then(|frame| frame.values.first()) {
                Some(Value::Num(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Byte(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Char(_)) => ". A character fill is set, but it is not a scalar.",
                Some(Value::Complex(_)) => ". A complex fill is set, but it is not a scalar.",
                Some(Value::Box(_)) => ". A box fill is set, but it is not a scalar.",
                None => {
                    if (self.other_value_fill)(self.env).is_some() {
                        self.other_error
                    } else {
                        ""
                    }
                }
            }
        } else {
            match self.frame().and_then(|frame| frame.values.first()) {
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
