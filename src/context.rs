use smallvec::SmallVec;

use crate::{
    Array, ArrayValue, Boxed, CodeSpan, Complex, Inputs, Span, SubSide, Uiua, UiuaError,
    UiuaErrorKind, Value,
};

#[derive(Clone, Copy)]
pub struct Context<'a> {
    src: Src<'a>,
    normal_fill: bool,
}

#[derive(Clone, Copy)]
enum Src<'a> {
    Env(&'a Uiua),
    Span(&'a CodeSpan, &'a Inputs),
    None,
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

impl<'a> Context<'a> {
    pub const NONE: Context<'static> = Context {
        src: Src::None,
        normal_fill: true,
    };
    pub fn new(env: &'a Uiua) -> Self {
        Self {
            src: Src::Env(env),
            normal_fill: true,
        }
    }
    pub fn from_span(span: &'a CodeSpan, inputs: &'a Inputs) -> Self {
        Self {
            src: Src::Span(span, inputs),
            normal_fill: true,
        }
    }
    pub fn un(mut self) -> Self {
        self.normal_fill = !self.normal_fill;
        self
    }
    pub fn value_fill(&self) -> Option<FillValue<&Value>> {
        match self.src {
            Src::Env(env) => env.value_fill(),
            _ => None,
        }
    }
    pub fn scalar_fill<T: ArrayValue>(&self) -> Result<FillValue<T>, &'static str> {
        T::get_scalar_fill(self)
    }
    pub fn array_fill<T: ArrayValue>(&self) -> Result<FillValue<Array<T>>, &'static str> {
        T::get_array_fill(self)
    }
    pub fn scalar_unfill<T: ArrayValue>(&self) -> Result<FillValue<T>, &'static str> {
        T::get_scalar_fill(&self.un())
    }
    pub fn array_unfill<T: ArrayValue>(&self) -> Result<FillValue<Array<T>>, &'static str> {
        T::get_array_fill(&self.un())
    }
    pub fn either_array_fill<T: ArrayValue>(&self) -> Result<FillValue<Array<T>>, &'static str> {
        self.array_fill::<T>().or_else(|_| self.array_unfill::<T>())
    }
    pub fn number_only_fill(&self) -> bool {
        self.array_fill::<f64>().is_ok() && self.array_fill::<u8>().is_err()
    }
    pub fn is_scalar_filled(&self, val: &Value) -> bool {
        match val {
            Value::Num(_) => self.scalar_fill::<f64>().is_ok(),
            Value::Byte(_) => self.scalar_fill::<u8>().is_ok(),
            Value::Complex(_) => self.scalar_fill::<Complex>().is_ok(),
            Value::Char(_) => self.scalar_fill::<char>().is_ok(),
            Value::Box(_) => self.scalar_fill::<Boxed>().is_ok(),
        }
    }
    pub fn error(&self, msg: impl ToString) -> UiuaError {
        match self.src {
            Src::Env(env) => env.error(msg),
            Src::Span(span, inputs) => UiuaErrorKind::Run {
                message: Span::Code(span.clone()).sp(msg.to_string()),
                info: Vec::new(),
                inputs: inputs.clone().into(),
            }
            .into(),
            Src::None => UiuaErrorKind::Unreachable.into(),
        }
    }
    fn frame(&self) -> Option<&FillFrame> {
        match self.src {
            Src::Env(env) => {
                if self.normal_fill {
                    env.fill_frame()
                } else {
                    env.unfill_frame()
                }
            }
            _ => None,
        }
    }
    fn other_value_fill(&self) -> Option<&FillFrame> {
        match self.src {
            Src::Env(env) => {
                if self.normal_fill {
                    env.unfill_frame()
                } else {
                    env.fill_frame()
                }
            }
            _ => None,
        }
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
                Err(error.unwrap_or_else(|| self.fill_error(false)))
            }
            None => Err(self.fill_error(false)),
        }
    }
    pub(crate) fn num_scalar(&self) -> Result<FillValue<f64>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) if n.rank() == 0 => Ok(n.data[0]),
            Value::Num(_) => Err(self.fill_error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(n.data[0] as f64),
            Value::Byte(_) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        })
    }
    pub(crate) fn num_array(&self) -> Result<FillValue<Array<f64>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.clone()),
            Value::Byte(n) => Ok(n.convert_ref()),
            _ => Err(self.fill_error(false)),
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
            Value::Num(n) if n.rank() == 0 => Err(self.fill_error(false)),
            Value::Num(_) => Err(self.fill_error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(n.data[0]),
            Value::Byte(_) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
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
            _ => Err(self.fill_error(false)),
        })
    }
    pub(crate) fn char_scalar(&self) -> Result<FillValue<char>, &'static str> {
        self.value_map(|val| match val {
            Value::Char(c) if c.rank() == 0 => Ok(c.data[0]),
            Value::Char(_) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        })
    }
    pub(crate) fn char_array(&self) -> Result<FillValue<Array<char>>, &'static str> {
        self.value_map(|val| match val {
            Value::Char(c) => Ok(c.clone()),
            _ => Err(self.fill_error(false)),
        })
    }
    pub(crate) fn box_scalar(&self) -> Result<FillValue<Boxed>, &'static str> {
        self.value_map(|val| match val {
            Value::Box(b) if b.rank() == 0 => Ok(b.data[0].clone()),
            Value::Box(_) => Err(self.fill_error(true)),
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
            Value::Num(_) => Err(self.fill_error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(Complex::new(n.data[0] as f64, 0.0)),
            Value::Byte(_) => Err(self.fill_error(true)),
            Value::Complex(c) if c.rank() == 0 => Ok(c.data[0]),
            Value::Complex(_) => Err(self.fill_error(true)),
            _ => Err(self.fill_error(false)),
        })
    }
    pub(crate) fn complex_array(&self) -> Result<FillValue<Array<Complex>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.convert_ref()),
            Value::Byte(n) => Ok(n.convert_ref()),
            Value::Complex(c) => Ok(c.clone()),
            _ => Err(self.fill_error(false)),
        })
    }
    #[cfg(feature = "ga")]
    pub(crate) fn multivector_scalar(&self) -> Result<FillValue<crate::Multivector>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) if n.rank() == 0 => Ok(n.data[0].into()),
            Value::Num(_) => Err(self.error(true)),
            Value::Byte(n) if n.rank() == 0 => Ok(n.data[0].into()),
            Value::Byte(_) => Err(self.error(true)),
            Value::Complex(c) if c.rank() == 0 => Ok(c.data[0].into()),
            Value::Complex(_) => Err(self.error(true)),
            Value::Mv(m) if m.rank() == 0 => Ok(m.data[0].clone()),
            Value::Mv(_) => Err(self.error(true)),
            _ => Err(self.error(false)),
        })
    }
    #[cfg(feature = "ga")]
    pub(crate) fn multivector_array(
        &self,
    ) -> Result<FillValue<Array<crate::Multivector>>, &'static str> {
        self.value_map(|val| match val {
            Value::Num(n) => Ok(n.convert_ref()),
            Value::Byte(n) => Ok(n.convert_ref()),
            Value::Complex(c) => Ok(c.convert_ref()),
            Value::Mv(m) => Ok(m.clone()),
            _ => Err(self.error(false)),
        })
    }
    pub(crate) fn value_for(&self, val: &Value) -> Option<FillValue<&Value>> {
        let frame = self.frame()?;
        (frame.values.iter())
            .find(|v| val.type_id() == v.type_id())
            .map(|value| FillValue::new(value, frame.side))
    }
    fn fill_error(&self, scalar: bool) -> &'static str {
        if scalar {
            match self.frame().and_then(|frame| frame.values.first()) {
                Some(Value::Num(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Byte(_)) => ". A number fill is set, but it is not a scalar.",
                Some(Value::Char(_)) => ". A character fill is set, but it is not a scalar.",
                Some(Value::Complex(_)) => ". A complex fill is set, but it is not a scalar.",
                Some(Value::Box(_)) => ". A box fill is set, but it is not a scalar.",
                #[cfg(feature = "ga")]
                Some(Value::Mv(_)) => ". A multivector fill is set, but it is not a scalar.",
                None => {
                    if self.other_value_fill().is_some() {
                        if self.normal_fill {
                            ". An unfill is set, but not a normal fill."
                        } else {
                            ". A normal fill is set, but not an unfill."
                        }
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
                #[cfg(feature = "ga")]
                Some(Value::Mv(_)) => {
                    ". A multivector fill is set, but the array is not multivectors."
                }
                None => {
                    if self.other_value_fill().is_some() {
                        if self.normal_fill {
                            ". An unfill is set, but not a normal fill."
                        } else {
                            ". A normal fill is set, but not an unfill."
                        }
                    } else {
                        ""
                    }
                }
            }
        }
    }
}
