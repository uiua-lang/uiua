use std::{fmt, sync::Arc};

use nanbox::{NanBox, NanBoxable};

use crate::value::Value;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub(crate) start: u32,
    pub(crate) params: u16,
}

impl Default for Function {
    fn default() -> Self {
        Self::nil()
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_nil() {
            write!(f, "nil")
        } else {
            write!(f, "fn({} {})", self.start, self.params)
        }
    }
}

impl Function {
    #[inline]
    pub const fn nil() -> Self {
        Self {
            start: 0,
            params: 1,
        }
    }
    #[inline]
    pub const fn is_nil(&self) -> bool {
        self.start == 0
    }
}

impl NanBoxable for Function {
    unsafe fn from_nan_box(n: NanBox) -> Self {
        let [a, b, c, d, e, f]: [u8; 6] = NanBoxable::from_nan_box(n);
        Self {
            start: u32::from_le_bytes([a, b, c, d]),
            params: u16::from_le_bytes([e, f]),
        }
    }
    fn into_nan_box(self) -> NanBox {
        let [a, b, c, d] = self.start.to_le_bytes();
        let [e, f] = self.params.to_le_bytes();
        NanBoxable::into_nan_box([a, b, c, d, e, f])
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Partial {
    pub(crate) function: Function,
    pub(crate) args: Arc<[Value]>,
    pub(crate) span: usize,
}

impl fmt::Debug for Partial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for Partial {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({} {}/{})",
            self.function.start,
            self.args.len(),
            self.function.params
        )
    }
}
