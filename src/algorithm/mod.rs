use std::convert::Infallible;

use crate::{array::ArrayValue, Uiua, UiuaError};

mod dyadic;
pub(crate) mod invert;
pub mod loops;
mod monadic;
pub mod pervade;

fn max_shape(a: &[usize], b: &[usize]) -> Vec<usize> {
    let mut new_shape = vec![0; a.len().max(b.len())];
    for i in 0..new_shape.len() {
        let j = new_shape.len() - i - 1;
        if a.len() > i {
            new_shape[j] = a[a.len() - i - 1];
        }
        if b.len() > i {
            new_shape[j] = new_shape[j].max(b[b.len() - i - 1]);
        }
    }
    new_shape
}

pub trait FillContext: Copy {
    type Error;
    fn error(self, msg: impl ToString) -> Self::Error;
    fn fill<T: ArrayValue>(self) -> Option<T>;
}

impl FillContext for &Uiua {
    type Error = UiuaError;
    fn error(self, msg: impl ToString) -> Self::Error {
        self.error(msg)
    }
    fn fill<T: ArrayValue>(self) -> Option<T> {
        T::get_fill(self)
    }
}

impl FillContext for () {
    type Error = Infallible;
    fn error(self, msg: impl ToString) -> Self::Error {
        panic!("{}", msg.to_string())
    }
    fn fill<T: ArrayValue>(self) -> Option<T> {
        None
    }
}
