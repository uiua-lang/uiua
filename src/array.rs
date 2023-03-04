use std::{fmt, sync::Arc};

use crate::value::Value;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Array(Arc<Vec<Value>>);

impl Array {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }
    pub fn push(&mut self, value: Value) {
        Arc::make_mut(&mut self.0).push(value)
    }
    pub fn pop(&mut self) -> Option<Value> {
        Arc::make_mut(&mut self.0).pop()
    }
}

impl FromIterator<Value> for Array {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(Arc::new(iter.into_iter().collect()))
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}
