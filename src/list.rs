use std::fmt;

use im::{vector, Vector};

use crate::value::Value;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct List(Vector<Value>);

impl List {
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
        self.0.push_back(value);
    }
    pub fn pop(&mut self) -> Option<Value> {
        self.0.pop_back()
    }
    pub fn iter(&self) -> vector::Iter<Value> {
        self.0.iter()
    }
    pub fn iter_mut(&mut self) -> vector::IterMut<Value> {
        self.0.iter_mut()
    }
}

impl IntoIterator for List {
    type Item = Value;
    type IntoIter = vector::ConsumingIter<Value>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<Value> for List {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.0.iter()).finish()
    }
}

impl Extend<Value> for List {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}
