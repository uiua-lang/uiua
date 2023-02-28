use std::rc::Rc;

use mlua::UserData;

#[derive(Debug, Clone)]
pub struct List<T>(Rc<Vec<T>>);

impl<T> Default for List<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> List<T> {
    pub fn new() -> Self {
        Self(Rc::new(Vec::new()))
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<T: Clone> List<T> {
    pub fn push(mut self, item: T) -> Self {
        Rc::make_mut(&mut self.0).push(item);
        self
    }
}

impl<T> UserData for List<T> {}
