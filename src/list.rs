use std::{
    any::Any,
    cell::RefCell,
    fmt,
    iter::{self, empty, Empty},
    mem::replace,
    rc::Rc,
};

use im::Vector;

use crate::value::Value;

#[derive(Clone)]
pub struct List(Rc<RefCell<ListInner>>);

#[allow(clippy::len_without_is_empty)]
pub trait ListIterator: Iterator<Item = Value> + Any + 'static {
    fn clone_to(&self, target: &mut Box<dyn ListIterator>);
    fn get(&self, index: usize) -> Option<Value>;
    fn len(&self) -> Option<usize>;
}

impl ListIterator for Empty<Value> {
    fn clone_to(&self, target: &mut Box<dyn ListIterator>) {
        *target = Box::new(empty());
    }
    fn get(&self, _: usize) -> Option<Value> {
        None
    }
    fn len(&self) -> Option<usize> {
        Some(0)
    }
}

struct ListInner {
    cache: Vector<Value>,
    iter: Box<dyn ListIterator>,
}

impl Clone for ListInner {
    fn clone(&self) -> Self {
        let mut iter: Box<dyn ListIterator> = Box::new(empty());
        self.iter.clone_to(&mut iter);
        Self {
            cache: self.cache.clone(),
            iter,
        }
    }
}

impl Default for List {
    fn default() -> Self {
        List(Rc::new(RefCell::new(ListInner {
            cache: Vector::new(),
            iter: Box::new(empty()),
        })))
    }
}

impl List {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn from_items<I: IntoIterator<Item = Value>>(items: I) -> Self {
        List(Rc::new(RefCell::new(ListInner {
            cache: items.into_iter().collect(),
            iter: Box::new(empty()),
        })))
    }
    #[allow(clippy::should_implement_trait)]
    pub fn from_iter<I: ListIterator>(iter: I) -> Self {
        List(Rc::new(RefCell::new(ListInner {
            cache: Vector::new(),
            iter: Box::new(iter),
        })))
    }
    pub fn len(&self) -> usize {
        let mut inner = self.0.borrow_mut();
        if let Some(len) = inner.iter.len() {
            return len;
        }
        while let Some(value) = inner.iter.next() {
            inner.cache.push_back(value);
        }
        inner.cache.len()
    }
    pub fn is_empty(&self) -> bool {
        let inner = self.0.borrow();
        if let Some(len) = inner.iter.len() {
            if len == 0 {
                return true;
            }
        }
        inner.cache.is_empty() && matches!(inner.iter.size_hint(), (0, Some(0)))
    }
    pub fn get(&self, index: usize) -> Option<Value> {
        let mut inner = self.0.borrow_mut();
        if let Some(value) = inner.iter.get(index) {
            return Some(value);
        }
        let len = inner.cache.len();
        if index < len {
            return inner.cache.get(index).cloned();
        }
        while inner.cache.len() <= index {
            match inner.iter.next() {
                Some(value) => inner.cache.push_back(value),
                None => return None,
            }
        }
        inner.cache.get(index).cloned()
    }
    fn ensure_uniqueness(&mut self) {
        Rc::make_mut(&mut self.0);
    }
    pub fn push(&mut self, value: Value) {
        self.ensure_uniqueness();
        let mut inner = self.0.borrow_mut();
        while let Some(value) = inner.iter.next() {
            inner.cache.push_back(value);
        }
        inner.cache.push_back(value);
    }
    pub fn set(&mut self, index: usize, value: Value) -> Option<Value> {
        self.ensure_uniqueness();
        let mut inner = self.0.borrow_mut();
        let len = inner.cache.len();
        if index < len {
            return inner.cache.get_mut(index).map(|v| replace(v, value));
        }
        while inner.cache.len() <= index {
            match inner.iter.next() {
                Some(value) => inner.cache.push_back(value),
                None => return None,
            }
        }
        inner.cache.get_mut(index).map(|v| replace(v, value))
    }
    pub fn pop(&mut self) -> Option<Value> {
        self.ensure_uniqueness();
        let mut inner = self.0.borrow_mut();
        while let Some(value) = inner.iter.next() {
            inner.cache.push_back(value);
        }
        inner.cache.pop_back()
    }
    pub fn iter(&self) -> impl Iterator<Item = Value> + '_ {
        let mut i = 0;
        iter::from_fn(move || {
            let val = self.get(i)?;
            i += 1;
            Some(val)
        })
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        self.iter().eq(other.iter())
    }
}

impl Eq for List {}

impl fmt::Debug for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Extend<Value> for List {
    fn extend<T: IntoIterator<Item = Value>>(&mut self, iter: T) {
        for value in iter {
            self.push(value);
        }
    }
}
