mod signature;

use std::{fmt, ops::Deref};

use smallvec::SmallVec;

pub use signature::*;

pub trait Exec<S>: HasStack {
    type Output;
    fn exec(&mut self, f: S) -> Result<Self::Output, Self::Error>;
}

impl<T, F, R> Exec<F> for T
where
    T: HasStack,
    F: FnOnce(&mut T) -> Result<R, T::Error>,
{
    type Output = R;
    fn exec(&mut self, f: F) -> Result<Self::Output, Self::Error> {
        f(self)
    }
}

pub trait HasStack {
    type Item;
    type Error;
    fn stack(&self) -> &Vec<Self::Item>;
    fn stack_mut(&mut self) -> &mut Vec<Self::Item>;
    fn underflow_error<A: StackArg>(&self, arg: A) -> Self::Error;
    fn stack_len(&self) -> usize {
        self.stack().len()
    }
    fn stack_is_empty(&self) -> bool {
        self.stack().is_empty()
    }
    fn push<U: Into<Self::Item>>(&mut self, val: U) {
        self.stack_mut().push(val.into())
    }
    fn push_all(
        &mut self,
        vals: impl IntoIterator<Item = Self::Item, IntoIter: DoubleEndedIterator>,
    ) {
        self.stack_mut().extend(vals.into_iter().rev())
    }
    fn insert<U: Into<Self::Item>>(&mut self, i: usize, val: U) -> Result<(), Self::Error> {
        let i = self.require_height(i)?;
        self.stack_mut().insert(i, val.into());
        Ok(())
    }
    fn insert_all(
        &mut self,
        i: usize,
        vals: impl IntoIterator<Item = Self::Item, IntoIter: DoubleEndedIterator>,
    ) -> Result<(), Self::Error> {
        let above = self.pop_n(i)?;
        self.push_all(vals);
        self.push_all(above);
        Ok(())
    }
    fn require_height(&self, n: usize) -> Result<usize, Self::Error> {
        if self.stack().len() < n {
            return Err(self.underflow_error(n + 1));
        }
        Ok(self.stack().len() - n)
    }
    fn pop<A: StackArg>(&mut self, arg: A) -> Result<Self::Item, Self::Error> {
        (self.stack_mut().pop()).ok_or_else(|| self.underflow_error(arg))
    }
    fn pop_n(&mut self, n: usize) -> Result<SmallVec<[Self::Item; 1]>, Self::Error> {
        self.pop_n_down(n, 0)
    }
    fn top<A: StackArg>(&self, arg: A) -> Result<&Self::Item, Self::Error> {
        self.stack().last().ok_or_else(|| self.underflow_error(arg))
    }
    fn top_mut<A: StackArg>(&mut self, arg: A) -> Result<&mut Self::Item, Self::Error> {
        let error = self.underflow_error(arg);
        self.stack_mut().last_mut().ok_or(error)
    }
    fn top_n(&self, n: usize) -> Result<impl DoubleEndedIterator<Item = &Self::Item>, Self::Error> {
        Ok(self.stack()[self.require_height(n)?..].iter().rev())
    }
    fn top_n_mut(
        &mut self,
        n: usize,
    ) -> Result<impl DoubleEndedIterator<Item = &mut Self::Item>, Self::Error> {
        let height = self.require_height(n)?;
        Ok(self.stack_mut()[height..].iter_mut().rev())
    }
    /// Pop values possibly skipping some at the top
    fn pop_n_down(
        &mut self,
        n: usize,
        down: usize,
    ) -> Result<SmallVec<[Self::Item; 1]>, Self::Error> {
        Ok(match (n, down) {
            (0, 0) => SmallVec::new(),
            (1, 0) => SmallVec::from_buf([self.pop(1)?]),
            (n, down) => {
                let i = self.require_height(n + down)?;
                self.stack_mut().drain(i..i + n).rev().collect()
            }
        })
    }
    fn copy_top(&self) -> Result<Self::Item, Self::Error>
    where
        Self::Item: Clone,
    {
        self.require_height(1)?;
        Ok(self.stack().last().unwrap().clone())
    }
    fn copy_n(&self, n: usize) -> Result<SmallVec<[Self::Item; 1]>, Self::Error>
    where
        Self::Item: Clone,
    {
        Ok(match n {
            0 => SmallVec::new(),
            1 => SmallVec::from_buf([self.copy_top()?]),
            n => {
                let i = self.require_height(n)?;
                (&self.stack()[i..]).into()
            }
        })
    }
    fn copy_nth(&self, n: usize) -> Result<Self::Item, Self::Error>
    where
        Self::Item: Clone,
    {
        self.require_height(n)?;
        Ok(self.stack()[n].clone())
    }
    fn dup(&mut self) -> Result<(), Self::Error>
    where
        Self::Item: Clone,
    {
        let x = self.copy_top()?;
        self.push(x);
        Ok(())
    }
    fn flip(&mut self) -> Result<(), Self::Error> {
        let i = self.require_height(2)?;
        self.stack_mut().swap(i, i + 1);
        Ok(())
    }
    fn over(&mut self) -> Result<(), Self::Error>
    where
        Self::Item: Clone,
    {
        let a = self.pop(1)?;
        let b = self.copy_nth(1)?;
        self.push(a);
        self.push(b);
        Ok(())
    }
    fn dip<F>(&mut self, f: F) -> Result<Self::Output, Self::Error>
    where
        Self: Exec<F>,
    {
        let dipped = self.pop(1)?;
        let res = self.exec(f);
        self.push(dipped);
        res
    }
    fn dip_n<F>(&mut self, n: usize, f: F) -> Result<Self::Output, Self::Error>
    where
        Self: Exec<F>,
    {
        match n {
            0 => self.exec(f),
            1 => self.dip(f),
            n => {
                let dipped = self.pop_n(n)?;
                let res = self.exec(f);
                self.stack_mut().extend(dipped);
                res
            }
        }
    }
    fn fork<S>(
        &mut self,
        ops: impl IntoIterator<Item = S, IntoIter: DoubleEndedIterator + ExactSizeIterator> + Clone,
    ) -> Result<(), Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let ops_clone = ops.clone();
        let mut ops = ops.into_iter();
        if ops.len() == 0 {
            return Ok(());
        }
        if ops.len() == 2 {
            drop(ops_clone);
            let f = ops.next().unwrap();
            let g = ops.next().unwrap();
            let f_args = if f.args() > g.args() {
                let mut args = self.copy_n(g.args())?;
                args.extend(self.pop_n_down(f.args() - g.args(), g.args())?);
                args
            } else {
                self.copy_n(f.args())?
            };
            self.exec(g)?;
            self.push_all(f_args);
            self.exec(f)?;
        } else {
            let arg_count =
                self.pop_n(ops_clone.into_iter().map(|s| s.args()).max().unwrap_or(0))?;
            let last = ops.next().unwrap();
            for op in ops.rev() {
                self.push_all(arg_count[..op.args()].iter().cloned());
                self.exec(op)?;
            }
            self.push_all(arg_count.into_iter().take(last.args()));
            self.exec(last)?;
        }
        Ok(())
    }
    fn both_n<S>(&mut self, n: usize, f: S) -> Result<(), Self::Error>
    where
        S: HasSig + Clone,
        Self: Exec<S>,
    {
        match n {
            0 => {}
            1 => {
                self.exec(f)?;
            }
            n => {
                let mut args = self.pop_n((n - 1) * f.args())?;
                self.exec(f.clone())?;
                for _ in 0..n - 2 {
                    self.push_all(args.drain(args.len() - f.args()..));
                    self.exec(f.clone())?;
                }
                self.push_all(args);
                self.exec(f)?;
            }
        }
        Ok(())
    }
    fn both<S>(&mut self, f: S) -> Result<(), Self::Error>
    where
        S: HasSig + Clone,
        Self: Exec<S>,
    {
        self.both_n(2, f)
    }
    fn bracket<S>(
        &mut self,
        ops: impl IntoIterator<Item = S, IntoIter: DoubleEndedIterator + ExactSizeIterator> + Clone,
    ) -> Result<(), Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
    {
        let ops_clone = ops.clone();
        let mut ops = ops.into_iter();
        if ops.len() == 0 {
            return Ok(());
        }
        if ops.len() == 2 {
            drop(ops_clone);
            let f = ops.next().unwrap();
            let g = ops.next().unwrap();
            let f_args = self.pop_n(f.args())?;
            self.exec(g)?;
            self.push_all(f_args);
            self.exec(f)?;
        } else {
            let mut args = self.pop_n(
                (ops_clone.into_iter().rev().skip(1))
                    .map(|s| s.args())
                    .sum::<usize>(),
            )?;
            let mut ops = ops.rev();
            self.exec(ops.next().unwrap())?;
            for op in ops {
                self.push_all(args.drain(args.len() - op.args()..));
                self.exec(op)?;
            }
        }
        Ok(())
    }
    fn on<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let first = self.copy_top()?;
        let res = self.exec(f);
        self.push(first);
        res
    }
    fn by<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let last = self.copy_nth(f.args().saturating_sub(1))?;
        let outputs = f.outputs();
        let res = self.exec(f);
        self.insert(outputs, last)?;
        res
    }
    fn with<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let last = self.copy_nth(f.args().saturating_sub(1))?;
        let res = self.exec(f);
        self.push(last);
        res
    }
    fn off<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let first = self.copy_top()?;
        let outputs = f.outputs();
        let res = self.exec(f);
        self.insert(outputs, first)?;
        res
    }
    fn below<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let args = self.copy_n(f.args())?;
        let outputs = f.outputs();
        let res = self.exec(f);
        self.insert_all(outputs, args)?;
        res
    }
    fn above<S>(&mut self, f: S) -> Result<Self::Output, Self::Error>
    where
        S: HasSig,
        Self: Exec<S>,
        Self::Item: Clone,
    {
        let args = self.copy_n(f.args())?;
        let res = self.exec(f);
        self.push_all(args);
        res
    }
}

impl<T> HasStack for Vec<T> {
    type Item = T;
    type Error = String;
    fn stack(&self) -> &Vec<Self::Item> {
        self
    }
    fn stack_mut(&mut self) -> &mut Vec<Self::Item> {
        self
    }
    fn underflow_error<A: StackArg>(&self, arg: A) -> Self::Error {
        arg.underflow_message()
    }
}

pub trait HasSig {
    fn signature(&self) -> Signature;
    #[inline]
    fn args(&self) -> usize {
        self.signature().args()
    }
    #[inline]
    fn outputs(&self) -> usize {
        self.signature().outputs()
    }
}

impl<S: HasSig> HasSig for &S {
    #[inline]
    fn signature(&self) -> Signature {
        (*self).signature()
    }
}

impl HasSig for Signature {
    #[inline]
    fn signature(&self) -> Signature {
        *self
    }
}

/// A trait for types that can be used as argument specifiers for [`Uiua::pop`]
///
/// If the stack is empty, the error message will be "Missing {arg_name}"
pub trait StackArg {
    type Name<'a>: fmt::Display
    where
        Self: 'a;
    /// Get the name of the argument
    fn arg_name<'a>(&'a self) -> Self::Name<'a>;
    fn underflow_message(&self) -> String {
        format!("Not enough arguments to provide {}", self.arg_name())
    }
}

impl StackArg for () {
    type Name<'a> = &'static str;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        "value"
    }
}

impl StackArg for usize {
    type Name<'a> = String;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        format!("argument {self}")
    }
}
impl StackArg for u8 {
    type Name<'a> = String;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        format!("argument {self}")
    }
}
impl StackArg for i32 {
    type Name<'a> = String;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        format!("argument {self}")
    }
}
impl StackArg for &str {
    type Name<'a>
        = &'a str
    where
        Self: 'a;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        self
    }
}

impl StackArg for String {
    type Name<'a> = &'a str;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        self
    }
}

impl<A: fmt::Display + 'static, B: fmt::Display + 'static> StackArg for (A, B) {
    type Name<'a> = String;
    fn arg_name<'a>(&'a self) -> Self::Name<'a> {
        format!("{}{}", self.0, self.1)
    }
}

pub trait Indexable: IntoIterator + Deref<Target = [Self::Item]> {}

impl<T> Indexable for T where T: IntoIterator + Deref<Target = [T::Item]> {}
