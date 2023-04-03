mod dyadic;
pub mod loops;
mod monadic;
pub mod pervade;

use std::mem::take;

use crate::{
    array::Array,
    value::{Type, Value},
    Uiua, UiuaResult,
};

impl Value {
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        if self.is_array() {
            self.array().len()
        } else {
            1
        }
    }
    pub fn rank(&self) -> usize {
        if self.is_array() {
            self.array().rank()
        } else {
            0
        }
    }
    pub fn shape(&self) -> &[usize] {
        if self.is_array() {
            self.array().shape()
        } else {
            &[]
        }
    }
    pub fn as_indices(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<isize>> {
        self.as_number_list(env, requirement, |f| f % 1.0 == 0.0, |f| f as isize)
    }
    pub fn as_naturals(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<Vec<usize>> {
        self.as_number_list(
            env,
            requirement,
            |f| f % 1.0 == 0.0 && f >= 0.0,
            |f| f as usize,
        )
    }
    fn as_number_list<T>(
        &self,
        env: &Uiua,
        requirement: &'static str,
        test: fn(f64) -> bool,
        convert: fn(f64) -> T,
    ) -> UiuaResult<Vec<T>> {
        Ok(match self.ty() {
            Type::Num => {
                let f = self.number();
                if !test(f) {
                    return Err(env.error(requirement));
                }
                vec![convert(f)]
            }
            Type::Byte => {
                let f = self.byte() as f64;
                if !test(f) {
                    return Err(env.error(requirement));
                }
                vec![convert(f)]
            }
            Type::Array => {
                let arr = self.array();
                let mut index = Vec::with_capacity(arr.len());
                arr.try_iter_numbers(env, requirement, |f, env| {
                    if !test(f) {
                        return Err(env.error(requirement));
                    }
                    index.push(convert(f));
                    Ok(())
                })?;
                index
            }
            _ => return Err(env.error(requirement)),
        })
    }
    pub fn as_string(&self, env: &Uiua, requirement: &'static str) -> UiuaResult<String> {
        if !self.is_array() {
            return Err(env.error(format!("{}, it is {}", requirement, self.ty())));
        }
        let arr = self.array();
        if !arr.is_chars() {
            return Err(env.error(format!("{}, it is {}", requirement, arr.ty())));
        }
        Ok(arr.chars().iter().collect())
    }
    pub fn coerce_array(&mut self) -> &mut Array {
        if !self.is_array() {
            *self = match self.ty() {
                Type::Num => Array::from(self.number()),
                Type::Char => Array::from(self.char()),
                _ => Array::from(take(self)),
            }
            .into();
        }
        self.array_mut()
    }
    pub fn coerce_into_array(mut self) -> Array {
        self.coerce_array();
        self.into_array()
    }
    pub fn fill_value(&self, env: &Uiua) -> UiuaResult<Value> {
        Ok(match self.ty() {
            Type::Num => 0.0.into(),
            Type::Byte => 0.into(),
            Type::Char => ' '.into(),
            Type::Function => return Err(env.error("Functions do not have a fill value")),
            Type::Array => {
                let array = self.array();
                let values: Vec<Value> = array
                    .clone()
                    .into_values()
                    .into_iter()
                    .map(|val| val.fill_value(env))
                    .collect::<UiuaResult<_>>()?;
                Array::from((array.shape().to_vec(), values))
                    .normalized()
                    .into()
            }
        })
    }
}
