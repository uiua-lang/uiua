use crate::function::Function;

use super::array::Array;

pub enum Value {
    Number(Array<f64>),
    Byte(Array<u8>),
    Char(Array<char>),
    Function(Array<Function>),
}
