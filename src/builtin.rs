use std::{fmt, ptr};

use enum_iterator::Sequence;

use crate::{lex::Span, value::*, RuntimeResult};

/// 1-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp1 {
    Len,
}

impl fmt::Display for BuiltinOp1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp1::Len => write!(f, "len"),
        }
    }
}

type BuiltinOp1Fn = fn(*mut Value, &Span) -> RuntimeResult;
macro_rules! op1_table {
    ($(($op:ident, $name:ident, $f:expr)),* $(,)*) => {
        $(static $name: [BuiltinOp1Fn; Type::ARITY] = unsafe { type_line!($f) };)*

        impl Value {
            pub fn op1(&mut self, op: BuiltinOp1, span: &Span) -> RuntimeResult {
                match op {
                    $(BuiltinOp1::$op => $name[self.ty() as usize](self, span),)*
                }
            }
        }
    };
}

op1_table!((Len, LEN_TABLE, len_fn),);
const unsafe fn len_fn(a: Type) -> BuiltinOp1Fn {
    match a {
        Type::List => |a, _| {
            let len = (*a).list_mut().0.len();
            *a = (len as i64).into();
            Ok(())
        },
        _ => |a, span| Err(span.error(format!("Cannot get length of {}", (*a).ty()))),
    }
}

/// 2-parameter built-in operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
pub enum BuiltinOp2 {
    Push,
}

impl fmt::Display for BuiltinOp2 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinOp2::Push => write!(f, "push"),
        }
    }
}

type BuiltinOp2Fn = fn(*mut Value, Value, &Span) -> RuntimeResult;
macro_rules! op2_table {
    ($(($op:ident, $name:ident, $f:expr)),* $(,)*) => {
        $(static $name: [[BuiltinOp2Fn; Type::ARITY]; Type::ARITY] = unsafe { type_square!($f) };)*

        impl Value {
            pub fn op2(&mut self, other: Value, op: BuiltinOp2, span: &Span) -> RuntimeResult {
                match op {
                    $(BuiltinOp2::$op => $name[self.ty() as usize][other.ty() as usize](self, other, span),)*
                }
            }
        }
    };
}

op2_table!((Push, PUSH_TABLE, push_fn),);
const unsafe fn push_fn(a: Type, b: Type) -> BuiltinOp2Fn {
    match (a, b) {
        (_, Type::List) => |a, mut b, _| {
            ptr::swap(a, &mut b);
            (*a).list_mut().0.push_back(b);
            Ok(())
        },
        _ => |_, b, span| Err(span.error(format!("Cannot push onto {}", b.ty()))),
    }
}
