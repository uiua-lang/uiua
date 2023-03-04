use std::{fmt, ptr};

use enum_iterator::Sequence;

use crate::{lex::Span, value::*, RuntimeResult};

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

const fn push_fn(a: Type, b: Type) -> BuiltinOp2Fn {
    unsafe {
        match (a, b) {
            (_, Type::List) => |a, mut b, _| {
                ptr::swap(a, &mut b);
                (*a).list_mut().0.push_back(b);
                Ok(())
            },
            _ => |_, b, span| Err(span.error(format!("Cannot push onto {}", b.ty()))),
        }
    }
}
macro_rules! op2_table {
    ($(($op:ident, $name:ident, $f:expr)),* $(,)*) => {
        $(static $name: [[BuiltinOp2Fn; TYPE_ARITY]; TYPE_ARITY] = type_square!($f);)*

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
