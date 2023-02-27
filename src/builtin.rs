use std::collections::HashMap;

use crate::ir::Type;

pub fn builtin_types() -> HashMap<String, Type> {
    [
        ("nat", Type::Nat),
        ("int", Type::Int),
        ("real", Type::Real),
        ("bool", Type::Bool),
    ]
    .map(|(name, ty)| (name.to_string(), ty))
    .into()
}
