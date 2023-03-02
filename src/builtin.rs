use crate::{types::Type, Runtime};

impl Runtime {
    pub(crate) fn initialize_builtins(&mut self) {
        // Add default types
        for (name, ty) in [
            ("bool", Type::Bool),
            ("nat", Type::Nat),
            ("int", Type::Int),
            ("real", Type::Real),
            ("unit", Type::Unit),
            ("type", Type::Type),
        ] {
            self.transpiler.checker.types.insert(name.into(), ty);
        }
    }
}
