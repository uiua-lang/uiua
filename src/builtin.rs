use crate::{
    types::{FunctionType, Type},
    Runtime,
};

impl Runtime {
    pub(crate) fn initialize_builtins(&mut self) {
        use Type::*;
        // Add default types
        for (name, ty) in [
            ("bool", Bool),
            ("nat", Nat),
            ("int", Int),
            ("real", Real),
            ("unit", Unit),
        ] {
            self.transpiler.checker.types.insert(name.into(), ty);
        }
        // Create Uiua functions
        let checker = &mut self.transpiler.checker;
        // Math
        for ty in [Nat, Int] {
            checker
                .add_function("add", FunctionType::new([ty.clone(), ty.clone()], ty))
                .unwrap();
        }
    }
}
