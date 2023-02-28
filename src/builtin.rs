use mlua::{Lua, Value};

use crate::{
    list::List,
    types::{FunctionType, Type},
    Runtime,
};

impl Runtime {
    pub(crate) fn initialize_builtins(&mut self) {
        // Add default types
        for (name, ty) in [
            ("bool", Type::Bool),
            ("nat", Type::Nat),
            ("int", Type::Int),
            ("real", Type::Real),
            ("unit", Type::Unit),
        ] {
            self.transpiler.checker.types.insert(name.into(), ty);
        }
        // Create Uiua functions
        let lua = &self.lua;
        let uiua = lua.create_table().unwrap();
        lua.globals().set("uiua", uiua.clone()).unwrap();
        for (uiua_name, lua_name, function) in [
            (
                None,
                "new_list",
                lua.create_function(|lua: &Lua, ()| lua.create_userdata(List::<Value>::new())),
            ),
            (
                Some(("println", FunctionType::new([], Type::Unit))),
                "println",
                lua.create_function(|_, value: Value| {
                    println!("{value:?}");
                    Ok(())
                }),
            ),
        ] {
            uiua.set(lua_name, function.unwrap()).unwrap();
            if let Some((uiua_name, uiua_type)) = uiua_name {
                self.transpiler
                    .function_replacements
                    .insert(uiua_name.into(), format!("uiua.{}", lua_name));
                self.transpiler
                    .checker
                    .scope_mut()
                    .bindings
                    .insert(uiua_name.into(), Type::Function(uiua_type.into()));
            }
        }
    }
}
