use enum_iterator::{all, Sequence};

use crate::{
    check::{Expr, Function, FunctionDef, Param},
    lex::Span,
    types::{FunctionType, Type},
    Runtime,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl MathOp {
    pub fn name(&self) -> &'static str {
        match self {
            MathOp::Add => "add",
            MathOp::Sub => "sub",
            MathOp::Mul => "mul",
            MathOp::Div => "div",
        }
    }
    pub fn lua_op(&self) -> &'static str {
        match self {
            MathOp::Add => "+",
            MathOp::Sub => "-",
            MathOp::Mul => "*",
            MathOp::Div => "/",
        }
    }
    pub fn lua_name(&self) -> String {
        format!("uiua_{}", self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl CmpOp {
    pub fn name(&self) -> &'static str {
        match self {
            CmpOp::Eq => "eq",
            CmpOp::Ne => "ne",
            CmpOp::Lt => "lt",
            CmpOp::Gt => "gt",
            CmpOp::Le => "le",
            CmpOp::Ge => "ge",
        }
    }
    pub fn lua_op(&self) -> &'static str {
        match self {
            CmpOp::Eq => "==",
            CmpOp::Ne => "~=",
            CmpOp::Lt => "<",
            CmpOp::Gt => ">",
            CmpOp::Le => "<=",
            CmpOp::Ge => ">=",
        }
    }
    pub fn lua_name(&self) -> String {
        format!("uiua_{}", self.name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltinFn {
    Math(MathOp, NumType),
    Cmp(CmpOp, Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Sequence)]
pub enum NumType {
    Nat,
    Int,
    Real,
}

impl From<NumType> for Type {
    fn from(num: NumType) -> Self {
        match num {
            NumType::Nat => Type::Nat,
            NumType::Int => Type::Int,
            NumType::Real => Type::Real,
        }
    }
}

impl BuiltinFn {
    pub fn ty(&self) -> FunctionType {
        match self {
            BuiltinFn::Math(op, ty) => {
                let name = Some(op.name().into());
                FunctionType::new(name, [*ty, *ty], *ty)
            }
            BuiltinFn::Cmp(op, ty) => {
                let name = Some(op.name().into());
                FunctionType::new(name, [ty.clone(), ty.clone()], Type::Bool)
            }
        }
    }
    pub fn lua_name(&self) -> String {
        match self {
            BuiltinFn::Math(op, _) => op.lua_name(),
            BuiltinFn::Cmp(op, _) => op.lua_name(),
        }
    }
}

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
            self.checker.types.insert(name.into(), ty);
        }
        // Add Uiua functions
        // Cmp
        for op in all::<CmpOp>() {
            // Add the concrete function to Lua
            self.checker.exec_lua(&format!(
                "function {}(a, b) return a {} b end",
                op.lua_name(),
                op.lua_op()
            ));
            // Add the polymorphic function to the checker
            self.checker
                .add_function(
                    op.name(),
                    Function {
                        params: vec![
                            Param::new("a".into(), Type::Type),
                            Param::new("b".into(), Type::Type),
                        ],
                        body: Expr::BuiltinFn(BuiltinFn::Cmp(op, Type::Type))
                            .typed(Type::Bool)
                            .into(),
                    },
                    Span::default(),
                )
                .unwrap();
        }
        // Math
        for op in all::<MathOp>() {
            // Add the concrete function to Lua
            self.checker.exec_lua(&format!(
                "function {}(a, b) return a {} b end",
                op.lua_name(),
                op.lua_op()
            ));
            // Add functions for each type to the checker
            for num_ty in all::<NumType>() {
                let builtin_fn = BuiltinFn::Math(op, num_ty);
                self.checker
                    .add_function(
                        op.name(),
                        Function {
                            params: vec![
                                Param::new("a".into(), num_ty.into()),
                                Param::new("b".into(), num_ty.into()),
                            ],
                            body: Expr::BuiltinFn(builtin_fn).typed(num_ty.into()).into(),
                        },
                        Span::default(),
                    )
                    .unwrap();
            }
            // Add the polymorphic function to Lua
            let function = self.checker.functions.get(op.name()).unwrap();
            let def = FunctionDef {
                name: op.name().into(),
                func: function.clone(),
            };
            self.checker.transpiler.function_def(def);
            let lua_code = self.checker.transpiler.take();
            self.checker.exec_lua(&lua_code);
        }
    }
}
