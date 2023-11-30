use crate::{ImplPrimitive, Primitive, Signature, TempStack, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function2 {
    pub unit: usize,
    pub start: usize,
    pub end: usize,
    pub sig: Signature,
}

impl Function2 {
    pub fn sig(&self) -> Signature {
        self.sig
    }
}

/// A Uiua bytecode instruction
#[derive(Clone)]
#[repr(u8)]
#[allow(missing_docs)]
pub enum Instr2 {
    /// Push a value onto the stack
    Push(Value) = 0,
    /// Begin an array
    BeginArray,
    /// End an array
    EndArray {
        boxed: bool,
        span: usize,
    },
    /// Execute a primitive
    Prim(Primitive, usize),
    /// Execute an implementation primitive
    ImplPrim(ImplPrimitive, usize),
    /// Call a function
    Call(usize),
    /// Push a function onto the function stack
    PushFunc(Function2),
    /// Execute a switch function
    Switch {
        count: usize,
        sig: Signature,
        span: usize,
    },
    /// Call a dynamic function
    Dynamic(usize, Signature),
    PushTempFunctions(usize),
    PopTempFunctions(usize),
    GetTempFunction {
        offset: usize,
        sig: Signature,
        span: usize,
    },
    PushTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    PopTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    CopyToTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    CopyFromTemp {
        stack: TempStack,
        offset: usize,
        count: usize,
        span: usize,
    },
    DropTemp {
        stack: TempStack,
        count: usize,
        span: usize,
    },
    PushSig(Signature),
    PopSig,
}
