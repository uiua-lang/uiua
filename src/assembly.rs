use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use dashmap::DashMap;
use ecow::{eco_vec, EcoString, EcoVec};
use serde::*;

use crate::{
    DynamicFunction, FuncSlice, Function, Global, Ident, ImplPrimitive, InputSrc, Instr,
    IntoInputSrc, Primitive, Signature, Span, TempStack, Value,
};

/// A compiled Uiua assembly
#[derive(Clone)]
pub struct Assembly {
    pub(crate) instrs: EcoVec<Instr>,
    pub(crate) top_slices: Vec<FuncSlice>,
    pub(crate) globals: EcoVec<Global>,
    pub(crate) import_inputs: HashMap<PathBuf, EcoString>,
    pub(crate) spans: EcoVec<Span>,
    pub(crate) inputs: Inputs,
}

impl Default for Assembly {
    fn default() -> Self {
        Self {
            instrs: EcoVec::new(),
            top_slices: Vec::new(),
            import_inputs: HashMap::new(),
            spans: eco_vec![Span::Builtin],
            globals: EcoVec::new(),
            inputs: Inputs::default(),
        }
    }
}

/// A repository of code strings input to the compiler
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Inputs {
    /// A map of file paths to their string contents
    pub files: DashMap<PathBuf, EcoString>,
    /// A list of input strings without paths
    pub strings: EcoVec<EcoString>,
}

impl Inputs {
    pub(crate) fn add_src(
        &mut self,
        src: impl IntoInputSrc,
        input: impl Into<EcoString>,
    ) -> InputSrc {
        let mut src = src.into_input_src(self.strings.len());
        match &mut src {
            InputSrc::File(path) => {
                self.files
                    .insert(PathBuf::from(path.as_str()), input.into());
            }
            InputSrc::Str(i) => {
                *i = self.strings.len();
                self.strings.push(input.into())
            }
        }
        src
    }
    /// Get an input string
    pub fn get(&self, src: &InputSrc) -> EcoString {
        match src {
            InputSrc::File(path) => self
                .files
                .get(Path::new(path.as_str()))
                .unwrap_or_else(|| panic!("File {:?} not found", path))
                .clone(),
            InputSrc::Str(index) => self
                .strings
                .get(*index)
                .unwrap_or_else(|| panic!("String {} not found", index))
                .clone(),
        }
    }
}

#[derive(Serialize, Deserialize)]
enum InstrRep {
    /// A comment
    Comment(Ident),
    /// Push a value onto the stack
    Push(Value),
    /// Push a global value onto the stack
    CallGlobal {
        index: usize,
        call: bool,
    },
    /// Bind a global value
    BindGlobal {
        name: Ident,
        span: usize,
        index: usize,
    },
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
    PushFunc(Function),
    /// Execute a switch function
    Switch {
        count: usize,
        sig: Signature,
        span: usize,
    },
    Format(EcoVec<EcoString>, usize),
    /// Call a dynamic function
    Dynamic(DynamicFunction),
    Unpack {
        count: usize,
        span: usize,
        unbox: bool,
    },
    PushTempFunctions(usize),
    PopTempFunctions(usize),
    GetTempFunction {
        offset: usize,
        sig: Signature,
        span: usize,
    },
    TouchStack {
        count: usize,
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
}
