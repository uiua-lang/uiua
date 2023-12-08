use std::{collections::HashMap, path::PathBuf, sync::Arc};

use ecow::{eco_vec, EcoVec};

use crate::{FuncSlice, Global, Instr, Span};

/// A compiled Uiua assembly
#[derive(Clone)]
pub struct Assembly {
    pub(crate) instrs: EcoVec<Instr>,
    pub(crate) top_slices: Vec<FuncSlice>,
    pub(crate) globals: EcoVec<Global>,
    pub(crate) import_inputs: HashMap<PathBuf, Arc<str>>,
    pub(crate) spans: EcoVec<Span>,
}

impl Default for Assembly {
    fn default() -> Self {
        Self {
            instrs: EcoVec::new(),
            top_slices: Vec::new(),
            import_inputs: HashMap::new(),
            spans: eco_vec![Span::Builtin],
            globals: EcoVec::new(),
        }
    }
}
