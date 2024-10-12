use std::{
    collections::{hash_map::DefaultHasher, HashSet},
    fmt,
    hash::{Hash, Hasher},
};

use ecow::{EcoString, EcoVec};
use enum_iterator::Sequence;
use serde::*;

use crate::{
    algorithm::invert::CustomInverse,
    check::instrs_signature,
    function::*,
    primitive::{ImplPrimitive, Primitive},
    value::Value,
    Assembly, BindingKind, Ident,
};

macro_rules! instr {
    ($(
        $(#[$attr:meta])*
        (
            $num:literal,
            $name:ident
            $(($($tup_name:ident($tup_type:ty)),* $(,)?))?
            $({$($field_name:ident : $field_type:ty),* $(,)?})?
        )
    ),* $(,)?) => {
        /// A Uiua bytecode instruction
        #[derive(Clone, Serialize, Deserialize)]
        #[repr(u8)]
        #[allow(missing_docs)]
        #[serde(from = "InstrRep", into = "InstrRep")]
        pub enum Instr {
            $(
                $(#[$attr])*
                $name $(($($tup_type),*))? $({$($field_name : $field_type),*})? = $num,
            )*
        }

        impl PartialEq for Instr {
            #[allow(unused_variables)]
            fn eq(&self, other: &Self) -> bool {
                let mut hasher = DefaultHasher::new();
                self.hash(&mut hasher);
                let hash = hasher.finish();
                let mut other_hasher = DefaultHasher::new();
                other.hash(&mut other_hasher);
                let other_hash = other_hasher.finish();
                hash == other_hash
            }
        }

        impl Eq for Instr {}

        impl Hash for Instr {
            #[allow(unused_variables)]
            fn hash<H: Hasher>(&self, state: &mut H) {
                macro_rules! hash_field {
                    (span) => {};
                    ($nm:ident) => {Hash::hash($nm, state)};
                }
                match self {
                    $(
                        Self::$name $(($($tup_name),*))? $({$($field_name),*})? => {
                            $num.hash(state);
                            $($(hash_field!($field_name);)*)?
                            $($(hash_field!($tup_name);)*)?
                        }
                    )*
                }
            }
        }

        #[derive(Serialize, Deserialize)]
        pub(crate) enum InstrRep {
            $(
                $name(
                    $($($tup_type),*)?
                    $($($field_type),*)?
                ),
            )*
        }

        impl From<InstrRep> for Instr {
            fn from(rep: InstrRep) -> Self {
                match rep {
                    $(
                        InstrRep::$name (
                            $($($tup_name,)*)?
                            $($($field_name,)*)?
                        ) => Self::$name $(($($tup_name),*))? $({$($field_name),*})?,
                    )*
                }
            }
        }

        impl From<Instr> for InstrRep {
            fn from(instr: Instr) -> Self {
                match instr {
                    $(
                        Instr::$name $(($($tup_name),*))? $({$($field_name),*})? => InstrRep::$name (
                            $($($tup_name),*)?
                            $($($field_name),*)?
                        ),
                    )*
                }
            }
        }
    };
}

instr!(
    (0, Comment(text(Ident))),
    (1, Push(val(Value))),
    (
        2,
        CallGlobal {
            index: usize,
            call: bool,
            sig: Signature,
        }
    ),
    (
        3,
        BindGlobal {
            index: usize,
            span: usize,
        }
    ),
    (4, BeginArray),
    (
        5,
        EndArray {
            boxed: bool,
            span: usize,
        }
    ),
    (6, Prim(prim(Primitive), span(usize))),
    (7, ImplPrim(prim(ImplPrimitive), span(usize))),
    (8, Call(span(usize))),
    (9, CallRecursive(span(usize))),
    (10, Recur(span(usize))),
    (11, PushFunc(func(Function))),
    (
        12,
        Switch {
            count: usize,
            sig: Signature,
            under_cond: bool,
            span: usize,
        }
    ),
    (
        13,
        Format {
            parts: FmtParts,
            span: usize,
        }
    ),
    (
        14,
        MatchFormatPattern {
            parts: FmtParts,
            span: usize,
        }
    ),
    (
        15,
        Label {
            label: EcoString,
            remove: bool,
            span: usize,
        }
    ),
    (
        16,
        ValidateType {
            index: usize,
            type_num: u8,
            name: EcoString,
            span: usize,
        }
    ),
    (17, Dynamic(func(DynamicFunction))),
    (
        18,
        Unpack {
            count: usize,
            unbox: bool,
            span: usize,
        }
    ),
    (
        19,
        TouchStack {
            count: usize,
            span: usize,
        }
    ),
    (
        20,
        PushTemp {
            stack: TempStack,
            count: usize,
            span: usize,
        }
    ),
    (
        21,
        PopTemp {
            stack: TempStack,
            count: usize,
            span: usize,
        }
    ),
    (
        22,
        CopyToTemp {
            stack: TempStack,
            count: usize,
            span: usize,
        }
    ),
    (23, SetOutputComment { i: usize, n: usize }),
    (24, PushSig(sig(Signature))),
    (25, PopSig),
    /// Call a function with a custom inverse
    (26, CustomInverse(cust(CustomInverse), span(usize))),
);

type FmtParts = EcoVec<EcoString>;

/// A type of temporary stacks
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence, Serialize, Deserialize,
)]
pub enum TempStack {
    /// A stack used to hold values need to undo a function
    #[serde(rename = "u")]
    Under,
    /// A stack used when inlining some functions
    #[serde(rename = "i")]
    Inline,
}

impl fmt::Display for TempStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Under => write!(f, "under"),
            Self::Inline => write!(f, "inline"),
        }
    }
}

impl Instr {
    /// Create a new push instruction
    pub fn push(val: impl Into<Value>) -> Self {
        let val = val.into();
        Self::Push(val)
    }
    pub(crate) fn push_inline(count: usize, span: usize) -> Self {
        Self::PushTemp {
            stack: TempStack::Inline,
            count,
            span,
        }
    }
    pub(crate) fn pop_inline(count: usize, span: usize) -> Self {
        Self::PopTemp {
            stack: TempStack::Inline,
            count,
            span,
        }
    }
    pub(crate) fn copy_inline(span: usize) -> Self {
        Self::CopyToTemp {
            stack: TempStack::Inline,
            count: 1,
            span,
        }
    }
    pub(crate) fn is_compile_only(&self) -> bool {
        matches!(self, Self::PushSig(_) | Self::PopSig)
    }
    #[allow(dead_code)]
    pub(crate) fn span(&self) -> Option<usize> {
        self.span_impl().copied()
    }
}

macro_rules! instr_span {
    ($name:ident, $self_ty:ty, $out_ty:ty) => {
        impl Instr {
            #[allow(dead_code)]
            pub(crate) fn $name(self: $self_ty) -> Option<$out_ty> {
                Some(match self {
                    Self::BindGlobal { span, .. } => span,
                    Self::EndArray { span, .. } => span,
                    Self::Call(span) => span,
                    Self::Prim(_, span) => span,
                    Self::ImplPrim(_, span) => span,
                    Self::CallRecursive(span) => span,
                    Self::Recur(span) => span,
                    Self::Switch { span, .. } => span,
                    Self::Format { span, .. } => span,
                    Self::MatchFormatPattern { span, .. } => span,
                    Self::PushTemp { span, .. } => span,
                    Self::PopTemp { span, .. } => span,
                    Self::CopyToTemp { span, .. } => span,
                    Self::Label { span, .. } => span,
                    Self::ValidateType { span, .. } => span,
                    Self::Unpack { span, .. } => span,
                    Self::TouchStack { span, .. } => span,
                    _ => return None,
                })
            }
        }
    };
}

instr_span!(span_impl, &Self, &usize);
instr_span!(span_mut, &mut Self, &mut usize);

pub(crate) struct FmtInstrs<'a>(pub &'a [Instr], pub &'a Assembly);
impl<'a> fmt::Debug for FmtInstrs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, instr) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            match instr {
                Instr::PushFunc(func) => {
                    FmtInstrs(func.instrs(self.1), self.1).fmt(f)?;
                }
                instr => instr.fmt(f)?,
            }
        }
        write!(f, ")")?;
        Ok(())
    }
}
impl<'a> fmt::Display for FmtInstrs<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, instr) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            match instr {
                Instr::PushFunc(func) => FmtInstrs(func.instrs(self.1), self.1).fmt(f),
                instr => instr.fmt(f),
            }?
        }
        Ok(())
    }
}

/// Levels of purity for an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Purity {
    /// The operation visibly affects the environment
    Mutating,
    /// The operation reads from the environment but does not visibly affect it
    Impure,
    /// The operation is completely pure
    Pure,
}

/// Whether some instructions are pure
pub(crate) fn instrs_are_pure(instrs: &[Instr], asm: &Assembly, min_purity: Purity) -> bool {
    instrs_are_pure_impl(instrs, asm, min_purity, &mut HashSet::new())
}
fn instrs_are_pure_impl<'a>(
    instrs: &'a [Instr],
    asm: &'a Assembly,
    min_purity: Purity,
    visited: &mut HashSet<&'a FunctionId>,
) -> bool {
    'instrs: for (i, instr) in instrs.iter().enumerate() {
        match instr {
            Instr::CallGlobal { index, .. } => {
                if let Some(binding) = asm.bindings.get(*index) {
                    match &binding.kind {
                        BindingKind::Const(Some(_)) => {}
                        BindingKind::Func(f) => {
                            if visited.insert(&f.id)
                                && !instrs_are_pure_impl(f.instrs(asm), asm, min_purity, visited)
                            {
                                return false;
                            }
                        }
                        _ => return false,
                    }
                }
            }
            Instr::BindGlobal { .. } => {
                let prev = &instrs[..i];
                for j in (0..i).rev() {
                    let frag = &prev[j..i];
                    if instrs_signature(frag).is_ok_and(|sig| sig == (0, 1))
                        && instrs_are_pure_impl(frag, asm, min_purity, visited)
                    {
                        continue 'instrs;
                    }
                }
                return false;
            }
            Instr::Prim(prim, _) => {
                if prim.purity() < min_purity {
                    return false;
                }
            }
            Instr::ImplPrim(prim, _) => {
                if prim.purity() < min_purity {
                    return false;
                }
            }
            Instr::PushFunc(f) => {
                if visited.insert(&f.id)
                    && !instrs_are_pure_impl(f.instrs(asm), asm, min_purity, visited)
                {
                    return false;
                }
            }
            Instr::Dynamic(_) => return false,
            Instr::SetOutputComment { .. } => return false,
            _ => {}
        }
    }
    true
}

/// Whether some instructions can be propertly bounded by the runtime execution limit
pub(crate) fn instrs_are_limit_bounded(instrs: &[Instr], asm: &Assembly) -> bool {
    use Primitive::*;
    for instr in instrs {
        match instr {
            Instr::CallGlobal { index, .. } => {
                if let Some(binding) = asm.bindings.get(*index) {
                    match &binding.kind {
                        BindingKind::Const(Some(_)) => {}
                        BindingKind::Func(f) => {
                            if !instrs_are_limit_bounded(f.instrs(asm), asm) {
                                return false;
                            }
                        }
                        _ => return false,
                    }
                }
            }
            Instr::Prim(Send | Recv, _) => return false,
            Instr::Prim(Sys(op), _) if op.purity() <= Purity::Mutating => return false,
            Instr::PushFunc(f) => {
                if f.is_recursive() || !instrs_are_limit_bounded(f.instrs(asm), asm) {
                    return false;
                }
            }
            Instr::Dynamic(_) => return false,
            _ => {}
        }
    }
    true
}

impl fmt::Debug for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Push(val) => {
                if val.element_count() < 50 && val.shape().len() <= 1 {
                    write!(f, "push {val:?}")
                } else {
                    write!(f, "push {} array", val.shape())
                }
            }
            _ => write!(f, "{self}"),
        }
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Comment(comment) => write!(f, "# {comment}"),
            Instr::Push(val) => write!(f, "{val:?}"),
            Instr::CallGlobal { index, .. } => write!(f, "<call global {index}>"),
            Instr::BindGlobal { index, .. } => write!(f, "<bind global {index}>"),
            Instr::BeginArray => write!(f, "begin array"),
            Instr::EndArray { boxed: false, .. } => write!(f, "end array"),
            Instr::EndArray { boxed: true, .. } => write!(f, "end box array"),
            Instr::Prim(prim @ Primitive::Over, _) => write!(f, "`{prim}`"),
            Instr::Prim(prim, _) => write!(f, "{prim}"),
            Instr::ImplPrim(prim, _) => write!(f, "{prim}"),
            Instr::Call(_) => write!(f, "call"),
            Instr::CallRecursive(_) => write!(f, "call recursive"),
            Instr::Recur(_) => write!(f, "recur"),
            Instr::PushFunc(func) => write!(f, "push({func})"),
            Instr::Switch { count, .. } => write!(f, "<switch {count}>"),
            Instr::Format { parts, .. } => {
                write!(f, "$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Instr::MatchFormatPattern { parts, .. } => {
                write!(f, "°$\"")?;
                for (i, part) in parts.iter().enumerate() {
                    if i > 0 {
                        write!(f, "_")?
                    }
                    write!(f, "{part}")?
                }
                write!(f, "\"")
            }
            Instr::Label { label, .. } => write!(f, "${label}"),
            Instr::ValidateType { name, type_num, .. } => {
                write!(f, "<validate {name} as {type_num}>")
            }
            Instr::Dynamic(df) => write!(f, "{df:?}"),
            Instr::Unpack {
                count,
                unbox: false,
                ..
            } => write!(f, "<unpack {count}>"),
            Instr::Unpack {
                count, unbox: true, ..
            } => write!(f, "<unpack (unbox) {count}>"),
            Instr::TouchStack { count, .. } => write!(f, "<touch {count}>"),
            Instr::PushTemp { stack, count, .. } => write!(
                f,
                "push-{}-{count}",
                stack.to_string().chars().next().unwrap()
            ),
            Instr::PopTemp { stack, count, .. } => write!(
                f,
                "pop-{}-{count}",
                stack.to_string().chars().next().unwrap()
            ),
            Instr::CopyToTemp { stack, count, .. } => {
                write!(
                    f,
                    "copy-to-{}-{count}",
                    stack.to_string().chars().next().unwrap()
                )
            }
            Instr::SetOutputComment { i, n, .. } => write!(f, "<set output comment {i}({n})>"),
            Instr::PushSig(sig) => write!(f, "{sig}"),
            Instr::PopSig => write!(f, "-|"),
            Instr::CustomInverse(inv, _) => write!(f, "<custom inverse {inv:?}>"),
        }
    }
}
