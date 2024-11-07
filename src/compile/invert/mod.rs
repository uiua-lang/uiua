mod un;
mod under;

use std::{boxed, cell::RefCell, collections::HashMap, error::Error, fmt};

use ecow::eco_vec;
use serde::*;

use crate::{
    assembly::{Assembly, Function},
    check::{nodes_clean_sig, SigCheckError},
    ArrayLen, FunctionId,
    ImplPrimitive::{self, *},
    Node::{self, *},
    Primitive::{self, *},
    Purity, SigNode, Signature, SysOp, Uiua, UiuaResult,
};

use un::*;

pub(crate) const DEBUG: bool = false;

macro_rules! dbgln {
    ($($arg:tt)*) => {
        if DEBUG {
            println!($($arg)*); // Allow println
        }
    }
}
use dbgln;

trait AsNode: fmt::Debug + Sync {
    fn as_node(&self, span: usize) -> Node;
}

impl AsNode for Node {
    fn as_node(&self, _: usize) -> Node {
        self.clone()
    }
}

impl AsNode for Primitive {
    fn as_node(&self, span: usize) -> Node {
        Node::Prim(*self, span)
    }
}

impl AsNode for ImplPrimitive {
    fn as_node(&self, span: usize) -> Node {
        Node::ImplPrim(*self, span)
    }
}

impl AsNode for i32 {
    fn as_node(&self, _: usize) -> Node {
        Node::new_push(*self)
    }
}

impl AsNode for crate::Complex {
    fn as_node(&self, _: usize) -> Node {
        Node::new_push(*self)
    }
}

macro_rules! as_node {
    ($($T:ident),*) => {
        impl<$($T),*> AsNode for ($($T),*)
        where
            $($T: AsNode),*
        {
            #[allow(non_snake_case)]
            fn as_node(&self, span: usize) -> Node {
                let ($($T),*) = self;
                Node::from_iter([$($T.as_node(span)),*])
            }
        }
    };
}
as_node!(A, B);
as_node!(A, B, C);
as_node!(A, B, C, D);
as_node!(A, B, C, D, E);
as_node!(A, B, C, D, E, F);
as_node!(A, B, C, D, E, F, G, H, I);

trait SpanFromNodes: Sized + fmt::Debug + Sync {
    fn span_from_nodes<'a>(
        &self,
        nodes: &'a [Node],
        asm: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)>;
}

impl SpanFromNodes for Primitive {
    fn span_from_nodes<'a>(
        &self,
        nodes: &'a [Node],
        _: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)> {
        match nodes {
            [Node::Prim(prim, span), rest @ ..] if self == prim => Some((rest, Some(*span))),
            _ => None,
        }
    }
}

impl SpanFromNodes for ImplPrimitive {
    fn span_from_nodes<'a>(
        &self,
        nodes: &'a [Node],
        _: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)> {
        match nodes {
            [Node::ImplPrim(prim, span), rest @ ..] if self == prim => Some((rest, Some(*span))),
            _ => None,
        }
    }
}

impl SpanFromNodes for i32 {
    fn span_from_nodes<'a>(
        &self,
        nodes: &'a [Node],
        _: &Assembly,
    ) -> Option<(&'a [Node], Option<usize>)> {
        match nodes {
            [Node::Push(n), rest @ ..] if *n == *self => Some((rest, None)),
            _ => None,
        }
    }
}

macro_rules! span_from_nodes {
    ($($T:ident),*) => {
        impl<$($T),*> SpanFromNodes for ($($T),*)
        where
            $($T: SpanFromNodes),*
        {
            #[allow(non_snake_case)]
            fn span_from_nodes<'a>(&self, mut nodes: &'a [Node], asm: &Assembly) -> Option<(&'a [Node], Option<usize>)> {
                let ($($T),*) = self;
                let mut span = None;
                $(
                    let (new, sp) = $T.span_from_nodes(nodes, asm)?;
                    span = span.or(sp);
                    nodes = new;
                )*
                Some((nodes, span))
            }
        }
    };
}
span_from_nodes!(A, B);
span_from_nodes!(A, B, C, D);
span_from_nodes!(A, B, C, D, E);

/// Optionally allow a leading value
///
/// The value will not be pushed during the "undo" step
#[derive(Debug)]
struct MaybeVal<P>(P);

/// Require a leading value
///
/// The value will not be pushed during the "undo" step
#[derive(Debug)]
struct RequireVal<P>(P);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize)]
pub enum InversionError {
    #[default]
    Generic,
    TooManyNodeuctions,
    Signature(SigCheckError),
    InnerFunc(Vec<FunctionId>, boxed::Box<Self>),
    AsymmetricUnderSig(Signature),
    ComplexInvertedUnder,
    UnderExperimental,
    AlgebraError(AlgebraError),
    UnUnderExperimental,
}

pub type InversionResult<T = ()> = Result<T, InversionError>;

impl fmt::Display for InversionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InversionError::Generic => write!(f, "No inverse found"),
            InversionError::TooManyNodeuctions => {
                write!(f, "Function has too many instructions to invert")
            }
            InversionError::Signature(e) => write!(f, "Cannot invert invalid signature: {e}"),
            InversionError::InnerFunc(ids, inner) => {
                write!(f, "Inversion failed:")?;
                for id in ids {
                    write!(f, " cannot invert {id} because")?;
                }
                let inner = inner.to_string().to_lowercase();
                write!(f, " {inner}")
            }
            InversionError::AsymmetricUnderSig(sig) => {
                write!(
                    f,
                    "Cannot invert under with asymmetric \
                    second function signature {sig}"
                )
            }
            InversionError::ComplexInvertedUnder => {
                write!(f, "This under itself is too complex to invert")
            }
            InversionError::UnderExperimental => {
                write!(
                    f,
                    "Inversion of {} is experimental. To enable it, \
                    add `# Experimental!` to the top of the file.",
                    Primitive::Under.format()
                )
            }
            InversionError::AlgebraError(e) => e.fmt(f),
            InversionError::UnUnderExperimental => {
                write!(
                    f,
                    "{} {} is experimental. To enable it, \
                    add `# Experimental!` to the top of the file.",
                    Primitive::Un.format(),
                    Primitive::Under.format()
                )
            }
        }
    }
}

impl InversionError {
    fn func(self, f: &Function) -> Self {
        match self {
            InversionError::InnerFunc(mut ids, inner) => {
                ids.push(f.id.clone());
                InversionError::InnerFunc(ids, inner)
            }
            e => InversionError::InnerFunc(vec![f.id.clone()], e.into()),
        }
    }
}

impl From<SigCheckError> for InversionError {
    fn from(e: SigCheckError) -> Self {
        InversionError::Signature(e)
    }
}
impl From<()> for InversionError {
    fn from(_: ()) -> Self {
        InversionError::Generic
    }
}

impl Error for InversionError {}

use ecow::{EcoString, EcoVec};
use regex::Regex;
use InversionError::Generic;

use super::algebra::AlgebraError;
/// A generic inversion error
fn generic<T>() -> InversionResult<T> {
    Err(InversionError::Generic)
}

pub(crate) fn match_format_pattern(parts: EcoVec<EcoString>, env: &mut Uiua) -> UiuaResult {
    let val = env
        .pop(1)?
        .as_string(env, "Matching a format pattern expects a string")?;
    match parts.as_slice() {
        [] => {}
        [part] => {
            if val != part.as_ref() {
                return Err(env.error("String did not match pattern exactly"));
            }
        }
        _ => {
            thread_local! {
                static CACHE: RefCell<HashMap<EcoVec<EcoString>, Regex>> = RefCell::new(HashMap::new());
            }
            CACHE.with(|cache| {
                let mut cache = cache.borrow_mut();
                let re = cache.entry(parts.clone()).or_insert_with(|| {
                    let mut re = String::new();
                    re.push_str("(?s)^");
                    for (i, part) in parts.iter().enumerate() {
                        if i > 0 {
                            re.push_str("(.+?|.*)");
                        }
                        re.push_str(&regex::escape(part));
                    }
                    re.push('$');
                    Regex::new(&re).unwrap()
                });
                if !re.is_match(val.as_ref()) {
                    return Err(
                        if let Some(part) = parts.iter().find(|part| !val.contains(part.as_str())) {
                            env.error(format!("String does not contain {:?}", part))
                        } else {
                            env.error("String did not match format string pattern")
                        },
                    );
                }
                let captures = re.captures(val.as_ref()).unwrap();
                let caps: Vec<_> = captures.iter().skip(1).flatten().collect();
                for cap in caps.into_iter().rev() {
                    env.push(cap.as_str());
                }
                Ok(())
            })?;
        }
    }
    Ok(())
}
