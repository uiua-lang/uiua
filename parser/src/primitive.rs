use std::{
    f64::consts::{PI, TAU},
    fmt,
};

use enum_iterator::{all, Sequence};

use crate::{AsciiToken, NumericSubscript, Primitive, Signature, Subscript};

/// Categories of primitives
#[derive(Clone, Copy, PartialEq, Eq, Hash, Sequence)]
#[allow(missing_docs)]
pub enum PrimClass {
    Stack,
    Constant,
    MonadicPervasive,
    DyadicPervasive,
    MonadicArray,
    DyadicArray,
    IteratingModifier,
    AggregatingModifier,
    InversionModifier,
    Planet,
    OtherModifier,
    Comptime,
    Debug,
    Thread,
    Map,
    Encoding,
    Algorithm,
    Misc,
    Rng,
    Time,
    Environment,
    Sys(SysOpClass),
}

impl PrimClass {
    /// Get an iterator over all primitive classes
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
    /// Check if this class is pervasive
    pub fn is_pervasive(&self) -> bool {
        matches!(
            self,
            PrimClass::MonadicPervasive | PrimClass::DyadicPervasive
        )
    }
    /// Get an iterator over all primitives in this class
    pub fn primitives(self) -> impl Iterator<Item = Primitive> {
        Primitive::all().filter(move |prim| prim.class() == self)
    }
}

impl fmt::Debug for PrimClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PrimClass::*;
        match self {
            Stack => write!(f, "Stack"),
            Constant => write!(f, "Constant"),
            MonadicPervasive => write!(f, "MonadicPervasive"),
            DyadicPervasive => write!(f, "DyadicPervasive"),
            MonadicArray => write!(f, "MonadicArray"),
            DyadicArray => write!(f, "DyadicArray"),
            IteratingModifier => write!(f, "IteratingModifier"),
            AggregatingModifier => write!(f, "AggregatingModifier"),
            InversionModifier => write!(f, "InversionModifier"),
            Planet => write!(f, "Planet"),
            OtherModifier => write!(f, "OtherModifier"),
            Comptime => write!(f, "Comptime"),
            Debug => write!(f, "Debug"),
            Thread => write!(f, "Thread"),
            Map => write!(f, "Map"),
            Encoding => write!(f, "Encoding"),
            Algorithm => write!(f, "Algorithm"),
            Rng => write!(f, "RNG"),
            Time => write!(f, "Time"),
            Environment => write!(f, "Environment"),
            Misc => write!(f, "Misc"),
            Sys(op) => op.fmt(f),
        }
    }
}

/// The names of a primitive
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PrimNames {
    /// The text name
    pub text: &'static str,
    /// An ASCII token that formats to the primitive
    pub ascii: Option<AsciiToken>,
    /// The primitive's glyph
    pub glyph: Option<char>,
}

impl From<&'static str> for PrimNames {
    fn from(text: &'static str) -> Self {
        Self {
            text,
            ascii: None,
            glyph: None,
        }
    }
}
impl From<(&'static str, char)> for PrimNames {
    fn from((text, glyph): (&'static str, char)) -> Self {
        Self {
            text,
            ascii: None,
            glyph: Some(glyph),
        }
    }
}
impl From<(&'static str, AsciiToken, char)> for PrimNames {
    fn from((text, ascii, glyph): (&'static str, AsciiToken, char)) -> Self {
        Self {
            text,
            ascii: Some(ascii),
            glyph: Some(glyph),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(c) = self.glyph() {
            write!(f, "{}", c)
        } else if let Some(s) = self.ascii() {
            write!(f, "{}", s)
        } else {
            write!(f, "{}", self.name())
        }
    }
}

/// A wrapper that nicely prints a `Primitive`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FormatPrimitive(pub Primitive);

impl fmt::Debug for FormatPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl fmt::Display for FormatPrimitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.glyph().is_none() {
            self.0.fmt(f)
        } else {
            write!(f, "{} {}", self.0, self.0.name())
        }
    }
}

/// Levels of purity for an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Purity {
    /// The operation visibly affects the environment
    Mutating,
    /// The operation reads from the environment but does not visibly affect it
    Impure,
    /// The operation is completely pure
    Pure,
}

impl Primitive {
    /// Get an iterator over all primitives
    pub fn all() -> impl Iterator<Item = Self> + Clone {
        all()
    }
    /// Get an iterator over all non-deprecated primitives
    pub fn non_deprecated() -> impl Iterator<Item = Self> + Clone {
        Self::all().filter(|p| !p.is_deprecated())
    }
    /// Get the primitive's name
    ///
    /// This is the name that is used for formatting
    pub fn name(&self) -> &'static str {
        self.names().text
    }
    /// Get the ASCII token that formats to the primitive
    pub fn ascii(&self) -> Option<AsciiToken> {
        self.names().ascii
    }
    /// Get the primitive's glyph
    pub fn glyph(&self) -> Option<char> {
        self.names().glyph
    }
    /// Find a primitive by its text name
    pub fn from_name(name: &str) -> Option<Self> {
        Self::all().find(|p| p.name() == name)
    }
    /// Find a primitive by its ASCII token
    pub fn from_ascii(s: AsciiToken) -> Option<Self> {
        Self::all().find(|p| p.ascii() == Some(s))
    }
    /// Find a primitive by its glyph
    pub fn from_glyph(c: char) -> Option<Self> {
        Self::all().find(|p| p.glyph() == Some(c))
    }
    /// Get the primitive's signature, if it is always well-defined
    pub fn sig(&self) -> Option<Signature> {
        let (args, outputs) = self.args().zip(self.outputs())?;
        Some(Signature::new(args, outputs))
    }
    /// Check if this primitive is a modifier
    pub fn is_modifier(&self) -> bool {
        self.modifier_args().is_some()
    }
    /// Check if this primitive is a constant
    pub fn is_constant(&self) -> bool {
        self.constant().is_some()
    }
    /// Get the a constant's value
    pub fn constant(&self) -> Option<f64> {
        use Primitive::*;
        match self {
            Eta => Some(PI / 2.0),
            Pi => Some(PI),
            Tau => Some(TAU),
            Infinity => Some(f64::INFINITY),
            _ => None,
        }
    }
    /// Get a pretty-printable wrapper for this primitive
    pub fn format(&self) -> FormatPrimitive {
        FormatPrimitive(*self)
    }
    /// The modified signature of the primitive given a subscript
    pub fn subscript_sig(&self, sub: Option<Subscript>) -> Option<Signature> {
        use Primitive::*;
        let sub = sub?;
        let n = match sub.num? {
            NumericSubscript::N(n) => Some(n),
            _ => None,
        };
        Some(match (self, n) {
            (prim, Some(_)) if prim.class() == PrimClass::DyadicPervasive => Signature::new(1, 1),
            (Select | Pick | Take | Drop | Rerank | Rotate | Orient | Windows | Base, Some(_)) => {
                Signature::new(1, 1)
            }
            (First | Last, Some(n)) if n >= 0 => Signature::new(1, n as usize),
            (Couple | Join | Box, Some(n)) if n >= 0 => Signature::new(n as usize, 1),
            (Couple | Join, None) => Signature::new(2, 1),
            (Box, None) => Signature::new(1, 1),
            (
                Transpose | Sqrt | Ln | Round | Floor | Ceil | Rand | Utf8 | Len | Shape | Range,
                _,
            ) => return self.sig(),
            (Stack, Some(n)) if n >= 0 => Signature::new(n as usize, n as usize),
            _ => return None,
        })
    }
    /// A suggested replacement if the primitive is deprecated
    pub fn deprecation_suggestion(&self) -> Option<String> {
        use Primitive::*;
        Some(match self {
            Rerank => format!(
                "use subscripted {} or {Un}{By}({Len}{Shape}) instead",
                Deshape.format()
            ),
            Windows => format!("use {} {} instead", Stencil.format(), Identity.format()),
            Each => format!("use {} instead", Rows.format()),
            Tag => "use data variants instead".into(),
            ProgressiveIndexOf => format!("use {} instead", Occurrences.format()),
            _ => return None,
        })
    }
    /// Check if this primitive is deprecated
    pub fn is_deprecated(&self) -> bool {
        self.deprecation_suggestion().is_some()
    }
}

/// Categories of system functions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Sequence)]
#[allow(missing_docs)]
pub enum SysOpClass {
    Filesystem,
    StdIO,
    Env,
    Stream,
    Command,
    Media,
    Tcp,
    Ffi,
    Misc,
}

impl SysOpClass {
    /// All system function classes
    pub fn all() -> impl Iterator<Item = Self> {
        all()
    }
}
