pub mod ast;
mod complex;
mod defs;
mod error;
mod inputs;
mod lex;
pub mod parse;
mod primitive;
mod signature;
mod split;
mod subscript;

use bytemuck::must_cast;
pub use {
    complex::*, defs::*, error::*, inputs::*, lex::*, parse::parse, primitive::*, signature::*,
    split::*, subscript::*,
};

/// A Uiua identifier
pub type Ident = ecow::EcoString;

// NOTE: must_cast to f64 only works if f64 and u64 have same endianness.
//       This is true of all currently supported platforms for rust,
//       but may not be true in general. Swap out for f64::from_bits when
//       the MSRV passes 1.83 to ensure correctness on all future platforms.
/// A NaN value that always compares as equal
pub const WILDCARD_NAN: f64 = must_cast(0x7ff8_0000_0000_0000u64 | 0x0000_0000_0000_0003);
/// A character value used as a wildcard that will equal any character
pub const WILDCARD_CHAR: char = '\u{100000}';
