//! Types used through multiple phases of the Lox project, mostly
//! in an auxiliary fashion to support (better) error reporting.

use core::fmt::{Display, Formatter};

/// A Location simply consists of a line and column position.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[expect(
    clippy::exhaustive_structs,
    reason = "Locations are always line+col - if we ever switch to offset-based locations, this is a breaking change."
)]
pub struct Location {
    /// Line of the location, 0-indexed.
    pub line: usize,
    /// Column of the location, 0-indexed.
    pub col: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Source Spans define locations of elements in the source code,
/// given both their start and end positions as [`Locations`](Location).
/// Both positions are to be treated as inclusive.
#[derive(Debug, PartialEq)]
#[non_exhaustive] // Filename might be added.
pub struct Span {
    /// Start of the Span
    pub start: Location,
    /// End of the Span
    pub end: Location,
}

impl Span {
    /// Construct a source span from given start and end positions
    #[must_use]
    pub fn from(start: Location, end: Location) -> Self {
        Span { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        if self.start == self.end {
            write!(f, "{}", self.start)
        } else {
            write!(f, "{}-{}", self.start, self.end)
        }
    }
}

/// A Located value pairs a value with a source span.
#[derive(Debug)]
#[expect(clippy::exhaustive_structs, reason = "this is set in stone")]
pub struct Located<T>(pub T, pub Span);

/// Raw Literals inside the Lox Language, they carry with their value
/// the raw source code that was used to declare them.
#[derive(Debug, PartialEq)]
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum RawLiteral {
    /// A string, with no escape sequences supported currently.
    String {
        /// The parsed value of the string, used for any computations
        value: String,
        /// The raw value of the string as it appeared in source code.
        /// Only used for error reporting purposes.
        raw: String,
    },
    /// A number, represented as a double-precision floating point number.
    Number {
        /// The parsed and converted value of the number, used for computation.
        value: f64,
        /// The raw value of the number as it appeared in source code.
        /// Only used for error reporting purposes.
        raw: String,
    },
}

impl RawLiteral {
    /// Extract the raw representation as it occurred
    /// in the source code.
    #[must_use]
    pub fn to_raw(&self) -> &str {
        match *self {
            RawLiteral::String { ref raw, .. } | RawLiteral::Number { ref raw, .. } => raw.as_str(),
        }
    }
}

/// Identifiers inside the Lox Language.
#[derive(Debug, PartialEq)]
#[expect(clippy::exhaustive_structs, reason = "Identifiers stay as Strings.")]
pub struct Identifier(pub String);
