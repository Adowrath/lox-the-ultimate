//! Types used through multiple phases of the Lox project, mostly
//! in an auxiliary fashion to support (better) error reporting.

use core::cmp::{Ordering, max, min};
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

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        self.line.cmp(&other.line).then(self.col.cmp(&other.col))
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Source Spans define locations of elements in the source code,
/// given both their start and end positions as [`Locations`](Location).
/// Both positions are to be treated as inclusive.
#[derive(Copy, Clone, Debug, PartialEq)]
#[non_exhaustive] // Filename might be added.
pub enum Span {
    Empty,
    Actual {
        /// Start of the Span
        start: Location,
        /// End of the Span
        end: Location,
    },
}

impl Span {
    /// Construct a source span from given start and end positions
    #[must_use]
    pub const fn from(start: Location, end: Location) -> Self {
        Span::Actual { start, end }
    }

    /// Construct a Located value from this Span.
    #[must_use]
    pub const fn locate<T>(self, value: T) -> Located<T> {
        Located(value, self)
    }

    /// Merges the given iterable of Spans. Returns an empty span if the iterable
    /// is empty, or all inner Spans are empty themselves.
    #[must_use]
    pub fn merge<'a, I: IntoIterator<Item = &'a Span>>(spans: I) -> Span {
        let mut full_span = Span::Empty;

        for &span in spans {
            match full_span {
                Span::Empty => full_span = span,
                Span::Actual {
                    ref mut start,
                    ref mut end,
                } => match span {
                    Span::Empty => {}
                    Span::Actual {
                        start: next_start,
                        end: next_end,
                    } => {
                        *start = min(*start, next_start);
                        *end = max(*end, next_end);
                    }
                },
            }
        }

        full_span
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Span::Empty => write!(f, "empty span"),
            Span::Actual { start, end } => {
                if start == end {
                    write!(f, "{}", start)
                } else {
                    write!(f, "{}-{}", start, end)
                }
            }
        }
    }
}

/// A Located value pairs a value with a source span.
#[derive(Debug, PartialEq, Clone)]
#[expect(clippy::exhaustive_structs, reason = "this is set in stone")]
pub struct Located<T>(pub T, pub Span);

/// This instance is for making comparisons between located
/// values and the underlying types easier.
///
/// This notably *ignores* the Location information! Thus, no
/// implementation in the other direction can be provided, as
/// this would otherwise violate the transitivity rule.
impl<T: PartialEq> PartialEq<T> for Located<T> {
    fn eq(&self, other: &T) -> bool {
        self.0 == *other
    }
}

impl<T> Located<T> {
    #[must_use]
    pub fn map<U, F>(&self, f: F) -> Located<U>
    where
        F: FnOnce(&T) -> U,
    {
        Located(f(&self.0), self.1)
    }
}

/// Raw Literals inside the Lox Language, they carry with their value
/// the raw source code that was used to declare them.
#[derive(Clone, Debug, PartialEq)]
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
#[derive(Clone, Debug, PartialEq)]
#[expect(clippy::exhaustive_structs, reason = "Identifiers stay as Strings.")]
pub struct Identifier(pub String);
