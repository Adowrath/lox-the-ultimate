pub mod lispy_printer;

use crate::lox::types::{Identifier, RawLiteral, Span};

/// Utility helper macro to write Located types.
#[expect(clippy::min_ident_chars, reason = "unintrusive helper macro")]
macro_rules! l(($t:ty) => {
    crate::lox::types::Located<$t>
});

/// Expressions in the Lox language.
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum Expr {
    /// Application of a prefix operator.
    PrefixExpression {
        operator: l!(PrefixOp),
        expr: Box<Expr>,
    },
    /// Application of an infix operator.
    InfixOperation {
        operator: l!(InfixOp),
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    /// Parenthesized expression
    Parenthesized {
        expr: Box<Expr>,
        /// This separate field is necessary as we do not keep track of the
        /// parentheses themselves. The alternative is to attach a location
        /// to every embedded Expression instead - this way, we can keep spans
        /// lazily computed as much as possible.
        source_span: Span,
    },
    /// An identifier
    Identifier(l!(Identifier)),
    /// A literal
    Literal(l!(Literal)),
}

// Atomic pieces.

/// Literals, either a raw literal, or one covered by
/// keywords (true/false, nil).
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum Literal {
    /// Raw literal tokens
    Raw(RawLiteral),
    /// true/false booleans
    Boolean(bool),
    /// A nil value
    Nil,
}

/// Prefix operators.
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum PrefixOp {
    /// `!`
    Not,
    /// `-`
    Negate,
}

/// Infix operators.
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum InfixOp {
    // Arithmetic
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Multiply,
    /// `/`
    Divide,

    // Logical
    /// `==`
    Equals,
    /// `!=`
    NotEquals,
    /// `<`
    LessThan,
    /// `<=`
    LessThanEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEqual,
}
