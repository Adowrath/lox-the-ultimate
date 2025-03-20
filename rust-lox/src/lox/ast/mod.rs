#![allow(unused_macros, clippy::exhaustive_enums, clippy::exhaustive_structs, reason = "WIP Code.")]

pub mod lispy_printer;

use crate::lox::types::{Identifier, RawLiteral, Span};

/// Located
#[expect(clippy::min_ident_chars, reason = "unintrusive helper macro")]
macro_rules! l(($t:ty) => {
    crate::lox::types::Located<$t>
});

pub enum Expr {
    PrefixExpression {
        operator: l!(PrefixOp),
        expr: Box<Expr>,
    },
    BinaryOperation {
        operator: l!(BinaryOp),
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Parenthesized {
        expr: Box<Expr>,
        /// This separate field is necessary as we do not keep track of the
        /// parentheses themselves. The alternative is to attach a location
        /// to every embedded Expression instead - this way, we can keep spans
        /// lazily computed as much as possible.
        source_span: Span,
    },
    Identifier(l!(Identifier)),
    Literal(l!(Literal)),
}

// Atomic pieces.

pub enum Literal {
    Raw(RawLiteral),
    Boolean(bool),
    Nil,
}

pub enum PrefixOp {
    Not,
    Negate,
}

pub enum BinaryOp {
    // Arithmetic
    Plus,
    Minus,
    Multiply,
    Divide,

    // Logical
    Equals,
    NotEquals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}
