//! The Abstract Syntax Tree representing the Lox language.
//! Currently, the state of this is according to Chapter 5
//! of the Crafting Interpreters book.
//! It will be expanded with more of the syntax as the book progresses.
pub mod lispy_printer;

use crate::lox::types::{Identifier, RawLiteral, Span};

/// Utility helper macro to write Located types.
#[expect(clippy::min_ident_chars, reason = "unintrusive helper macro")]
macro_rules! l(($t:ty) => {
    crate::lox::types::Located<$t>
});

#[derive(Debug, PartialEq)]
pub struct Program {
    declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    VariableDeclaration(Span, l!(Identifier), Option<l!(Expr)>),
    // FunctionDeclaration,
    Statement(Statement),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    ExpressionStatement(Expr),
    PrintStatement {
        span: Span,
        printed_expr: Expr
    },
}

/// Expressions in the Lox language.
/// Their Span will be computed via [`Span::merge`] from all its
/// sub-spans, except for the following, which pre-compute it:
///
/// - Parenthesized expressions (both parentheses)
/// - Function calls (trailing parenthesis)
#[derive(Debug, PartialEq)]
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
        /// This separate field is necessary as we do not keep track of the
        /// parentheses themselves. The alternative is to attach a location
        /// to every embedded Expression instead - this way, we can keep spans
        /// lazily computed as much as possible.
        source_span: Span,
        expr: Box<Expr>,
    },
    /// An identifier
    Identifier(l!(Identifier)),
    // TODO: Resolved.
    /// A literal
    Literal(l!(Literal)),
    /// A Function call: `expr(expr, expr, ...)`
    CallExpression {
        /// Like for the parenthesized expression, this is necessary as we do not
        /// separately keep track of the closing parenthesis.
        source_span: Span,
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    /// A dotted path expression: `expr.name`
    PathExpression {
        receiver: Box<Expr>,
        field_name: l!(Literal),
    },
}

// Atomic pieces.

/// Literals, either a raw literal, or one covered by
/// keywords (true/false, nil).
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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
#[derive(Debug, PartialEq)]
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

#[cfg(test)]
pub(crate) mod test {
    use std::sync::LazyLock;
    use crate::lox::types::{Located, Location, RawLiteral, Span};
    use super::{Expr, Literal, PrefixOp, InfixOp};

    pub const EMPTY_SPAN: Span = Span::from(
        Location { col: 0, line: 0 },
        Location { col: 0, line: 0 },
    );

    pub trait Unlocate {
        fn unlocate(&mut self);
    }

    impl<T> Unlocate for Located<T> {
        fn unlocate(&mut self) {
            self.1 = EMPTY_SPAN;
        }
    }

    impl Unlocate for Expr {
        fn unlocate(&mut self) {
            match self {
                Expr::PrefixExpression { operator, expr } => {
                    operator.unlocate();
                    expr.unlocate();
                },
                Expr::InfixOperation { operator, lhs, rhs } => {
                    operator.unlocate();
                    lhs.unlocate();
                    rhs.unlocate();
                },
                Expr::Parenthesized { source_span, expr } => {
                    *source_span = EMPTY_SPAN;
                    expr.unlocate();
                },
                Expr::Identifier(id) => {
                    id.unlocate();
                },
                Expr::Literal(lit) => {
                    lit.unlocate();
                },
                Expr::CallExpression { source_span, callee, arguments } => {
                    *source_span = EMPTY_SPAN;
                    callee.unlocate();
                    arguments.iter_mut().for_each(Expr::unlocate);
                }
                Expr::PathExpression { receiver, field_name } => {
                    receiver.unlocate();
                    field_name.unlocate();
                }
            }
        }
    }

    pub static EXPR_EXAMPLE: LazyLock<Expr> = LazyLock::new(|| {
        Expr::InfixOperation {
            operator: Located(InfixOp::Multiply, EMPTY_SPAN),
            lhs: Box::new(Expr::PrefixExpression {
                operator: Located(PrefixOp::Negate, EMPTY_SPAN),
                expr: Box::new(Expr::Literal(
                    Located(
                        Literal::Raw(RawLiteral::Number { raw: "123".to_owned(), value: 123f64 }),
                        EMPTY_SPAN
                    ))),
            }),
            rhs: Box::new(Expr::Parenthesized {
                expr: Box::new(Expr::Literal(
                    Located(
                        Literal::Raw(RawLiteral::Number { raw: "45.67".to_owned(), value: 45.67f64 }),
                        EMPTY_SPAN,
                    )
                )),
                source_span: EMPTY_SPAN
            }),
        }
    });
}
