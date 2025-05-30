//! The Abstract Syntax Tree representing the Lox language.
//! Currently, the state of this is according to Chapter 5
//! of the Crafting Interpreters book.
//! It will be expanded with more of the syntax as the book progresses.
#![expect(
    clippy::exhaustive_structs,
    clippy::exhaustive_enums,
    reason = "Extensions to the AST need to be handled."
)]

pub mod lispy_printer;

use crate::lox::types::{Identifier, RawLiteral, Span};

/// Utility helper macro to write Located types.
#[expect(clippy::min_ident_chars, reason = "unintrusive helper macro")]
macro_rules! l(($t:ty) => {
    crate::lox::types::Located<$t>
});

#[derive(Debug, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub enum Declaration {
    VariableDeclaration {
        assignee: Reference,
        value: Option<Expr>,
    },
    ClassDeclaration {
        name: Reference,
        funcs: Vec<FunctionDeclaration>,
    },
    FunctionDeclaration(FunctionDeclaration),
    Statement(Statement),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub span: Span,
    pub name: Reference,
    pub parameters: Vec<Reference>,
    pub body: Vec<Declaration>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    ExpressionStatement(Expr),
    IfStatement {
        // TODO Correct Spans?
        // span: Span,
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    ForLoop {
        // span: Span,
        initializer: Option<ForInitializer>,
        condition: Option<Expr>,
        step: Option<Expr>,
        body: Box<Statement>,
    },
    WhileLoop {
        // span: Span,
        condition: Expr,
        body: Box<Statement>,
    },
    BlockStatement {
        // span: Span,
        body: Vec<Declaration>,
    },
    ReturnStatement {
        return_value: Option<Expr>,
    },
    PrintStatement {
        print_span: Span,
        printed_expr: Expr,
    },
}

#[derive(Debug, PartialEq)]
pub enum ForInitializer {
    VariableDeclaration {
        assignee: Reference,
        value: Option<Expr>,
    },
    Expression(Expr),
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
    // TODO Closure
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
    /// A reference to another value
    Reference(Reference),
    /// A literal
    Literal(l!(Literal)),
    /// A Function call: `expr(expr, expr, ...)`
    CallExpression {
        /// Like for the parenthesized expression, this is necessary as we do not
        /// separately keep track of the closing parenthesis.
        // source_span: Span,
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    /// A dotted path expression: `expr.name`
    PathExpression {
        receiver: Box<Expr>,
        field_name: l!(Identifier),
    },
}

// Atomic pieces.

#[derive(Debug, PartialEq)]
#[expect(
    clippy::exhaustive_enums,
    reason = "adding a new variant MUST be handled and is a breaking change."
)]
pub enum Reference {
    Identifier(l!(Identifier)),
    Resolved {
        name: l!(Identifier),
        distance: usize,
        index: usize,
    },
    Global(l!(Identifier)),
}

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

    // Equality
    /// `==`
    Equals,
    /// `!=`
    NotEquals,

    // Comparison
    /// `<`
    LessThan,
    /// `<=`
    LessThanEqual,
    /// `>`
    GreaterThan,
    /// `>=`
    GreaterThanEqual,

    // Logical
    /// `or`
    Or,
    /// `and`
    And,

    // Assignment
    /// `=`
    Assign,
}

impl InfixOp {
    /// Higher Precedence binds tighter
    #[must_use]
    pub fn precedence(&self) -> Precedence {
        match *self {
            InfixOp::Assign => Precedence::Right(1),
            InfixOp::Or => Precedence::Left(2),
            InfixOp::And => Precedence::Left(3),

            InfixOp::Equals
            | InfixOp::NotEquals
            | InfixOp::LessThan
            | InfixOp::LessThanEqual
            | InfixOp::GreaterThan
            | InfixOp::GreaterThanEqual => Precedence::Left(4),

            InfixOp::Plus | InfixOp::Minus => Precedence::Left(5),
            InfixOp::Multiply | InfixOp::Divide => Precedence::Left(6),
        }
    }
}

pub enum Precedence {
    Left(usize),
    Right(usize),
}

#[cfg(test)]
pub(crate) mod test {
    use super::{Expr, InfixOp, Literal, PrefixOp, Reference};
    use crate::lox::types::{Located, RawLiteral, Span};
    use std::sync::LazyLock;

    pub trait Unlocate {
        fn unlocate(&mut self);
    }

    impl Unlocate for Span {
        fn unlocate(&mut self) {
            *self = Span::Empty;
        }
    }

    impl<T> Unlocate for Located<T> {
        fn unlocate(&mut self) {
            self.1.unlocate();
        }
    }

    impl Unlocate for Expr {
        fn unlocate(&mut self) {
            match self {
                Expr::PrefixExpression { operator, expr } => {
                    operator.unlocate();
                    expr.unlocate();
                }
                Expr::InfixOperation { operator, lhs, rhs } => {
                    operator.unlocate();
                    lhs.unlocate();
                    rhs.unlocate();
                }
                Expr::Parenthesized { source_span, expr } => {
                    source_span.unlocate();
                    expr.unlocate();
                }
                Expr::Reference(reference) => {
                    reference.unlocate();
                }
                Expr::Literal(lit) => {
                    lit.unlocate();
                }
                Expr::CallExpression {
                    /*source_span, */ callee,
                    arguments,
                } => {
                    // source_span.unlocate();
                    callee.unlocate();
                    arguments.iter_mut().for_each(Expr::unlocate);
                }
                Expr::PathExpression {
                    receiver,
                    field_name,
                } => {
                    receiver.unlocate();
                    field_name.unlocate();
                }
            }
        }
    }

    impl Unlocate for Reference {
        fn unlocate(&mut self) {
            match self {
                Reference::Identifier(id) => id.unlocate(),
                Reference::Resolved { name, .. } => name.unlocate(),
                Reference::Global(name) => name.unlocate(),
            }
        }
    }

    pub static EXPR_EXAMPLE: LazyLock<Expr> = LazyLock::new(|| Expr::InfixOperation {
        operator: Located(InfixOp::Multiply, Span::Empty),
        lhs: Box::new(Expr::PrefixExpression {
            operator: Located(PrefixOp::Negate, Span::Empty),
            expr: Box::new(Expr::Literal(Located(
                Literal::Raw(RawLiteral::Number {
                    raw: "123".to_owned(),
                    value: 123f64,
                }),
                Span::Empty,
            ))),
        }),
        rhs: Box::new(Expr::Parenthesized {
            expr: Box::new(Expr::Literal(Located(
                Literal::Raw(RawLiteral::Number {
                    raw: "45.67".to_owned(),
                    value: 45.67f64,
                }),
                Span::Empty,
            ))),
            source_span: Span::Empty,
        }),
    });
}
