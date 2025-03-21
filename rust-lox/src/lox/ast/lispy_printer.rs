use crate::lox::types::{Identifier, Located, RawLiteral};
use super::{InfixOp, Expr, Literal, PrefixOp};

/// A Lisp-y pretty print as defined by the book in Chapter 5.
pub trait PrettyPrint {
    /// Generate a Lisp-y prettyprint.
    /// The default implementation simply calls [`Self::pretty_print_into`]
    /// with a newly allocated String.
    fn pretty_print(&self) -> String {
        let mut accumulator = String::new();
        self.pretty_print_into(&mut accumulator);
        accumulator
    }

    /// Generate the pretty-print into the given String buffer.
    fn pretty_print_into(&self, target: &mut String);
}

/// Expand into Tuple Implementations for [`PrettyPrint`].
macro_rules! tuple_impl {
    ($param:ident) => {
        tuple_impl!(@impl $param);
    };
    ($first:ident $($param:ident)+) => {
        tuple_impl!($($param)+);
        tuple_impl!(@impl $first $($param)+);
    };
    (@impl $($param:ident)+) => {
        impl<$($param: PrettyPrint,)+> PrettyPrint for ($($param,)+) {
            #[inline(always)]
            #[expect(
                non_snake_case,
                clippy::min_ident_chars,
                reason = "simpler to do this than to figure out how to get the index numbers"
            )]
            fn pretty_print_into(&self, target: &mut String) {
                let ($(ref $param,)+) = *self;
                $($param.pretty_print_into(target);)*
            }
        }
    }
}

/// Simple wrapper that expands to a tuple of the values,
/// calling [`PrettyPrint::pretty_print_into`] for the target.
macro_rules! pp(($target:expr, $($val:expr),+ $(,)?) => {
    ($($val),+).pretty_print_into($target)
});

tuple_impl!(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);

impl<T: PrettyPrint + ?Sized> PrettyPrint for &T {
    #[inline(always)]
    #[expect(clippy::inline_always, reason = "always inlining these is always correct")]
    fn pretty_print_into(&self, target: &mut String) {
        pp!(target, **self);
    }
}

impl<T: PrettyPrint + ?Sized> PrettyPrint for Box<T> {
    #[inline(always)]
    #[expect(clippy::inline_always, reason = "always inlining these is always correct")]
    fn pretty_print_into(&self, target: &mut String) {
        pp!(target, **self);
    }
}

impl PrettyPrint for char {
    #[inline(always)]
    #[expect(clippy::inline_always, reason = "always inlining these is always correct")]
    fn pretty_print_into(&self, target: &mut String) {
        target.push(*self);
    }
}

impl PrettyPrint for str {
    #[inline(always)]
    #[expect(clippy::inline_always, reason = "always inlining these is always correct")]
    fn pretty_print_into(&self, target: &mut String) {
        target.push_str(self);
    }
}

impl<T: PrettyPrint> PrettyPrint for Located<T> {
    #[inline(always)]
    #[expect(clippy::inline_always, reason = "always inlining these is always correct")]
    fn pretty_print_into(&self, target: &mut String) {
        pp!(target, self.0);
    }
}

impl PrettyPrint for Expr {
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            Expr::PrefixExpression { ref operator, ref expr } => {
                pp!(target, '(', operator, ' ', expr, ')');
            }
            Expr::InfixOperation { ref operator, ref lhs, ref rhs } => {
                pp!(target, '(', operator, ' ', lhs, ' ', rhs, ')');
            }
            Expr::Parenthesized { ref expr, .. } => {
                pp!(target, "(group", expr, ')');
            }
            Expr::Identifier(ref id) => pp!(target, id),
            Expr::Literal(ref lit) => pp!(target, lit),
        }
    }
}

impl PrettyPrint for Identifier {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        pp!(target, self.0);
    }
}

impl PrettyPrint for Literal {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            Literal::Raw(
                RawLiteral::Number { ref raw, .. }
                | RawLiteral::String { ref raw, .. }
            ) => pp!(target, raw),
            Literal::Boolean(ref value) => pp!(target, value.to_string()),
            Literal::Nil => pp!(target, "nil"),
        }
    }
}

impl PrettyPrint for PrefixOp {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            PrefixOp::Not => target.push('!'),
            PrefixOp::Negate => target.push('-'),
        }
    }
}

impl PrettyPrint for InfixOp {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            InfixOp::Plus => target.push('+'),
            InfixOp::Minus => target.push('-'),
            InfixOp::Multiply => target.push('*'),
            InfixOp::Divide => target.push('/'),
            InfixOp::Equals => target.push_str("=="),
            InfixOp::NotEquals => target.push_str("!="),
            InfixOp::LessThan => target.push('<'),
            InfixOp::LessThanEqual => target.push_str("<="),
            InfixOp::GreaterThan => target.push('>'),
            InfixOp::GreaterThanEqual => target.push_str(">="),
        }
    }
}

#[cfg(test)]
mod test {
    use super::PrettyPrint;
    use crate::lox::types::{Located, Location, RawLiteral, Span};
    use super::super::{Expr, Literal, PrefixOp, InfixOp};

    #[test]
    fn simple_test() {
        let empty_span = Span::from(
            Location { col: 0, line: 0 },
            Location { col: 0, line: 0 },
        );
        assert_eq!(
            "(* (- 123) (group 45.67))",
            Expr::InfixOperation {
                operator: Located(InfixOp::Multiply, empty_span),
                lhs: Box::new(Expr::PrefixExpression {
                    operator: Located(PrefixOp::Negate, empty_span),
                    expr: Box::new(Expr::Literal(
                        Located(
                            Literal::Raw(RawLiteral::Number { raw: "123".to_owned(), value: 123f64 }),
                            empty_span
                        ))),
                }),
                rhs: Box::new(Expr::Parenthesized {
                    expr: Box::new(Expr::Literal(
                        Located(
                            Literal::Raw(RawLiteral::Number { raw: "45.67".to_owned(), value: 45.67f64 }),
                            empty_span,
                        )
                    )),
                    source_span: empty_span
                }),
            }.pretty_print()
        );
    }
}
