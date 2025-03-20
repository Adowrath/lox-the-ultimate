use crate::lox::types::{Identifier, Located, RawLiteral};
use super::{BinaryOp, Expr, Literal, PrefixOp};

pub trait PrettyPrint {
    /// Generate a Lisp-y prettyprint.
    /// The default implementation simply calls [`Self::pretty_print_into`]
    /// with a newly allocated String.
    fn pretty_print(&self) -> String {
        let mut accumulator = String::new();
        self.pretty_print_into(&mut accumulator);
        accumulator
    }

    fn pretty_print_into(&self, target: &mut String);
}

impl<T: PrettyPrint> PrettyPrint for Located<T> {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        self.0.pretty_print_into(target);
    }
}

impl PrettyPrint for Expr {
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            Expr::PrefixExpression { ref operator, ref expr } => {
                target.push('(');
                operator.pretty_print_into(target);
                target.push(' ');
                expr.pretty_print_into(target);
                target.push(')');
            }
            Expr::BinaryOperation { ref operator, ref lhs, ref rhs } => {
                target.push('(');
                operator.pretty_print_into(target);
                target.push(' ');
                lhs.pretty_print_into(target);
                target.push(' ');
                rhs.pretty_print_into(target);
                target.push(')');
            }
            Expr::Parenthesized { ref expr, .. } => {
                target.push_str("(group ");
                expr.pretty_print_into(target);
                target.push(')');
            },
            Expr::Identifier(ref id) => id.pretty_print_into(target),
            Expr::Literal(ref lit) => lit.pretty_print_into(target),
        }
    }
}

impl PrettyPrint for Identifier {
    fn pretty_print_into(&self, target: &mut String) {
        target.push_str(self.0.as_str());
    }
}

impl PrettyPrint for Literal {
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            Literal::Raw(
                RawLiteral::Number { ref raw, .. }
                | RawLiteral::String { ref raw, ..}
            ) => target.push_str(raw.as_str()),
            Literal::Boolean(ref value) => target.push_str(value.to_string().as_ref()),
            Literal::Nil => target.push_str("nil"),
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

impl PrettyPrint for BinaryOp {
    #[inline]
    fn pretty_print_into(&self, target: &mut String) {
        match *self {
            BinaryOp::Plus => target.push('+'),
            BinaryOp::Minus => target.push('-'),
            BinaryOp::Multiply => target.push('*'),
            BinaryOp::Divide => target.push('/'),
            BinaryOp::Equals => target.push_str("=="),
            BinaryOp::NotEquals => target.push_str("!="),
            BinaryOp::LessThan => target.push('<'),
            BinaryOp::LessThanEqual => target.push_str("<="),
            BinaryOp::GreaterThan => target.push('>'),
            BinaryOp::GreaterThanEqual => target.push_str(">="),
        }
    }
}

#[cfg(test)]
mod test {
    use super::PrettyPrint;
    use crate::lox::types::{Located, Location, RawLiteral, Span};
    use super::super::{Expr, Literal, PrefixOp, BinaryOp};

    #[test]
    fn simple_test() {
        let empty_span = Span::from(
            Location { col: 0, line: 0 },
            Location { col: 0, line: 0 },
        );
        assert_eq!(
            "(* (- 123) (group 45.67))",
            Expr::BinaryOperation {
                operator: Located(BinaryOp::Multiply, empty_span),
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
