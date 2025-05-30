mod lox_types;

use crate::lox::ast::{Declaration, Expr, InfixOp, Literal, PrefixOp, Program, Statement};
use crate::lox::eval::lox_types::LoxValue;
use crate::lox::types;
use crate::lox::types::RawLiteral;

pub struct Evaluator {}

impl Evaluator {
    pub fn default() -> Self {
        Evaluator {}
    }

    pub fn evaluate(&mut self, program: Program) -> LoxValue {
        program.evaluate(self)
    }
}

// TODO Split Evaluate and EvaluateValue; most evaluations don't return anything.
trait Evaluate {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue;
}

impl<T: Evaluate> Evaluate for types::Located<T> {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        self.0.evaluate(evaluator)
    }
}

impl Evaluate for Program {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        let mut result = LoxValue::Nil;
        for decl in &self.declarations {
            result = decl.evaluate(evaluator);
        }
        result
    }
}

impl Evaluate for Declaration {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        match self {
            Declaration::VariableDeclaration { .. } => todo!("VariableDeclaration"),
            Declaration::ClassDeclaration { .. } => todo!("ClassDeclaration"),
            Declaration::FunctionDeclaration(_) => todo!("FunctionDeclaration"),
            Declaration::Statement(stmt) => stmt.evaluate(evaluator),
        }
    }
}

impl Evaluate for Statement {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        match self {
            Statement::ExpressionStatement(expr) => expr.evaluate(evaluator),
            Statement::IfStatement { .. } => todo!("IfStatement"),
            Statement::ForLoop { .. } => todo!("ForLoop"),
            Statement::WhileLoop { .. } => todo!("WhileLoop"),
            Statement::BlockStatement { .. } => todo!("BlockStatement"),
            Statement::ReturnStatement { .. } => todo!("ReturnStatement"),
            Statement::PrintStatement { .. } => todo!("PrintStatement"),
        }
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        match self {
            Expr::Identifier(_) => todo!("Identifier"),
            Expr::CallExpression { .. } => todo!("CallExpression"),
            Expr::PathExpression { .. } => todo!("PathExpression"),
            Expr::PrefixExpression { operator, expr } => {
                let expr = expr.evaluate(evaluator);
                match operator.0 {
                    PrefixOp::Not => LoxValue::Boolean(!expr.is_truthy()),
                    PrefixOp::Negate => {
                        if let LoxValue::Number(n) = expr {
                            LoxValue::Number(-n)
                        } else {
                            // TODO Proper error, attach location
                            panic!("Cannot apply prefix - to a non-number expression {expr}")
                        }
                    }
                }
            }
            Expr::InfixOperation { operator, lhs, rhs } => {
                let lhs = lhs.evaluate(evaluator);
                let rhs = rhs.evaluate(evaluator);

                fn numeric_op<F>(lhs: LoxValue, rhs: LoxValue, op: F) -> LoxValue
                where
                    F: FnOnce(f64, f64) -> LoxValue,
                {
                    match (lhs, rhs) {
                        (LoxValue::Number(lhs), LoxValue::Number(rhs)) => op(lhs, rhs),
                        (lhs, rhs) => panic!(
                            "Cannot apply numeric infix operator to non-numeric values: {lhs}, {rhs}"
                        ),
                    }
                }

                match operator.0 {
                    InfixOp::Plus => match (lhs, rhs) {
                        (LoxValue::Number(lhs), LoxValue::Number(rhs)) => {
                            LoxValue::Number(lhs + rhs)
                        }
                        (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                            LoxValue::String(lhs + &rhs)
                        }
                        (lhs, rhs) => panic!(
                            "Cannot apply + to except for two numbers or two strings: {lhs} + {rhs}"
                        ),
                    },
                    InfixOp::Minus => numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Number(lhs - rhs)),
                    InfixOp::Multiply => {
                        numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Number(lhs * rhs))
                    }
                    InfixOp::Divide => numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Number(lhs / rhs)),
                    InfixOp::Equals => LoxValue::Boolean(lhs == rhs),
                    InfixOp::NotEquals => LoxValue::Boolean(lhs != rhs),
                    InfixOp::LessThan => {
                        numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Boolean(lhs < rhs))
                    }
                    InfixOp::LessThanEqual => {
                        numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Boolean(lhs <= rhs))
                    }
                    InfixOp::GreaterThan => {
                        numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Boolean(lhs > rhs))
                    }
                    InfixOp::GreaterThanEqual => {
                        numeric_op(lhs, rhs, |lhs, rhs| LoxValue::Boolean(lhs >= rhs))
                    }
                    InfixOp::Or => todo!("Or"),
                    InfixOp::And => todo!("And"),
                    InfixOp::Assign => todo!("Assign"),
                }
            }
            Expr::Parenthesized { expr, .. } => expr.evaluate(evaluator),
            Expr::Literal(lit) => lit.evaluate(evaluator),
        }
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, evaluator: &mut Evaluator) -> LoxValue {
        match self {
            Literal::Raw(raw) => raw.evaluate(evaluator),
            Literal::Boolean(b) => LoxValue::Boolean(*b),
            Literal::Nil => LoxValue::Nil,
        }
    }
}

impl Evaluate for RawLiteral {
    fn evaluate(&self, _evaluator: &mut Evaluator) -> LoxValue {
        match self {
            RawLiteral::String { value, .. } => LoxValue::String(value.clone()),
            RawLiteral::Number { value, .. } => LoxValue::Number(*value),
        }
    }
}
