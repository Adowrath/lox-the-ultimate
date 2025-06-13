pub mod lox_types;

use crate::lox::ast::{
    Declaration, Expr, ForInitializer, InfixOp, Literal, PrefixOp, Program, Reference, Statement,
};
use crate::lox::eval::lox_types::{LoxClass, LoxFunction, LoxObject, LoxScope, LoxValue};
use crate::lox::types;
use crate::lox::types::{Identifier, Located, RawLiteral, Span};
use alloc::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

// TODO Locations?
#[derive(Debug)]
pub enum EvaluationError {
    Return(LoxValue),
    InternalError(String),
    UnknownFieldError(Located<Identifier>),
    TypeError(String, LoxValue, Option<LoxValue>),
    // Span instead of Expr
    ArityError(Box<Expr>, usize, usize),
}

#[derive(Default)]
pub struct Evaluator {
    pub globals: HashMap<Identifier, LoxValue>,
    pub scopes: Vec<LoxScope>,
}

impl Evaluator {
    pub fn evaluate(&mut self, program: Program) -> Result<LoxValue, EvaluationError> {
        program.evaluate(self)
    }

    pub fn in_scope<F, T>(&mut self, fun: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.push(Vec::new());
        let res = fun(self);
        self.scopes.pop();
        res
    }

    pub fn freeze_scope(&self) -> Vec<LoxScope> {
        self.scopes.clone()
    }

    pub fn with_scopes<F, T>(&mut self, scopes: Vec<LoxScope>, fun: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let old_scopes = std::mem::replace(&mut self.scopes, scopes);
        let res = fun(self);
        self.scopes = old_scopes;
        res
    }

    pub fn define(
        &mut self,
        reference: &Reference,
        value: LoxValue,
    ) -> Result<(), EvaluationError> {
        match reference {
            Reference::Identifier(id) => Err(EvaluationError::InternalError(format!(
                "Cannot define an unresolved variable: {id:?}"
            ))),
            Reference::Resolved {
                name,
                distance,
                index,
            } => {
                if *distance != 0 {
                    return Err(EvaluationError::InternalError(format!(
                        "Cannot define a value inside an outer scope: {name:?}"
                    )));
                }
                let env = self.scopes.last_mut().ok_or_else(|| {
                    EvaluationError::InternalError(format!(
                        "Defining a Resolved value inside Global scope: {name:?}"
                    ))
                })?;
                if &env.len() != index {
                    return Err(EvaluationError::InternalError(format!(
                        "Defining a new value should happen in order: {name:?}"
                    )));
                }

                env.push((name.0.clone(), Rc::new(RefCell::new(value))));
                Ok(())
            }
            Reference::Global(id) => {
                self.globals.insert(id.0.clone(), value);
                Ok(())
            }
        }
    }

    pub fn assign(
        &mut self,
        reference: &Reference,
        value: &LoxValue,
    ) -> Result<(), EvaluationError> {
        match reference {
            Reference::Identifier(id) => Err(EvaluationError::InternalError(format!(
                "Cannot assign to an unresolved variable: {id:?}"
            ))),
            Reference::Resolved {
                name,
                distance,
                index,
            } => {
                let env_count = self.scopes.len();
                let entry = self
                    .scopes
                    .get_mut(env_count - distance)
                    .ok_or_else(|| {
                        EvaluationError::InternalError(format!(
                            "Assigning a value in a non-existent scope: {name:?}"
                        ))
                    })?
                    .get_mut(*index)
                    .ok_or_else(|| {
                        EvaluationError::InternalError(format!(
                            "Invalid Index for Resolved value: {name:?}"
                        ))
                    })?;

                *entry.1.borrow_mut() = value.clone();
                Ok(())
            }
            Reference::Global(id) => {
                let entry = self.globals.get_mut(&id.0).ok_or_else(|| {
                    EvaluationError::InternalError(format!(
                        "Cannot assign to an undeclared global variable {id:?}"
                    ))
                })?;

                *entry = value.clone();
                Ok(())
            }
        }
    }

    pub fn resolve(&mut self, reference: &Reference) -> Result<LoxValue, EvaluationError> {
        match reference {
            Reference::Identifier(id) => Err(EvaluationError::InternalError(format!(
                "Cannot resolve an unresolved variable: {id:?}"
            ))),
            Reference::Resolved {
                name,
                distance,
                index,
            } => Ok(self
                .scopes
                .get(self.scopes.len() - 1 - distance)
                .ok_or_else(|| {
                    EvaluationError::InternalError(format!(
                        "Resolving a value in a non-existent scope: {name:?}"
                    ))
                })?
                .get(*index)
                .ok_or_else(|| {
                    EvaluationError::InternalError(format!(
                        "Invalid Index for Resolved value: {name:?}"
                    ))
                })?
                .1
                .as_ref()
                .borrow()
                .clone()),
            Reference::Global(id) => Ok(self
                .globals
                .get(&id.0)
                .ok_or_else(|| {
                    EvaluationError::InternalError(format!(
                        "Could not resolve global value: {id:?}"
                    ))
                })?
                .clone()),
        }
    }
}

trait Evaluate {
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError>;
}

impl<T: Evaluate> Evaluate for Box<T> {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        self.as_ref().evaluate(evaluator)
    }
}
impl<T: Evaluate> Evaluate for Option<T> {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            Some(value) => value.evaluate(evaluator),
            None => Ok(LoxValue::Nil),
        }
    }
}

impl<T: Evaluate> Evaluate for types::Located<T> {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        self.0.evaluate(evaluator)
    }
}

impl Evaluate for Program {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        let mut result = LoxValue::Nil;
        for decl in &self.declarations {
            result = decl.evaluate(evaluator)?;
        }
        Ok(result)
    }
}

impl Evaluate for Declaration {
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            Declaration::VariableDeclaration { assignee, value } => {
                let value = value.evaluate(evaluator);
                evaluator.define(assignee, value?)?;
                Ok(LoxValue::Nil)
            }
            Declaration::ClassDeclaration {
                name,
                superclass,
                funcs,
            } => {
                let superclass = match superclass {
                    Some(superclass) => match evaluator.resolve(superclass)? {
                        LoxValue::Class(class) => Some(class),
                        other => {
                            return Err(EvaluationError::TypeError(
                                "Invalid value for a superclass".to_owned(),
                                other,
                                None,
                            ));
                        }
                    },
                    None => None,
                };
                let func_scope = if let Some(ref superclass) = superclass {
                    evaluator.in_scope(|evaluator| {
                        evaluator.define(
                            &Reference::Resolved {
                                name: Located(Identifier("super".to_owned()), Span::Empty),
                                distance: 0,
                                index: 0,
                            },
                            LoxValue::Class(superclass.clone()),
                        )?;
                        Ok(evaluator.freeze_scope())
                    })?
                } else {
                    evaluator.freeze_scope()
                };

                let class = Rc::new(RefCell::new(LoxClass {
                    name: name.clone(),
                    superclass,
                    functions: Default::default(),
                }));

                for func in funcs {
                    let func_name = match &func.name {
                        Reference::Identifier(id) => id.clone().0,
                        reference => {
                            return Err(EvaluationError::InternalError(format!(
                                "Method name should remain a simple identifier and not be resolved: {reference:?}",
                            )));
                        }
                    };
                    let is_initializer = func_name.0 == "this";
                    class.borrow_mut().functions.insert(
                        func_name,
                        LoxFunction {
                            declaring_class: Some(class.clone()),
                            scopes: func_scope.clone(),
                            params: func.parameters.clone(),
                            is_initializer,
                            body: func.body.clone(),
                        },
                    );
                }

                evaluator.define(name, LoxValue::Class(class))?;

                Ok(LoxValue::Nil)
            }
            Declaration::FunctionDeclaration(func) => {
                evaluator.define(
                    &func.name,
                    LoxValue::Function(LoxFunction {
                        declaring_class: None,
                        scopes: evaluator.freeze_scope(),
                        params: func.parameters.clone(),
                        is_initializer: false,
                        body: func.body.clone(),
                    }),
                )?;

                Ok(LoxValue::Nil)
            }
            Declaration::Statement(stmt) => stmt.evaluate(evaluator),
        }
    }
}

impl Evaluate for Statement {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            Statement::ExpressionStatement(expr) => expr.evaluate(evaluator),
            Statement::IfStatement {
                condition,
                then_branch,
                else_branch,
            } => {
                if condition.evaluate(evaluator)?.is_truthy() {
                    then_branch.evaluate(evaluator)?;
                } else {
                    else_branch.evaluate(evaluator)?;
                }
                Ok(LoxValue::Nil)
            }
            Statement::ForLoop {
                initializer,
                condition,
                step,
                body,
            } => {
                if let Some(initializer) = initializer {
                    initializer.evaluate(evaluator)?;
                }
                loop {
                    if let Some(condition) = condition {
                        if !condition.evaluate(evaluator)?.is_truthy() {
                            break;
                        }

                        // TODO Break/Continue
                        body.evaluate(evaluator)?;

                        if let Some(step) = step {
                            step.evaluate(evaluator)?;
                        }
                    }
                }
                Ok(LoxValue::Nil)
            }
            Statement::WhileLoop { condition, body } => {
                loop {
                    if condition.evaluate(evaluator)?.is_truthy() {
                        break;
                    }
                    // TODO Break/Continue
                    body.evaluate(evaluator)?;
                }
                Ok(LoxValue::Nil)
            }
            Statement::BlockStatement { body } => {
                for decl in body {
                    decl.evaluate(evaluator)?;
                }
                Ok(LoxValue::Nil)
            }
            Statement::ReturnStatement { return_value } => {
                Err(EvaluationError::Return(return_value.evaluate(evaluator)?))
            }
            Statement::PrintStatement {
                print_span: _,
                printed_expr,
            } => {
                let expr = printed_expr.evaluate(evaluator)?;
                println!("{expr}");
                Ok(LoxValue::Nil)
            }
        }
    }
}

impl Evaluate for ForInitializer {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            ForInitializer::Expression(expr) => expr.evaluate(evaluator),
            ForInitializer::VariableDeclaration { assignee, value } => {
                let value = match value {
                    None => LoxValue::Nil,
                    Some(value) => value.evaluate(evaluator)?,
                };

                evaluator.define(assignee, value)?;
                Ok(LoxValue::Nil)
            }
        }
    }
}

impl Evaluate for Expr {
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            Expr::Reference(reference) => evaluator.resolve(reference),
            Expr::CallExpression { callee, arguments } => {
                let callee_value = callee.evaluate(evaluator)?;
                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|arg| arg.evaluate(evaluator))
                    .collect::<Result<_, _>>()?;

                match callee_value {
                    LoxValue::Class(class) => {
                        let obj =
                            LoxValue::Object(Rc::new(RefCell::new(LoxObject::new(class.clone()))));

                        if let Some(init) =
                            class.borrow().get_method(&Identifier("init".to_owned()))
                        {
                            if init.params.len() != arguments.len() {
                                return Err(EvaluationError::ArityError(
                                    callee.clone(),
                                    init.params.len(),
                                    arguments.len(),
                                ));
                            }

                            evaluator.with_scopes(init.scopes, |evaluator| {
                                evaluator.in_scope(|evaluator| {
                                    evaluator.define(
                                        &Reference::Resolved {
                                            name: Located(
                                                Identifier("this".to_owned()),
                                                Span::Empty,
                                            ),
                                            distance: 0,
                                            index: 0,
                                        },
                                        obj.clone(),
                                    )?;

                                    evaluator.in_scope(|evaluator| {
                                        for (param, value) in
                                            init.params.iter().zip(arguments.into_iter())
                                        {
                                            evaluator.define(param, value)?;
                                        }

                                        for decl in init.body {
                                            match decl.evaluate(evaluator) {
                                                Ok(_) => {}
                                                Err(EvaluationError::Return(_)) => {
                                                    // No return value needed as we discard it either way.
                                                    break;
                                                }
                                                Err(e) => return Err(e),
                                            }
                                        }
                                        Ok(LoxValue::Nil)
                                    })
                                })
                            })?;
                        } else {
                            // NO Init Method, Nothing to do.
                        };

                        Ok(obj)
                    }
                    LoxValue::Function(function) => {
                        if function.params.len() != arguments.len() {
                            return Err(EvaluationError::ArityError(
                                callee.clone(),
                                function.params.len(),
                                arguments.len(),
                            ));
                        }

                        evaluator.with_scopes(function.scopes.clone(), |evaluator| {
                            evaluator.in_scope(|evaluator| {
                                for (param, value) in
                                    function.params.iter().zip(arguments.into_iter())
                                {
                                    evaluator.define(param, value)?;
                                }

                                for decl in function.body {
                                    match decl.evaluate(evaluator) {
                                        Ok(_) => {}
                                        Err(EvaluationError::Return(ret_value)) => {
                                            if function.is_initializer {
                                                return evaluator.resolve(&Reference::Resolved {
                                                    name: Span::Empty
                                                        .locate(Identifier("this".to_owned())),
                                                    distance: 1,
                                                    index: 0,
                                                });
                                            }
                                            // TODO Overwrite Return in case of Initializer
                                            return Ok(ret_value);
                                        }
                                        Err(e) => return Err(e),
                                    }
                                }
                                Ok(LoxValue::Nil)
                            })
                        })
                    }
                    _ => Err(EvaluationError::TypeError(
                        "Tried calling non-callable value".to_string(),
                        callee_value,
                        None,
                    )),
                }
            }
            Expr::PathExpression {
                receiver,
                field_name,
            } => match receiver.evaluate(evaluator)? {
                LoxValue::Object(obj) => {
                    let value = obj
                        .borrow()
                        .get(&field_name.0)
                        .ok_or_else(|| EvaluationError::UnknownFieldError(field_name.clone()))?;

                    match value {
                        LoxValue::Function(function) => {
                            // TODO BIND,
                            match function.declaring_class {
                                None => Ok(LoxValue::Function(function)),
                                Some(_) => {
                                    let mut scopes = function.scopes.clone();
                                    scopes.push(vec![(
                                        Identifier("this".to_string()),
                                        Rc::new(RefCell::new(LoxValue::Object(obj))),
                                    )]);

                                    Ok(LoxValue::Function(LoxFunction {
                                        params: function.params.clone(),
                                        body: function.body.clone(),
                                        scopes,
                                        is_initializer: function.is_initializer,
                                        declaring_class: function.declaring_class.clone(),
                                    }))
                                }
                            }
                        }
                        other => Ok(other),
                    }
                }
                value => Err(EvaluationError::TypeError(
                    format!("Could not access field {field_name:?} on non-object value"),
                    value,
                    None,
                )),
            },
            Expr::PrefixExpression { operator, expr } => {
                let expr = expr.evaluate(evaluator)?;
                match operator.0 {
                    PrefixOp::Not => Ok(LoxValue::Boolean(!expr.is_truthy())),
                    PrefixOp::Negate => {
                        if let LoxValue::Number(n) = expr {
                            Ok(LoxValue::Number(-n))
                        } else {
                            Err(EvaluationError::TypeError(
                                "Cannot apply prefix - to a non-number value".to_owned(),
                                expr,
                                None,
                            ))
                        }
                    }
                }
            }
            Expr::InfixOperation { operator, lhs, rhs } => {
                fn numeric_op<F>(
                    lhs: &Box<Expr>,
                    rhs: &Box<Expr>,
                    evaluator: &mut Evaluator,
                    op: F,
                ) -> Result<LoxValue, EvaluationError>
                where
                    F: FnOnce(f64, f64) -> LoxValue,
                {
                    let lhs = lhs.evaluate(evaluator)?;
                    let rhs = rhs.evaluate(evaluator)?;
                    match (lhs, rhs) {
                        (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(op(lhs, rhs)),
                        (lhs, rhs) => Err(EvaluationError::TypeError(
                            "Cannot apply numeric infix operator to non-numeric values".to_owned(),
                            lhs,
                            Some(rhs),
                        )),
                    }
                }

                match operator.0 {
                    InfixOp::Plus => match (lhs.evaluate(evaluator)?, rhs.evaluate(evaluator)?) {
                        (LoxValue::Number(lhs), LoxValue::Number(rhs)) => {
                            Ok(LoxValue::Number(lhs + rhs))
                        }
                        (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                            Ok(LoxValue::String(lhs + &rhs))
                        }
                        (lhs, rhs) => Err(EvaluationError::TypeError(
                            "Cannot apply + to except for two numbers or two strings".to_owned(),
                            lhs,
                            Some(rhs),
                        )),
                    },
                    InfixOp::Minus => {
                        numeric_op(lhs, rhs, evaluator, |lhs, rhs| LoxValue::Number(lhs - rhs))
                    }
                    InfixOp::Multiply => {
                        numeric_op(lhs, rhs, evaluator, |lhs, rhs| LoxValue::Number(lhs * rhs))
                    }
                    InfixOp::Divide => {
                        numeric_op(lhs, rhs, evaluator, |lhs, rhs| LoxValue::Number(lhs / rhs))
                    }
                    InfixOp::Equals => {
                        let lhs = lhs.evaluate(evaluator)?;
                        let rhs = rhs.evaluate(evaluator)?;
                        Ok(LoxValue::Boolean(lhs == rhs))
                    }
                    InfixOp::NotEquals => {
                        let lhs = lhs.evaluate(evaluator)?;
                        let rhs = rhs.evaluate(evaluator)?;
                        Ok(LoxValue::Boolean(lhs != rhs))
                    }
                    InfixOp::LessThan => {
                        numeric_op(lhs, rhs, evaluator, |lhs, rhs| LoxValue::Boolean(lhs < rhs))
                    }
                    InfixOp::LessThanEqual => numeric_op(lhs, rhs, evaluator, |lhs, rhs| {
                        LoxValue::Boolean(lhs <= rhs)
                    }),
                    InfixOp::GreaterThan => {
                        numeric_op(lhs, rhs, evaluator, |lhs, rhs| LoxValue::Boolean(lhs > rhs))
                    }
                    InfixOp::GreaterThanEqual => numeric_op(lhs, rhs, evaluator, |lhs, rhs| {
                        LoxValue::Boolean(lhs >= rhs)
                    }),
                    InfixOp::Or => {
                        let lhs = lhs.evaluate(evaluator)?;
                        if lhs.is_truthy() {
                            Ok(lhs)
                        } else {
                            Ok(rhs.evaluate(evaluator)?)
                        }
                    }
                    InfixOp::And => {
                        let lhs = lhs.evaluate(evaluator)?;
                        if !lhs.is_truthy() {
                            Ok(lhs)
                        } else {
                            Ok(rhs.evaluate(evaluator)?)
                        }
                    }
                    InfixOp::Assign => {
                        fn execute_assignment(
                            lhs: &Expr,
                            rhs: &Expr,
                            evaluator: &mut Evaluator,
                        ) -> Result<LoxValue, EvaluationError> {
                            match lhs {
                                // TODO Verify these in the verify phase!
                                Expr::PrefixExpression { .. } => {
                                    Err(EvaluationError::InternalError(format!(
                                        "Cannot assign to PrefixExpression: {lhs:?}"
                                    )))
                                }
                                Expr::InfixOperation { .. } => Err(EvaluationError::InternalError(
                                    format!("Cannot assign to InfixOperation: {lhs:?}"),
                                )),
                                Expr::Literal(_) => Err(EvaluationError::InternalError(format!(
                                    "Cannot assign to Literal: {lhs:?}"
                                ))),
                                Expr::CallExpression { .. } => Err(EvaluationError::InternalError(
                                    format!("Cannot assign to CallExpression: {lhs:?}"),
                                )),
                                Expr::PathExpression {
                                    receiver,
                                    field_name,
                                } => match receiver.evaluate(evaluator)? {
                                    LoxValue::Object(obj) => {
                                        let rhs = rhs.evaluate(evaluator)?;
                                        obj.borrow_mut().set(&field_name.0, &rhs);
                                        Ok(rhs)
                                    }
                                    value => Err(EvaluationError::TypeError(
                                        format!(
                                            "Cannot set field {field_name:?} on non-object value"
                                        ),
                                        value,
                                        None,
                                    )),
                                },
                                Expr::Parenthesized {
                                    expr,
                                    source_span: _,
                                } => execute_assignment(expr, rhs, evaluator),
                                Expr::Reference(reference) => {
                                    let rhs = rhs.evaluate(evaluator)?;
                                    evaluator.assign(reference, &rhs)?;
                                    Ok(rhs)
                                }
                            }
                        }

                        execute_assignment(lhs, rhs, evaluator)
                    }
                }
            }
            Expr::Parenthesized {
                expr,
                source_span: _,
            } => expr.evaluate(evaluator),
            Expr::Literal(lit) => lit.evaluate(evaluator),
        }
    }
}

impl Evaluate for Literal {
    #[inline]
    fn evaluate(&self, evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        match self {
            Literal::Raw(raw) => raw.evaluate(evaluator),
            Literal::Boolean(b) => Ok(LoxValue::Boolean(*b)),
            Literal::Nil => Ok(LoxValue::Nil),
        }
    }
}

impl Evaluate for RawLiteral {
    #[inline]
    fn evaluate(&self, _evaluator: &mut Evaluator) -> Result<LoxValue, EvaluationError> {
        Ok(match self {
            RawLiteral::String { value, raw: _ } => LoxValue::String(value.clone()),
            RawLiteral::Number { value, raw: _ } => LoxValue::Number(*value),
        })
    }
}
