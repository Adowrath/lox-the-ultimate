use crate::lox::ast::{
    Declaration, Expr, ForInitializer, FunctionDeclaration, Program, Reference, Statement,
};
use crate::lox::types::{Identifier, Located, Span};
use itertools::Itertools;

#[derive(Debug)]
pub enum ValidateError {
    Internal(String),
    Invalid(&'static str, Span),
    UnknownReference(Located<Identifier>),
    EarlyReference(Located<Identifier>),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Context {
    Global,
    Initializer { subclass: bool },
    Function { method: bool, subclass: bool },
}

pub struct ValidationState {
    scopes: Vec<Vec<(Identifier, bool)>>,
    contexts: Vec<Context>,
    errors: Vec<ValidateError>,
}

impl ValidationState {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            contexts: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[inline]
    pub fn report(&mut self, err: ValidateError) {
        self.errors.push(err);
    }

    #[inline]
    pub fn active_context(&self) -> Context {
        *self.contexts.last().unwrap_or(&Context::Global)
    }

    #[inline]
    pub fn in_scope<F>(&mut self, f: F) -> usize
    where
        F: FnOnce(&mut Self),
    {
        self.scopes.push(Vec::new());
        f(self);
        self.scopes.pop().expect("we pushed").len()
    }

    #[inline]
    pub fn in_context<F>(&mut self, context: Context, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.contexts.push(context);
        f(self);
        self.contexts.pop();
    }

    #[inline]
    /// Declare a new identifier in the current scope, returning an
    /// updated [`Reference`] to it.
    pub fn declare(&mut self, reference: &mut Reference) {
        self.declaring(reference, |_| ())
    }

    #[inline]
    /// Declare a new identifier in the current scope, separating declaration
    /// and definition phase split up by the callback.
    pub fn declaring<F>(&mut self, reference: &mut Reference, func: F)
    where
        F: FnOnce(&mut Self),
    {
        if let Reference::Identifier(id) = reference {
            if self.scopes.is_empty() {
                func(self);
                *reference = Reference::Global(id.clone());
                return;
            }

            self.scopes.last_mut().unwrap().push((id.0.clone(), false));
            func(self);

            let scope = self.scopes.last_mut().unwrap();
            scope.last_mut().expect("We just pushed").1 = true;

            *reference = Reference::Resolved {
                name: id.clone(),
                distance: 0,
                index: scope.len() - 1,
            };
        } else {
            func(self)
        }
    }

    #[inline]
    /// Lookup and resolve an identifier to a reference
    pub fn resolve(&mut self, id: Located<Identifier>) -> Reference {
        let mut dist = 0;
        for scope in self.scopes.iter().rev() {
            match scope.iter().find_position(|binding| binding.0 == id.0) {
                None => dist += 1,
                Some((idx, (_, true))) => {
                    return Reference::Resolved {
                        name: id.clone(),
                        distance: dist,
                        index: idx,
                    };
                }
                _ => {
                    self.report(ValidateError::EarlyReference(id.clone()));
                    break;
                }
            }
        }
        // We only error out if we are at the top-level
        if self.scopes.is_empty() {
            self.report(ValidateError::UnknownReference(id.clone()));
        }
        Reference::Global(id)
    }
}

pub fn validate_program(mut program: Program) -> Result<Program, Vec<ValidateError>> {
    let mut state = ValidationState::new();
    program.validate(&mut state);

    if state.errors.is_empty() {
        Ok(program)
    } else {
        Err(state.errors)
    }
}

// TODO Attach Locals size info to Block-creating nodes?
trait Validate {
    /// Validates this AST component.
    fn validate(&mut self, state: &mut ValidationState);
}

impl<T: Validate> Validate for Box<T> {
    fn validate(&mut self, state: &mut ValidationState) {
        self.as_mut().validate(state)
    }
}

impl<T: Validate> Validate for Option<T> {
    fn validate(&mut self, state: &mut ValidationState) {
        if let Some(x) = self {
            x.validate(state);
        }
    }
}

impl Validate for Program {
    fn validate(&mut self, state: &mut ValidationState) {
        for decl in &mut self.declarations {
            decl.validate(state);
        }
    }
}

impl Validate for Declaration {
    fn validate(&mut self, state: &mut ValidationState) {
        match self {
            Declaration::VariableDeclaration { assignee, value } => {
                state.declaring(assignee, |state| {
                    if let Some(value) = value {
                        value.validate(state);
                    }
                });
            }
            Declaration::ClassDeclaration { name, funcs } => {
                state.declare(name);

                let subclass = false;
                let define_methods = |state: &mut ValidationState| {
                    state.in_scope(|state| {
                        state.declare(&mut Reference::Identifier(Located(
                            Identifier("this".to_owned()),
                            Span::Empty,
                        )));

                        for func_decl in funcs {
                            let func_name = match func_decl.name {
                                Reference::Identifier(ref id) => id,
                                Reference::Resolved { ref name, .. } => name,
                                Reference::Global(ref name) => name,
                            };
                            let ctx = if func_name.0 == Identifier("init".to_owned()) {
                                Context::Initializer { subclass }
                            } else {
                                Context::Function {
                                    method: true,
                                    subclass,
                                }
                            };

                            state.in_context(ctx, |state| {
                                func_decl.validate(state);
                            });
                        }
                    })
                };

                // if subclass scope for super
                if subclass {
                    state.in_scope(|state| {
                        state.declare(&mut Reference::Identifier(Located(
                            Identifier("super".to_owned()),
                            Span::Empty,
                        )));

                        define_methods(state);
                    });
                } else {
                    define_methods(state);
                }
            }
            Declaration::FunctionDeclaration(func_decl) => {
                state.declare(&mut func_decl.name);

                let ctx = match state.active_context() {
                    Context::Function { method, subclass } => {
                        Context::Function { method, subclass }
                    }
                    Context::Initializer { subclass } => Context::Function {
                        method: true,
                        subclass,
                    },
                    Context::Global => Context::Function {
                        method: false,
                        subclass: false,
                    },
                };

                state.in_context(ctx, |state| func_decl.validate(state))
            }
            Declaration::Statement(stmt) => stmt.validate(state),
        }
    }
}

impl Validate for FunctionDeclaration {
    fn validate(&mut self, state: &mut ValidationState) {
        let FunctionDeclaration {
            parameters, body, ..
        } = self;
        let _ = state.in_scope(|state| {
            for parameter in parameters {
                state.declare(parameter);
            }
            for decl in body {
                decl.validate(state);
            }
        });
    }
}

impl Validate for Statement {
    fn validate(&mut self, state: &mut ValidationState) {
        match self {
            Statement::ExpressionStatement(expr) => expr.validate(state),

            Statement::IfStatement {
                condition,
                then_branch,
                else_branch,
            } => {
                condition.validate(state);
                then_branch.validate(state);
                else_branch.validate(state);
            }

            Statement::ForLoop {
                initializer,
                condition,
                step,
                body,
            } => {
                let _ = state.in_scope(|state| {
                    initializer.validate(state);
                    condition.validate(state);
                    step.validate(state);
                    body.validate(state);
                });
            }

            Statement::WhileLoop { condition, body } => {
                let _ = state.in_scope(|state| {
                    condition.validate(state);
                    body.validate(state);
                });
            }

            Statement::BlockStatement { body } => {
                let _ = state.in_scope(|state| {
                    for decl in body {
                        decl.validate(state);
                    }
                });
            }

            Statement::ReturnStatement { return_value } => {
                match state.active_context() {
                    Context::Global => state.report(ValidateError::Invalid(
                        "Cannot return a value from an initializer",
                        Span::Empty, // TODO Span of entire thing
                    )),
                    Context::Initializer { .. } => {
                        if let Some(_return_value) = return_value {
                            state.report(ValidateError::Invalid(
                                "Cannot return a value from an initializer",
                                Span::Empty,
                            ))
                        }
                    }
                    Context::Function { .. } => {}
                }
                return_value.validate(state)
            }
            Statement::PrintStatement { printed_expr, .. } => printed_expr.validate(state),
        }
    }
}

impl Validate for ForInitializer {
    fn validate(&mut self, state: &mut ValidationState) {
        match self {
            ForInitializer::VariableDeclaration { assignee, value } => {
                value.validate(state);
                state.declare(assignee);
            }
            ForInitializer::Expression(expr) => expr.validate(state),
        }
    }
}

impl Validate for Expr {
    fn validate(&mut self, state: &mut ValidationState) {
        match self {
            Expr::PrefixExpression { expr, .. } => expr.validate(state),
            Expr::InfixOperation { lhs, rhs, .. } => {
                lhs.validate(state);
                rhs.validate(state);
            }
            Expr::Parenthesized { expr, .. } => expr.validate(state),
            Expr::Reference(reference) => reference.validate(state),
            Expr::Literal(_) => (),
            Expr::CallExpression { callee, arguments } => {
                callee.validate(state);
                for arg in arguments {
                    arg.validate(state);
                }
            }
            Expr::PathExpression { receiver, .. } => {
                receiver.validate(state);
            }
        }
    }
}

impl Validate for Reference {
    fn validate(&mut self, state: &mut ValidationState) {
        match self {
            Reference::Identifier(id) => *self = state.resolve(id.clone()),
            _ => state.report(ValidateError::Internal(format!(
                "Reference already resolved during validation, this should not happen {self:?}"
            ))),
        }
    }
}
