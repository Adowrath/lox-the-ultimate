use crate::lox::ast::{
    Declaration, Expr, ForInitializer, FunctionDeclaration, Program, Reference, Statement,
};
use crate::lox::types::{Identifier, Located};

#[derive(Debug)]
pub enum ValidateError {
    E,
}

pub struct ValidationState {
    scopes: Vec<Vec<Identifier>>,
    errors: Vec<ValidateError>,
}

impl ValidationState {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn in_scope<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.scopes.push(Vec::new());
        let res = f(self);
        self.scopes.pop();
        res
    }

    /// Declare a new identifier in the current scope, returning an updated Reference to it.
    pub fn declare(&mut self, reference: &mut Reference) {
        if let Reference::Identifier(id) = reference {
            *reference = match self.scopes.last_mut() {
                None => Reference::Global(id.clone()),
                Some(scope) => {
                    scope.push(id.0.clone());
                    Reference::Resolved {
                        name: id.clone(),
                        distance: 0,
                        index: scope.len() - 1,
                    }
                }
            }
        } else {
            // Already resolved, no need to do anything.
        }
    }

    /// Lookup and resolve an identifier to a reference
    pub fn resolve(&self, id: Located<Identifier>) -> Reference {
        let mut dist = 0;
        for scope in self.scopes.iter().rev() {
            match scope.iter().position(|binding| binding == &id.0) {
                None => dist += 1,
                Some(idx) => {
                    return Reference::Resolved {
                        name: id.clone(),
                        distance: dist,
                        index: idx,
                    };
                }
            }
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
                if let Some(value) = value {
                    value.validate(state);
                }
                state.declare(assignee);
            }
            Declaration::ClassDeclaration { name, funcs } => {
                state.declare(name);
                for func_decl in funcs {
                    func_decl.validate(state);
                }
            }
            Declaration::FunctionDeclaration(func_decl) => func_decl.validate(state),
            Declaration::Statement(stmt) => stmt.validate(state),
        }
    }
}

impl Validate for FunctionDeclaration {
    fn validate(&mut self, state: &mut ValidationState) {
        let FunctionDeclaration {
            name,
            parameters,
            body,
            ..
        } = self;
        state.declare(name);
        state.in_scope(|state| {
            for parameter in parameters {
                state.declare(parameter);
            }
            for decl in body {
                decl.validate(state);
            }
        })
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
            } => state.in_scope(|state| {
                initializer.validate(state);
                condition.validate(state);
                step.validate(state);
                body.validate(state);
            }),

            Statement::WhileLoop { condition, body } => state.in_scope(|state| {
                condition.validate(state);
                body.validate(state);
            }),

            Statement::BlockStatement { body } => state.in_scope(|state| {
                for decl in body {
                    decl.validate(state);
                }
            }),

            Statement::ReturnStatement { return_value } => return_value.validate(state),
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
            _ => panic!("Reference already resolved during validation: {self:?}"),
        }
    }
}
