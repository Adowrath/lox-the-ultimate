use crate::lox::ast;

#[derive(Debug)]
pub enum ValidateError {
    E,
}

pub fn validate_program(mut program: ast::Program) -> Result<ast::Program, Vec<ValidateError>> {
    let mut errors = Vec::new();
    program.validate(&mut errors);

    if errors.is_empty() {
        Ok(program)
    } else {
        Err(errors)
    }
}

trait Validate {
    /// Validates this AST component.
    fn validate(&mut self, errors: &mut Vec<ValidateError>);
}

impl Validate for ast::Program {
    fn validate(&mut self, _errors: &mut Vec<ValidateError>) {
        for _decl in &mut self.declarations {
            //decl.validate(errors);
        }
    }
}
