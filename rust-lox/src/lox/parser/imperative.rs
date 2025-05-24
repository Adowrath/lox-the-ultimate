//! An imperative parser that roughly follows the same LR(1)
//! structure of the book's parser implementation.
#![expect(
    clippy::wildcard_enum_match_arm,
    reason = "Default cases are the only way to keep parsers sane"
)]
#![expect(
    clippy::missing_docs_in_private_items,
    clippy::exhaustive_enums,
    reason = "Temporary"
)]

use crate::lox::ast;
use crate::lox::token::tokens;
use crate::lox::types;

/// TODO Fault-tolerance.

pub struct ParseInput<'a, Token>(std::cell::Cell<&'a [Token]>);

impl<'a, Token> ParseInput<'a, Token> {
    pub fn new<T>(token: &'a T) -> Self
    where
        T: std::ops::Deref<Target = [Token]>,
    {
        ParseInput(std::cell::Cell::new(&token))
    }

    pub fn is_empty(&self) -> bool {
        self.0.get().is_empty()
    }

    pub fn next(&self) -> &'a Token {
        let (next, rest) = self.0.get().split_first().unwrap();
        self.0.set(rest);
        next
    }

    pub fn peek(&self) -> &'a Token {
        self.0.get().first().unwrap()
    }

    pub fn advance(&self) {
        self.0.set(self.0.get().split_first().unwrap().1);
    }
}

pub trait Parse<Token>: Sized {
    type ParseResult;

    fn parse(tokens: &ParseInput<'_, Token>) -> Self::ParseResult;
}

impl Parse<tokens::Token> for ast::Program {
    type ParseResult = Self;

    fn parse(tokens: &ParseInput<'_, tokens::Token>) -> Self::ParseResult {
        let mut declarations = Vec::new();
        while !tokens.is_empty() && !matches!(tokens.peek().0, tokens::TokenType::EndOfInput) {
            let decl = ast::Declaration::parse(tokens);
            declarations.push(decl);
        }
        ast::Program { declarations }
    }
}

impl Parse<tokens::Token> for ast::Declaration {
    type ParseResult = Self;

    fn parse(tokens: &ParseInput<'_, tokens::Token>) -> Self::ParseResult {
        let original_tokens = tokens.0.get();

        let types::Located(next, start_span) = tokens.next();

        match next {
            tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                let (id, value) = parse_var_declaration(tokens);

                Self::VariableDeclaration {
                    assignee: id,
                    value,
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::Class) => {
                let name = types::Located::<types::Identifier>::parse(tokens);

                match tokens.next().0 {
                    tokens::TokenType::LeftBrace => {}
                    _ => panic!("{{ expected"),
                }

                let mut funcs = Vec::new();
                loop {
                    let next = tokens.next();
                    match next.0 {
                        tokens::TokenType::RightBrace => {
                            break;
                        }
                        tokens::TokenType::Identifier(ref id) => {
                            funcs.push(parse_function_parts(
                                next.1,
                                types::Located(id.clone(), next.1),
                                tokens,
                            ));
                        }
                        _ => panic!("Invalid token inside class declaration"),
                    }
                }

                Self::ClassDeclaration { name, funcs }
            }
            tokens::TokenType::Keyword(tokens::Keyword::Fun) => {
                Self::FunctionDeclaration(parse_function_parts(
                    *start_span,
                    types::Located::<types::Identifier>::parse(tokens),
                    tokens,
                ))
            }
            _ => {
                tokens.0.set(original_tokens);
                Self::Statement(ast::Statement::parse(tokens))
            }
        }
    }
}

fn parse_var_declaration(
    tokens: &ParseInput<'_, tokens::Token>,
) -> (types::Located<types::Identifier>, Option<ast::Expr>) {
    let id = types::Located::<types::Identifier>::parse(tokens);
    let value = match tokens.peek().0 {
        tokens::TokenType::Semi => None,
        tokens::TokenType::Assign => {
            tokens.advance();
            Some(ast::Expr::parse(tokens))
        }
        _ => panic!("Invalid token inside variable declaration"),
    };

    match tokens.next().0 {
        tokens::TokenType::Semi => {}
        _ => panic!("; expected"),
    }

    (id, value)
}

fn parse_function_parts(
    start_span: types::Span,
    name: types::Located<types::Identifier>,
    tokens: &ParseInput<'_, tokens::Token>,
) -> ast::FunctionDeclaration {
    match tokens.next().0 {
        tokens::TokenType::LeftParen => {}
        _ => panic!("( expected"),
    }

    let mut parameters = Vec::new();
    let mut first = true;
    loop {
        match tokens.peek().0 {
            tokens::TokenType::RightParen => {
                tokens.advance();
                break;
            }
            tokens::TokenType::Comma if first => panic!("Expected identifier or )"),
            tokens::TokenType::Comma => tokens.advance(),
            _ => {} // will be consumed by identifier parsing
        };
        first = false;

        parameters.push(types::Located::<types::Identifier>::parse(tokens));
    }

    match tokens.next().0 {
        tokens::TokenType::LeftBrace => {}
        _ => panic!("{{ expected"),
    }

    let mut body = Vec::new();
    let last_span = loop {
        match tokens.peek() {
            types::Located(tokens::TokenType::RightBrace, last_loc) => {
                tokens.advance();
                break last_loc;
            }
            _ => {} // New declaration
        };

        body.push(ast::Declaration::parse(tokens));
    };

    ast::FunctionDeclaration {
        span: types::Span::merge(&vec![start_span, *last_span]),
        name,
        parameters,
        body,
    }
}

impl Parse<tokens::Token> for ast::Statement {
    type ParseResult = Self;

    fn parse(tokens: &ParseInput<'_, tokens::Token>) -> Self::ParseResult {
        let original_tokens = tokens.0.get();
        let types::Located(next, kw_loc) = tokens.next();

        match next {
            tokens::TokenType::Keyword(tokens::Keyword::Print) => {
                let expr = ast::Expr::parse(tokens);

                match tokens.next().0 {
                    tokens::TokenType::Semi => {}
                    _ => panic!("; expected"),
                }

                Self::PrintStatement {
                    print_span: *kw_loc,
                    printed_expr: expr,
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::If) => {
                match tokens.next().0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let condition = ast::Expr::parse(tokens);

                match tokens.next().0 {
                    tokens::TokenType::RightParen => {}
                    _ => panic!(") expected"),
                }

                let then_branch = ast::Statement::parse(tokens);

                let else_branch = match tokens.peek().0 {
                    tokens::TokenType::Keyword(tokens::Keyword::Else) => {
                        tokens.advance();
                        Some(ast::Statement::parse(tokens))
                    }
                    _ => None,
                };

                Self::IfStatement {
                    condition,
                    then_branch: Box::new(then_branch),
                    else_branch: else_branch.map(Box::new),
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::For) => {
                match tokens.next().0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let initializer = match tokens.peek().0 {
                    tokens::TokenType::Semi => {
                        tokens.advance();
                        None
                    }
                    tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                        tokens.advance();
                        let (id, value) = parse_var_declaration(tokens);

                        Some(ast::ForInitializer::VariableDeclaration {
                            assignee: id,
                            value,
                        })
                    }
                    _ => {
                        let expr = ast::Expr::parse(tokens);

                        let semi = tokens.next();
                        match semi.0 {
                            tokens::TokenType::Semi => {}
                            _ => panic!("; expected"),
                        }

                        Some(ast::ForInitializer::Expression(expr))
                    }
                };

                let condition = match tokens.peek().0 {
                    tokens::TokenType::Semi => {
                        tokens.advance();
                        None
                    }
                    _ => {
                        let expr = ast::Expr::parse(tokens);
                        match tokens.next().0 {
                            tokens::TokenType::Semi => {}
                            _ => panic!("; expected"),
                        };
                        Some(expr)
                    }
                };

                let step = match tokens.peek().0 {
                    tokens::TokenType::RightParen => {
                        tokens.advance();
                        None
                    }
                    _ => {
                        let expr = ast::Expr::parse(tokens);
                        match tokens.next().0 {
                            tokens::TokenType::RightParen => {}
                            _ => panic!(") expected"),
                        }
                        Some(expr)
                    }
                };

                Self::ForLoop {
                    initializer,
                    condition,
                    step,
                    body: Box::new(ast::Statement::parse(tokens)),
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::While) => {
                match tokens.next().0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let condition = ast::Expr::parse(tokens);

                match tokens.next().0 {
                    tokens::TokenType::RightParen => {}
                    _ => panic!(") expected"),
                }

                Self::WhileLoop {
                    condition,
                    body: Box::new(ast::Statement::parse(tokens)),
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::Return) => {
                let return_value = match tokens.peek().0 {
                    tokens::TokenType::Semi => {
                        tokens.advance();
                        None
                    }
                    _ => {
                        let expr = ast::Expr::parse(tokens);
                        match tokens.next().0 {
                            tokens::TokenType::Semi => {}
                            _ => panic!("; expected"),
                        }

                        Some(expr)
                    }
                };

                Self::ReturnStatement(return_value)
            }
            tokens::TokenType::LeftBrace => {
                let mut body = Vec::new();

                loop {
                    match tokens.peek().0 {
                        tokens::TokenType::RightBrace => {
                            tokens.advance();
                            break;
                        }
                        _ => {
                            body.push(ast::Declaration::parse(tokens));
                        }
                    }
                }

                Self::BlockStatement { body }
            }
            _ => {
                tokens.0.set(original_tokens);
                let expr = ast::Expr::parse(tokens);

                match tokens.next().0 {
                    tokens::TokenType::Semi => {}
                    _ => panic!("; expected"),
                }

                Self::ExpressionStatement(expr)
            }
        }
    }
}

impl Parse<tokens::Token> for ast::Expr {
    type ParseResult = Self;

    fn parse(tokens: &ParseInput<'_, tokens::Token>) -> Self::ParseResult {
        fn simple_expr(tokens: &ParseInput<'_, tokens::Token>) -> ast::Expr {
            let raw_next @ types::Located(next, start_span) = tokens.next();

            match next {
                tokens::TokenType::LeftParen => {
                    let expr = ast::Expr::parse(tokens);

                    let semi = tokens.next();
                    let last_span = match semi {
                        types::Located(tokens::TokenType::RightParen, last_span) => last_span,
                        _ => panic!(") expected"),
                    };

                    ast::Expr::Parenthesized {
                        source_span: types::Span::merge(vec![start_span, last_span]),
                        expr: Box::new(expr),
                    }
                }
                tokens::TokenType::Minus => ast::Expr::PrefixExpression {
                    operator: types::Located(ast::PrefixOp::Negate, *start_span),
                    expr: Box::new(simple_expr(tokens)),
                },
                tokens::TokenType::Not => ast::Expr::PrefixExpression {
                    operator: types::Located(ast::PrefixOp::Not, *start_span),
                    expr: Box::new(simple_expr(tokens)),
                },
                tokens::TokenType::Literal(lit) => {
                    ast::Expr::Literal(types::Located(ast::Literal::Raw(lit.clone()), *start_span))
                }
                tokens::TokenType::Identifier(id) => {
                    ast::Expr::Identifier(types::Located(id.clone(), *start_span))
                }
                tokens::TokenType::Keyword(tokens::Keyword::Nil) => {
                    ast::Expr::Literal(types::Located(ast::Literal::Nil, *start_span))
                }
                tokens::TokenType::Keyword(tokens::Keyword::True) => {
                    ast::Expr::Literal(types::Located(ast::Literal::Boolean(true), *start_span))
                }
                tokens::TokenType::Keyword(tokens::Keyword::False) => {
                    ast::Expr::Literal(types::Located(ast::Literal::Boolean(false), *start_span))
                }
                _ => panic!("Unexpected token {raw_next}"),
            }
        }

        fn continuation<'a>(
            lhs: &mut ast::Expr,
            tokens: &ParseInput<'_, tokens::Token>,
        ) -> Option<(types::Located<ast::InfixOp>, ast::Expr)> {
            let mut previous_tokens;
            loop {
                previous_tokens = tokens.0.get();

                let types::Located(next, start_span) = tokens.next();

                let wrap_op =
                    |op: ast::InfixOp| Some((types::Located(op, *start_span), simple_expr(tokens)));

                break match next {
                    tokens::TokenType::LeftParen => {
                        let mut arguments = Vec::new();
                        let mut first = true;
                        let last_span;

                        loop {
                            match tokens.peek() {
                                types::Located(tokens::TokenType::RightParen, span) => {
                                    tokens.advance();
                                    last_span = span;
                                    break;
                                }
                                types::Located(tokens::TokenType::Comma, _) if first => {
                                    panic!("expr or ) expected")
                                }
                                types::Located(tokens::TokenType::Comma, _) => {
                                    tokens.advance();
                                }
                                _ => {} // any other token will be handled by the next expr check.
                            };
                            first = false;

                            arguments.push(ast::Expr::parse(tokens));
                        }

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs =
                            ast::Expr::Literal(types::Located(ast::Literal::Nil, *last_span));
                        core::mem::swap(lhs, &mut temp_lhs);
                        *lhs = ast::Expr::CallExpression {
                            callee: Box::new(temp_lhs),
                            // TODO Merge start and last span?
                            arguments,
                        };
                        continue;
                    }
                    tokens::TokenType::Dot => {
                        let field_name = types::Located::<types::Identifier>::parse(tokens);

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs =
                            ast::Expr::Literal(types::Located(ast::Literal::Nil, *start_span));
                        core::mem::swap(lhs, &mut temp_lhs);
                        *lhs = ast::Expr::PathExpression {
                            receiver: Box::new(temp_lhs),
                            field_name,
                        };
                        continue;
                    }
                    tokens::TokenType::Plus => wrap_op(ast::InfixOp::Plus),
                    tokens::TokenType::Minus => wrap_op(ast::InfixOp::Minus),
                    tokens::TokenType::Slash => wrap_op(ast::InfixOp::Divide),
                    tokens::TokenType::Star => wrap_op(ast::InfixOp::Multiply),
                    tokens::TokenType::DoubleEquals => wrap_op(ast::InfixOp::Equals),
                    tokens::TokenType::NotEquals => wrap_op(ast::InfixOp::NotEquals),
                    tokens::TokenType::GreaterThan => wrap_op(ast::InfixOp::GreaterThan),
                    tokens::TokenType::GreaterThanEqual => wrap_op(ast::InfixOp::GreaterThanEqual),
                    tokens::TokenType::LessThan => wrap_op(ast::InfixOp::LessThan),
                    tokens::TokenType::LessThanEqual => wrap_op(ast::InfixOp::LessThanEqual),
                    tokens::TokenType::Assign => wrap_op(ast::InfixOp::Assign),
                    _ => {
                        tokens.0.set(previous_tokens);
                        None
                    }
                };
            }
        }

        fn rebalance(
            mut lhs: ast::Expr,
            mut first_op: types::Located<ast::InfixOp>,
            mut first_rhs: ast::Expr,
            continuations: &mut std::vec::Drain<'_, (types::Located<ast::InfixOp>, ast::Expr)>,
        ) -> (ast::Expr, types::Located<ast::InfixOp>, ast::Expr, bool) {
            let mut anything_happened = false;
            loop {
                let (mut next_op, mut next_rhs) = match continuations.next() {
                    None => return (lhs, first_op, first_rhs, anything_happened),
                    Some((op, rhs)) => (op, rhs),
                };
                anything_happened = true;
                loop {
                    let merge_lhs = match (first_op.0.precedence(), next_op.0.precedence()) {
                        (ast::Precedence::Left(prec_l), ast::Precedence::Left(prec_r)) => {
                            prec_l >= prec_r
                        }
                        (ast::Precedence::Right(prec_l), ast::Precedence::Right(prec_r)) => {
                            prec_l > prec_r
                        }
                        (ast::Precedence::Left(prec_l), ast::Precedence::Right(prec_r))
                        | (ast::Precedence::Right(prec_l), ast::Precedence::Left(prec_r)) => {
                            if prec_l == prec_r {
                                panic!(
                                    "Cannot mix operators with the same precedence but different associativity!"
                                )
                            }
                            prec_l > prec_r
                        }
                    };

                    if merge_lhs {
                        lhs = ast::Expr::InfixOperation {
                            operator: core::mem::replace(&mut first_op, next_op),
                            lhs: Box::new(lhs),
                            rhs: Box::new(core::mem::replace(&mut first_rhs, next_rhs)),
                        };
                        break;
                    }

                    let inner_happen;
                    (first_rhs, next_op, next_rhs, inner_happen) =
                        rebalance(first_rhs, next_op, next_rhs, continuations);
                    if !inner_happen {
                        // If nothing happened inside, we're at the end - just merge.
                        first_rhs = ast::Expr::InfixOperation {
                            operator: next_op,
                            lhs: Box::new(first_rhs),
                            rhs: Box::new(next_rhs),
                        };
                        break;
                    }
                }
            }
        }

        let mut lhs = simple_expr(tokens);

        // TODO Right now, this allocates the Vec for all continuations,
        // TODO but it would be nice to try if I can do this in a Streaming fashion
        // TODO with a custom iterator instead...
        let mut continuations = Vec::new();
        let mut lhs_ref = &mut lhs;
        loop {
            match continuation(lhs_ref, tokens) {
                Some((op, new_rhs)) => {
                    continuations.push((op, new_rhs));
                    lhs_ref = &mut continuations.last_mut().unwrap().1;
                }
                None => break,
            }
        }

        let mut continuations = continuations.drain(..);
        let (lhs, op, rhs, _) = match continuations.next() {
            Some((op, rhs)) => rebalance(lhs, op, rhs, &mut continuations),
            None => return lhs,
        };

        ast::Expr::InfixOperation {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

impl Parse<tokens::Token> for types::Located<types::Identifier> {
    type ParseResult = Self;

    fn parse(tokens: &ParseInput<'_, tokens::Token>) -> Self::ParseResult {
        let types::Located(next, loc) = tokens.next();

        match next {
            tokens::TokenType::Identifier(id) => types::Located(id.clone(), *loc),
            _ => panic!("Expected identifier"),
        }
    }
}
