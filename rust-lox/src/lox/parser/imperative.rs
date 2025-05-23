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

use std::mem;
use crate::lox::ast;
use crate::lox::token::tokens;
use crate::lox::types;

/// TODO Fault-tolerance.

pub trait Parse<Token>: Sized {
    type ParseResult;

    fn parse(tokens: &[Token]) -> (Self::ParseResult, &[Token]);
}

impl Parse<tokens::Token> for ast::Program {
    type ParseResult = Self;

    fn parse(mut tokens: &[tokens::Token]) -> (Self::ParseResult, &[tokens::Token]) {
        let mut declarations = Vec::new();
        while !tokens.is_empty()
            && !matches!(tokens.first().unwrap().0, tokens::TokenType::EndOfInput)
        {
            let decl: ast::Declaration;
            (decl, tokens) = ast::Declaration::parse(tokens);
            declarations.push(decl);
        }
        (ast::Program { declarations }, tokens)
    }
}

impl Parse<tokens::Token> for ast::Declaration {
    type ParseResult = Self;

    fn parse(tokens: &[tokens::Token]) -> (Self::ParseResult, &[tokens::Token]) {
        let (mut next, mut next_tokens) = tokens.split_first().unwrap();

        match next.0 {
            tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                let id;
                (id, next_tokens) = types::Located::<types::Identifier>::parse(next_tokens);

                let mut value = None;
                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::Semi => {}
                    tokens::TokenType::Assign => {
                        let assign_value;
                        (assign_value, next_tokens) = ast::Expr::parse(next_tokens);
                        value = Some(assign_value)
                    }
                    _ => panic!("Invalid token after var declaration"),
                }

                (
                    Self::VariableDeclaration {
                        assignee: id,
                        value,
                    },
                    next_tokens,
                )
            }
            tokens::TokenType::Keyword(tokens::Keyword::Class) => {
                let name;
                (name, next_tokens) = types::Located::<types::Identifier>::parse(next_tokens);
                println!("Name: {name:?}");
                (next, next_tokens) = next_tokens.split_first().unwrap();
                println!("Next: {next:?}");
                match next.0 {
                    tokens::TokenType::LeftBrace => {}
                    _ => panic!("{{ expected"),
                }

                let mut funcs = Vec::new();
                loop {
                    (next, next_tokens) = next_tokens.split_first().unwrap();
                    match next.0 {
                        tokens::TokenType::RightBrace => {
                            break;
                        }
                        tokens::TokenType::Identifier(ref id) => {
                            let func_decl;
                            (func_decl, next_tokens) = parse_function_parts(
                                next.1,
                                types::Located(id.clone(), next.1),
                                next_tokens,
                            );

                            funcs.push(func_decl);
                        }
                        _ => panic!("Invalid token inside class declaration"),
                    }
                }

                (Self::ClassDeclaration { name, funcs }, next_tokens)
            }
            tokens::TokenType::Keyword(tokens::Keyword::Fun) => {
                let start_span = next.1;

                let id: types::Located<types::Identifier>;
                (id, next_tokens) = types::Located::<types::Identifier>::parse(next_tokens);

                let func_decl;
                (func_decl, next_tokens) = parse_function_parts(start_span, id, next_tokens);

                (Self::FunctionDeclaration(func_decl), next_tokens)
            }
            _ => {
                let statement;
                (statement, next_tokens) = ast::Statement::parse(tokens);

                println!("Parsed statement: {statement:?}, left: {next_tokens:?}");

                (Self::Statement(statement), next_tokens)
            }
        }
    }
}

fn parse_function_parts(
    start_span: types::Span,
    name: types::Located<types::Identifier>,
    tokens: &[tokens::Token],
) -> (ast::FunctionDeclaration, &[tokens::Token]) {
    let (mut next, mut next_tokens) = tokens.split_first().unwrap();
    match next.0 {
        tokens::TokenType::LeftParen => {}
        _ => panic!("( expected"),
    }

    let mut parameters = Vec::new();
    (next, next_tokens) = next_tokens.split_first().unwrap();
    if let tokens::TokenType::Identifier(ref id) = next.0 {
        parameters.push(types::Located(id.clone(), next.1));

        loop {
            (next, next_tokens) = next_tokens.split_first().unwrap();
            match next.0 {
                tokens::TokenType::RightParen => break,
                tokens::TokenType::Comma => {}
                _ => panic!(", or ) expected"),
            }

            (next, next_tokens) = next_tokens.split_first().unwrap();
            match next.0 {
                tokens::TokenType::Identifier(ref id) => {
                    parameters.push(types::Located(id.clone(), next.1));
                }
                _ => panic!("Expected identifier"),
            }
        }
    }

    (next, next_tokens) = next_tokens.split_first().unwrap();
    match next.0 {
        tokens::TokenType::LeftBrace => {}
        _ => panic!("{{ expected"),
    }

    let mut body = Vec::new();
    let last_span;
    loop {
        let last_new_next_tokens;
        (next, last_new_next_tokens) = next_tokens.split_first().unwrap();
        match next.0 {
            tokens::TokenType::RightBrace => {
                last_span = next.1;
                next_tokens = last_new_next_tokens;
                break;
            }
            _ => {} // New declaration.
        };

        let decl;
        (decl, next_tokens) = ast::Declaration::parse(next_tokens);
        body.push(decl);
    }

    (
        ast::FunctionDeclaration {
            span: types::Span::merge(&vec![start_span, last_span]),
            name,
            parameters,
            body,
        },
        next_tokens,
    )
}

impl Parse<tokens::Token> for ast::Statement {
    type ParseResult = Self;

    fn parse(tokens: &[tokens::Token]) -> (Self::ParseResult, &[tokens::Token]) {
        let (mut next, mut next_tokens) = tokens.split_first().unwrap();

        match next.0 {
            tokens::TokenType::Keyword(tokens::Keyword::Print) => {
                let expr;
                (expr, next_tokens) = ast::Expr::parse(next_tokens);

                let semi;
                (semi, next_tokens) = next_tokens.split_first().unwrap();
                match semi.0 {
                    tokens::TokenType::Semi => {}
                    _ => panic!("; expected"),
                }

                (
                    Self::PrintStatement {
                        print_span: next.1,
                        printed_expr: expr,
                    },
                    next_tokens,
                )
            }
            tokens::TokenType::Keyword(tokens::Keyword::If) => {
                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let condition;
                (condition, next_tokens) = ast::Expr::parse(next_tokens);

                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::RightParen => {}
                    _ => panic!(") expected"),
                }

                let then_branch;
                (then_branch, next_tokens) = ast::Statement::parse(next_tokens);

                let mut else_branch = None;
                let else_next_tokens;
                (next, else_next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::Keyword(tokens::Keyword::Else) => {
                        next_tokens = else_next_tokens;
                        let raw_else_branch;
                        (raw_else_branch, next_tokens) = ast::Statement::parse(next_tokens);
                        else_branch = Some(raw_else_branch);
                    }
                    _ => {} // no else found.
                }

                (
                    Self::IfStatement {
                        condition,
                        then_branch: Box::new(then_branch),
                        else_branch: else_branch.map(Box::new),
                    },
                    next_tokens,
                )
            }
            tokens::TokenType::Keyword(tokens::Keyword::For) => {
                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let initializer = {
                    let peek;
                    let peek_next_tokens;
                    (peek, peek_next_tokens) = next_tokens.split_first().unwrap();
                    match peek.0 {
                        tokens::TokenType::Semi => {
                            next_tokens = peek_next_tokens;
                            None
                        }
                        tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                            next_tokens = peek_next_tokens;

                            let id;
                            (id, next_tokens) =
                                types::Located::<types::Identifier>::parse(next_tokens);

                            let mut value = None;
                            (next, next_tokens) = next_tokens.split_first().unwrap();
                            match next.0 {
                                tokens::TokenType::Semi => {}
                                tokens::TokenType::Assign => {
                                    let assign_value;
                                    (assign_value, next_tokens) = ast::Expr::parse(next_tokens);
                                    value = Some(assign_value);

                                    (next, next_tokens) = next_tokens.split_first().unwrap();
                                    match next.0 {
                                        tokens::TokenType::Semi => {}
                                        _ => panic!("; expected"),
                                    }
                                }
                                _ => panic!("Invalid token after var declaration"),
                            }

                            Some(ast::ForInitializer::VariableDeclaration {
                                assignee: id,
                                value,
                            })
                        }
                        _ => {
                            let expr;
                            (expr, next_tokens) = ast::Expr::parse(next_tokens);
                            (next, next_tokens) = next_tokens.split_first().unwrap();
                            match next.0 {
                                tokens::TokenType::Semi => {}
                                _ => panic!("; expected"),
                            }
                            Some(ast::ForInitializer::Expression(expr))
                        }
                    }
                };

                let condition = {
                    let peek;
                    let peek_next_tokens;
                    (peek, peek_next_tokens) = next_tokens.split_first().unwrap();
                    match peek.0 {
                        tokens::TokenType::Semi => {
                            next_tokens = peek_next_tokens;
                            None
                        }
                        _ => {
                            let expr;
                            (expr, next_tokens) = ast::Expr::parse(next_tokens);
                            (next, next_tokens) = next_tokens.split_first().unwrap();
                            match next.0 {
                                tokens::TokenType::Semi => {}
                                _ => panic!("; expected"),
                            }
                            Some(expr)
                        }
                    }
                };

                let step = {
                    let peek;
                    let peek_next_tokens;
                    (peek, peek_next_tokens) = next_tokens.split_first().unwrap();
                    match peek.0 {
                        tokens::TokenType::RightParen => {
                            next_tokens = peek_next_tokens;
                            None
                        }
                        _ => {
                            let expr;
                            (expr, next_tokens) = ast::Expr::parse(next_tokens);
                            (next, next_tokens) = next_tokens.split_first().unwrap();
                            match next.0 {
                                tokens::TokenType::RightParen => {}
                                _ => panic!(") expected"),
                            }
                            Some(expr)
                        }
                    }
                };

                let body;
                (body, next_tokens) = ast::Statement::parse(next_tokens);

                (
                    Self::ForLoop {
                        initializer,
                        condition,
                        step,
                        body: Box::new(body),
                    },
                    next_tokens,
                )
            }
            tokens::TokenType::Keyword(tokens::Keyword::While) => {
                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::LeftParen => {}
                    _ => panic!("( expected"),
                }

                let condition;
                (condition, next_tokens) = ast::Expr::parse(next_tokens);

                (next, next_tokens) = next_tokens.split_first().unwrap();
                match next.0 {
                    tokens::TokenType::RightParen => {}
                    _ => panic!(") expected"),
                }

                let body;
                (body, next_tokens) = ast::Statement::parse(next_tokens);

                (
                    Self::WhileLoop {
                        condition,
                        body: Box::new(body),
                    },
                    next_tokens,
                )
            }
            tokens::TokenType::Keyword(tokens::Keyword::Return) => {
                let return_value = match next_tokens.split_first().unwrap() {
                    (types::Located(tokens::TokenType::Semi, _loc), rest) => {
                        next_tokens = rest;
                        None
                    }
                    _ => {
                        let expr;
                        (expr, next_tokens) = ast::Expr::parse(next_tokens);

                        (next, next_tokens) = next_tokens.split_first().unwrap();
                        match next.0 {
                            tokens::TokenType::Semi => {}
                            _ => panic!("; expected"),
                        }

                        Some(expr)
                    }
                };

                (Self::ReturnStatement(return_value), next_tokens)
            }
            tokens::TokenType::LeftBrace => {
                let mut body = Vec::new();

                loop {
                    let peek;
                    let peek_next_tokens;
                    (peek, peek_next_tokens) = next_tokens.split_first().unwrap();

                    match peek.0 {
                        tokens::TokenType::RightBrace => {
                            next_tokens = peek_next_tokens;
                            break;
                        }
                        _ => {
                            let decl;
                            (decl, next_tokens) = ast::Declaration::parse(next_tokens);
                            body.push(decl);
                        }
                    }
                }

                (Self::BlockStatement { body }, next_tokens)
            }
            _ => {
                let expr;
                (expr, next_tokens) = ast::Expr::parse(tokens);

                let semi;
                (semi, next_tokens) = next_tokens.split_first().unwrap();
                match semi.0 {
                    tokens::TokenType::Semi => {}
                    _ => panic!("; expected"),
                }

                (Self::ExpressionStatement(expr), next_tokens)
            }
        }
    }
}

impl Parse<tokens::Token> for ast::Expr {
    type ParseResult = Self;

    fn parse(tokens: &[tokens::Token]) -> (Self::ParseResult, &[tokens::Token]) {
        // This automatically takes care of the right-associativity
        // and the tight binding of prefix operators.
        fn prefix_expr(
            op: types::Located<ast::PrefixOp>,
            tokens: &[tokens::Token],
        ) -> (ast::Expr, &[tokens::Token]) {
            let next_tokens;
            let expr;
            (expr, next_tokens) = simple_expr(tokens);

            (
                ast::Expr::PrefixExpression {
                    operator: op,
                    expr: Box::new(expr),
                },
                next_tokens,
            )
        }

        fn simple_expr(tokens: &[tokens::Token]) -> (ast::Expr, &[tokens::Token]) {
            let (mut next, mut next_tokens) = tokens.split_first().unwrap();
            match next.0 {
                tokens::TokenType::LeftParen => {
                    let start_span = next.1;
                    let expr;
                    (expr, next_tokens) = ast::Expr::parse(next_tokens);

                    (next, next_tokens) = next_tokens.split_first().unwrap();
                    let last_span = match next.0 {
                        tokens::TokenType::RightParen => next.1,
                        _ => panic!(") expected"),
                    };

                    (
                        ast::Expr::Parenthesized {
                            source_span: types::Span::merge(&vec![start_span, last_span]),
                            expr: Box::new(expr),
                        },
                        next_tokens,
                    )
                }
                tokens::TokenType::Minus => {
                    prefix_expr(types::Located(ast::PrefixOp::Negate, next.1), next_tokens)
                }
                tokens::TokenType::Not => {
                    prefix_expr(types::Located(ast::PrefixOp::Not, next.1), next_tokens)
                }
                tokens::TokenType::Literal(ref lit) => (
                    ast::Expr::Literal(types::Located(ast::Literal::Raw(lit.clone()), next.1)),
                    next_tokens,
                ),
                tokens::TokenType::Identifier(ref id) => (
                    ast::Expr::Identifier(types::Located(id.clone(), next.1)),
                    next_tokens,
                ),

                tokens::TokenType::Keyword(tokens::Keyword::Nil) => (
                    ast::Expr::Literal(types::Located(ast::Literal::Nil, next.1)),
                    next_tokens,
                ),
                tokens::TokenType::Keyword(tokens::Keyword::True) => (
                    ast::Expr::Literal(types::Located(ast::Literal::Boolean(true), next.1)),
                    next_tokens,
                ),
                tokens::TokenType::Keyword(tokens::Keyword::False) => (
                    ast::Expr::Literal(types::Located(ast::Literal::Boolean(false), next.1)),
                    next_tokens,
                ),
                _ => panic!("Unexpected token {next}"),
            }
        }

        fn infix_continuation(
            op: types::Located<ast::InfixOp>,
            tokens: &[tokens::Token],
        ) -> (Option<(types::Located<ast::InfixOp>, ast::Expr)>, &[tokens::Token]) {
            let (expr, next_tokens) = simple_expr(tokens);

            (Some((op, expr)), next_tokens)
        }

        fn continuation<'a>(
            lhs: &mut ast::Expr,
            tokens: &'a [tokens::Token],
        ) -> (Option<(types::Located<ast::InfixOp>, ast::Expr)>, &'a [tokens::Token]) {
            let mut pre_split_tokens;
            let mut next;
            let mut next_tokens= tokens;

            loop {
                pre_split_tokens = next_tokens;
                (next, next_tokens) = pre_split_tokens.split_first().unwrap();

                break match next.0 {
                    tokens::TokenType::LeftParen => {
                        let mut arguments = Vec::new();
                        let mut first = true;

                        loop {
                            let peek_next_tokens;
                            (next, peek_next_tokens) = next_tokens.split_first().unwrap();
                            println!("Next token: {next:?}");
                            match next.0 {
                                tokens::TokenType::RightParen => {
                                    next_tokens = peek_next_tokens;
                                    break;
                                },
                                tokens::TokenType::Comma if first => panic!("expr or ) expected"),
                                tokens::TokenType::Comma => {
                                    next_tokens = peek_next_tokens;
                                },
                                _ => {}
                            };
                            first = false;

                            let arg;
                            (arg, next_tokens) = ast::Expr::parse(next_tokens);
                            arguments.push(arg);
                        }

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs = ast::Expr::Literal(types::Located(ast::Literal::Nil, next.1));
                        mem::swap(lhs, &mut temp_lhs);
                        *lhs = ast::Expr::CallExpression {
                            callee: Box::new(temp_lhs),
                            arguments,
                        };
                        continue;
                    }
                    tokens::TokenType::Dot => {
                        let field_name;
                        (field_name, next_tokens) = types::Located::<types::Identifier>::parse(next_tokens);

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs = ast::Expr::Literal(types::Located(ast::Literal::Nil, next.1));
                        mem::swap(lhs, &mut temp_lhs);
                        *lhs = ast::Expr::PathExpression {
                            receiver: Box::new(temp_lhs),
                            field_name,
                        };
                        continue;
                    }
                    tokens::TokenType::Plus => infix_continuation(types::Located(ast::InfixOp::Plus, next.1), next_tokens),
                    tokens::TokenType::Minus => infix_continuation(types::Located(ast::InfixOp::Minus, next.1), next_tokens),
                    tokens::TokenType::Slash => infix_continuation(types::Located(ast::InfixOp::Divide, next.1), next_tokens),
                    tokens::TokenType::Star => infix_continuation(types::Located(ast::InfixOp::Multiply, next.1), next_tokens),
                    tokens::TokenType::DoubleEquals => infix_continuation(types::Located(ast::InfixOp::Equals, next.1), next_tokens),
                    tokens::TokenType::NotEquals => infix_continuation(types::Located(ast::InfixOp::NotEquals, next.1), next_tokens),
                    tokens::TokenType::GreaterThan => infix_continuation(types::Located(ast::InfixOp::GreaterThan, next.1), next_tokens),
                    tokens::TokenType::GreaterThanEqual => infix_continuation(types::Located(ast::InfixOp::GreaterThanEqual, next.1), next_tokens),
                    tokens::TokenType::LessThan => infix_continuation(types::Located(ast::InfixOp::LessThan, next.1), next_tokens),
                    tokens::TokenType::LessThanEqual => infix_continuation(types::Located(ast::InfixOp::LessThanEqual, next.1), next_tokens),
                    tokens::TokenType::Assign => infix_continuation(types::Located(ast::InfixOp::Assign, next.1), next_tokens),

                    _ => (None, pre_split_tokens),
                }
            }
        }

        fn rebalance(mut lhs: ast::Expr,
                     mut first_op: types::Located<ast::InfixOp>,
                     mut first_rhs: ast::Expr,
                     continuations: &mut std::vec::Drain<'_, (types::Located<ast::InfixOp>, ast::Expr)>
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
                                panic!("Cannot mix operators with the same precedence but different associativity!")
                            }
                            prec_l > prec_r
                        }
                    };

                    if merge_lhs {
                        lhs = ast::Expr::InfixOperation {
                            operator: mem::replace(&mut first_op, next_op),
                            lhs: Box::new(lhs),
                            rhs: Box::new(mem::replace(&mut first_rhs, next_rhs)),
                        };
                        break;
                    }

                    let inner_happen;
                    (first_rhs, next_op, next_rhs, inner_happen) = rebalance(first_rhs, next_op, next_rhs, continuations);
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

        let (mut lhs, mut next_tokens) = simple_expr(tokens);

        let mut continuations = Vec::new();
        let mut lhs_ref = &mut lhs;
        loop {
            match continuation(lhs_ref, next_tokens) {
                (Some((op, new_rhs)), new_next_tokens) => {
                    continuations.push((op, new_rhs));
                    next_tokens = new_next_tokens;
                    lhs_ref = &mut continuations.last_mut().unwrap().1;
                }
                (None, new_next_tokens) => {
                    next_tokens = new_next_tokens;
                    break;
                }
            }
        }

        let mut continuations = continuations.drain(..);
        let (lhs, op, rhs, _) = match continuations.next() {
            Some((op, rhs)) => rebalance(lhs, op, rhs, &mut continuations),
            None => return (lhs, next_tokens),
        };

        (ast::Expr::InfixOperation {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }, next_tokens)
    }
}

impl Parse<tokens::Token> for types::Located<types::Identifier> {
    type ParseResult = Self;

    fn parse(mut tokens: &[tokens::Token]) -> (Self::ParseResult, &[tokens::Token]) {
        let next: &tokens::Token;
        (next, tokens) = tokens.split_first().unwrap();

        match next.0 {
            tokens::TokenType::Identifier(ref id) => (types::Located(id.clone(), next.1), tokens),
            _ => panic!("Expected identifier"),
        }
    }
}
