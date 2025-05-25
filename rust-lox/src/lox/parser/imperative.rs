//! An imperative parser that roughly follows the same LR(1)
//! structure of the book's parser implementation.
#![expect(
    clippy::wildcard_enum_match_arm,
    reason = "Default cases are the only way to keep parsers sane"
)]
#![expect(
    clippy::pattern_type_mismatch,
    reason = "Enabling this would require constant & and ref placements that \
    do not help the parsing at all."
)]
#![expect(
    clippy::missing_docs_in_private_items,
    clippy::exhaustive_enums,
    reason = "Temporary"
)]

use crate::lox::ast;
use crate::lox::token::tokens;
use crate::lox::types;

// TODO Fault-tolerance.

pub struct ParseInput<'a, Token>(core::cell::Cell<&'a [Token]>);

#[derive(Debug)]
pub enum ParseError<'a, Token> {
    EndOfInput,
    UnexpectedToken {
        expected: &'a str,
        actual: &'a Token,
    },
    GenericError {
        message: String,
    },
}

type Result<'a, T, Token> = core::result::Result<T, ParseError<'a, Token>>;

impl<'a, Token> ParseInput<'a, Token> {
    pub fn new<T>(token: &'a T) -> Self
    where
        T: core::ops::Deref<Target = [Token]>,
    {
        ParseInput(core::cell::Cell::new(token))
    }

    // TODO Backup, Restore.

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.get().is_empty()
    }

    /// # Errors
    ///
    /// Returns an Error if we are at the end of input.
    #[inline]
    pub fn next(&self) -> Result<'a, &'a Token, Token> {
        let (next, rest) = self.0.get().split_first().ok_or(ParseError::EndOfInput)?;
        self.0.set(rest);
        Ok(next)
    }

    /// Consumes a required token. Do not use this to check for optional tokens.
    ///
    /// # Errors
    ///
    /// Returns an Error if we are at the end of input,
    /// or an unexpected token was found.
    #[inline]
    pub fn consume<U>(&self, expected: U, expected_msg: &'a str) -> Result<'a, &'a Token, Token>
    where
        Token: PartialEq<U>,
    {
        let next = self.next()?;
        if *next == expected {
            drop(expected);
            Ok(next)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: expected_msg,
                actual: next,
            })
        }
    }

    /// # Errors
    ///
    /// Returns an Error if we are at the end of input.
    #[inline]
    pub fn peek(&self) -> Result<'a, &'a Token, Token> {
        self.0.get().first().ok_or(ParseError::EndOfInput)
    }

    /// # Errors
    ///
    /// Returns an Error if we are at the end of input.
    #[inline]
    pub fn advance(&self) -> Result<'a, (), Token> {
        let (_, rest) = self.0.get().split_first().ok_or(ParseError::EndOfInput)?;
        self.0.set(rest);
        Ok(())
    }
}

pub trait Parse<Token>: Sized {
    type ParseResult;

    /// # Errors
    ///
    /// - End of Input was reached before parse was finished.
    /// - An unexpected token was encountered.
    /// - An error occurred while parsing that could not be resolved.
    fn parse<'a>(tokens: &ParseInput<'a, Token>) -> Result<'a, Self::ParseResult, Token>;

    // TODO: fn recover(tokens: &ParseInput<'_, Token>) -> bool;
    // Return value of true means "I am recovered, you can now continue".
    // Return value of false means "I am not able to recover."
    // If recovery can FAIL, you should not offer recovery. The higher level parser should handle it.
}

impl Parse<tokens::Token> for ast::Program {
    type ParseResult = Self;

    fn parse<'a>(
        tokens: &ParseInput<'a, tokens::Token>,
    ) -> Result<'a, Self::ParseResult, tokens::Token> {
        let mut declarations = Vec::new();
        // TODO: EndOfInput should be removed as a fucking token it just makes problems...
        while !tokens.is_empty() && !matches!(tokens.peek()?.0, tokens::TokenType::EndOfInput) {
            declarations.push(ast::Declaration::parse(tokens)?);
        }
        Ok(ast::Program { declarations })
    }
}

impl Parse<tokens::Token> for ast::Declaration {
    type ParseResult = Self;

    fn parse<'a>(
        tokens: &ParseInput<'a, tokens::Token>,
    ) -> Result<'a, Self::ParseResult, tokens::Token> {
        let original_tokens = tokens.0.get();

        let types::Located(next, start_span) = tokens.next()?;

        Ok(match next {
            tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                let (id, value) = parse_var_declaration(tokens)?;

                Self::VariableDeclaration {
                    assignee: id,
                    value,
                }
            }
            tokens::TokenType::Keyword(tokens::Keyword::Class) => Self::ClassDeclaration {
                name: types::Located::parse(tokens)?,
                funcs: {
                    tokens.consume(tokens::TokenType::LeftBrace, "{")?;
                    let mut funcs = Vec::new();
                    loop {
                        let next = tokens.next()?;
                        match next.0 {
                            tokens::TokenType::RightBrace => {
                                break funcs;
                            }
                            tokens::TokenType::Identifier(ref id) => {
                                funcs.push(parse_function_parts(
                                    next.1,
                                    types::Located(id.clone(), next.1),
                                    tokens,
                                )?);
                            }
                            _ => {
                                return Err(ParseError::UnexpectedToken {
                                    expected: "method declaration or closing }",
                                    actual: next,
                                });
                            }
                        }
                    }
                },
            },
            tokens::TokenType::Keyword(tokens::Keyword::Fun) => Self::FunctionDeclaration(
                parse_function_parts(*start_span, types::Located::parse(tokens)?, tokens)?,
            ),
            _ => {
                tokens.0.set(original_tokens);
                Self::Statement(ast::Statement::parse(tokens)?)
            }
        })
    }
}

fn parse_var_declaration<'a>(
    tokens: &ParseInput<'a, tokens::Token>,
) -> Result<'a, (types::Located<types::Identifier>, Option<ast::Expr>), tokens::Token> {
    let id = types::Located::parse(tokens)?;
    let value = match tokens.peek()? {
        types::Located(tokens::TokenType::Semi, _) => None,
        types::Located(tokens::TokenType::Assign, _) => {
            tokens.advance()?;
            Some(ast::Expr::parse(tokens)?)
        }
        other => {
            return Err(ParseError::UnexpectedToken {
                expected: "; or =",
                actual: other,
            });
        }
    };
    tokens.consume(tokens::TokenType::Semi, ";")?;

    Ok((id, value))
}

fn parse_function_parts<'a>(
    start_span: types::Span,
    name: types::Located<types::Identifier>,
    tokens: &ParseInput<'a, tokens::Token>,
) -> Result<'a, ast::FunctionDeclaration, tokens::Token> {
    tokens.consume(tokens::TokenType::LeftParen, "(")?;

    let mut first = true;
    let parameters = {
        let mut parameters = Vec::new();
        loop {
            let next = tokens.peek()?;
            match next.0 {
                tokens::TokenType::RightParen => {
                    tokens.advance()?;
                    break parameters;
                }
                tokens::TokenType::Comma if first => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "identifier or )",
                        actual: next,
                    });
                }
                tokens::TokenType::Comma => tokens.advance()?,
                _ => {} // will be consumed by identifier parsing
            }
            first = false;

            parameters.push(types::Located::parse(tokens)?);
        }
    };
    tokens.consume(tokens::TokenType::LeftBrace, "{")?;

    let mut body = Vec::new();
    let last_span = loop {
        if let types::Located(tokens::TokenType::RightBrace, last_loc) = tokens.peek()? {
            tokens.advance()?;
            break last_loc;
        }

        body.push(ast::Declaration::parse(tokens)?);
    };

    Ok(ast::FunctionDeclaration {
        span: types::Span::merge(&vec![start_span, *last_span]),
        name,
        parameters,
        body,
    })
}

impl Parse<tokens::Token> for ast::Statement {
    type ParseResult = Self;

    #[expect(
        clippy::too_many_lines,
        reason = "Making this smaller does not improve readability."
    )]
    fn parse<'a>(
        tokens: &ParseInput<'a, tokens::Token>,
    ) -> Result<'a, Self::ParseResult, tokens::Token> {
        let original_tokens = tokens.0.get();
        let types::Located(next, kw_loc) = tokens.next()?;

        Ok(match next {
            tokens::TokenType::Keyword(tokens::Keyword::Print) => Self::PrintStatement {
                print_span: *kw_loc,
                printed_expr: {
                    let expr = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::Semi, ";")?;
                    expr
                },
            },
            tokens::TokenType::Keyword(tokens::Keyword::If) => Self::IfStatement {
                condition: {
                    tokens.consume(tokens::TokenType::LeftParen, "(")?;
                    let condition = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::RightParen, ")")?;
                    condition
                },
                then_branch: Box::new(ast::Statement::parse(tokens)?),
                else_branch: match tokens.peek()?.0 {
                    tokens::TokenType::Keyword(tokens::Keyword::Else) => {
                        tokens.advance()?;
                        Some(Box::new(ast::Statement::parse(tokens)?))
                    }
                    // TODO EndOfInput would be valid here! And should not advance!
                    _ => None,
                },
            },
            tokens::TokenType::Keyword(tokens::Keyword::For) => Self::ForLoop {
                initializer: {
                    tokens.consume(tokens::TokenType::LeftParen, "(")?;
                    match tokens.peek()?.0 {
                        tokens::TokenType::Semi => {
                            tokens.advance()?;
                            None
                        }
                        tokens::TokenType::Keyword(tokens::Keyword::Var) => {
                            tokens.advance()?;
                            let (id, value) = parse_var_declaration(tokens)?;

                            Some(ast::ForInitializer::VariableDeclaration {
                                assignee: id,
                                value,
                            })
                        }
                        _ => {
                            let expr = ast::Expr::parse(tokens)?;
                            tokens.consume(tokens::TokenType::Semi, ";")?;

                            Some(ast::ForInitializer::Expression(expr))
                        }
                    }
                },
                condition: if let tokens::TokenType::Semi = tokens.peek()?.0 {
                    tokens.advance()?;
                    None
                } else {
                    let expr = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::Semi, ";")?;
                    Some(expr)
                },
                step: if let tokens::TokenType::RightParen = tokens.peek()?.0 {
                    tokens.advance()?;
                    None
                } else {
                    let expr = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::RightParen, ")")?;
                    Some(expr)
                },
                body: Box::new(ast::Statement::parse(tokens)?),
            },
            tokens::TokenType::Keyword(tokens::Keyword::While) => Self::WhileLoop {
                condition: {
                    tokens.consume(tokens::TokenType::LeftParen, "(")?;
                    let condition = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::RightParen, ")")?;
                    condition
                },
                body: Box::new(ast::Statement::parse(tokens)?),
            },
            tokens::TokenType::Keyword(tokens::Keyword::Return) => Self::ReturnStatement {
                return_value: if let tokens::TokenType::Semi = tokens.peek()?.0 {
                    tokens.advance()?;
                    None
                } else {
                    let expr = ast::Expr::parse(tokens)?;
                    tokens.consume(tokens::TokenType::Semi, ";")?;
                    Some(expr)
                },
            },
            tokens::TokenType::LeftBrace => Self::BlockStatement {
                body: {
                    let mut body = Vec::new();
                    loop {
                        match tokens.peek()?.0 {
                            tokens::TokenType::RightBrace => {
                                tokens.advance()?;
                                break body;
                            }
                            _ => {
                                body.push(ast::Declaration::parse(tokens)?);
                            }
                        }
                    }
                },
            },
            _ => {
                tokens.0.set(original_tokens);
                let expr = ast::Expr::parse(tokens)?;
                tokens.consume(tokens::TokenType::Semi, ";")?;

                Self::ExpressionStatement(expr)
            }
        })
    }
}

impl Parse<tokens::Token> for ast::Expr {
    type ParseResult = Self;

    #[expect(
        clippy::too_many_lines,
        reason = "Making this smaller does not improve readability."
    )]
    #[expect(
        clippy::unwrap_in_result,
        reason = "The expect is guaranteed to not fail."
    )]
    fn parse<'a>(
        tokens: &ParseInput<'a, tokens::Token>,
    ) -> Result<'a, Self::ParseResult, tokens::Token> {
        fn simple_expr<'a>(
            tokens: &ParseInput<'a, tokens::Token>,
        ) -> Result<'a, ast::Expr, tokens::Token> {
            let raw_next @ types::Located(next, start_span) = tokens.next()?;

            Ok(match next {
                tokens::TokenType::LeftParen => {
                    let expr = ast::Expr::parse(tokens)?;
                    let last_span = tokens.consume(tokens::TokenType::RightParen, ")")?.1;

                    ast::Expr::Parenthesized {
                        source_span: types::Span::merge(vec![start_span, &last_span]),
                        expr: Box::new(expr),
                    }
                }
                tokens::TokenType::Minus => ast::Expr::PrefixExpression {
                    operator: start_span.locate(ast::PrefixOp::Negate),
                    expr: Box::new(simple_expr(tokens)?),
                },
                tokens::TokenType::Not => ast::Expr::PrefixExpression {
                    operator: start_span.locate(ast::PrefixOp::Not),
                    expr: Box::new(simple_expr(tokens)?),
                },
                tokens::TokenType::Literal(lit) => {
                    ast::Expr::Literal(start_span.locate(ast::Literal::Raw(lit.clone())))
                }
                tokens::TokenType::Identifier(id) => {
                    ast::Expr::Identifier(start_span.locate(id.clone()))
                }
                tokens::TokenType::Keyword(tokens::Keyword::Nil) => {
                    ast::Expr::Literal(start_span.locate(ast::Literal::Nil))
                }
                tokens::TokenType::Keyword(tokens::Keyword::True) => {
                    ast::Expr::Literal(start_span.locate(ast::Literal::Boolean(true)))
                }
                tokens::TokenType::Keyword(tokens::Keyword::False) => {
                    ast::Expr::Literal(start_span.locate(ast::Literal::Boolean(false)))
                }
                _ => {
                    return Err(ParseError::UnexpectedToken {
                        expected: "parenthesized expression, -, !, literal, identifier, nil, true, or false",
                        actual: raw_next,
                    });
                }
            })
        }

        fn continuation<'a>(
            lhs: &mut ast::Expr,
            tokens: &ParseInput<'a, tokens::Token>,
        ) -> Result<'a, Option<(types::Located<ast::InfixOp>, ast::Expr)>, tokens::Token> {
            let mut previous_tokens;
            loop {
                previous_tokens = tokens.0.get();

                let types::Located(next, start_span) = tokens.next()?;

                let wrap_op =
                    |op: ast::InfixOp| Ok(Some((start_span.locate(op), simple_expr(tokens)?)));

                break match next {
                    tokens::TokenType::LeftParen => {
                        let mut arguments = Vec::new();
                        let mut first = true;
                        let last_span;

                        loop {
                            let next = tokens.peek()?;
                            match next {
                                types::Located(tokens::TokenType::RightParen, span) => {
                                    tokens.advance()?;
                                    last_span = span;
                                    break;
                                }
                                types::Located(tokens::TokenType::Comma, _) if first => {
                                    return Err(ParseError::UnexpectedToken {
                                        expected: "identifier or )",
                                        actual: next,
                                    });
                                }
                                types::Located(tokens::TokenType::Comma, _) => {
                                    tokens.advance()?;
                                }
                                _ => {} // any other token will be handled by the next expr check.
                            }
                            first = false;

                            arguments.push(ast::Expr::parse(tokens)?);
                        }

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs = ast::Expr::Literal(last_span.locate(ast::Literal::Nil));
                        core::mem::swap(lhs, &mut temp_lhs);
                        *lhs = ast::Expr::CallExpression {
                            callee: Box::new(temp_lhs),
                            // TODO Merge start and last span?
                            arguments,
                        };
                        continue;
                    }
                    tokens::TokenType::Dot => {
                        let field_name = types::Located::parse(tokens)?;

                        // Temporary Expression, taking a random location as it is not relevant.
                        let mut temp_lhs = ast::Expr::Literal(start_span.locate(ast::Literal::Nil));
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
                        Ok(None)
                    }
                };
            }
        }

        // Continuation after rebalancing - lhs, an operator, and the first rhs.
        type RebalanceContinuation = (ast::Expr, types::Located<ast::InfixOp>, ast::Expr, bool);
        // Error of two incompatible operators
        type OpAssociativityError = (types::Located<ast::InfixOp>, types::Located<ast::InfixOp>);

        fn rebalance(
            mut lhs: ast::Expr,
            mut first_op: types::Located<ast::InfixOp>,
            mut first_rhs: ast::Expr,
            continuations: &mut alloc::vec::Drain<'_, (types::Located<ast::InfixOp>, ast::Expr)>,
        ) -> core::result::Result<RebalanceContinuation, OpAssociativityError> {
            let mut anything_happened = false;
            loop {
                let (mut next_op, mut next_rhs) = match continuations.next() {
                    None => return Ok((lhs, first_op, first_rhs, anything_happened)),
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
                                return Err((first_op, next_op));
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
                        rebalance(first_rhs, next_op, next_rhs, continuations)?;
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

        let mut lhs = simple_expr(tokens)?;

        // TODO Right now, this allocates the Vec for all continuations,
        // TODO but it would be nice to try if I can do this in a Streaming fashion
        // TODO with a custom iterator instead...
        let mut continuations = Vec::new();
        let mut lhs_ref = &mut lhs;
        while let Some(cont) = continuation(lhs_ref, tokens)? {
            continuations.push(cont);

            lhs_ref = &mut continuations
                .last_mut()
                .expect("We just inserted a value, it cannot be empty.")
                .1;
        }

        let mut continuations = continuations.drain(..);
        let (lhs, op, rhs, _) = match continuations.next() {
            Some((op, rhs)) =>
                rebalance(lhs, op, rhs, &mut continuations)
                    .map_err(|(left_op, right_op)| ParseError::GenericError {
                        message: format!("{left_op:?} and {right_op:?} are not compatible - different associativities with the same precedence cannot appear ungrouped.")
                    })?,
            None => return Ok(lhs),
        };

        Ok(ast::Expr::InfixOperation {
            operator: op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }
}

impl Parse<tokens::Token> for types::Located<types::Identifier> {
    type ParseResult = Self;

    fn parse<'a>(
        tokens: &ParseInput<'a, tokens::Token>,
    ) -> Result<'a, Self::ParseResult, tokens::Token> {
        let next = tokens.next()?;

        Ok(match next.0 {
            tokens::TokenType::Identifier(ref id) => next.1.locate(id.clone()),
            _ => {
                return Err(ParseError::UnexpectedToken {
                    expected: "identifier",
                    actual: next,
                });
            }
        })
    }
}
