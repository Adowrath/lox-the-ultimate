//! Encapsulates all behaviour necessary to properly lex Lox code.
//!
//! Note: Lexing is also commonly categorized as tokenizing. The term "lexing"
//! is used for the module in accordance with the Crafting Interpreters book,
//! yet the function (represented by the Scanner class in the book) is simply
//! named [tokenize].
//!
//! ```rust
//! let source = r#"
//!     var x = "foo" + 20;
//!     class
//! "#;
//! ```
#![allow(
    clippy::min_ident_chars,
    reason = "short names do not decrease readability here."
)]

use core::iter::Peekable;
use core::str::{Chars, FromStr};

use crate::lox::token::tokens::{Token, TokenType, KEYWORDS};
use crate::lox::types::{Identifier, Location, LoxLiteral, Span};

/// Errors that can happen during lexing.
#[derive(Debug)]
pub enum LexingError {
    /// Unknown symbol in the source code
    UnknownSymbol(Vec<char>, Span),
    /// A string was started but not terminated until the end of input/file
    UnterminatedString(Span),
    /// A comment was started but not terminated until the end of input/file
    UnterminatedComment(Span),
}

/// Tokenizes the given source code of Lox into either a [Vec] of [`Tokens`](Token),
/// or returns all [`LexingErrors`](LexingError) if any did occur.
/// Thus, you cannot proceed with the list of tokens if lexing had any errors.
pub fn tokenize<S: AsRef<str>>(source: S) -> Result<Vec<Token>, Vec<LexingError>> {
    let mut source = source.as_ref().chars().peekable();

    let mut line = 0usize;
    let mut col = 0usize;
    let mut errs: Vec<LexingError> = vec![];
    let mut tokens: Vec<Token> = vec![];
    loop {
        match next_token(&mut source, &mut line, &mut col) {
            Ok(Token {
                token_type: TokenType::EndOfInput,
                ..
            }) => break,

            Ok(token) => {
                tokens.push(token);
            }
            Err(err) => {
                errs.push(err);
            }
        }
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

/// Lexes the next token, advancing the source iterator appropriately
/// and also updates the line and col values.
///
/// In case of an error, the characters stay consumed, so that lexing can continue
/// past the error.
#[expect(
    clippy::too_many_lines,
    reason = "Splitting this function into multiple would massively hurt its simplicity."
)]
#[inline]
fn next_token(
    source: &mut Peekable<Chars<'_>>,
    line: &mut usize,
    col: &mut usize,
) -> Result<Token, LexingError> {
    loop {
        /// Generates a location value from the current line and column values.
        macro_rules! loc(() => {
            Location {
                line: *line,
                col: *col,
            }
        });
        let start_loc = loc!();
        /// Gets the next character, updating line and/or col values.
        macro_rules! next_char(() => {{
            match source.next() {
                None => None,
                Some(c) => {
                    #[expect(clippy::arithmetic_side_effects, reason = "If these ever overflow, you got bigger problems (also usize overflows safely).")]
                    if c == '\n' {
                        *col = 0;
                        *line += 1;
                    } else {
                        *col += 1;
                    }
                    Some(c)
                }
            }
        }});
        /// Gets the next character only if it matches the predicate.
        macro_rules! next_char_if(($test:expr) => {{
            match source.peek() {
                Some(c) if $test(*c) => next_char!(),
                _ => None,
            }
        }});
        /// Gets the next character only if it is exactly the supplied character.
        macro_rules! next_char_is(($expected:expr) => {{
            next_char_if!(|c| c == $expected).is_some()
        }});
        /// Peeks if the next character matches the predicate.
        macro_rules! peek_next_matches(($test:expr) => {{
            match source.peek() {
                Some(c) if $test(*c) => true,
                _ => false,
            }
        }});
        /// Peeks if the next character is equal to the given character.
        macro_rules! peek_next(($expected:expr) => {{
            peek_next_matches!(|c| c == $expected)
        }});
        /// Peeks if the character one beyond the next matches the predicate.
        macro_rules! peek_two_ahead_matches(($test:expr) => {{
            match source.clone().nth(1) {
                Some(c) if $test(c) => true,
                _ => false,
            }
        }});
        /// Peeks if the character one beyond the next is equal to the given character.
        #[expect(unused_macros, reason = "parity with peek_next")]
        macro_rules! peek_two_ahead(($expected:expr) => {{
            peek_two_ahead_matches!(|c| c == $expected)
        }});
        /// Emits a token of the given type, with a span from
        /// the start to the current position.
        macro_rules! emit_token(($token_type:expr) => {{
            Ok(Token {
                token_type: $token_type,
                span: Span::from(start_loc, loc!())
            })
        }});

        #[expect(
            clippy::redundant_else,
            reason = "False positive in clippy::pedantic - if doesn't always exit the loop."
        )]
        if let Some(char) = next_char!() {
            match char {
                // Grouping
                '(' => break emit_token!(TokenType::LeftParen),
                ')' => break emit_token!(TokenType::RightParen),
                '{' => break emit_token!(TokenType::LeftBrace),
                '}' => break emit_token!(TokenType::RightBrace),

                // Arith Operators
                '+' => break emit_token!(TokenType::Plus),
                '-' => break emit_token!(TokenType::Minus),
                '*' => break emit_token!(TokenType::Star),
                '/' if !peek_next!('/')
                    && !peek_next!('*') => break emit_token!(TokenType::Slash),

                // Boolean Operators
                '=' if next_char_is!('=') => break emit_token!(TokenType::DoubleEquals),
                '!' if next_char_is!('=') => break emit_token!(TokenType::NotEquals),
                '!'                       => break emit_token!(TokenType::Not),
                '>' if next_char_is!('=') => break emit_token!(TokenType::GreaterThanEqual),
                '>'                       => break emit_token!(TokenType::GreaterThan),
                '<' if next_char_is!('=') => break emit_token!(TokenType::LessThanEqual),
                '<'                       => break emit_token!(TokenType::LessThan),

                // Special Operators
                ',' => break emit_token!(TokenType::Comma),
                '.' => break emit_token!(TokenType::Dot),
                ';' => break emit_token!(TokenType::Semi),
                '=' => break emit_token!(TokenType::Assign),

                // Literals
                '"' => {
                    let mut raw_string = vec!['"'];
                    let mut string = vec![];
                    // TODO Parsing.
                    while let Some(c) = next_char_if!(|c| c != '"') {
                        raw_string.push(c);
                        string.push(c);
                    }
                    if let Some('"') = next_char!() {
                        raw_string.push('"');
                        let raw_string = raw_string.iter().collect::<String>();
                        let string = string.iter().collect::<String>();
                        break emit_token!(TokenType::Literal(LoxLiteral::String {
                            value: string,
                            raw: raw_string,
                        }));
                    } else {
                        // Correctness: Some(x) where x != '"' cannot happen,
                        // as the while loop consumed such a character.
                        break Err(LexingError::UnterminatedString(Span::from(
                            start_loc,
                            loc!(),
                        )));
                    }
                }
                c if is_digit(c) => {
                    let mut num = vec![c];
                    while let Some(c) = next_char_if!(is_digit) {
                        num.push(c);
                    }
                    if peek_next!('.') && peek_two_ahead_matches!(is_digit) {
                        num.push(next_char!().unwrap());
                    }
                    while let Some(c) = next_char_if!(is_digit) {
                        num.push(c);
                    }
                    let num = num.into_iter().collect::<String>();
                    break emit_token!(TokenType::Literal(LoxLiteral::Number {
                        value: f64::from_str(&num)
                            .expect("if this fails, the parsing above failed already"),
                        raw: num,
                    }));
                }

                // Identifiers and Keywords
                c if is_alpha(c) => {
                    let mut ident: Vec<char> = vec![c];
                    while let Some(c) = next_char_if!(is_alpha_num) {
                        ident.push(c);
                    }
                    let ident = ident.into_iter().collect::<String>();
                    match KEYWORDS.get(ident.as_str()) {
                        Some(kw) => break emit_token!(TokenType::Keyword(*kw)),
                        None => break emit_token!(TokenType::Identifier(Identifier(ident))),
                    }
                }

                // Whitespace
                ' ' | '\t' | '\r' | '\n' => {} // just skip

                // Comments
                '/' if next_char_is!('/') => {
                    while next_char_if!(|c| c != '\n').is_some() {
                        // do nothing...
                    }
                    // continue to next try
                }
                '/' if next_char_is!('*') => {
                    let mut nesting = 1usize;
                    #[expect(
                        clippy::arithmetic_side_effects,
                        reason = "If nesting actually manages to overflow, you got bigger problems. (also, it's unsigned, so overflow is perfectly safe.)"
                    )]
                    if let Err(err) = loop {
                        match next_char!() {
                            None => {
                                break Err(LexingError::UnterminatedComment(Span::from(
                                    start_loc,
                                    loc!(),
                                )))
                            }
                            Some('/') if next_char_is!('*') => nesting += 1,
                            Some('*') if next_char_is!('/') => nesting -= 1,
                            _ => {}
                        }

                        if nesting == 0 {
                            break Ok(());
                        }
                    } {
                        break Err(err);
                    };
                }

                // ERROR
                _ => {
                    break Err(LexingError::UnknownSymbol(
                        vec![char],
                        Span::from(start_loc, loc!()),
                    ))
                }
            }
        } else {
            break emit_token!(TokenType::EndOfInput);
        }
    }
}

/// Is the character an ASCII digit?
#[inline]
fn is_digit(c: char) -> bool {
    c.is_ascii_digit()
}

/// Is the character in the ASCII alphabet?
#[inline]
fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || (c == '_')
}

/// Is the character an alphanumeric ASCII character?
#[inline]
fn is_alpha_num(c: char) -> bool {
    c.is_alphanumeric() || (c == '_')
}

#[cfg(test)]
mod test {
    use crate::lox::token::tokens::Keyword::*;
    use crate::lox::token::tokens::TokenType::{self, *};
    use crate::lox::types::Identifier;
    use crate::lox::types::LoxLiteral::*;

    fn tokenize_no_spans(source: impl AsRef<str>) -> Vec<TokenType> {
        super::tokenize(source)
            .expect("Tokenizing failed")
            .into_iter()
            .map(|token| token.token_type)
            .collect()
    }

    #[test]
    fn simple_tokenizations() {
        assert_eq!(
            tokenize_no_spans("var foo = 20"),
            vec![
                Keyword(Var),
                TokenType::Identifier(Identifier("foo".to_owned())),
                Assign,
                Literal(Number {
                    value: 20.0,
                    raw: "20".to_owned()
                })
            ]
        );
    }
}
