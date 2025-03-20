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

use crate::lox::token::tokens::{Token, TokenType, KEYWORDS};
use crate::lox::types::{Identifier, Located, Location, RawLiteral, Span};
use core::iter::Peekable;
use core::str::FromStr;
#[cfg(nightly)]
use itertools::Itertools;

/// Errors that can happen during lexing.
#[derive(Debug)]
#[expect(clippy::exhaustive_enums, reason = "lexing is a finished chapter")]
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
///
/// # Errors
///
/// - Unknown Symbols in the Lox Source code.
/// - Unterminated Strings or Block comments.
#[cfg_attr(nightly, expect(
    unused_mut,
    reason = "nightly uses generators and itertools instead of manual pushing"
))]
pub fn tokenize<S: AsRef<str>>(source: S) -> Result<Vec<Token>, Vec<LexingError>> {
    let source = source.as_ref();
    let mut tokenize_input = TokenizeInput::new(source.chars());

    let mut errs: Vec<LexingError>;
    let mut tokens: Vec<Token>;
    #[cfg(nightly)]
    {
        let token_stream = next_token(tokenize_input);
        (tokens, errs) = token_stream.partition_result();
    };
    #[cfg(not(nightly))]
    {
        errs = vec![];
        tokens = vec![];
        loop {
            match next_token(&mut tokenize_input) {
                Ok(token @ Located(
                    TokenType::EndOfInput,
                    ..
                )) => {
                    tokens.push(token);
                    break;
                }

                Ok(token) => {
                    tokens.push(token);
                }
                Err(err) => {
                    errs.push(err);
                }
            }
        }
    }

    if errs.is_empty() {
        Ok(tokens)
    } else {
        Err(errs)
    }
}

/// Input/State holder to the tokenizer, created from the source code.
pub struct TokenizeInput<I: Iterator> {
    /// The source iterator the tokenizing happens on,
    /// wrapped in a Peekable.
    source: Peekable<I>,

    /// The current line position in the source code.
    #[cfg(not(nightly))]
    line: usize,
    /// The current column position in the source code.
    #[cfg(not(nightly))]
    col: usize,
}

impl<I: Iterator<Item=char>> TokenizeInput<I> {
    /// Create a new tokenizer input from the given source code.
    ///
    /// The only restriction is that it needs to be convertible
    /// into an Iterator over characters.
    pub fn new<II>(source: II) -> Self
    where
        II: IntoIterator<IntoIter=I>,
    {
        Self {
            source: source.into_iter().peekable(),
            #[cfg(not(nightly))]
            line: 0,
            #[cfg(not(nightly))]
            col: 0,
        }
    }
}

/// Tokenizer Step Result, to support Generators on Nightly.
#[cfg(nightly)]
macro_rules! tokenize_result(() => {
    impl Iterator<Item = Result<Token, LexingError>>
});
/// Tokenizer Step Result, to support Generators on Nightly.
#[cfg(not(nightly))]
macro_rules! tokenize_result(() => {
    Result<Token, LexingError>
});

/// Wrap in a generator to properly scope the captures.
#[cfg(nightly)]
macro_rules! wrap_gen(($body:expr) => {{
    gen { $body }
}});
/// No-op on Stable versions.
#[cfg(not(nightly))]
macro_rules! wrap_gen(($body:expr) => {{
    $body
}});

/// Lexes the next token, advancing the source iterator appropriately
/// and also updates the line and col values.
///
/// In case of an error, the characters stay consumed, so that lexing can continue
/// past the error.
///
/// # Errors
///
/// The following errors are currently handled during the tokenization phase:
///
/// - Unexpected Symbols
/// - Unterminated Multi-line Comments
/// - Unterminated Strings
#[expect(
    clippy::too_many_lines,
    reason = "Splitting this function into multiple would massively hurt its simplicity."
)]
#[inline]
pub fn next_token<I: Iterator<Item=char> + Clone>(
    // The nightly version should accept the input by move, not by mutable reference.
    // It is expected that the generator fully drains the input.
    #[cfg(nightly)]
    tokenize_input: TokenizeInput<I>,
    // On stable, we need a mutable reference to it so that we can pass it in over
    // and over again for each next token.
    #[cfg(not(nightly))]
    tokenize_input: &mut TokenizeInput<I>,
) -> tokenize_result!() {
    wrap_gen! {{
        let TokenizeInput {
            ref mut source,
            #[cfg(not(nightly))]
            ref mut line,
            #[cfg(not(nightly))]
            ref mut col,
        } = *tokenize_input;

        #[cfg(nightly)]
        let (mut line, mut col) = (0usize, 0usize);

        #[expect(unused_macro_rules, reason = "parity rules")]
        loop {
            /// No-op on Nightly
            #[cfg(nightly)]
            macro_rules! deref(($val:expr) => { $val });
            /// No-op on Nightly
            #[cfg(not(nightly))]
            macro_rules! deref(($val:expr) => { *$val });
            /// Generates a location value from the current line and column values.
            macro_rules! loc(() => {
                Location {
                    line: deref!(line),
                    col: deref!(col),
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
                            deref!(col) = 0;
                            deref!(line) += 1;
                        } else {
                            deref!(col) += 1;
                        }
                        Some(c)
                    }
                }
            }});
            /// Gets the next character only if it matches the predicate.
            /// Pass either `== $expr`, `!= $expr`, or a predicate closure.
            /// First two return true/false, while the predicate version returns
            /// the character directly.
            macro_rules! next_char_is {
                ($test:expr) => {{
                    match source.peek() {
                        Some(c) if $test(*c) => next_char!(),
                        _ => None,
                    }
                }};
                (== $test:expr) => {{
                    next_char_is!(|c| c == $test).is_some()
                }};
                (!= $test:expr) => {{
                    next_char_is!(|c| c != $test).is_some()
                }}
            }
            /// Peeks if the next character matches the predicate.
            macro_rules! peek_next {
                ($test:expr) => {{
                    match source.peek() {
                        Some(c) if $test(*c) => true,
                        _ => false,
                    }
                }};
                (== $expected:expr) => {{
                    peek_next!(|c| c == $expected)
                }};
                (!= $expected:expr) => {{
                    peek_next!(|c| c != $expected)
                }}
            }
            /// Peeks if the character one beyond the next matches the predicate.
            macro_rules! peek_two_ahead {
                ($test:expr) => {{
                    match source.clone().nth(1) {
                        Some(c) if $test(c) => true,
                        _ => false,
                    }
                }};
                (== $test:expr) => {{
                    peek_two_ahead!(|c| c == $test)
                }};
                (!= $test:expr) => {{
                    peek_two_ahead!(|c| c == $test)
                }}
            }
            /// Emits an item, via yield on nightly.
            #[cfg(nightly)]
            macro_rules! emit(($value:expr) => {{
                yield $value;
            }});
            /// Emits an item, via break on stable.
            #[cfg(not(nightly))]
            macro_rules! emit(($value:expr) => {{
                break $value;
            }});
            /// Emits a token of the given type, with a span from
            /// the start to the current position.
            macro_rules! emit_token(($token_type:expr) => {{
                emit!(Ok(Located(
                    $token_type,
                    Span::from(start_loc, loc!())
                )))
            }});
            /// Emits an error item.
            macro_rules! emit_error(($error_value:expr) => {{
                emit!(Err($error_value))
            }});

            let Some(char) = next_char!() else {
                emit_token!(TokenType::EndOfInput);
                #[cfg(nightly)] break
            };

            match char {
                // Grouping
                '(' => emit_token!(TokenType::LeftParen),
                ')' => emit_token!(TokenType::RightParen),
                '{' => emit_token!(TokenType::LeftBrace),
                '}' => emit_token!(TokenType::RightBrace),

                // Arith Operators
                '+' => emit_token!(TokenType::Plus),
                '-' => emit_token!(TokenType::Minus),
                '*' => emit_token!(TokenType::Star),
                '/' if peek_next!(!= '/')
                    && peek_next!(!= '*') => emit_token!(TokenType::Slash),

                // Boolean Operators
                '=' if next_char_is!(== '=') => emit_token!(TokenType::DoubleEquals),
                '!' if next_char_is!(== '=') => emit_token!(TokenType::NotEquals),
                '!'                          => emit_token!(TokenType::Not),
                '>' if next_char_is!(== '=') => emit_token!(TokenType::GreaterThanEqual),
                '>'                          => emit_token!(TokenType::GreaterThan),
                '<' if next_char_is!(== '=') => emit_token!(TokenType::LessThanEqual),
                '<'                          => emit_token!(TokenType::LessThan),

                // Special Operators
                ',' => emit_token!(TokenType::Comma),
                '.' => emit_token!(TokenType::Dot),
                ';' => emit_token!(TokenType::Semi),
                '=' => emit_token!(TokenType::Assign),

                // Literals
                '"' => {
                    let mut raw_string = vec!['"'];
                    let mut string = vec![];
                    // TODO Parsing.
                    while let Some(c) = next_char_is!(|c| c != '"') {
                        raw_string.push(c);
                        string.push(c);
                    }
                    let Some('"') = next_char!() else {
                        // Correctness: Some(x) where x != '"' cannot happen,
                        // as the while loop consumed such a character.
                        emit_error!(LexingError::UnterminatedString(Span::from(
                            start_loc,
                            loc!(),
                        )));
                        #[cfg(nightly)] break
                    };
                    raw_string.push('"');
                    let raw_string = raw_string.iter().collect::<String>();
                    let string = string.iter().collect::<String>();
                    emit_token!(TokenType::Literal(RawLiteral::String {
                        value: string,
                        raw: raw_string,
                    }));
                }
                c if is_digit(c) => {
                    let mut num = vec![c];
                    while let Some(c) = next_char_is!(is_digit) {
                        num.push(c);
                    }
                    if peek_next!(== '.') && peek_two_ahead!(is_digit) {
                        num.push(next_char!().unwrap());
                    }
                    while let Some(c) = next_char_is!(is_digit) {
                        num.push(c);
                    }
                    let num = num.into_iter().collect::<String>();
                    emit_token!(TokenType::Literal(RawLiteral::Number {
                        // SAFETY: The parsing above has verified a valid (floating-point) number literal.
                        value: unsafe {
                            f64::from_str(&num).unwrap_unchecked()
                        },
                        raw: num,
                    }));
                }

                // Identifiers and Keywords
                c if is_alpha(c) => {
                    let mut ident: Vec<char> = vec![c];
                    while let Some(c) = next_char_is!(is_alpha_num) {
                        ident.push(c);
                    }
                    let ident = ident.into_iter().collect::<String>();
                    match KEYWORDS.get(ident.as_str()) {
                        Some(kw) => emit_token!(TokenType::Keyword(*kw)),
                        None => emit_token!(TokenType::Identifier(Identifier(ident))),
                    }
                }

                // Whitespace
                ' ' | '\t' | '\r' | '\n' => {} // just skip

                // Comments
                '/' if next_char_is!(== '/') => {
                    while next_char_is!(!= '\n') {
                        // do nothing...
                    }
                    // continue to next try
                }
                '/' if next_char_is!(== '*') => {
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
                            Some('/') if next_char_is!(== '*') => nesting += 1,
                            Some('*') if next_char_is!(== '/') => nesting -= 1,
                            _ => {}
                        }

                        if nesting == 0 {
                            break Ok(());
                        }
                    } {
                        emit_error!(err);
                    }
                }

                // ERROR
                _ => {
                    emit_error!(LexingError::UnknownSymbol(
                        vec![char],
                        Span::from(start_loc, loc!()),
                    ));
                }
            }
        }
    }}
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
    use crate::lox::types::RawLiteral::*;

    fn tokenize_no_spans(source: impl AsRef<str>) -> Vec<TokenType> {
        super::tokenize(source)
            .expect("Tokenizing failed")
            .into_iter()
            .map(|token| token.0)
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
                }),
                EndOfInput
            ]
        );
    }
}
