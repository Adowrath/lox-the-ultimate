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
use crate::lox::types::{Identifier, Location, LoxLiteral, Span};
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
    //let mut source = source.as_ref().chars().peekable();
    let source = source.as_ref();
    let mut tokenizer_input = TokenizerInput::new(source.chars());

    let mut errs: Vec<LexingError>;
    let mut tokens: Vec<Token>;
    #[cfg(nightly)]
    {
        let token_stream = next_token(tokenizer_input);
        (tokens, errs) = token_stream.partition_result();
    };
    #[cfg(not(nightly))]
    {
        errs = vec![];
        tokens = vec![];
        loop {
            match next_token(&mut tokenizer_input) {
                Ok(token @ Token {
                    token_type: TokenType::EndOfInput,
                    ..
                }) => {
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

/// Wrap in a generator block on the nightly feature set.
#[cfg(nightly)]
macro_rules! wrap_gen(($body:expr) => {{
    gen { $body }
}});
/// No-Op on stable.
#[cfg(not(nightly))]
macro_rules! wrap_gen(($body:expr) => {{
    $body
}});

/// Input to the tokenizer.
///
/// On Nightly, this only wraps the iterator for the source code,
/// as positions are fully tracked within the generator block itself.
#[cfg(nightly)]
#[repr(transparent)]
pub struct TokenizerInput<I: Iterator<Item = char>>(Peekable<I>);

/// Input to the tokenizer.
///
/// On Stable, this wraps the iterator for the source code,
/// as well as holding position information for repeated invocations.
#[cfg(not(nightly))]
pub struct TokenizerInput<I: Iterator<Item = char>>(
    Peekable<I>,
    #[cfg(not(feature = "fancy-errors"))]
    usize,
    #[cfg(not(feature = "fancy-errors"))]
    usize,
    #[cfg(feature = "fancy-errors")]
    miette::ByteOffset,
);

impl<I: Iterator<Item=char>> TokenizerInput<I>
{
    /// Create a new Tokenizer Input, taking ownership of the passed
    /// source code iterator.
    #[cfg(nightly)]
    pub fn new<IntoI: IntoIterator<IntoIter=I>>(source_code: IntoI) -> Self
    {
        Self(source_code.into_iter().peekable())
    }
    /// Create a new Tokenizer Input, with initial positions
    /// set to the start of the source code.
    #[cfg(not(nightly))]
    fn new<IntoI: IntoIterator<IntoIter=I>>(source_code: IntoI) -> Self
    where
        I: IntoIterator<Item=char>,
    {
        #[cfg(not(feature = "fancy-errors"))]
        return Self(
            source_code.into_iter().peekable(),
            usize::default(),
            usize::default(),
        );
        #[cfg(feature = "fancy-errors")]
        return Self(
            source_code.iter().peekable(),
            miette::ByteOffset::default(),
        );
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
pub fn next_token<I>(
    #[cfg(nightly)]
    tokenizer_input: TokenizerInput<I>,
    #[cfg(not(nightly))]
    tokenizer_input: &mut TokenizerInput<I>,
) -> tokenize_result!()
where
    I: Iterator<Item=char> + Clone,
{
    wrap_gen! {{
        #[cfg(nightly)]
        let TokenizerInput(mut source) = tokenizer_input;
        #[cfg(all(nightly, not(feature = "fancy-errors")))]
        let (mut line, mut col) = (0usize, 0usize);
        #[cfg(all(nightly, feature = "fancy-errors"))]
        let mut offset = miette::ByteOffset::default();

        #[cfg(all(not(nightly), not(feature = "fancy-errors")))]
        let TokenizerInput(source, line, col) = tokenizer_input;

        loop {
            /// No-Op on Nightly.
            #[cfg(nightly)]
            macro_rules! deref_pos(($val:expr) => { $val });
            /// Derefs the given positional value.
            #[cfg(not(nightly))]
            macro_rules! deref_pos(($val:expr) => { *$val });

            /// Generates a location value from the current line and column values.
            macro_rules! loc(() => {
                Location {
                    line: deref_pos!(line),
                    col: deref_pos!(col),
                }
            });
            let start_loc = loc!();
            /// Gets the next character, updating line and/or col values.
            macro_rules! next_char(() => {{
                match source.next() {
                    None => None,
                    Some(c) => {
                        #[expect(clippy::arithmetic_side_effects, reason = "If these ever overflow, you got bigger problems (also usize overflows safely).")]
                        #[cfg(feature = "fancy-errors")]
                        if c == '\n' {
                            deref_pos!(col) = 0;
                            deref_pos!(line) += 1;
                        } else {
                            deref_pos!(col) += 1;
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
            /// Emits an item, via yield on nightly.
            #[cfg(nightly)]
            macro_rules! emit(($value:expr) => {{
                yield $value
            }});
            /// Emits an item, via break on nightly.
            #[cfg(not(nightly))]
            macro_rules! emit(($value:expr) => {{
                break $value
            }});
            /// Emits a token of the given type, with a span from
            /// the start to the current position.
            macro_rules! emit_token(($token_type:expr) => {{
                emit!(Ok(Token {
                    token_type: $token_type,
                    span: Span::from(start_loc, loc!())
                }))
            }});
            /// Emits an error item.
            macro_rules! emit_error(($error_value:expr) => {{
                emit!(Err($error_value))
            }});

            #[cfg_attr(not(nightly), expect(
                clippy::redundant_else,
                reason = "False positive in clippy::pedantic - if doesn't always exit the loop."
            ))]
            if let Some(char) = next_char!() {
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
                    '/' if !peek_next!('/')
                        && !peek_next!('*') => emit_token!(TokenType::Slash),

                    // Boolean Operators
                    '=' if next_char_is!('=') => emit_token!(TokenType::DoubleEquals),
                    '!' if next_char_is!('=') => emit_token!(TokenType::NotEquals),
                    '!'                       => emit_token!(TokenType::Not),
                    '>' if next_char_is!('=') => emit_token!(TokenType::GreaterThanEqual),
                    '>'                       => emit_token!(TokenType::GreaterThan),
                    '<' if next_char_is!('=') => emit_token!(TokenType::LessThanEqual),
                    '<'                       => emit_token!(TokenType::LessThan),

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
                        while let Some(c) = next_char_if!(|c| c != '"') {
                            raw_string.push(c);
                            string.push(c);
                        }
                        if let Some('"') = next_char!() {
                            raw_string.push('"');
                            let raw_string = raw_string.iter().collect::<String>();
                            let string = string.iter().collect::<String>();
                            emit_token!(TokenType::Literal(LoxLiteral::String {
                                value: string,
                                raw: raw_string,
                            }));
                        } else {
                            // Correctness: Some(x) where x != '"' cannot happen,
                            // as the while loop consumed such a character.
                            emit_error!(LexingError::UnterminatedString(Span::from(
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
                        emit_token!(TokenType::Literal(LoxLiteral::Number {
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
                            Some(kw) => emit_token!(TokenType::Keyword(*kw)),
                            None => emit_token!(TokenType::Identifier(Identifier(ident))),
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
            } else {
                emit_token!(TokenType::EndOfInput);
                #[cfg(nightly)] break
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
