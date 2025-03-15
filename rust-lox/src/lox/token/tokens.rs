//! Data types representing tokens available in the Lox language.
use core::fmt::{Display, Formatter};
use std::collections::HashMap;
use std::sync::LazyLock;

use crate::lox::types::{Identifier, LoxLiteral, Span};
use crate::lox::util::map;

/// Keywords in the Lox language.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Keyword {
    // Constants
    /// `"nil"`
    Nil,
    /// `"false"`
    False,
    /// `"true"`
    True,

    // Logical Operators
    /// `"and"`
    And,
    /// `"or"`
    Or,

    // Control flow
    /// `"if"`
    If,
    /// `"else"`
    Else,
    /// `"for"`
    For,
    /// `"while"`
    While,
    /// `"return"`
    Return,

    // Declarations
    /// `"class"`
    Class,
    /// `"fun"`
    Fun,
    /// `"var"`
    Var,

    // Others
    /// `"print"`
    Print,
    /// `"super"`
    Super,
    /// `"this"`
    This,
}

impl Keyword {
    /// Extract the raw representation as it occurs in the source code.
    fn to_raw(self) -> &'static str {
        match self {
            Keyword::Nil => "nil",
            Keyword::False => "false",
            Keyword::True => "true",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::For => "for",
            Keyword::While => "while",
            Keyword::Return => "return",
            Keyword::Class => "class",
            Keyword::Fun => "fun",
            Keyword::Var => "var",
            Keyword::Print => "print",
            Keyword::Super => "super",
            Keyword::This => "this",
        }
    }
}

/// Lookup table for keywords to distinguish them from identifiers.
pub static KEYWORDS: LazyLock<HashMap<&'static str, Keyword>> = LazyLock::new(|| {
    map! {
        "nil"    => Keyword::Nil,
        "false"  => Keyword::False,
        "true"   => Keyword::True,

        "and"    => Keyword::And,
        "or"     => Keyword::Or,

        "if"     => Keyword::If,
        "else"   => Keyword::Else,
        "for"    => Keyword::For,
        "while"  => Keyword::While,
        "return" => Keyword::Return,

        "class"  => Keyword::Class,
        "fun"    => Keyword::Fun,
        "var"    => Keyword::Var,

        "print"  => Keyword::Print,
        "super"  => Keyword::Super,
        "this"   => Keyword::This,
    }
});

/// An enum covering all possible variations a token can take on.
#[derive(Debug, PartialEq)]
pub enum TokenType {
    // Grouping
    /// `"("`
    LeftParen,
    /// `")"`
    RightParen,
    /// `"{"`
    LeftBrace,
    /// `"}"`
    RightBrace,

    // Arith Operators
    /// `"+"`
    Plus,
    /// `"-"`
    Minus,
    /// `"/"`
    Slash,
    /// `"*"`
    Star,

    // Boolean Operators
    /// `"=="`
    DoubleEquals,
    /// `"!="`
    NotEquals,
    /// `">"`
    GreaterThan,
    /// `">="`
    GreaterThanEqual,
    /// `"<"`
    LessThan,
    /// `"<="`
    LessThanEqual,
    /// `"!"`
    Not,

    // Special Operators
    /// `","`
    Comma,
    /// `"."`
    Dot,
    /// `";"`
    Semi,
    /// `"="`
    Assign,

    // Literals
    /// A literal in the source code.
    Literal(LoxLiteral),

    // Identifiers and Keywords
    /// A custom identifier
    Identifier(Identifier),
    /// A specific keyword
    Keyword(Keyword),

    /// End of Input, either end of line in REPL mode, or End of File in normal mode.
    EndOfInput,
}

impl TokenType {
    /// Extract the raw representation as it occurs in the source code.
    /// Most tokens only have a singular possible representation, so they
    /// do not have to store anything by themselves.
    fn to_raw(&self) -> &str {
        match *self {
            TokenType::LeftParen => "(",
            TokenType::RightParen => ")",
            TokenType::LeftBrace => "{",
            TokenType::RightBrace => "}",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::DoubleEquals => "==",
            TokenType::NotEquals => "!=",
            TokenType::GreaterThan => ">",
            TokenType::GreaterThanEqual => ">=",
            TokenType::LessThan => "<",
            TokenType::LessThanEqual => "<=",
            TokenType::Not => "!",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::Semi => ";",
            TokenType::Assign => "=",
            TokenType::Literal(ref literal) => literal.to_raw(),
            TokenType::Identifier(Identifier(ref id)) => id.as_str(),
            TokenType::Keyword(kw) => kw.to_raw(),
            TokenType::EndOfInput => "",
        }
    }
}

/// A thin wrapper that bundles the token type with a source span.
#[derive(Debug, PartialEq)]
pub struct Token {
    /// Type of this token.
    pub token_type: TokenType,
    /// Span the token takes up in source code.
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "<{} @ {}>", self.token_type.to_raw(), self.span)
    }
}
