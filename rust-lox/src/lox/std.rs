//! Helper functionality to make Rust-Lox optionally fully Standards-conformant

use crate::lox::token::tokens::{Token, TokenType};
use crate::lox::types::LoxLiteral;

pub trait LoxStdDisplay {
    fn std_display(&self) -> String;
}

impl LoxStdDisplay for Token {
    fn std_display(&self) -> String {
        let Token { ref token_type, .. } = *self;

        match *token_type {
            TokenType::LeftParen        => "LEFT_PAREN ( null".to_owned(),
            TokenType::RightParen       => "RIGHT_PAREN ) null".to_owned(),
            TokenType::LeftBrace        => "LEFT_BRACE { null".to_owned(),
            TokenType::RightBrace       => "RIGHT_BRACE } null".to_owned(),
            TokenType::Plus             => "PLUS + null".to_owned(),
            TokenType::Minus            => "MINUS - null".to_owned(),
            TokenType::Slash            => "SLASH / null".to_owned(),
            TokenType::Star             => "STAR * null".to_owned(),
            TokenType::DoubleEquals     => "EQUAL_EQUAL == null".to_owned(),
            TokenType::NotEquals        => "BANG_EQUAL != null".to_owned(),
            TokenType::GreaterThan      => "GREATER > null".to_owned(),
            TokenType::GreaterThanEqual => "GREATER_EQUAL >= null".to_owned(),
            TokenType::LessThan         => "LESS < null".to_owned(),
            TokenType::LessThanEqual    => "LESS_EQUAL <= null".to_owned(),
            TokenType::Not              => "BANG ! null".to_owned(),
            TokenType::Comma            => "COMMA , null".to_owned(),
            TokenType::Dot              => "DOT . null".to_owned(),
            TokenType::Semi             => "SEMICOLON ; null".to_owned(),
            TokenType::Assign           => "EQUAL = null".to_owned(),
            TokenType::EndOfInput       => "EOF  null".to_owned(),
            TokenType::Literal(ref lit) => match *lit {
                LoxLiteral::String { ref value, ref raw }
                    => format!("STRING {raw} {value}"),
                LoxLiteral::Number { ref value, ref raw }
                    => format!("NUMBER {raw} {value:.?}"),
            },
            TokenType::Keyword(ref kw)
                => format!("{} {} null", kw.to_raw().to_uppercase(), kw.to_raw()),
            TokenType::Identifier(ref id)
                => format!("IDENTIFIER {} null", id.0),
        }
    }
}
