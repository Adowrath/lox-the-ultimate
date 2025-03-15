//! This is the Lexing or Tokenization module, split into two submodules.
//!
//! - [tokens] specifies the data types making up the tokens of the Lox language.
//! - [lexer] contains the code for tokenizing source code, alongside with the error
//!   definitions that can occur during this phase.
//!
//! This module mirrors, but doesn't directly follow, the [Scanning](https://craftinginterpreters.com/scanning.html)
//! chapter of the "Crafting Interpreters" book.
pub mod lexer;
pub mod tokens;
