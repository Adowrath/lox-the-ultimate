//! Errors that can occur during the lifetime of the tool.
use std::io::Error as IOError;
use std::process::ExitCode;

use crate::lox::token::lexer;

/// An error that occurred inside the Lox engine.
/// This enum will be extended bit by bit as new phases
/// get added.
///
/// To support the [`std::process::Termination`] trait such that, just like
/// the Crafting Interpreters book, we can use exit codes as specified by
/// the [`<sysexits.h>`](https://man.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD+4.3-RELEASE&format=html)
/// header, a `From<EngineError> for ExitCode` implementation is provided.
///
/// This is done for parity, not for any actual useful reason - this header
/// attempts to define a standard, but it hasn't actually caught on much from what I could tell.
#[derive(Debug)]
#[non_exhaustive]
pub enum EngineError {
    /// An error in how the tool is called.
    UsageError(String),
    /// Errors that happened when trying to load the file
    /// or read the REPL line.
    FileError(IOError),
    /// Errors that happened during the lexing phase.
    LexingErrors(Vec<lexer::LexingError>),
}

impl EngineError {
    /// Turns the error into a String that can be printed to standard error.
    #[must_use]
    pub fn display_error(&self) -> String {
        match *self {
            EngineError::UsageError(ref prog_name) => format!("Usage: {prog_name} [script]"),
            EngineError::FileError(ref error) => format!("Error reading source file: {error}"),
            EngineError::LexingErrors(ref errs) => format!(
                "Errors when parsing: \n{}",
                errs.iter()
                    .map(|err| format!("{err:?}"))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
        }
    }
}

impl From<IOError> for EngineError {
    fn from(value: IOError) -> Self {
        // For fully correct exit codes, we would need to
        // inspect the ErrorKind of the IO Error and use
        // different constructors appropriately.
        EngineError::FileError(value)
    }
}

impl From<EngineError> for ExitCode {
    fn from(value: EngineError) -> Self {
        ExitCode::from(match value {
            EngineError::UsageError(_) => 64, // EX_USAGE
            // Technically, 66 only specifies missing or unreadable files
            // any other errors during I/O for both the file, and the REPL command,
            // should be presented as 74, EX_IOERR
            EngineError::FileError(_) => 66,    // EX_NOINPUT
            EngineError::LexingErrors(_) => 65, // EX_DATAERR
        })
    }
}

// Continuation helpers for the REPL

/// Whether an error occurred because the input was too short.
/// Such errors can be recoverable by allowing further input on the REPL.
pub trait UnterminatedError {
    /// Was this error caused by sudden end of input?
    fn is_unterminated(&self) -> bool;
}

impl UnterminatedError for EngineError {
    fn is_unterminated(&self) -> bool {
        #[expect(
            clippy::indexing_slicing,
            reason = "access is checked by length before"
        )]
        if let EngineError::LexingErrors(ref errs) = *self {
            errs.len() == 1 && errs[0].is_unterminated()
        } else {
            false
        }
    }
}

impl UnterminatedError for lexer::LexingError {
    fn is_unterminated(&self) -> bool {
        use lexer::LexingError;
        match *self {
            LexingError::UnknownSymbol(_, _) => false,
            LexingError::UnterminatedString(_) | LexingError::UnterminatedComment(_) => true,
        }
    }
}
