//! The Lox language, split into submodules according to their functionality.
//! See the crate-level documentation for further information.

// Shared functionality
pub mod errors;
pub mod types;
pub mod ast;
pub mod std;
mod util;

// Specific Phases
pub mod token;
