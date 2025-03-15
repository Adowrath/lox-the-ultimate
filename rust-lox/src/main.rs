//! # Rust-Lox - An implementation of Crafting Interpreters' Lox programming language
//!
//! The goal of this crate is to implement the entire functionality that is taught in
//! the book "Crafting Interpreters"[^1] by Robert Nystrom, from start to finish, while
//! adapting it to take advantage of various features Rust offers to aid in such tasks.
//!
//! ## Goals
//!
//! As "Crafting Interpreters" is split into piecewise parts, this code too attempts to
//! mimic the modular nature. Majorly, an attempt is made to introduce one submodule
//! per chapter which represents the functionality described in that chapter.
//! Once the second half of the book, implementing a virtual machine, has been reached,
//! the interpreter-specific parts will be moved into proper submodules as well.
//!
//! The implementation of Lox attempts to follow the book as closely as possible,
//! abstaining from extensions unless they are deemed to be useful enough, yet simple
//! as well. Notably, this means everything is implemented from first principles, and
//! as such, this crate has no functional dependencies other than on the standard library.
//!
//! ## Extensions
//!
//! - The [lexer](lexer::tokenize) has support for nestable multiline block comments.
//!   Single-line comments do not cancel an end-of-comment marker on the same line.
//! - There exists preliminary support for multi-line REPL input, though currently only
//!   by the lexer (as nothing else exists).
//!
//!   This is modeled by categorizing errors during lexing/parsing as to whether they
//!   are caused by unterminated input - if that is the case, the REPL can instead ask
//!   for a continuation line instead of raising the error.
//!
//!   This, however, does not allow for editing of any previous line, and additional
//!   support like the ability to cancel input via Ctrl-C is also not enabled currently.
//!
//! ## Aspirations
//!
//! I (the author) wishes to attempt a faithful implementation of the Lox language, in
//! the following components:
//!
//! - The Interpreter, as described by the book.
//! - The Bytecode-interpreting VM, as described by the book.
//!   - This should include a file format to dump the bytecode into.
//! - A compilation backend to a dynamic language (most likely JavaScript).
//! - (very aspirational) A LLVM/native backend?
//!
//! Aside from what is offered directly by the book, the following are also taken as a
//! guideline when implementing this:
//!
//! - `#![deny(warnings)]`, including most optional lints and also a lot from clippy.
//!   Circumventing these via `#[expect(...)]` should be taken as a last precaution,
//!   where the alternative would complicate or make the code less readable.
//! - Comprehensive documentation.
//! - Tests would be great to have.
//! - Possibly, make this project as close as possible to be `#![no_std]`-compliant.
//!   While definitely not a feasible goal for the entire project, this might be a
//!   useful restriction to challenge and foster learning.
//!
//! [^1]: <https://craftinginterpreters.com/>
#![deny(
    warnings,
)]
#![deny(
    future_incompatible,
    keyword_idents,
    let_underscore,
    nonstandard_style,
    refining_impl_trait,
)]
#![deny(
    rust_2018_compatibility,
    rust_2021_compatibility,
    rust_2024_compatibility,
)]
#![deny(
    clippy::all,
    clippy::pedantic,
)]
#![deny(
    clippy::absolute_paths,
    clippy::alloc_instead_of_core,
    clippy::allow_attributes,
    clippy::allow_attributes_without_reason,
    clippy::arithmetic_side_effects,
    clippy::as_conversions,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    clippy::big_endian_bytes,
    clippy::cfg_not_test,
    clippy::clone_on_ref_ptr,
    clippy::create_dir,
    clippy::dbg_macro,
    clippy::decimal_literal_representation,
    clippy::default_numeric_fallback,
    clippy::default_union_representation,
    clippy::deref_by_slicing,
    clippy::disallowed_script_idents,
    clippy::else_if_without_else,
    clippy::empty_drop,
    clippy::empty_enum_variants_with_brackets,
    clippy::empty_structs_with_brackets,
    clippy::error_impl_error,
    clippy::exhaustive_enums,
    clippy::exhaustive_structs,
    clippy::exit,
    clippy::field_scoped_visibility_modifiers,
    clippy::filetype_is_file,
    clippy::float_arithmetic,
    clippy::float_cmp_const,
    clippy::fn_to_numeric_cast_any,
    clippy::get_unwrap,
    clippy::host_endian_bytes,
    clippy::if_then_some_else_none,
    clippy::impl_trait_in_params,
    clippy::indexing_slicing,
    clippy::infinite_loop,
    clippy::inline_asm_x86_att_syntax,
    clippy::inline_asm_x86_intel_syntax,
    clippy::integer_division,
    clippy::integer_division_remainder_used,
    clippy::iter_over_hash_type,
    clippy::large_include_file,
    clippy::let_underscore_must_use,
    clippy::let_underscore_untyped,
    clippy::little_endian_bytes,
    clippy::lossy_float_literal,
    clippy::map_err_ignore,
    clippy::mem_forget,
    clippy::min_ident_chars,
    clippy::missing_assert_message,
    clippy::missing_asserts_for_indexing,
    clippy::missing_docs_in_private_items,
    clippy::missing_inline_in_public_items,
    clippy::missing_trait_methods,
    clippy::mixed_read_write_in_expression,
    clippy::module_name_repetitions,
    clippy::modulo_arithmetic,
    clippy::multiple_inherent_impl,
    clippy::multiple_unsafe_ops_per_block,
    clippy::mutex_atomic,
    clippy::mutex_integer,
    clippy::needless_raw_strings,
    clippy::non_ascii_literal,
    clippy::panic,
    clippy::panic_in_result_fn,
    clippy::partial_pub_fields,
    clippy::pathbuf_init_then_push,
    clippy::pattern_type_mismatch,
    clippy::pub_with_shorthand,
    clippy::pub_without_shorthand,
    clippy::rc_buffer,
    clippy::rc_mutex,
    clippy::redundant_type_annotations,
    clippy::renamed_function_params,
    clippy::rest_pat_in_fully_bound_structs,
    clippy::same_name_method,
    clippy::self_named_module_files,
    clippy::semicolon_inside_block,
    clippy::semicolon_outside_block,
    clippy::separated_literal_suffix,
    clippy::single_char_lifetime_names,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::str_to_string,
    clippy::string_add,
    clippy::string_lit_chars_any,
    clippy::string_slice,
    clippy::string_to_string,
    clippy::suspicious_xor_used_as_pow,
    clippy::tests_outside_test_module,
    clippy::todo,
    clippy::try_err,
    clippy::undocumented_unsafe_blocks,
    clippy::unimplemented,
    clippy::unnecessary_safety_comment,
    clippy::unnecessary_safety_doc,
    clippy::unnecessary_self_imports,
    clippy::unneeded_field_pattern,
    clippy::unreachable,
    clippy::unused_result_ok,
    clippy::unwrap_in_result,
    clippy::unwrap_used,
    clippy::verbose_file_reads,
    clippy::wildcard_enum_match_arm
)]
#![warn(unused)]
#![allow(
    edition_2024_expr_fragment_specifier,
    reason = "the macros expect the 2024 edition behaviour."
)]
pub mod lox;

use clap::{Parser, Subcommand};

use lox::errors::EngineError;
use lox::token::lexer;

use std::fs;
use std::io::{Error as IOError, Write};
use std::process::{ExitCode, Termination};

/// Load a file and run it through the interpreter.
/// TODO: Currently, only lexes. Still needs to Parse and Validate.
/// TODO: Configure VM/X86 backend?
fn run_file(file: String) -> Result<(), EngineError> {
    let source = fs::read_to_string(file)?;
    let tokens = lexer::tokenize(source)
        .map_err(EngineError::LexingErrors)?;

    for token in tokens {
        println!("{token:?}");
    }
    Ok(())
}

/// Run the REPL Prompt.
/// TODO: Add command support (mainly :quit)
fn run_prompt() -> Result<(), IOError> {
    use std::io::{stdin, stdout};

    let mut line = String::new();
    let stdin = stdin();
    loop {
        print!("> ");
        stdout().flush().expect("flushed output");

        line.clear();
        let _: usize = stdin.read_line(&mut line)?;

        let tokens = lexer::tokenize(&line);
        println!("{tokens:?}");
    }
}

/// Isomorphic to `Result<T, EngineError>`,
/// this allows for overriding the [Termination]
/// trait impl and report custom exit codes instead.
///
/// As this is only supposed to be used on the very
/// outer shell, T defaults to `()`.
#[derive(Debug)]
enum EngineResult<T = ()> {
    /// Ok variant.
    Ok(T),
    /// Error variant
    Err(EngineError),
}

impl Termination for EngineResult {
    fn report(self) -> ExitCode {
        if let EngineResult::Err(err) = self {
            eprintln!("{}", err.display_error());
            err.into()
        } else {
            ExitCode::SUCCESS
        }
    }
}

impl<T, E> From<Result<T, E>> for EngineResult<T>
where
    EngineError: From<E>,
{
    fn from(value: Result<T, E>) -> Self {
        match value {
            Ok(value) => EngineResult::Ok(value),
            Err(err) => EngineResult::Err(err.into()),
        }
    }
}

/// rust-lox is a work-in-progress implementation of the Lox Programming Language,
/// planning to cover all parts from lexing through to both an interpreter, and
/// the bytecode VM as described in the second part, with planned extensions for
/// a further compilation target (JavaScript, LLVM, or both).
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct LoxArgs {
    /// Subcommands, either this or [`source_file`] needs to be specified.
    #[command(subcommand)]
    command: Option<LoxCommands>,

    /// Source File for the program
    #[arg(required = true)]
    source_file: Option<String>,
}

/// Available commands in Rust-Lox
#[derive(Subcommand, Debug)]
#[command(subcommand_negates_reqs = true)]
enum LoxCommands {
    /// run the lox repl.
    ///
    /// In the future, this may optionally load a source file first, making it available on the REPL.
    Repl,
    /// tokenize the given file and print its contents.
    Tokenize {
        source_file: String,
    },
}

fn main() -> EngineResult {
    let LoxArgs {
        command,
        source_file
    } = LoxArgs::parse();

    match (command, source_file) {
        (None, Some(source_file))
        | (Some(LoxCommands::Tokenize { source_file }), None) => run_file(source_file).into(),

        (Some(LoxCommands::Repl), None) => run_prompt().into(),

        (Some(_), Some(_))
        | (None, None) => unreachable!("clap verifies this cannot happen."),
    }
}
