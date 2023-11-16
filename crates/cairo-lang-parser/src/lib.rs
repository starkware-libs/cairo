//! Cairo parser.
//!
//! This crate is responsible for parsing Cairo code.
pub use diagnostic::ParserDiagnostic;

pub mod colored_printer;
pub mod consts;
pub mod db;
pub mod diagnostic;
pub mod lexer;
pub mod operators;
pub mod parser;
pub mod printer;
pub mod recovery;
pub mod utils;
mod validation;

#[cfg(any(feature = "testing", test))]
pub mod test_utils;
