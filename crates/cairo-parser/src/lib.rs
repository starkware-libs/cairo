//! Cairo parser.
//!
//! This crate is responsible for parsing Cairo code.
pub mod colored_printer;
pub mod db;
pub mod diagnostic;
pub mod lexer;
pub mod operators;
pub mod parser;
pub mod printer;
pub mod recovery;
pub mod test_utils;
pub mod utils;

pub use diagnostic::ParserDiagnostic;
