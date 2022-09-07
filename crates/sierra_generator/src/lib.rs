//! Lowering from the semantic model down to Sierra. See [semantic] and  [sierra]

pub mod db;
mod diagnostic;
mod expr_generator;
mod expr_generator_context;
mod function_generator;
mod id_allocator;
mod pre_sierra;
mod program_generator;
mod resolve_labels;
pub mod test_utils;
mod utils;

pub use diagnostic::{Diagnostic, SierraGeneratorDiagnostic};
