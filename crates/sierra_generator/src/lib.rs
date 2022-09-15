//! Lowering from the semantic model down to Sierra. See [semantic] and  [sierra]

pub mod db;
mod diagnostic;
mod dup_and_drop;
mod expr_generator;
mod expr_generator_context;
mod function_generator;
mod id_allocator;
pub mod pre_sierra;
mod program_generator;
mod resolve_labels;
mod store_variables;
pub mod test_utils;
mod utils;

pub use diagnostic::{Diagnostic, SierraGeneratorDiagnostic};
