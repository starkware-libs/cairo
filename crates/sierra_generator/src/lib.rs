//! Lowering from the semantic model down to Sierra. See [semantic] and  [sierra]

mod ap_change;
mod block_generator;
pub mod canonical_id_replacer;
pub mod db;
mod diagnostic;
mod dup_and_drop;
mod expr_generator_context;
mod function_generator;
#[cfg(any(feature = "testing", test))]
pub mod function_generator_test_utils;
mod id_allocator;
mod local_variables;
mod next_statement_index_fetch;
pub mod pre_sierra;
mod program_generator;
pub mod replace_ids;
mod resolve_labels;
mod specialization_context;
mod store_variables;
#[cfg(any(feature = "testing", test))]
pub mod test_utils;
mod types;
mod utils;

pub use diagnostic::SierraGeneratorDiagnostic;
