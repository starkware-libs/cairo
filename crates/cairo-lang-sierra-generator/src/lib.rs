//! Lowering from the semantic model down to Sierra. See [cairo_lang_semantic] and
//! [cairo_lang_sierra].

mod ap_change;
mod ap_tracking;
mod block_generator;
pub mod canonical_id_replacer;
pub mod db;
mod expr_generator_context;
mod extra_sierra_info;
mod function_generator;
#[cfg(any(feature = "testing", test))]
pub mod function_generator_test_utils;
mod id_allocator;
mod lifetime;
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
