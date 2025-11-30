use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

mod function_debug_info;
mod statements_locations;

pub use function_debug_info::FunctionDebugInfo;
pub use statements_locations::StatementsLocations;
pub use statements_locations::statements_code_locations::{
    SourceCodeLocation, SourceCodeSpan, SourceFileFullPath, StatementsSourceCodeLocations,
};
pub use statements_locations::statements_functions::StatementsFunctions;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraProgramDebugInfo<'db> {
    pub statements_locations: StatementsLocations<'db>,
    pub functions_info: OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>,
}
