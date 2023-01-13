use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// Ap change information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct ApChangeInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<StatementIdx, usize>,
    /// The ap_change of calling the given function.
    pub function_ap_change: OrderedHashMap<FunctionId, usize>,
}
