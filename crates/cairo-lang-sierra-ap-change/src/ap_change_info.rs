use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;

/// Ap change information for a Sierra program.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct ApChangeInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<StatementIdx, usize>,
    /// The ap_change of calling the given function.
    pub function_ap_change: OrderedHashMap<FunctionId, usize>,
}

impl std::fmt::Display for ApChangeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (statement_idx, value) in self
            .variable_values
            .iter()
            .filter(|(_, value)| **value != 0)
            .sorted_by(|(a, _), (b, _)| a.0.cmp(&b.0))
        {
            writeln!(f, "#{statement_idx}: {value}")?;
        }
        writeln!(f)?;
        for (function_id, costs) in self.function_ap_change.iter() {
            writeln!(f, "{function_id}: {costs:?}")?;
        }
        Ok(())
    }
}
