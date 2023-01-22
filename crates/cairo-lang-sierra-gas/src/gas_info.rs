use std::fmt::Display;

use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

/// Gas information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<(StatementIdx, CostTokenType), i64>,
    /// The costs of calling the given function.
    pub function_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i64>>,
}

impl Display for GasInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ((statement_idx, cost_type), value) in self.variable_values.iter() {
            writeln!(f, "#{statement_idx}: {}({value})", cost_type.name())?;
        }
        writeln!(f)?;
        for (function_id, costs) in self.function_costs.iter() {
            writeln!(f, "{}:", function_id)?;
            for (cost_type, value) in costs.iter() {
                writeln!(f, "{}({value})", cost_type.name())?;
            }
        }
        Ok(())
    }
}
