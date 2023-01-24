use std::fmt::Display;

use cairo_lang_sierra::extensions::builtin_cost::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::collection_arithmetics::add_maps;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::{chain, Itertools};

/// Gas information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: OrderedHashMap<(StatementIdx, CostTokenType), i64>,
    /// The costs of calling the given function.
    pub function_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i64>>,
}
impl GasInfo {
    pub fn add(mut self, mut other: GasInfo) -> GasInfo {
        let variable_values = add_maps(self.variable_values, other.variable_values);
        let function_costs = chain!(self.function_costs.keys(), other.function_costs.keys())
            .unique()
            .cloned()
            .collect_vec()
            .into_iter()
            .map(|i| {
                let costs0 = self.function_costs.swap_remove(&i).unwrap_or_default();
                let costs1 = other.function_costs.swap_remove(&i).unwrap_or_default();
                (i, add_maps(costs0, costs1))
            })
            .collect();
        GasInfo { variable_values, function_costs }
    }
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
