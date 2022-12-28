use std::collections::HashMap;

use cairo_sierra::extensions::builtin_cost::CostTokenType;
use cairo_sierra::ids::FunctionId;
use cairo_sierra::program::StatementIdx;
use cairo_utils::ordered_hash_map::OrderedHashMap;

/// Gas information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: HashMap<(StatementIdx, CostTokenType), i64>,
    /// The costs of calling the given function.
    pub function_costs: HashMap<FunctionId, OrderedHashMap<CostTokenType, i64>>,
}
