use std::collections::HashMap;

use sierra::ids::FunctionId;
use sierra::program::StatementIdx;

/// Gas information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct GasInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: HashMap<StatementIdx, i64>,
    /// The costs of calling the given function ids.
    pub function_costs: HashMap<FunctionId, i64>,
}
