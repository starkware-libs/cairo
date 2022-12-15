use std::collections::HashMap;

use sierra::ids::FunctionId;
use sierra::program::StatementIdx;

/// Ap change information for a Sierra program.
#[derive(Debug, Eq, PartialEq)]
pub struct ApChangeInfo {
    /// The values of variables at matching libfuncs at given statements indices.
    pub variable_values: HashMap<StatementIdx, usize>,
    /// The ap_change of calling the given function.
    pub function_ap_change: HashMap<FunctionId, usize>,
}
