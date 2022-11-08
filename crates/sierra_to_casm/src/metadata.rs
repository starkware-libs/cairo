use std::collections::HashMap;

use sierra::extensions::lib_func::SierraApChange;
use sierra::ids::FunctionId;
use sierra_gas::gas_info::GasInfo;

/// Metadata provided with a Sierra program to simplify the compilation to casm.
pub struct Metadata {
    /// AP changes information for Sierra user functions.
    pub function_ap_change: HashMap<FunctionId, SierraApChange>,
    /// Gas information for validating Sierra code and taking the apporiate amount of gas.
    pub gas_info: GasInfo,
}
