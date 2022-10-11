use std::collections::HashMap;

use casm::ap_change::ApChange;
use sierra::ids::FunctionId;

/// Metadata provided with a Sierra program to simplify the compilation to casm.
pub struct Metadata {
    /// AP changes information for Sierra user functions.
    pub function_ap_change: HashMap<FunctionId, ApChange>,
}
