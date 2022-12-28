use sierra_ap_change::ap_change_info::ApChangeInfo;
use sierra_gas::gas_info::GasInfo;

/// Metadata provided with a Sierra program to simplify the compilation to casm.
pub struct Metadata {
    /// AP changes information for Sierra user functions.
    pub ap_change_info: ApChangeInfo,
    /// Gas information for validating Sierra code and taking the apporiate amount of gas.
    pub gas_info: GasInfo,
}
