use core::gas::GasBuiltin;

/// Returns the amount of gas available in the `GasBuiltin`.
///
/// Useful for asserting that a certain amount of gas was consumed.
/// For example:
/// ```
/// let gas_before = get_available_gas();
/// // Making sure that gas for the call is not pre-paid.
/// // GasBuiltin will be reduced by the `gas_heavy_function` static price.
/// core::gas::withdraw_gas().unwrap();
/// // Calling the function that consumes gas.
/// // GasBuiltin will be reduced by the `gas_heavy_function` dynamic price (i.e. internal Calls
/// // to `withdraw_gas`).
/// gas_heavy_function();
/// let gas_after = get_available_gas();
/// // Making sure the gas for the comparison calculation is not pre-paid, and won't be counted
/// // here.
/// core::gas::withdraw_gas().unwrap();
/// assert_lt!(gas_after - gas_before, 100000);
/// ```
pub extern fn get_available_gas() -> u128 implicits(GasBuiltin) nopanic;
