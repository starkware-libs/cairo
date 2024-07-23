use core::gas::GasBuiltin;

/// Returns the amount of gas available for the current execution.
///
/// Useful for asserting that a certain amount of gas was consumed.
/// For example:
/// ```cairo
/// let gas_before = get_available_gas();
/// // Making sure that gas for the call is not pre-paid.
/// core::gas::withdraw_gas().unwrap();
/// // Calling the function that consumes gas.
/// gas_heavy_function();
/// let gas_after = get_available_gas();
/// assert_lt!(gas_after - gas_before, 100000);
/// ```
pub extern fn get_available_gas() -> u128 implicits(GasBuiltin) nopanic;
