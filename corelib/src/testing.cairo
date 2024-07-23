use core::gas::GasBuiltin;

/// Returns the amount of gas available in the `GasBuiltin`.
///
/// Useful for asserting that a certain amount of gas was consumed.
/// Note: The actual gas consumption observed by calls to `get_available_gas` is only exact
/// immediately before calls to `withdraw_gas`.
///
/// For example:
/// ```
/// let gas_before = get_available_gas();
/// // Making sure `gas_before` is exact.
/// core::gas::withdraw_gas().unwrap();
/// gas_heavy_function();
/// let gas_after = get_available_gas();
/// // Making sure `gas_after` is exact
/// core::gas::withdraw_gas().unwrap();
/// assert_lt!(gas_after - gas_before, 100000);
/// ```
pub extern fn get_available_gas() -> u128 implicits(GasBuiltin) nopanic;
