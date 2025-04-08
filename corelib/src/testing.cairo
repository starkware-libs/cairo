//! Measurement of gas consumption for testing purpose.
//!
//! This module provides the `get_available_gas` function, useful for asserting the amount of gas
//! consumed by a particular operation or function call.
//! By calling `get_available_gas` before and after the operation, you can calculate the exact
//! amount of gas used.

use crate::gas::GasBuiltin;

/// Returns the amount of gas available in the `GasBuiltin`.
///
/// Useful for asserting that a certain amount of gas was consumed.
/// Note: The actual gas consumption observed by calls to `get_available_gas` is only exact
/// immediately before calls to `withdraw_gas`.
///
/// # Examples
///
/// ```
/// use core::testing::get_available_gas;
///
/// fn gas_heavy_function() {
///     // ... some gas-intensive code
/// }
///
/// fn test_gas_consumption() {
///     let gas_before = get_available_gas();
///     // Making sure `gas_before` is exact.
///     core::gas::withdraw_gas().unwrap();
///
///     gas_heavy_function();
///
///     let gas_after = get_available_gas();
///     // Making sure `gas_after` is exact
///     core::gas::withdraw_gas().unwrap();
///
///     assert!(gas_after - gas_before < 100_000);
/// }
/// ```
pub extern fn get_available_gas() -> u128 implicits(GasBuiltin) nopanic;

/// Returns the amount of gas available in the `GasBuiltin`, as well as the amount of gas unused in
/// the local wallet.
///
/// Useful for asserting that a certain amount of gas was used.
/// Note: This function call costs exactly `2300` gas, so this may be ignored in calculations.
/// # Examples
///
/// ```
/// use core::testing::get_unspent_gas;
///
/// fn gas_heavy_function() {
///     // ... some gas-intensive code
/// }
///
/// fn test_gas_consumption() {
///     let gas_before = get_unspent_gas();
///     gas_heavy_function();
///     let gas_after = get_unspent_gas();
///     assert!(gas_after - gas_before < 100_000);
/// }
/// ```
pub extern fn get_unspent_gas() -> u128 implicits(GasBuiltin) nopanic;
