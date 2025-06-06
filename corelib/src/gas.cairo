//! Utilities for handling gas in Cairo code.

#[cfg(not(gas: "disabled"))]
use crate::RangeCheck;

/// Type representing the table of the costs of the different builtin usages.
#[cfg(not(gas: "disabled"))]
pub extern type BuiltinCosts;

/// Placeholder when gas mechanism is disabled.
#[cfg(gas: "disabled")]
pub struct BuiltinCosts {}

impl BuiltinCostsCopy of Copy<BuiltinCosts>;
impl BuiltinCostsDrop of Drop<BuiltinCosts>;

/// The gas builtin.
/// This type is used to handle gas in the Cairo code.
/// Contains the amount of gas available for the current run.
pub extern type GasBuiltin;

/// Withdraws gas from the `GasBuiltin` to handle the success case flow.
/// Returns `Some(())` if there is sufficient gas to handle the success case, otherwise
/// returns `None`.
///
/// # Examples
///
/// ```
/// // The success branch is the following lines, the failure branch is the `panic` caused by the
/// // `unwrap` call.
/// withdraw_gas().unwrap();
/// ```
///
/// ```
/// // Direct handling of `withdraw_gas`.
/// match withdraw_gas() {
///     Some(()) => success_case(),
///     None => cheap_not_enough_gas_case(),
/// }
/// ```
#[cfg(not(gas: "disabled"))]
pub extern fn withdraw_gas() -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;

/// Placeholder when gas mechanism is disabled.
#[cfg(gas: "disabled")]
pub fn withdraw_gas() -> Option<()> nopanic {
    Some(())
}

/// Same as `withdraw_gas`, but directly receives `BuiltinCosts`, which enables optimizations
/// by removing the need for repeated internal calls for fetching the table of constants that may
/// internally happen in calls to `withdraw_gas`.
/// Should be used with caution.
#[cfg(not(gas: "disabled"))]
pub extern fn withdraw_gas_all(
    costs: BuiltinCosts,
) -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;

/// Placeholder when gas mechanism is disabled.
#[cfg(gas: "disabled")]
pub fn withdraw_gas_all(costs: BuiltinCosts) -> Option<()> nopanic {
    Some(())
}

/// Returns unused gas into the gas builtin.
///
/// Useful for cases where different branches take different amounts of gas, but gas withdrawal is
/// the same for both.
pub extern fn redeposit_gas() implicits(GasBuiltin) nopanic;

/// Returns the `BuiltinCosts` table to be used in `withdraw_gas_all`.
#[cfg(not(gas: "disabled"))]
pub extern fn get_builtin_costs() -> BuiltinCosts nopanic;

/// Placeholder when gas mechanism is disabled.
#[cfg(gas: "disabled")]
pub fn get_builtin_costs() -> BuiltinCosts nopanic {
    BuiltinCosts {}
}

/// Represents a gas reserve.
///
/// Gas reserves can be created at any point using gas from the gas counter,
/// and can be utilized at a later point in time.
pub extern type GasReserve;

/// Creates a new gas reserve by withdrawing the specified amount from the gas counter.
/// Returns `Some(GasReserve)` if there is sufficient gas, otherwise returns `None`.
#[cfg(not(gas: "disabled"))]
pub extern fn gas_reserve_create(
    amount: u128,
) -> Option<GasReserve> implicits(RangeCheck, GasBuiltin) nopanic;

/// Adds the gas stored in the reserve back to the gas counter.
/// The reserve is consumed in the process.
pub extern fn gas_reserve_utilize(reserve: GasReserve) implicits(GasBuiltin) nopanic;
