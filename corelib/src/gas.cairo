//! Utilities for handling gas in Cairo code.
//!
//! This module includes various features:
//! - `withdraw_gas`: A function that withdraws gas from the `GasBuiltin` to handle the success case
//! flow.
//! - `withdraw_gas_all`: A function that is similar to `withdraw_gas`, but directly receives the
//! `BuiltinCosts`, enabling optimizations.
//! - `redeposit_gas`: A function that returns unused gas into the `GasBuiltin`.
//! - `get_builtin_costs`: A function that returns the `BuiltinCosts` table to be used with
//! `withdraw_gas_all`.

use crate::RangeCheck;

/// A type representing the table of the costs of the different builtin usages.
#[derive(Copy, Drop)]
pub extern type BuiltinCosts;

/// The gas builtin.
/// This type is used to handle gas in the Cairo code.
/// Contains the amount of gas available for the current run.
pub extern type GasBuiltin;

/// Withdraws gas from the `GasBuiltin` to handle the success case flow.
/// Returns `Option::Some(())` if there is sufficient gas to handle the success case, otherwise
/// returns `Option::None`.
///
/// # Panics
///
/// Panics if there is not enough available gas for the withdrawal.
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
///     Option::Some(()) => success_case(),
///     Option::None => cheap_not_enough_gas_case(),
/// }
/// ```
pub extern fn withdraw_gas() -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;

/// Same as `withdraw_gas`, but directly receives `BuiltinCosts`, which enables optimizations
/// by removing the need for repeated internal calls for fetching the table of consts that may
/// internally happen in calls to `withdraw_gas`.
/// Should be used with caution.
pub extern fn withdraw_gas_all(
    costs: BuiltinCosts,
) -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;


/// Returns unused gas into the gas builtin.
///
/// Useful for cases where different branches take different amounts of gas, but gas withdrawal is
/// the same for both.
pub extern fn redeposit_gas() implicits(GasBuiltin) nopanic;

/// Returns the `BuiltinCosts` table to be used in `withdraw_gas_all`.
pub extern fn get_builtin_costs() -> BuiltinCosts nopanic;
