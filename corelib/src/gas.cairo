use core::RangeCheck;

/// Type representing the table of the costs of the different builtin usages.
#[derive(Copy, Drop)]
pub extern type BuiltinCosts;

/// The gas builtin.
/// This type is used to handle gas in the Cairo code.
/// Contains the amount of gas available for the current run.
pub extern type GasBuiltin;

/// Withdraws gas from the `GasBuiltin` to handle the branch handling `Option::Some(())`.
/// In case of not enough gas to handle that branch, `Option::None` is returned.
///
/// Example:
/// ```
/// // The success branch is the following lines, the failure branch is the `panic` caused by the
/// // `unwrap` call.
/// withdraw_gas().unwrap();
/// ```
pub extern fn withdraw_gas() -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;

/// Same as `withdraw_gas`, but directly receives `BuiltinCosts`, which enables optimizations
/// by removing the need for repeated internal calls for fetching the table of consts that may
/// internally happen in calls to `withdraw_gas`.
/// Should be used with caution.
pub extern fn withdraw_gas_all(
    costs: BuiltinCosts
) -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;

/// Returns the `BuiltinCosts` table to be used in `withdraw_gas_all`.
pub extern fn get_builtin_costs() -> BuiltinCosts nopanic;
