#[derive(Copy, Drop)]
pub extern type BuiltinCosts;
pub extern type GasBuiltin;
use core::RangeCheck;

pub extern fn withdraw_gas() -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;
pub extern fn withdraw_gas_all(
    costs: BuiltinCosts
) -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;
pub extern fn get_builtin_costs() -> BuiltinCosts nopanic;
