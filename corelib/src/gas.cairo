#[derive(Copy, Drop)]
pub extern type BuiltinCosts;
pub extern type GasBuiltin;

pub extern fn withdraw_gas() -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;
pub extern fn withdraw_gas_all(
    costs: BuiltinCosts
) -> Option<()> implicits(RangeCheck, GasBuiltin) nopanic;
pub extern fn get_builtin_costs() -> BuiltinCosts nopanic;
