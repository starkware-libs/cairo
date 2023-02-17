#[derive(Copy, Drop)]
extern type BuiltinCosts;
extern type GasBuiltin;

extern fn get_gas() -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
extern fn get_gas_all(
    costs: BuiltinCosts
) -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
extern fn get_builtin_costs() -> BuiltinCosts nopanic;
