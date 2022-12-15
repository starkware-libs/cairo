extern type BuiltinCosts;
extern type GasBuiltin;

impl BuiltinCostsCopy of Copy::<BuiltinCosts>;
impl BuiltinCostsDrop of Drop::<BuiltinCosts>;

extern func get_gas() -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
extern func get_gas_all(
    costs: BuiltinCosts
) -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
