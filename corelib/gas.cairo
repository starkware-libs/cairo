extern type GasBuiltin;

extern func get_gas() -> Option::<()> implicits(RangeCheck, GasBuiltin) nopanic;
