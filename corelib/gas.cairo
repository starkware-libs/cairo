extern type GasBuiltin;

extern func get_gas() -> Option::<()> implicits(rc: RangeCheck, gb: GasBuiltin) nopanic;
