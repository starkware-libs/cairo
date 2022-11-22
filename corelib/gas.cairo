extern type GasBuiltin;

extern func get_gas() -> Option::<()> use (rc: RangeCheck, gb: GasBuiltin) nopanic;
