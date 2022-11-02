extern type GasBuiltin;

enum GetGasResult { Success: (), Failure: (), }

extern func get_gas() -> GetGasResult implicits (rc: RangeCheck, gb: GasBuiltin);
