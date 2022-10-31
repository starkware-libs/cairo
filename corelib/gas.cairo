extern type GasBuiltin;

enum GetGasResult { Success: (), Failure: (), }

extern func get_gas(ref rc: RangeCheck, ref gb: GasBuiltin) -> GetGasResult;
