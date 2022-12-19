#[contract]
mod TestContract { struct Storage { my_storage_var: felt, }

func internal_func(ref system: System) -> felt {
    1
}


// TODO(ilya): Remove implicits once the order is consistent.
#[external]
func test(
    ref system: System, ref arg: felt, arg1: felt, arg2: felt
) -> felt implicits(RangeCheck, GasBuiltin) {
    let x = super::my_storage_var::read(system);
    super::my_storage_var::write(system, x + 1);
    x + internal_func(system)
}
}
