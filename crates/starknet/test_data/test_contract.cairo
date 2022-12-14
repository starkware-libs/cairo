#[contract]
mod TestContract { func internal_func(ref system: System) -> felt {
    1
}

#[external]
func test(ref system: System, ref arg: felt, arg1: felt, arg2: felt) -> felt {
    internal_func(system) + super::my_storage_var::read(system)
}
}


// # TODO(ilya): Convert to new format.
#[contract]
struct Storage { my_storage_var: felt, }
