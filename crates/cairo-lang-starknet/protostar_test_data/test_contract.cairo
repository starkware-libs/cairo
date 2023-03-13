#[contract]
mod TestContract {
    struct Storage {
        my_storage_var: felt, 
    }

    fn internal_func(ref system: System) -> felt {
        1
    }

    fn internal_func2(ref system: System) -> felt {
        roll(1, 2);
        2
    }


    // TODO(ilya): Remove implicits once the order is consistent.
    #[external]
    fn test(
        ref system: System, ref arg: felt, arg1: felt, arg2: felt
    ) -> felt implicits(RangeCheck, GasBuiltin) {
        let x = super::my_storage_var::read(system);
        super::my_storage_var::write(system, x + 1);
        x + internal_func(system);
        x + internal_func2(system)
    }
}
