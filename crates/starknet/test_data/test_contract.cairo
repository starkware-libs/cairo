#[contract]
mod TestContract {
    struct Storage { my_storage_var: felt, }

    fn internal_func(ref system: System) -> felt {
        1
    }

    #[external]
    fn test(ref system: System, ref arg: felt, arg1: felt, arg2: felt) -> felt {
        let x = super::my_storage_var::read(system);
        super::my_storage_var::write(system, x + 1);
        x + internal_func(system)
    }
}
