#[abi]
trait IAnotherContract {
    fn foo();
}


#[contract]
mod TestContract {
    struct Storage { my_storage_var: felt, }

    fn internal_func() -> felt {
        1
    }

    #[external]
    fn test(ref arg: felt, arg1: felt, arg2: felt) -> felt {
        let x = my_storage_var::read();
        my_storage_var::write(x + 1);
        x + internal_func()
    }

    #[external]
    fn empty() {
    }
}
