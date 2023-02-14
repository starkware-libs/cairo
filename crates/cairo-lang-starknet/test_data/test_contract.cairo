#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[contract]
mod TestContract {
    struct Storage {
        my_storage_var: felt
    }

    fn internal_func() -> felt {
        1
    }

    #[external]
    fn test(ref arg: felt, arg1: felt, arg2: felt) -> felt {
        let mut x = my_storage_var::read();
        x += 1;
        my_storage_var::write(x);
        x + internal_func()
    }

    #[external]
    fn empty() {}

    #[external]
    fn call_foo(another_contract_address: ContractAddress, a: u128) -> u128 {
        super::IAnotherContractDispatcher::foo(another_contract_address, a)
    }
}
