#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[contract]
mod TestContract {
    use super::IAnotherContractDispatcherTrait;
    use super::IAnotherContractDispatcher;
    use super::IAnotherContractLibraryDispatcher;
    use dict::DictFeltToTrait;

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
    fn call_foo(another_contract_address: ContractAddress, a: u128) -> u128 {
        IAnotherContractDispatcher { contract_address: another_contract_address }.foo(a)
    }

    #[external]
    fn libcall_foo(a: u128) -> u128 {
        IAnotherContractLibraryDispatcher { class_hash: starknet::class_hash_const::<0>() }.foo(a)
    }

    /// An external method that requires the `segment_arena` builtin.
    #[external]
    fn segment_arena_builtin() {
        let x = dict_felt_to_new::<felt>();
        x.squash();
    }

    #[l1_handler]
    fn l1_handle(arg: felt) -> felt {
        arg
    }
}
