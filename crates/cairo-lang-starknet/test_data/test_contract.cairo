#[abi]
trait IAnotherContract {
    fn foo(a: u128) -> u128;
}


#[contract]
mod TestContract {
    use super::IAnotherContractDispatcherTrait;
    use super::IAnotherContractDispatcher;
    use super::IAnotherContractLibraryDispatcher;
    use dict::Felt252DictTrait;

    struct Storage {
        my_storage_var: felt252
    }

    fn internal_func() -> felt252 {
        1
    }

    #[external]
    fn test(ref self: Storage, ref arg: felt252, arg1: felt252, arg2: felt252) -> felt252 {
        let mut x = self.my_storage_var.read();
        x += 1;
        self.my_storage_var.write(x);
        x + internal_func()
    }

    #[external]
    fn call_foo(
        ref self: Storage, another_contract_address: starknet::ContractAddress, a: u128
    ) -> u128 {
        IAnotherContractDispatcher { contract_address: another_contract_address }.foo(a)
    }

    #[external]
    fn libcall_foo(ref self: Storage, a: u128) -> u128 {
        IAnotherContractLibraryDispatcher { class_hash: starknet::class_hash_const::<0>() }.foo(a)
    }

    /// An external method that requires the `segment_arena` builtin.
    #[external]
    fn segment_arena_builtin(ref self: Storage, ) {
        let x = felt252_dict_new::<felt252>();
        x.squash();
    }

    #[l1_handler]
    fn l1_handle(ref self: Storage, from_address: felt252, arg: felt252) -> felt252 {
        arg
    }
}
