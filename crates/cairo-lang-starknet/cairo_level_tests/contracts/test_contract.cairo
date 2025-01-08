#[starknet::interface]
trait IAnotherContract<T> {
    fn foo(ref self: T, a: u128) -> u128;
}

#[starknet::interface]
trait OutsideTrait<TContractState> {
    fn ret_3(self: @TContractState) -> felt252;
}
#[starknet::embeddable]
impl OutsideImpl<TContractState, +Drop<TContractState>> of OutsideTrait<TContractState> {
    fn ret_3(self: @TContractState) -> felt252 {
        3
    }
}

#[starknet::contract]
mod test_contract {
    use core::dict::Felt252Dict;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::{
        IAnotherContractDispatcher, IAnotherContractDispatcherTrait,
        IAnotherContractLibraryDispatcher, MyType,
    };

    #[storage]
    struct Storage {
        my_storage_var: felt252,
        core: felt252,
    }

    fn internal_func() -> felt252 {
        -1
    }

    #[abi(embed_v0)]
    impl WorkingUsage = super::OutsideImpl<ContractState>;

    #[abi(per_item)]
    #[generate_trait]
    impl Impl of Trait {
        #[constructor]
        fn constructor(ref self: ContractState, initial: felt252) {
            self.my_storage_var.write(initial);
        }

        #[external(v0)]
        fn test(
            ref self: ContractState, ref arg: felt252, arg1: felt252, arg2: felt252,
        ) -> felt252 {
            let mut x = self.my_storage_var.read();
            x += 1;
            self.my_storage_var.write(x);
            x + internal_func()
        }

        #[external(v0)]
        fn another_function(ref self: ContractState, x: MyType) {}

        #[external(v0)]
        fn call_foo(
            ref self: ContractState, another_contract_address: starknet::ContractAddress, a: u128,
        ) -> u128 {
            IAnotherContractDispatcher { contract_address: another_contract_address }.foo(a)
        }

        #[external(v0)]
        fn libcall_foo(ref self: ContractState, a: u128) -> u128 {
            IAnotherContractLibraryDispatcher { class_hash: core::num::traits::Zero::zero() }.foo(a)
        }

        /// An external method that requires the `segment_arena` builtin.
        #[external(v0)]
        fn segment_arena_builtin(ref self: ContractState) {
            let x: Felt252Dict<felt252> = Default::default();
            x.squash();
        }

        #[l1_handler]
        fn l1_handle(ref self: ContractState, from_address: felt252, arg: felt252) -> felt252 {
            arg
        }

        #[external(v0)]
        fn fixed_sized_array_usage(ref self: ContractState, value: [felt252; 5]) -> [felt252; 5] {
            value
        }
    }
}

#[derive(Copy, Drop, Serde)]
struct MyType {
    a: felt252,
    b: bool,
}
