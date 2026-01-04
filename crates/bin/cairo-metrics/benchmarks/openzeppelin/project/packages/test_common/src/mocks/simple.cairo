#[starknet::interface]
pub trait ISimpleMock<TContractState> {
    fn increase_balance(ref self: TContractState, amount: felt252) -> bool;
    fn set_balance(ref self: TContractState, value: felt252, panic: bool);
    fn get_balance(self: @TContractState) -> felt252;
}

#[starknet::contract]
pub mod SimpleMock {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    pub struct Storage {
        pub balance: felt252,
    }

    #[abi(embed_v0)]
    impl SimpleMockImpl of super::ISimpleMock<ContractState> {
        fn increase_balance(ref self: ContractState, amount: felt252) -> bool {
            self.balance.write(self.balance.read() + amount);
            true
        }

        fn set_balance(ref self: ContractState, value: felt252, panic: bool) {
            if panic {
                #[allow(panic)]
                panic!("Some error");
            }
            self.balance.write(value);
        }

        fn get_balance(self: @ContractState) -> felt252 {
            self.balance.read()
        }
    }
}
