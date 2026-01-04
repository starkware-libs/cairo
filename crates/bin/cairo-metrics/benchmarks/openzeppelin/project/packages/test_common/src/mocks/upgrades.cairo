use starknet::ClassHash;

#[starknet::interface]
pub trait IUpgradesV1<TState> {
    fn upgrade(ref self: TState, new_class_hash: ClassHash);
    fn upgrade_and_call(
        ref self: TState, new_class_hash: ClassHash, selector: felt252, calldata: Span<felt252>,
    ) -> Span<felt252>;
    fn set_value(ref self: TState, val: felt252);
    fn get_value(self: @TState) -> felt252;
    fn remove_selector(self: @TState);
}

#[starknet::contract]
#[with_components(Upgradeable)]
pub mod UpgradesV1 {
    use starknet::ClassHash;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    pub struct Storage {
        pub value: felt252,
    }

    #[abi(embed_v0)]
    impl UpgradesV1Impl of super::IUpgradesV1<ContractState> {
        fn upgrade(ref self: ContractState, new_class_hash: ClassHash) {
            self.upgradeable.upgrade(new_class_hash);
        }

        fn upgrade_and_call(
            ref self: ContractState,
            new_class_hash: ClassHash,
            selector: felt252,
            calldata: Span<felt252>,
        ) -> Span<felt252> {
            self.upgradeable.upgrade_and_call(new_class_hash, selector, calldata)
        }

        fn set_value(ref self: ContractState, val: felt252) {
            self.value.write(val);
        }

        fn get_value(self: @ContractState) -> felt252 {
            self.value.read()
        }

        fn remove_selector(self: @ContractState) {}
    }
}

#[starknet::interface]
pub trait IUpgradesV2<TState> {
    fn upgrade(ref self: TState, new_class_hash: ClassHash);
    fn upgrade_and_call(
        ref self: TState, new_class_hash: ClassHash, selector: felt252, calldata: Span<felt252>,
    ) -> Span<felt252>;
    fn set_value(ref self: TState, val: felt252);
    fn set_value2(ref self: TState, val: felt252);
    fn get_value(self: @TState) -> felt252;
    fn get_value2(self: @TState) -> felt252;
}

#[starknet::contract]
#[with_components(Upgradeable)]
pub mod UpgradesV2 {
    use starknet::ClassHash;
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    pub struct Storage {
        pub value: felt252,
        pub value2: felt252,
    }

    #[abi(embed_v0)]
    impl UpgradesV2Impl of super::IUpgradesV2<ContractState> {
        fn upgrade(ref self: ContractState, new_class_hash: ClassHash) {
            self.upgradeable.upgrade(new_class_hash);
        }

        fn upgrade_and_call(
            ref self: ContractState,
            new_class_hash: ClassHash,
            selector: felt252,
            calldata: Span<felt252>,
        ) -> Span<felt252> {
            self.upgradeable.upgrade_and_call(new_class_hash, selector, calldata)
        }

        fn set_value(ref self: ContractState, val: felt252) {
            self.value.write(val);
        }

        fn set_value2(ref self: ContractState, val: felt252) {
            self.value2.write(val);
        }

        fn get_value(self: @ContractState) -> felt252 {
            self.value.read()
        }

        fn get_value2(self: @ContractState) -> felt252 {
            self.value2.read()
        }
    }
}
