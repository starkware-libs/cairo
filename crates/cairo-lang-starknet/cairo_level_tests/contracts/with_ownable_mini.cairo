#[starknet::contract]
mod ownable_mini_contract {
    use starknet::ContractAddress;
    use starknet::storage::{
        StorageAsPath, StorageNode, StorageNodeMut, StoragePointerReadAccess,
        StoragePointerWriteAccess,
    };
    use crate::components::ownable_mini;

    #[storage]
    struct Storage {
        ownable: ownable_mini::OwnableStorage,
        balance: u128,
    }

    #[abi(embed_v0)]
    impl OwnershipTransfer = ownable_mini::TransferImpl<ContractState>;

    impl OwnershipHelper = ownable_mini::OwnableHelperImpl<ContractState>;

    impl OwnableHasStorage of ownable_mini::HasStorage<
        ContractState, ownable_mini::OwnableStorage,
    > {
        fn storage(self: @ContractState) -> StorageNode::<ownable_mini::OwnableStorage>::NodeType {
            self.ownable.as_path().storage_node()
        }
        fn storage_mut(
            ref self: ContractState,
        ) -> StorageNodeMut::<ownable_mini::OwnableStorage>::NodeType {
            self.ownable.as_path().storage_node_mut()
        }
    }

    #[abi(per_item)]
    #[generate_trait]
    impl OwnableBalanceImpl of OwnableBalance {
        #[constructor]
        fn constructor(ref self: ContractState, owner: ContractAddress, initial: u128) {
            self.init_ownable(owner);
            self.balance.write(initial);
        }
        #[external(v0)]
        fn get_balance(self: @ContractState) -> u128 {
            self.balance.read()
        }
        #[external(v0)]
        fn set_balance(ref self: ContractState, new_balance: u128) {
            self.validate_ownership();
            self.balance.write(new_balance);
        }
    }
}
