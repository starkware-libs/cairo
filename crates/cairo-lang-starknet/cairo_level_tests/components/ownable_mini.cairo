use starknet::ContractAddress;
use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

#[starknet::interface]
pub trait TransferTrait<TContractState> {
    fn owner(self: @TContractState) -> ContractAddress;
    fn transfer_ownership(ref self: TContractState, new_owner: ContractAddress);
}

/// A trait that, given a path to a storage, provides the storage node and mutable storage node.
pub trait HasStorage<
    TContractState,
    /// The storage type.
    Storage,
    /// The storage node.
    impl Node: starknet::storage::StorageNode<Storage>,
    /// The mutable storage node.
    impl NodeMut: starknet::storage::StorageNodeMut<Storage>,
> {
    fn storage(self: @TContractState) -> Node::NodeType;
    fn storage_mut(ref self: TContractState) -> NodeMut::NodeType;
}

#[starknet::storage_node]
pub struct OwnableStorage {
    owner: ContractAddress,
}

#[starknet::embeddable]
pub impl TransferImpl<
    TContractState, +HasStorage<TContractState, OwnableStorage>, +Drop<TContractState>,
> of TransferTrait<TContractState> {
    fn owner(self: @TContractState) -> ContractAddress {
        self.storage().owner.read()
    }

    fn transfer_ownership(ref self: TContractState, new_owner: ContractAddress) {
        self.validate_ownership();
        self.storage_mut().owner.write(new_owner);
    }
}

#[generate_trait]
pub impl OwnableHelperImpl<
    TContractState, +HasStorage<TContractState, OwnableStorage>, +Drop<TContractState>,
> of OwnableHelperTrait<TContractState> {
    fn init_ownable(ref self: TContractState, owner: ContractAddress) {
        self.storage_mut().owner.write(owner);
    }
    fn validate_ownership(self: @TContractState) {
        assert(self.storage().owner.read() == starknet::get_caller_address(), 'Wrong owner.');
    }
}
