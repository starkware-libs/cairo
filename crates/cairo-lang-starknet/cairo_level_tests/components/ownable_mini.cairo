use starknet::ContractAddress;

#[starknet::interface]
pub trait TransferTrait<TContractState> {
    fn owner(self: @TContractState) -> ContractAddress;
    fn transfer_ownership(ref self: TContractState, new_owner: ContractAddress);
}

#[starknet::storage_node]
pub struct OwnableStorage {
    owner: ContractAddress,
}

#[starknet::embeddable]
pub impl TransferImpl<
    TContractState, impl X: starknet::storage::HasStorage<TContractState, OwnableStorage>, +Drop<TContractState>
> of TransferTrait<TContractState> {
    fn owner(self: @TContractState) -> ContractAddress {
        self.storage().owner.read()
    }

    fn transfer_ownership(
        ref self: TContractState, new_owner: ContractAddress
    ) {
        self.validate_ownership();
        self.storage_mut().owner.write(new_owner);
    }
}

#[generate_trait]
pub impl OwnableHelperImpl<
    TContractState, impl X: starknet::storage::HasStorage<TContractState, OwnableStorage>, +Drop<TContractState>
> of OwnableHelperTrait<TContractState, X> {
    fn init_ownable(ref self: TContractState, owner: ContractAddress) {
        self.storage_mut().owner.write(owner);
    }
    fn validate_ownership(self: @TContractState) {
        assert(self.storage().owner.read() == starknet::get_caller_address(), 'Wrong owner.');
    }
}
