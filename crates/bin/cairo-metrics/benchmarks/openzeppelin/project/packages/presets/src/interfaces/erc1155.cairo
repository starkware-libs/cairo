use starknet::{ClassHash, ContractAddress};

#[starknet::interface]
pub trait ERC1155UpgradeableABI<TState> {
    // IERC1155
    fn balance_of(self: @TState, account: ContractAddress, token_id: u256) -> u256;
    fn balance_of_batch(
        self: @TState, accounts: Span<ContractAddress>, token_ids: Span<u256>,
    ) -> Span<u256>;
    fn safe_transfer_from(
        ref self: TState,
        from: ContractAddress,
        to: ContractAddress,
        token_id: u256,
        value: u256,
        data: Span<felt252>,
    );
    fn safe_batch_transfer_from(
        ref self: TState,
        from: ContractAddress,
        to: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    );
    fn is_approved_for_all(
        self: @TState, owner: ContractAddress, operator: ContractAddress,
    ) -> bool;
    fn set_approval_for_all(ref self: TState, operator: ContractAddress, approved: bool);

    // ISRC5
    fn supports_interface(self: @TState, interface_id: felt252) -> bool;

    // IERC1155MetadataURI
    fn uri(self: @TState, token_id: u256) -> ByteArray;

    // IERC1155Camel
    fn balanceOf(self: @TState, account: ContractAddress, tokenId: u256) -> u256;
    fn balanceOfBatch(
        self: @TState, accounts: Span<ContractAddress>, tokenIds: Span<u256>,
    ) -> Span<u256>;
    fn safeTransferFrom(
        ref self: TState,
        from: ContractAddress,
        to: ContractAddress,
        tokenId: u256,
        value: u256,
        data: Span<felt252>,
    );
    fn safeBatchTransferFrom(
        ref self: TState,
        from: ContractAddress,
        to: ContractAddress,
        tokenIds: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    );
    fn isApprovedForAll(self: @TState, owner: ContractAddress, operator: ContractAddress) -> bool;
    fn setApprovalForAll(ref self: TState, operator: ContractAddress, approved: bool);

    // IOwnable
    fn owner(self: @TState) -> ContractAddress;
    fn transfer_ownership(ref self: TState, new_owner: ContractAddress);
    fn renounce_ownership(ref self: TState);

    // IOwnableCamelOnly
    fn transferOwnership(ref self: TState, newOwner: ContractAddress);
    fn renounceOwnership(ref self: TState);

    // IUpgradeable
    fn upgrade(ref self: TState, new_class_hash: ClassHash);
}
