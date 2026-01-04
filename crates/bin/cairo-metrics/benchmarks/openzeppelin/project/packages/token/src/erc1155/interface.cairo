// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc1155/interface.cairo)

use starknet::ContractAddress;

pub const IERC1155_ID: felt252 = 0x6114a8f75559e1b39fcba08ce02961a1aa082d9256a158dd3e64964e4b1b52;
pub const IERC1155_METADATA_URI_ID: felt252 =
    0xcabe2400d5fe509e1735ba9bad205ba5f3ca6e062da406f72f113feb889ef7;
pub const IERC1155_RECEIVER_ID: felt252 =
    0x15e8665b5af20040c3af1670509df02eb916375cdf7d8cbaf7bd553a257515e;

#[starknet::interface]
pub trait IERC1155<TState> {
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
}

#[starknet::interface]
pub trait IERC1155MetadataURI<TState> {
    fn uri(self: @TState, token_id: u256) -> ByteArray;
}

#[starknet::interface]
pub trait IERC1155Camel<TState> {
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
}

//
// ERC1155 ABI
//

#[starknet::interface]
pub trait ERC1155ABI<TState> {
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
}

//
// IERC1155Receiver
//

#[starknet::interface]
pub trait IERC1155Receiver<TState> {
    fn on_erc1155_received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        token_id: u256,
        value: u256,
        data: Span<felt252>,
    ) -> felt252;
    fn on_erc1155_batch_received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    ) -> felt252;
}

#[starknet::interface]
pub trait IERC1155ReceiverCamel<TState> {
    fn onERC1155Received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        tokenId: u256,
        value: u256,
        data: Span<felt252>,
    ) -> felt252;
    fn onERC1155BatchReceived(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        tokenIds: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    ) -> felt252;
}

#[starknet::interface]
pub trait ERC1155ReceiverABI<TState> {
    // IERC1155Receiver
    fn on_erc1155_received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        token_id: u256,
        value: u256,
        data: Span<felt252>,
    ) -> felt252;
    fn on_erc1155_batch_received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        token_ids: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    ) -> felt252;

    // IERC1155ReceiverCamel
    fn onERC1155Received(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        tokenId: u256,
        value: u256,
        data: Span<felt252>,
    ) -> felt252;
    fn onERC1155BatchReceived(
        self: @TState,
        operator: ContractAddress,
        from: ContractAddress,
        tokenIds: Span<u256>,
        values: Span<u256>,
        data: Span<felt252>,
    ) -> felt252;

    // ISRC5
    fn supports_interface(self: @TState, interface_id: felt252) -> bool;
}
