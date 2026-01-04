// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc20/interface.cairo)

use starknet::ContractAddress;


#[starknet::interface]
pub trait IERC20<TState> {
    /// Returns the total supply of tokens.
    fn total_supply(self: @TState) -> u256;

    /// Returns the amount of tokens owned by `account`.
    fn balance_of(self: @TState, account: ContractAddress) -> u256;

    /// Returns the remaining number of tokens that `spender` will be allowed to spend on behalf of
    /// `owner` through `transfer_from`.
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;

    /// Moves `amount` tokens from the caller's account to `recipient`.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;

    /// Moves `amount` tokens from `sender` to `recipient` using the allowance mechanism.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn transfer_from(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;

    /// Sets `amount` as the allowance of `spender` over the caller's tokens.
    ///
    /// Returns a boolean value indicating whether the operation succeeded.
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;
}


#[starknet::interface]
pub trait IERC20Metadata<TState> {
    /// Returns the name of the token.
    fn name(self: @TState) -> ByteArray;

    /// Returns the symbol of the token.
    fn symbol(self: @TState) -> ByteArray;

    /// Returns the number of decimals used to get its user representation.
    fn decimals(self: @TState) -> u8;
}

/// Adds camelCase support for 'ERC20'.
#[starknet::interface]
pub trait IERC20Camel<TState> {
    fn totalSupply(self: @TState) -> u256;
    fn balanceOf(self: @TState, account: ContractAddress) -> u256;
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;
    fn transferFrom(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;
}

/// Adds camelCase support for 'ERC20'.
#[starknet::interface]
pub trait IERC20CamelOnly<TState> {
    fn totalSupply(self: @TState) -> u256;
    fn balanceOf(self: @TState, account: ContractAddress) -> u256;
    fn transferFrom(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
}

//
// IERC20 Mixin
//

#[starknet::interface]
pub trait IERC20Mixin<TState> {
    // IERC20
    fn total_supply(self: @TState) -> u256;
    fn balance_of(self: @TState, account: ContractAddress) -> u256;
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;
    fn transfer_from(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;

    // IERC20Metadata
    fn name(self: @TState) -> ByteArray;
    fn symbol(self: @TState) -> ByteArray;
    fn decimals(self: @TState) -> u8;

    // IERC20CamelOnly
    fn totalSupply(self: @TState) -> u256;
    fn balanceOf(self: @TState, account: ContractAddress) -> u256;
    fn transferFrom(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
}


#[starknet::interface]
pub trait IERC20Permit<TState> {
    /// Sets `amount` as the allowance of `spender` over `owner`'s tokens, given the `signature`.
    ///
    /// Requirements:
    ///
    /// - `deadline` must be a timestamp in the future.
    /// - `signature` must be a valid secp256k1 signature from `owner` over the EIP712-formatted
    ///   function arguments.
    fn permit(
        ref self: TState,
        owner: ContractAddress,
        spender: ContractAddress,
        amount: u256,
        deadline: u64,
        signature: Span<felt252>,
    );

    /// Returns the current nonce for `owner`.
    fn nonces(self: @TState, owner: ContractAddress) -> felt252;

    /// Returns the domain separator used in the encoding of the signatures for permit.
    fn DOMAIN_SEPARATOR(self: @TState) -> felt252;
}

//
// ERC20 ABI
//

#[starknet::interface]
pub trait ERC20ABI<TState> {
    // IERC20
    fn total_supply(self: @TState) -> u256;
    fn balance_of(self: @TState, account: ContractAddress) -> u256;
    fn allowance(self: @TState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TState, recipient: ContractAddress, amount: u256) -> bool;
    fn transfer_from(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;
    fn approve(ref self: TState, spender: ContractAddress, amount: u256) -> bool;

    // IERC20Metadata
    fn name(self: @TState) -> ByteArray;
    fn symbol(self: @TState) -> ByteArray;
    fn decimals(self: @TState) -> u8;

    // IERC20CamelOnly
    fn totalSupply(self: @TState) -> u256;
    fn balanceOf(self: @TState, account: ContractAddress) -> u256;
    fn transferFrom(
        ref self: TState, sender: ContractAddress, recipient: ContractAddress, amount: u256,
    ) -> bool;

    // IERC20Permit
    fn permit(
        ref self: TState,
        owner: ContractAddress,
        spender: ContractAddress,
        amount: u256,
        deadline: u64,
        signature: Span<felt252>,
    );
    fn nonces(self: @TState, owner: ContractAddress) -> felt252;
    fn DOMAIN_SEPARATOR(self: @TState) -> felt252;

    // ISNIP12Metadata
    fn snip12_metadata(self: @TState) -> (felt252, felt252);
}
