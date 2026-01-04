// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/common/erc2981/interface.cairo)

use starknet::ContractAddress;

pub const IERC2981_ID: felt252 = 0x2d3414e45a8700c29f119a54b9f11dca0e29e06ddcb214018fc37340e165ed6;

/// Interface of the ERC2981 standard.
#[starknet::interface]
pub trait IERC2981<TState> {
    /// Returns how much royalty is owed and to whom, based on a sale price that may be denominated
    /// in any unit of exchange. The royalty amount is denominated and should be paid in that same
    /// unit of exchange.
    fn royalty_info(self: @TState, token_id: u256, sale_price: u256) -> (ContractAddress, u256);
}

/// Interface providing external read functions for discovering the state of ERC2981 component.
#[starknet::interface]
pub trait IERC2981Info<TState> {
    /// Returns the royalty information that all ids in this contract will default to.
    ///
    /// The returned tuple contains:
    ///
    /// - `t.0`: The receiver of the royalty payment.
    /// - `t.1`: The numerator of the royalty fraction.
    /// - `t.2`: The denominator of the royalty fraction.
    fn default_royalty(self: @TState) -> (ContractAddress, u128, u128);

    /// Returns the royalty information specific to a token.
    /// If no specific royalty information is set for the token, the default is returned.
    ///
    /// The returned tuple contains:
    ///
    /// - `t.0`: The receiver of the royalty payment.
    /// - `t.1`: The numerator of the royalty fraction.
    /// - `t.2`: The denominator of the royalty fraction.
    fn token_royalty(self: @TState, token_id: u256) -> (ContractAddress, u128, u128);
}

/// Interface providing external admin functions for managing the settings of ERC2981 component.
#[starknet::interface]
pub trait IERC2981Admin<TState> {
    /// Sets the royalty information that all ids in this contract will default to.
    fn set_default_royalty(ref self: TState, receiver: ContractAddress, fee_numerator: u128);

    /// Sets the default royalty percentage and receiver to zero.
    fn delete_default_royalty(ref self: TState);

    /// Sets the royalty information for a specific token id that takes precedence over the global
    /// default.
    fn set_token_royalty(
        ref self: TState, token_id: u256, receiver: ContractAddress, fee_numerator: u128,
    );

    /// Resets royalty information for the token id back to unset.
    fn reset_token_royalty(ref self: TState, token_id: u256);
}

/// Interface that aggregates all ERC2981 component's external functions.
#[starknet::interface]
pub trait IERC2981ABI<TState> {
    // IERC2981
    fn royalty_info(self: @TState, token_id: u256, sale_price: u256) -> (ContractAddress, u256);

    // IERC2981Info
    fn default_royalty(self: @TState) -> (ContractAddress, u128, u128);
    fn token_royalty(self: @TState, token_id: u256) -> (ContractAddress, u128, u128);

    // IERC2981Admin
    fn set_default_royalty(ref self: TState, receiver: ContractAddress, fee_numerator: u128);
    fn delete_default_royalty(ref self: TState);
    fn set_token_royalty(
        ref self: TState, token_id: u256, receiver: ContractAddress, fee_numerator: u128,
    );
    fn reset_token_royalty(ref self: TState, token_id: u256);
}
