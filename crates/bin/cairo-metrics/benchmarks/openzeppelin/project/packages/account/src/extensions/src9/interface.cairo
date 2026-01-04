// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/extensions/src9/interface.cairo)

use starknet::ContractAddress;
use starknet::account::Call;

pub const ISRC9_V2_ID: felt252 = 0x1d1144bb2138366ff28d8e9ab57456b1d332ac42196230c3a602003c89872;

#[derive(Copy, Drop, Serde)]
pub struct OutsideExecution {
    pub caller: ContractAddress,
    pub nonce: felt252,
    pub execute_after: u64,
    pub execute_before: u64,
    pub calls: Span<Call>,
}

#[starknet::interface]
pub trait ISRC9_V2<TContractState> {
    /// Allows anyone to submit a transaction on behalf of the account as long as they
    /// have the relevant signatures.
    ///
    /// This method allows reentrancy. A call to `__execute__` or `execute_from_outside_v2` can
    /// trigger another nested transaction to `execute_from_outside_v2` thus the implementation MUST
    /// verify that the provided `signature` matches the hash of `outside_execution` and that
    /// `nonce` was not already used.
    ///
    /// The implementation should expect version to be set to 2 in the domain separator.
    ///
    /// Arguments:
    ///
    /// - `outside_execution` - The parameters of the transaction to execute.
    /// - `signature` - A valid signature on the SNIP-12 message encoding of `outside_execution`.
    fn execute_from_outside_v2(
        ref self: TContractState, outside_execution: OutsideExecution, signature: Span<felt252>,
    ) -> Array<Span<felt252>>;

    /// Get the status of a given nonce. `true` if the nonce is available to use.
    fn is_valid_outside_execution_nonce(self: @TContractState, nonce: felt252) -> bool;
}
