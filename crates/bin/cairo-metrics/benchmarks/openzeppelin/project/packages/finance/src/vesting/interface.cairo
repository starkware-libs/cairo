// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (finance/src/vesting/interface.cairo)

use starknet::ContractAddress;

#[starknet::interface]
pub trait IVesting<TState> {
    /// Returns the timestamp marking the beginning of the vesting period.
    fn start(self: @TState) -> u64;

    /// Returns the timestamp marking the end of the cliff period.
    fn cliff(self: @TState) -> u64;

    /// Returns the total duration of the vesting period.
    fn duration(self: @TState) -> u64;

    /// Returns the timestamp marking the end of the vesting period.
    fn end(self: @TState) -> u64;

    /// Returns the already released amount for a given `token`.
    fn released(self: @TState, token: ContractAddress) -> u256;

    /// Returns the amount of a given `token` that can be released at the time of the call.
    fn releasable(self: @TState, token: ContractAddress) -> u256;

    /// Returns the total vested amount of a specified `token` at a given `timestamp`.
    fn vested_amount(self: @TState, token: ContractAddress, timestamp: u64) -> u256;

    /// Releases the amount of a given `token` that has already vested.
    fn release(ref self: TState, token: ContractAddress) -> u256;
}
