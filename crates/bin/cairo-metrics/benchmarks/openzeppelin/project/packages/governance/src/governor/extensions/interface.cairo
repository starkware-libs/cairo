// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0
// (governance/src/governor/extensions/interface.cairo)

use starknet::ContractAddress;

#[starknet::interface]
pub trait IQuorumFraction<TState> {
    /// Returns the token that voting power is sourced from.
    fn token(self: @TState) -> ContractAddress;

    /// Returns the current quorum numerator.
    fn current_quorum_numerator(self: @TState) -> u256;

    /// Returns the quorum numerator at a specific timepoint.
    fn quorum_numerator(self: @TState, timepoint: u64) -> u256;

    /// Returns the quorum denominator.
    fn quorum_denominator(self: @TState) -> u256;
}

#[starknet::interface]
pub trait IVotesToken<TState> {
    /// Returns the token that voting power is sourced from.
    fn token(self: @TState) -> ContractAddress;
}

#[starknet::interface]
pub trait ITimelocked<TState> {
    /// Returns address of the associated timelock.
    fn timelock(self: @TState) -> ContractAddress;

    /// Returns the timelock proposal id for a given proposal id.
    fn get_timelock_id(self: @TState, proposal_id: felt252) -> felt252;

    /// Updates the associated timelock.
    fn update_timelock(ref self: TState, new_timelock: ContractAddress);
}

#[starknet::interface]
pub trait IGovernorSettingsAdmin<TState> {
    /// Sets the voting delay.
    fn set_voting_delay(ref self: TState, new_voting_delay: u64);

    /// Sets the voting period.
    fn set_voting_period(ref self: TState, new_voting_period: u64);

    /// Sets the proposal threshold.
    fn set_proposal_threshold(ref self: TState, new_proposal_threshold: u256);
}
