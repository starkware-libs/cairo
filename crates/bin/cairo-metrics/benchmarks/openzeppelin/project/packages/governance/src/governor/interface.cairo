// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/governor/interface.cairo)

use starknet::ContractAddress;
use starknet::account::Call;

pub const IGOVERNOR_ID: felt252 = 0x1100a1f8546595b5bd75a6cd8fcc5b015370655e66f275963321c5cd0357ac9;

/// Interface for a contract that implements the ERC-6372 standard.
#[starknet::interface]
pub trait IERC6372<TState> {
    /// Clock used for flagging checkpoints.
    fn clock(self: @TState) -> u64;

    /// Description of the clock.
    /// See https://eips.ethereum.org/EIPS/eip-6372#clock_mode
    fn CLOCK_MODE(self: @TState) -> ByteArray;
}

#[derive(Copy, PartialEq, Drop, Serde, Debug)]
pub enum ProposalState {
    Pending,
    Active,
    Canceled,
    Defeated,
    Succeeded,
    Queued,
    Executed,
}

#[starknet::interface]
pub trait IGovernor<TState> {
    /// Name of the governor instance (used in building the SNIP-12 domain separator).
    fn name(self: @TState) -> felt252;

    /// Version of the governor instance (used in building SNIP-12 domain separator).
    fn version(self: @TState) -> felt252;

    /// A description of the possible `support` values for `cast_vote` and the way these votes are
    /// counted, meant to be consumed by UIs to show correct vote options and interpret the results.
    /// The string is a URL-encoded sequence of key-value pairs that each describe one aspect, for
    /// example `support=bravo&quorum=for,abstain`.
    ///
    /// There are 2 standard keys: `support` and `quorum`.
    ///
    /// - `support=bravo` refers to the vote options 0 = Against, 1 = For, 2 = Abstain, as in
    /// `GovernorBravo`.
    /// - `quorum=bravo` means that only For votes are counted towards quorum.
    /// - `quorum=for,abstain` means that both For and Abstain votes are counted towards quorum.
    ///
    /// If a counting module makes use of encoded `params`, it should include this under a `params`
    /// key with a unique name that describes the behavior. For example:
    ///
    /// - `params=fractional` might refer to a scheme where votes are divided fractionally between
    /// for/against/abstain.
    /// - `params=erc721` might refer to a scheme where specific NFTs are delegated to vote.
    ///
    /// NOTE: The string can be decoded by the standard
    /// https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams[`URLSearchParams`]
    /// JavaScript class.
    fn COUNTING_MODE(self: @TState) -> ByteArray;

    /// Hashing function used to (re)build the proposal id from the proposal details.
    fn hash_proposal(self: @TState, calls: Span<Call>, description_hash: felt252) -> felt252;

    /// Returns the state of a proposal, given its id.
    fn state(self: @TState, proposal_id: felt252) -> ProposalState;

    /// The number of votes required in order for a voter to become a proposer.
    fn proposal_threshold(self: @TState) -> u256;

    /// Timepoint used to retrieve user's votes and quorum. If using block number, the snapshot is
    /// performed at the end of this block. Hence, voting for this proposal starts at the beginning
    /// of the following block.
    fn proposal_snapshot(self: @TState, proposal_id: felt252) -> u64;

    /// Timepoint at which votes close. If using block number, votes close at the end of this block,
    /// so it is possible to cast a vote during this block.
    fn proposal_deadline(self: @TState, proposal_id: felt252) -> u64;

    /// The account that created a proposal.
    fn proposal_proposer(self: @TState, proposal_id: felt252) -> ContractAddress;

    /// The time when a queued proposal becomes executable ("ETA"). Unlike `proposal_snapshot` and
    /// `proposal_deadline`, this doesn't use the governor clock, and instead relies on the
    /// executor's clock which may be different. In most cases this will be a timestamp.
    fn proposal_eta(self: @TState, proposal_id: felt252) -> u64;

    /// Whether a proposal needs to be queued before execution.
    fn proposal_needs_queuing(self: @TState, proposal_id: felt252) -> bool;

    /// Delay between the proposal is created and the vote starts. The unit this duration is
    /// expressed in depends on the clock (see ERC-6372) this contract uses.
    ///
    /// This can be increased to leave time for users to buy voting power, or delegate it, before
    /// the voting of a proposal starts.
    fn voting_delay(self: @TState) -> u64;

    /// Delay between the vote start and vote end. The unit this duration is expressed in depends on
    /// the clock (see ERC-6372) this contract uses.
    ///
    /// NOTE: The `voting_delay` can delay the start of the vote. This must be considered when
    /// setting the voting duration compared to the voting delay.
    ///
    /// NOTE: This value is stored when the proposal is submitted so that possible changes to the
    /// value do not affect proposals that have already been submitted.
    fn voting_period(self: @TState) -> u64;

    /// Minimum number of cast voted required for a proposal to be successful.
    ///
    /// NOTE: The `timepoint` parameter corresponds to the snapshot used for counting vote. This
    /// allows the quorum to scale depending on values such as the total supply of a token at this
    /// timepoint.
    fn quorum(self: @TState, timepoint: u64) -> u256;

    /// Voting power of an `account` at a specific `timepoint`.
    ///
    /// NOTE: This can be implemented in a number of ways, for example by reading the delegated
    /// balance from one (or multiple) `ERC20Votes` tokens.
    fn get_votes(self: @TState, account: ContractAddress, timepoint: u64) -> u256;

    /// Voting power of an `account` at a specific `timepoint` given additional encoded parameters.
    fn get_votes_with_params(
        self: @TState, account: ContractAddress, timepoint: u64, params: Span<felt252>,
    ) -> u256;

    /// Returns whether `account` has cast a vote on `proposal_id`.
    fn has_voted(self: @TState, proposal_id: felt252, account: ContractAddress) -> bool;

    /// Creates a new proposal. Vote start after a delay specified by `voting_delay` and
    /// lasts for a duration specified by `voting_period`.
    ///
    /// NOTE: The state of the Governor and targets may change between the proposal creation and
    /// its execution. This may be the result of third party actions on the targeted contracts, or
    /// other governor proposals. For example, the balance of this contract could be updated or its
    /// access control permissions may be modified, possibly compromising the proposal's ability to
    /// execute successfully (e.g. the governor doesn't have enough value to cover a proposal with
    /// multiple transfers).
    ///
    /// Returns the id of the proposal.
    fn propose(ref self: TState, calls: Span<Call>, description: ByteArray) -> felt252;

    /// Queue a proposal. Some governors require this step to be performed before execution can
    /// happen. If queuing is not necessary, this function may revert.
    /// Queuing a proposal requires the quorum to be reached, the vote to be successful, and the
    /// deadline to be reached.
    ///
    /// Returns the id of the proposal.
    fn queue(ref self: TState, calls: Span<Call>, description_hash: felt252) -> felt252;

    /// Execute a successful proposal. This requires the quorum to be reached, the vote to be
    /// successful, and the deadline to be reached. Depending on the governor it might also be
    /// required that the proposal was queued and that some delay passed.
    ///
    /// NOTE: Some modules can modify the requirements for execution, for example by adding an
    /// additional timelock (See `timelock_controller`).
    ///
    /// Returns the id of the proposal.
    fn execute(ref self: TState, calls: Span<Call>, description_hash: felt252) -> felt252;

    /// Cancel a proposal. A proposal is cancellable by the proposer, but only while it is Pending
    /// state, i.e. before the vote starts.
    ///
    /// Returns the id of the proposal.
    fn cancel(ref self: TState, calls: Span<Call>, description_hash: felt252) -> felt252;

    /// Cast a vote.
    fn cast_vote(ref self: TState, proposal_id: felt252, support: u8) -> u256;

    /// Cast a vote with a `reason`.
    fn cast_vote_with_reason(
        ref self: TState, proposal_id: felt252, support: u8, reason: ByteArray,
    ) -> u256;

    /// Cast a vote with a `reason` and additional serialized `params`.
    fn cast_vote_with_reason_and_params(
        ref self: TState,
        proposal_id: felt252,
        support: u8,
        reason: ByteArray,
        params: Span<felt252>,
    ) -> u256;

    /// Cast a vote using the `voter`'s signature.
    fn cast_vote_by_sig(
        ref self: TState,
        proposal_id: felt252,
        support: u8,
        voter: ContractAddress,
        signature: Span<felt252>,
    ) -> u256;

    /// Cast a vote with a `reason` and additional serialized `params` using the `voter`'s
    /// signature.
    fn cast_vote_with_reason_and_params_by_sig(
        ref self: TState,
        proposal_id: felt252,
        support: u8,
        voter: ContractAddress,
        reason: ByteArray,
        params: Span<felt252>,
        signature: Span<felt252>,
    ) -> u256;

    /// Returns the next unused nonce for an address.
    fn nonces(self: @TState, voter: ContractAddress) -> felt252;

    /// Relays a transaction or function call to an arbitrary target.
    ///
    /// In cases where the governance executor is some contract other than the governor itself, like
    /// when using a timelock, this function can be invoked in a governance proposal to recover
    /// tokens that were sent to the governor contract by mistake.
    ///
    /// NOTE: If the executor is simply the governor itself, use of `relay` is redundant.
    fn relay(ref self: TState, call: Call);
}
