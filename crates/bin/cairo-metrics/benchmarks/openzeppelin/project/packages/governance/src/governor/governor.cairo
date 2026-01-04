// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/governor/governor.cairo)

/// # Governor Component
///
/// Core of the governance system.
#[starknet::component]
pub mod GovernorComponent {
    use core::hash::{HashStateExTrait, HashStateTrait};
    use core::num::traits::Zero;
    use core::pedersen::PedersenTrait;
    use core::traits::PartialEq;
    use openzeppelin_account::interface::{ISRC6Dispatcher, ISRC6DispatcherTrait};
    use openzeppelin_introspection::src5::SRC5Component;
    use openzeppelin_introspection::src5::SRC5Component::InternalImpl as SRC5InternalImpl;
    use openzeppelin_utils::bytearray::ByteArrayExtTrait;
    use openzeppelin_utils::cryptography::snip12::{OffchainMessageHash, SNIP12Metadata};
    use starknet::account::Call;
    use starknet::storage::{Map, StorageMapReadAccess, StorageMapWriteAccess};
    use starknet::{ContractAddress, SyscallResultTrait};
    use crate::governor::ProposalCore;
    use crate::governor::interface::{IGOVERNOR_ID, IGovernor, ProposalState};
    use crate::governor::vote::{Vote, VoteWithReasonAndParams};
    use crate::utils::call_impls::{HashCallImpl, HashCallsImpl};

    type ProposalId = felt252;

    #[storage]
    pub struct Storage {
        pub Governor_proposals: Map<ProposalId, ProposalCore>,
        pub Governor_nonces: Map<ContractAddress, felt252>,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    pub enum Event {
        ProposalCreated: ProposalCreated,
        ProposalQueued: ProposalQueued,
        ProposalExecuted: ProposalExecuted,
        ProposalCanceled: ProposalCanceled,
        VoteCast: VoteCast,
        VoteCastWithParams: VoteCastWithParams,
    }

    /// Emitted when `call` is scheduled as part of operation `id`.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ProposalCreated {
        #[key]
        pub proposal_id: felt252,
        #[key]
        pub proposer: ContractAddress,
        pub calls: Span<Call>,
        pub signatures: Span<Span<felt252>>,
        pub vote_start: u64,
        pub vote_end: u64,
        pub description: ByteArray,
    }

    /// Emitted when a proposal is queued.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ProposalQueued {
        #[key]
        pub proposal_id: felt252,
        pub eta_seconds: u64,
    }

    /// Emitted when a proposal is executed.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ProposalExecuted {
        #[key]
        pub proposal_id: felt252,
    }

    /// Emitted when a proposal is canceled.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct ProposalCanceled {
        #[key]
        pub proposal_id: felt252,
    }

    /// Emitted when a vote is cast without params.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct VoteCast {
        #[key]
        pub voter: ContractAddress,
        pub proposal_id: felt252,
        pub support: u8,
        pub weight: u256,
        pub reason: ByteArray,
    }

    /// Emitted when a vote is cast with params.
    ///
    /// NOTE: `support` values should be seen as buckets. Their interpretation depends on the voting
    /// module used. `params` are additional encoded parameters. Their interpretation also
    /// depends on the voting module used.
    #[derive(Drop, Debug, PartialEq, starknet::Event)]
    pub struct VoteCastWithParams {
        #[key]
        pub voter: ContractAddress,
        pub proposal_id: felt252,
        pub support: u8,
        pub weight: u256,
        pub reason: ByteArray,
        pub params: Span<felt252>,
    }

    impl CallPartialEq of PartialEq<Call> {
        fn eq(lhs: @Call, rhs: @Call) -> bool {
            lhs.to == rhs.to && lhs.selector == rhs.selector && lhs.calldata == rhs.calldata
        }
    }

    pub mod Errors {
        pub const EXECUTOR_ONLY: felt252 = 'Executor only';
        pub const PROPOSER_ONLY: felt252 = 'Proposer only';
        pub const NONEXISTENT_PROPOSAL: felt252 = 'Nonexistent proposal';
        pub const EXISTENT_PROPOSAL: felt252 = 'Existent proposal';
        pub const RESTRICTED_PROPOSER: felt252 = 'Restricted proposer';
        pub const INSUFFICIENT_PROPOSER_VOTES: felt252 = 'Insufficient votes';
        pub const UNEXPECTED_PROPOSAL_STATE: felt252 = 'Unexpected proposal state';
        pub const QUEUE_NOT_IMPLEMENTED: felt252 = 'Queue not implemented';
        pub const INVALID_SIGNATURE: felt252 = 'Invalid signature';
    }

    /// Constants expected to be defined at the contract level used to configure the component
    /// behaviour.
    ///
    /// - `DEFAULT_PARAMS`: Default additional encoded parameters used by cast_vote
    /// methods that don't include them.
    pub trait ImmutableConfig {
        /// Defined as a function since constant Span<felt252> is not supported.
        fn DEFAULT_PARAMS() -> Span<felt252>;
    }

    //
    // Extensions traits
    //

    pub trait GovernorSettingsTrait<TContractState> {
        /// See `interface::IGovernor::voting_delay`.
        fn voting_delay(self: @ComponentState<TContractState>) -> u64;

        /// See `interface::IGovernor::voting_period`.
        fn voting_period(self: @ComponentState<TContractState>) -> u64;

        /// See `interface::IGovernor::proposal_threshold`.
        fn proposal_threshold(self: @ComponentState<TContractState>) -> u256;
    }

    pub trait GovernorQuorumTrait<TContractState> {
        /// See `interface::IGovernor::quorum`.
        fn quorum(self: @ComponentState<TContractState>, timepoint: u64) -> u256;
    }

    pub trait GovernorCountingTrait<TContractState> {
        /// See `interface::IGovernor::COUNTING_MODE`.
        fn counting_mode(self: @ComponentState<TContractState>) -> ByteArray;

        /// Register a vote for `proposal_id` by `account` with a given `support`,
        /// voting `weight` and voting `params`.
        ///
        /// NOTE: Support is generic and can represent various things depending on the voting system
        /// used.
        fn count_vote(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            account: ContractAddress,
            support: u8,
            total_weight: u256,
            params: Span<felt252>,
        ) -> u256;

        /// See `interface::IGovernor::has_voted`.
        fn has_voted(
            self: @ComponentState<TContractState>, proposal_id: felt252, account: ContractAddress,
        ) -> bool;

        /// Returns whether amount of votes already cast passes the threshold limit.
        fn quorum_reached(self: @ComponentState<TContractState>, proposal_id: felt252) -> bool;

        /// Returns whether the proposal is successful or not.
        fn vote_succeeded(self: @ComponentState<TContractState>, proposal_id: felt252) -> bool;
    }

    pub trait GovernorVotesTrait<TContractState> {
        /// See `interface::IERC6372::clock`.
        fn clock(self: @ComponentState<TContractState>) -> u64;

        /// See `interface::IERC6372::CLOCK_MODE`.
        fn clock_mode(self: @ComponentState<TContractState>) -> ByteArray;

        /// See `interface::IGovernor::get_votes`.
        fn get_votes(
            self: @ComponentState<TContractState>,
            account: ContractAddress,
            timepoint: u64,
            params: Span<felt252>,
        ) -> u256;
    }

    pub trait GovernorExecutionTrait<TContractState> {
        /// See `interface::IGovernor::state`.
        fn state(self: @ComponentState<TContractState>, proposal_id: felt252) -> ProposalState;

        /// Address through which the governor executes action.
        /// Should be used to specify whether the module execute actions through another contract
        /// such as a timelock.
        ///
        /// NOTE: MUST be the governor itself, or an instance of TimelockController with the
        /// governor as the only proposer, canceller, and executor.
        ///
        /// WARNING: When the executor is not the governor itself (i.e. a timelock), it can call
        /// functions that are restricted with the `assert_only_governance` guard, and also
        /// potentially execute transactions on behalf of the governor. Because of this, this module
        /// is designed to work with the TimelockController as the unique potential external
        /// executor.
        fn executor(self: @ComponentState<TContractState>) -> ContractAddress;

        /// Execution mechanism. Can be used to modify the way execution is
        /// performed (for example adding a vault/timelock).
        fn execute_operations(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        );

        /// Queuing mechanism. Can be used to modify the way queuing is
        /// performed (for example adding a vault/timelock).
        ///
        /// Requirements:
        ///
        /// - Must return a timestamp that describes the expected ETA for execution. If the returned
        /// value is 0, the core will consider queueing did not succeed, and the public `queue`
        /// function will revert.
        fn queue_operations(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            calls: Span<Call>,
            description_hash: felt252,
        ) -> u64;

        /// See `interface::IGovernor::proposal_needs_queuing`.
        fn proposal_needs_queuing(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> bool;

        /// Cancel mechanism. Can be used to modify the way canceling is
        /// performed (for example adding a vault/timelock).
        fn cancel_operations(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            description_hash: felt252,
        );
    }

    //
    // External
    //

    #[embeddable_as(GovernorImpl)]
    impl Governor<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +GovernorVotesTrait<TContractState>,
        impl GovernorQuorum: GovernorQuorumTrait<TContractState>,
        impl GovernorCounting: GovernorCountingTrait<TContractState>,
        impl GovernorExecution: GovernorExecutionTrait<TContractState>,
        impl GovernorSettings: GovernorSettingsTrait<TContractState>,
        impl Metadata: SNIP12Metadata,
        impl Immutable: ImmutableConfig,
        +Drop<TContractState>,
    > of IGovernor<ComponentState<TContractState>> {
        /// Name of the governor instance (used in building the SNIP-12 domain separator).
        fn name(self: @ComponentState<TContractState>) -> felt252 {
            Metadata::name()
        }

        /// Version of the governor instance (used in building SNIP-12 domain separator).
        fn version(self: @ComponentState<TContractState>) -> felt252 {
            Metadata::version()
        }

        /// A description of the possible `support` values for `cast_vote` and the way these votes
        /// are counted, meant to be consumed by UIs to show correct vote options and interpret the
        /// results.
        /// The string is a URL-encoded sequence of key-value pairs that each describe one aspect,
        /// for example `support=bravo&quorum=for,abstain`.
        ///
        /// There are 2 standard keys: `support` and `quorum`.
        ///
        /// - `support=bravo` refers to the vote options 0 = Against, 1 = For, 2 = Abstain, as in
        /// `GovernorBravo`.
        /// - `quorum=bravo` means that only For votes are counted towards quorum.
        /// - `quorum=for,abstain` means that both For and Abstain votes are counted towards quorum.
        ///
        /// If a counting module makes use of encoded `params`, it should  include this under a
        /// `params`
        /// key with a unique name that describes the behavior. For example:
        ///
        /// - `params=fractional` might refer to a scheme where votes are divided fractionally
        /// between for/against/abstain.
        /// - `params=erc721` might refer to a scheme where specific NFTs are delegated to vote.
        ///
        /// NOTE: The string can be decoded by the standard
        /// https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams[`URLSearchParams`]
        /// JavaScript class.
        fn COUNTING_MODE(self: @ComponentState<TContractState>) -> ByteArray {
            self.counting_mode()
        }

        /// Hashing function used to (re)build the proposal id from the proposal details.
        fn hash_proposal(
            self: @ComponentState<TContractState>, calls: Span<Call>, description_hash: felt252,
        ) -> felt252 {
            self._hash_proposal(calls, description_hash)
        }

        /// Returns the state of a proposal, given its id.
        fn state(self: @ComponentState<TContractState>, proposal_id: felt252) -> ProposalState {
            GovernorExecution::state(self, proposal_id)
        }

        /// The number of votes required in order for a voter to become a proposer.
        fn proposal_threshold(self: @ComponentState<TContractState>) -> u256 {
            self._proposal_threshold()
        }

        /// Timepoint used to retrieve user's votes and quorum. If using block number, the snapshot
        /// is performed at the end of this block. Hence, voting for this proposal starts at the
        /// beginning of the following block.
        fn proposal_snapshot(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            self._proposal_snapshot(proposal_id)
        }

        /// Timepoint at which votes close. If using block number, votes close at the end of this
        /// block, so it is possible to cast a vote during this block.
        fn proposal_deadline(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            self._proposal_deadline(proposal_id)
        }

        /// The account that created a proposal.
        fn proposal_proposer(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> ContractAddress {
            self._proposal_proposer(proposal_id)
        }

        /// The time when a queued proposal becomes executable ("ETA"). Unlike `proposal_snapshot`
        /// and `proposal_deadline`, this doesn't use the governor clock, and instead relies on the
        /// executor's clock which may be different. In most cases this will be a timestamp.
        fn proposal_eta(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            self._proposal_eta(proposal_id)
        }

        /// Whether a proposal needs to be queued before execution.
        fn proposal_needs_queuing(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> bool {
            GovernorExecution::proposal_needs_queuing(self, proposal_id)
        }

        /// Delay between the proposal is created and the vote starts. The unit this duration is
        /// expressed in depends on the clock (see ERC-6372) this contract uses.
        ///
        /// This can be increased to leave time for users to buy voting power, or delegate it,
        /// before the voting of a proposal starts.
        fn voting_delay(self: @ComponentState<TContractState>) -> u64 {
            GovernorSettings::voting_delay(self)
        }

        /// Delay between the vote start and vote end. The unit this duration is expressed in
        /// depends on the clock (see ERC-6372) this contract uses.
        ///
        /// NOTE: The `voting_delay` can delay the start of the vote. This must be considered when
        /// setting the voting duration compared to the voting delay.
        ///
        /// NOTE: This value is stored when the proposal is submitted so that possible changes to
        /// the value do not affect proposals that have already been submitted.
        fn voting_period(self: @ComponentState<TContractState>) -> u64 {
            GovernorSettings::voting_period(self)
        }

        /// Minimum number of casted votes required for a proposal to be successful.
        ///
        /// NOTE: The `timepoint` parameter corresponds to the snapshot used for counting votes.
        /// This allows the quorum to scale depending on values such as the total supply of a token
        /// at this timepoint.
        fn quorum(self: @ComponentState<TContractState>, timepoint: u64) -> u256 {
            GovernorQuorum::quorum(self, timepoint)
        }

        /// Voting power of an `account` at a specific `timepoint`.
        ///
        /// NOTE: this can be implemented in a number of ways, for example by reading the delegated
        /// balance from one (or multiple) `ERC20Votes` tokens.
        fn get_votes(
            self: @ComponentState<TContractState>, account: ContractAddress, timepoint: u64,
        ) -> u256 {
            self._get_votes(account, timepoint, Immutable::DEFAULT_PARAMS())
        }

        /// Voting power of an `account` at a specific `timepoint` given additional encoded
        /// parameters.
        fn get_votes_with_params(
            self: @ComponentState<TContractState>,
            account: ContractAddress,
            timepoint: u64,
            params: Span<felt252>,
        ) -> u256 {
            self._get_votes(account, timepoint, params)
        }

        /// Returns whether `account` has cast a vote on `proposal_id`.
        fn has_voted(
            self: @ComponentState<TContractState>, proposal_id: felt252, account: ContractAddress,
        ) -> bool {
            GovernorCounting::has_voted(self, proposal_id, account)
        }

        /// Creates a new proposal. Voting starts after the delay specified by `voting_delay` and
        /// lasts for a duration specified by `voting_period`. Returns the id of the proposal.
        ///
        /// This function has opt-in frontrunning protection, described in
        /// `is_valid_description_for_proposer`.
        ///
        /// NOTE: The state of the Governor and targets may change between the proposal creation
        /// and its execution. This may be the result of third party actions on the targeted
        /// contracts, or other governor proposals. For example, the balance of this contract could
        /// be updated or its access control permissions may be modified, possibly compromising the
        /// proposal's ability to execute successfully (e.g. the governor doesn't have enough value
        /// to cover a proposal with multiple transfers).
        ///
        /// Requirements:
        ///
        /// - The proposer must be authorized to submit the proposal.
        /// - The proposer must have enough votes to submit the proposal if `proposal_threshold` is
        /// greater than zero.
        /// - The proposal must not already exist.
        ///
        /// Emits a `ProposalCreated` event.
        fn propose(
            ref self: ComponentState<TContractState>, calls: Span<Call>, description: ByteArray,
        ) -> felt252 {
            let proposer = starknet::get_caller_address();

            // Check description for restricted proposer
            assert(
                self.is_valid_description_for_proposer(proposer, @description),
                Errors::RESTRICTED_PROPOSER,
            );

            // Check proposal threshold
            let vote_threshold = self._proposal_threshold();
            if vote_threshold > 0 {
                let votes = self
                    ._get_votes(proposer, self.clock() - 1, Immutable::DEFAULT_PARAMS());
                assert(votes >= vote_threshold, Errors::INSUFFICIENT_PROPOSER_VOTES);
            }

            self._propose(calls, @description, proposer)
        }

        /// Queues a proposal. Some governors require this step to be performed before execution can
        /// happen. If queuing is not necessary, this function may revert.
        /// Queuing a proposal requires the quorum to be reached, the vote to be successful, and the
        /// deadline to be reached.
        ///
        /// Returns the id of the proposal.
        ///
        /// Requirements:
        ///
        /// - The proposal must be in the `Succeeded` state.
        /// - The queue operation must return a non-zero ETA.
        ///
        /// Emits a `ProposalQueued` event.
        fn queue(
            ref self: ComponentState<TContractState>, calls: Span<Call>, description_hash: felt252,
        ) -> felt252 {
            let proposal_id = self._hash_proposal(calls, description_hash);
            self.validate_state(proposal_id, array![ProposalState::Succeeded].span());

            let eta_seconds = self.queue_operations(proposal_id, calls, description_hash);
            assert(eta_seconds > 0, Errors::QUEUE_NOT_IMPLEMENTED);

            let mut proposal = self.Governor_proposals.read(proposal_id);
            proposal.eta_seconds = eta_seconds;
            self.Governor_proposals.write(proposal_id, proposal);

            self.emit(ProposalQueued { proposal_id, eta_seconds });

            proposal_id
        }

        /// Executes a successful proposal. This requires the quorum to be reached, the vote to be
        /// successful, and the deadline to be reached. Depending on the governor it might also be
        /// required that the proposal was queued and that some delay passed.
        ///
        /// NOTE: Some modules can modify the requirements for execution, for example by adding an
        /// additional timelock (See `timelock_controller`).
        ///
        /// Returns the id of the proposal.
        ///
        /// Requirements:
        ///
        /// - The proposal must be in the `Succeeded` or `Queued` state.
        ///
        /// Emits a `ProposalExecuted` event.
        fn execute(
            ref self: ComponentState<TContractState>, calls: Span<Call>, description_hash: felt252,
        ) -> felt252 {
            let proposal_id = self._hash_proposal(calls, description_hash);
            self
                .validate_state(
                    proposal_id, array![ProposalState::Succeeded, ProposalState::Queued].span(),
                );

            // Mark proposal as executed to avoid reentrancy
            let mut proposal = self.Governor_proposals.read(proposal_id);
            proposal.executed = true;
            self.Governor_proposals.write(proposal_id, proposal);

            self.execute_operations(proposal_id, calls, description_hash);

            self.emit(ProposalExecuted { proposal_id });

            proposal_id
        }

        /// Cancels a proposal. A proposal is cancellable by the proposer, but only while it is
        /// Pending state, i.e. before the vote starts.
        ///
        /// Returns the id of the proposal.
        ///
        /// Requirements:
        ///
        /// - The proposal must be in the `Pending` state.
        /// - The caller must be the proposer of the proposal.
        ///
        /// Emits a `ProposalCanceled` event.
        fn cancel(
            ref self: ComponentState<TContractState>, calls: Span<Call>, description_hash: felt252,
        ) -> felt252 {
            let proposal_id = self._hash_proposal(calls, description_hash);
            self.validate_state(proposal_id, array![ProposalState::Pending].span());

            assert(
                starknet::get_caller_address() == self.proposal_proposer(proposal_id),
                Errors::PROPOSER_ONLY,
            );
            self.cancel_operations(proposal_id, description_hash);

            self.emit(ProposalCanceled { proposal_id });

            proposal_id
        }

        /// Cast a vote.
        ///
        /// Requirements:
        ///
        /// - The proposal must be active.
        ///
        /// Emits a `VoteCast` event.
        fn cast_vote(
            ref self: ComponentState<TContractState>, proposal_id: felt252, support: u8,
        ) -> u256 {
            let voter = starknet::get_caller_address();
            self._cast_vote(proposal_id, voter, support, "", Immutable::DEFAULT_PARAMS())
        }

        /// Cast a vote with a `reason`.
        ///
        /// Requirements:
        ///
        /// - The proposal must be active.
        ///
        /// Emits a `VoteCast` event.
        fn cast_vote_with_reason(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            support: u8,
            reason: ByteArray,
        ) -> u256 {
            let voter = starknet::get_caller_address();
            self._cast_vote(proposal_id, voter, support, reason, Immutable::DEFAULT_PARAMS())
        }

        /// Cast a vote with a `reason` and additional serialized `params`.
        ///
        /// Requirements:
        ///
        /// - The proposal must be active.
        ///
        /// Emits either:
        /// - `VoteCast` event if no params are provided.
        /// - `VoteCastWithParams` event otherwise.
        fn cast_vote_with_reason_and_params(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            support: u8,
            reason: ByteArray,
            params: Span<felt252>,
        ) -> u256 {
            let voter = starknet::get_caller_address();
            self._cast_vote(proposal_id, voter, support, reason, params)
        }

        /// Cast a vote using the `voter`'s signature.
        ///
        /// Requirements:
        ///
        /// - The proposal must be active.
        /// - The nonce in the signed message must match the account's current nonce.
        /// - `voter` must implement `SRC6::is_valid_signature`.
        /// - `signature` should be valid for the message hash.
        ///
        /// Emits a `VoteCast` event.
        fn cast_vote_by_sig(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            support: u8,
            voter: ContractAddress,
            signature: Span<felt252>,
        ) -> u256 {
            // 1. Get and increase current nonce
            let nonce = self.use_nonce(voter);

            // 2. Build hash for calling `is_valid_signature`
            let verifying_contract = starknet::get_contract_address();
            let vote = Vote { verifying_contract, nonce, proposal_id, support, voter };
            let hash = vote.get_message_hash(voter);

            let is_valid_signature_felt = ISRC6Dispatcher { contract_address: voter }
                .is_valid_signature(hash, signature.into());

            // 3. Check either 'VALID' or true for backwards compatibility
            let is_valid_signature = is_valid_signature_felt == starknet::VALIDATED
                || is_valid_signature_felt == 1;

            assert(is_valid_signature, Errors::INVALID_SIGNATURE);

            // 4. Cast vote
            self._cast_vote(proposal_id, voter, support, "", Immutable::DEFAULT_PARAMS())
        }

        /// Cast a vote with a `reason` and additional serialized `params` using the `voter`'s
        /// signature.
        ///
        /// Requirements:
        ///
        /// - The proposal must be active.
        /// - The nonce in the signed message must match the account's current nonce.
        /// - `voter` must implement `SRC6::is_valid_signature`.
        /// - `signature` should be valid for the message hash.
        ///
        /// Emits either:
        /// - `VoteCast` event if no params are provided.
        /// - `VoteCastWithParams` event otherwise.
        fn cast_vote_with_reason_and_params_by_sig(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            support: u8,
            voter: ContractAddress,
            reason: ByteArray,
            params: Span<felt252>,
            signature: Span<felt252>,
        ) -> u256 {
            // 1. Get and increase current nonce
            let nonce = self.use_nonce(voter);

            // 2. Build hash for calling `is_valid_signature`
            let verifying_contract = starknet::get_contract_address();
            let reason_hash = reason.hash();
            let vote = VoteWithReasonAndParams {
                verifying_contract, nonce, proposal_id, support, voter, reason_hash, params,
            };
            let hash = vote.get_message_hash(voter);

            let is_valid_signature_felt = ISRC6Dispatcher { contract_address: voter }
                .is_valid_signature(hash, signature.into());

            // 3. Check either 'VALID' or true for backwards compatibility
            let is_valid_signature = is_valid_signature_felt == starknet::VALIDATED
                || is_valid_signature_felt == 1;

            assert(is_valid_signature, Errors::INVALID_SIGNATURE);

            // 4. Cast vote
            self._cast_vote(proposal_id, voter, support, reason, params)
        }

        /// Returns the next unused nonce for an address.
        fn nonces(self: @ComponentState<TContractState>, voter: ContractAddress) -> felt252 {
            self.Governor_nonces.read(voter)
        }

        /// Relays a transaction or function call to an arbitrary target.
        ///
        /// In cases where the governance executor is some contract other than the governor itself,
        /// like when using a timelock, this function can be invoked in a governance proposal to
        /// recover tokens that was sent to the governor contract by mistake.
        ///
        /// NOTE: If the executor is simply the governor itself, use of `relay` is redundant.
        fn relay(ref self: ComponentState<TContractState>, call: Call) {
            self.assert_only_governance();

            let Call { to, selector, calldata } = call;
            starknet::syscalls::call_contract_syscall(to, selector, calldata).unwrap_syscall();
        }
    }

    //
    // Internal
    //

    #[generate_trait]
    pub impl InternalImpl<
        TContractState,
        +HasComponent<TContractState>,
        impl SRC5: SRC5Component::HasComponent<TContractState>,
        +Drop<TContractState>,
    > of InternalTrait<TContractState> {
        /// Initializes the contract by registering the supported interface id.
        fn initializer(ref self: ComponentState<TContractState>) {
            let mut src5_component = get_dep_component_mut!(ref self, SRC5);
            src5_component.register_interface(IGOVERNOR_ID);
        }

        /// Returns the proposal object given its id.
        fn get_proposal(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> ProposalCore {
            self.Governor_proposals.read(proposal_id)
        }

        /// Checks if the proposer is authorized to submit a proposal with the given description.
        ///
        /// If the proposal description ends with `#proposer=0x???`, where `0x???` is an address
        /// written as a hex string (case insensitive), then the submission of this proposal will
        /// only be authorized to said address.
        ///
        /// This is used for frontrunning protection. By adding this pattern at the end of their
        /// proposal, one can ensure that no other address can submit the same proposal. An attacker
        /// would have to either remove or change that part, which would result in a different
        /// proposal id.
        ///
        /// NOTE: In Starknet, the Sequencer ensures the order of transactions, but frontrunning
        /// can still be achieved by nodes, and potentially other actors in the future with
        /// sequencer decentralization.
        ///
        /// If the description does not match this pattern, it is unrestricted and anyone can submit
        /// it. This includes:
        /// - If the `0x???` part is not a valid hex string.
        /// - If the `0x???` part is a valid hex string, but does not contain exactly 64 hex digits.
        /// - If it ends with the expected suffix followed by newlines or other whitespace.
        /// - If it ends with some other similar suffix, e.g. `#other=abc`.
        /// - If it does not end with any such suffix.
        fn is_valid_description_for_proposer(
            self: @ComponentState<TContractState>,
            proposer: ContractAddress,
            description: @ByteArray,
        ) -> bool {
            let length = description.len();

            // Length is too short to contain a valid proposer suffix
            if description.len() < 76 {
                return true;
            }

            // Extract what would be the `#proposer=` marker beginning the suffix
            let marker = description.read_n_bytes(length - 76, 10);

            // If the marker is not found, there is no proposer suffix to check
            if marker != "#proposer=" {
                return true;
            }

            let expected_address = description.read_n_bytes(length - 64, 64);

            proposer.to_byte_array(16, 64) == expected_address
        }

        /// Returns a hash of the proposal using the Pedersen hashing algorithm.
        fn _hash_proposal(
            self: @ComponentState<TContractState>, calls: Span<Call>, description_hash: felt252,
        ) -> felt252 {
            PedersenTrait::new(0).update_with(calls).update_with(description_hash).finalize()
        }

        /// Timepoint used to retrieve user's votes and quorum. If using block number, the snapshot
        /// is performed at the end of this block. Hence, voting for this proposal starts at the
        /// beginning of the following block.
        fn _proposal_snapshot(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            self.Governor_proposals.read(proposal_id).vote_start
        }

        /// Timepoint at which votes close. If using block number, votes close at the end of this
        /// block, so it is possible to cast a vote during this block.
        fn _proposal_deadline(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            let proposal = self.Governor_proposals.read(proposal_id);
            proposal.vote_start + proposal.vote_duration
        }

        /// The account that created a proposal.
        fn _proposal_proposer(
            self: @ComponentState<TContractState>, proposal_id: felt252,
        ) -> ContractAddress {
            self.Governor_proposals.read(proposal_id).proposer
        }

        /// The time when a queued proposal becomes executable ("ETA"). Unlike `proposal_snapshot`
        /// and `proposal_deadline`, this doesn't use the governor clock, and instead relies on the
        /// executor's clock which may be different. In most cases this will be a timestamp.
        fn _proposal_eta(self: @ComponentState<TContractState>, proposal_id: felt252) -> u64 {
            self.Governor_proposals.read(proposal_id).eta_seconds
        }
    }

    #[generate_trait]
    pub impl InternalExtendedImpl<
        TContractState,
        +HasComponent<TContractState>,
        +SRC5Component::HasComponent<TContractState>,
        +GovernorCountingTrait<TContractState>,
        +GovernorExecutionTrait<TContractState>,
        impl GovernorSettings: GovernorSettingsTrait<TContractState>,
        impl GovernorVotes: GovernorVotesTrait<TContractState>,
        +Drop<TContractState>,
    > of InternalExtendedTrait<TContractState> {
        /// Asserts that the caller is the governance executor.
        ///
        /// WARNING: When the executor is not the governor itself (i.e. a timelock), it can call
        /// functions that are restricted with this modifier, and also potentially execute
        /// transactions on behalf of the governor. Because of this, this module is designed to work
        /// with the TimelockController as the unique potential external executor. The timelock
        /// MUST have the governor as the only proposer, canceller, and executor.
        fn assert_only_governance(self: @ComponentState<TContractState>) {
            let executor = self.executor();
            assert(executor == starknet::get_caller_address(), Errors::EXECUTOR_ONLY);
        }

        /// Validates that the proposal is in one of the expected states.
        fn validate_state(
            self: @ComponentState<TContractState>,
            proposal_id: felt252,
            allowed_states: Span<ProposalState>,
        ) {
            let current_state = self.state(proposal_id);
            let mut found = false;
            for state in allowed_states {
                if current_state == *state {
                    found = true;
                    break;
                }
            }
            assert(found, Errors::UNEXPECTED_PROPOSAL_STATE);
        }

        /// Consumes a nonce, returns the current value, and increments nonce.
        fn use_nonce(ref self: ComponentState<TContractState>, voter: ContractAddress) -> felt252 {
            // For each account, the nonce has an initial value of 0, can only be incremented by
            // one, and cannot be decremented or reset. This guarantees that the nonce never
            // overflows.
            let nonce = self.Governor_nonces.read(voter);
            self.Governor_nonces.write(voter, nonce + 1);
            nonce
        }

        /// Internal wrapper for `GovernorVotesTrait::get_votes`.
        fn _get_votes(
            self: @ComponentState<TContractState>,
            account: ContractAddress,
            timepoint: u64,
            params: Span<felt252>,
        ) -> u256 {
            GovernorVotes::get_votes(self, account, timepoint, params)
        }

        /// Internal wrapper for `GovernorProposeTrait::proposal_threshold`.
        fn _proposal_threshold(self: @ComponentState<TContractState>) -> u256 {
            GovernorSettings::proposal_threshold(self)
        }

        /// Returns the state of a proposal, given its id.
        ///
        /// Requirements:
        ///
        /// - The proposal must exist.
        fn _state(self: @ComponentState<TContractState>, proposal_id: felt252) -> ProposalState {
            let proposal = self.get_proposal(proposal_id);

            if proposal.executed {
                return ProposalState::Executed;
            }
            if proposal.canceled {
                return ProposalState::Canceled;
            }

            let snapshot = self._proposal_snapshot(proposal_id);
            assert(snapshot.is_non_zero(), Errors::NONEXISTENT_PROPOSAL);

            let current_timepoint = self.clock();
            if current_timepoint < snapshot {
                return ProposalState::Pending;
            }

            let deadline = self._proposal_deadline(proposal_id);

            if current_timepoint <= deadline {
                return ProposalState::Active;
            } else if !self.quorum_reached(proposal_id) || !self.vote_succeeded(proposal_id) {
                return ProposalState::Defeated;
            } else if self._proposal_eta(proposal_id).is_zero() {
                return ProposalState::Succeeded;
            } else {
                return ProposalState::Queued;
            }
        }

        /// Internal propose mechanism. Returns the proposal id.
        ///
        /// Requirements:
        ///
        /// - The proposal must not already exist.
        ///
        /// Emits a `ProposalCreated` event.
        fn _propose(
            ref self: ComponentState<TContractState>,
            calls: Span<Call>,
            description: @ByteArray,
            proposer: ContractAddress,
        ) -> felt252 {
            let proposal_id = self._hash_proposal(calls, description.hash());

            assert(
                self.Governor_proposals.read(proposal_id).vote_start == 0,
                Errors::EXISTENT_PROPOSAL,
            );

            let snapshot = self.clock() + self.voting_delay();
            let duration = self.voting_period();

            let proposal = ProposalCore {
                proposer,
                vote_start: snapshot,
                vote_duration: duration,
                executed: false,
                canceled: false,
                eta_seconds: 0,
            };

            self.Governor_proposals.write(proposal_id, proposal);
            self
                .emit(
                    ProposalCreated {
                        proposal_id,
                        proposer,
                        calls,
                        signatures: array![].span(),
                        vote_start: snapshot,
                        vote_end: snapshot + duration,
                        description: description.clone(),
                    },
                );

            proposal_id
        }

        /// Internal cancel mechanism with minimal restrictions.
        /// A proposal can be cancelled in any state other than Canceled or Executed.
        ///
        /// NOTE: Once cancelled, a proposal can't be re-submitted.
        fn _cancel(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            description_hash: felt252,
        ) {
            let valid_states = array![
                ProposalState::Pending,
                ProposalState::Active,
                ProposalState::Defeated,
                ProposalState::Succeeded,
                ProposalState::Queued,
            ];
            self.validate_state(proposal_id, valid_states.span());

            let mut proposal = self.Governor_proposals.read(proposal_id);
            proposal.canceled = true;
            self.Governor_proposals.write(proposal_id, proposal);
        }

        /// Internal wrapper for `GovernorCountingTrait::count_vote`.
        fn _count_vote(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            account: ContractAddress,
            support: u8,
            total_weight: u256,
            params: Span<felt252>,
        ) -> u256 {
            self.count_vote(proposal_id, account, support, total_weight, params)
        }

        /// Internal vote-casting mechanism.
        ///
        /// Checks that the vote is pending and that it has not been cast yet.
        /// This function retrieves the voting weight using `get_votes` and then calls
        /// the `_count_vote` internal function.
        ///
        /// Emits either:
        /// - `VoteCast` event if no params are provided.
        /// - `VoteCastWithParams` event otherwise.
        fn _cast_vote(
            ref self: ComponentState<TContractState>,
            proposal_id: felt252,
            voter: ContractAddress,
            support: u8,
            reason: ByteArray,
            params: Span<felt252>,
        ) -> u256 {
            self.validate_state(proposal_id, array![ProposalState::Active].span());

            let snapshot = self._proposal_snapshot(proposal_id);
            let total_weight = self._get_votes(voter, snapshot, params);
            let voted_weight = self._count_vote(proposal_id, voter, support, total_weight, params);

            if params.len().is_zero() {
                self.emit(VoteCast { voter, proposal_id, support, weight: voted_weight, reason });
            } else {
                self
                    .emit(
                        VoteCastWithParams {
                            voter, proposal_id, support, weight: voted_weight, reason, params,
                        },
                    );
            }

            // TODO: add tally hook when the PreventLateQuorum extension gets added

            voted_weight
        }
    }
}

/// Implementation of the default Governor ImmutableConfig.
///
/// See
/// https://github.com/starknet-io/SNIPs/blob/main/SNIPS/snip-107.md#defaultconfig-implementation
///
/// The `DEFAULT_PARAMS` is set to an empty span of felts.
pub impl DefaultConfig of GovernorComponent::ImmutableConfig {
    fn DEFAULT_PARAMS() -> Span<felt252> {
        array![].span()
    }
}
