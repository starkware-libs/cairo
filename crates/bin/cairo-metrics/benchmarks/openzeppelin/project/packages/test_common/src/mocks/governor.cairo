#[starknet::contract]
#[with_components(Governor, GovernorVotes, GovernorCountingSimple, GovernorCoreExecution, SRC5)]
pub mod GovernorMock {
    use core::num::traits::Bounded;
    use openzeppelin_governance::governor::DefaultConfig;
    use openzeppelin_utils::cryptography::snip12::SNIP12Metadata;
    use starknet::ContractAddress;

    pub const VOTING_DELAY: u64 = 86400; // 1 day
    pub const VOTING_PERIOD: u64 = 604800; // 1 week
    pub const PROPOSAL_THRESHOLD: u256 = 10;
    pub const QUORUM: u256 = 100_000_000;

    // Governor
    #[abi(embed_v0)]
    impl GovernorImpl = GovernorComponent::GovernorImpl<ContractState>;

    // Extensions external
    #[abi(embed_v0)]
    impl VotesTokenImpl = GovernorVotesComponent::VotesTokenImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, votes_token: ContractAddress) {
        self.governor.initializer();
        self.governor_votes.initializer(votes_token);
    }

    //
    // SNIP12 Metadata
    //

    pub impl SNIP12MetadataImpl of SNIP12Metadata {
        fn name() -> felt252 {
            'DAPP_NAME'
        }

        fn version() -> felt252 {
            'DAPP_VERSION'
        }
    }

    //
    // Locally implemented extensions
    //

    impl GovernorQuorum of GovernorComponent::GovernorQuorumTrait<ContractState> {
        /// See `GovernorComponent::GovernorQuorumTrait::quorum`.
        fn quorum(self: @GovernorComponent::ComponentState<ContractState>, timepoint: u64) -> u256 {
            if timepoint == Bounded::MAX {
                Bounded::MAX
            } else {
                QUORUM
            }
        }
    }

    pub impl GovernorSettings of GovernorComponent::GovernorSettingsTrait<ContractState> {
        /// See `GovernorComponent::GovernorSettingsTrait::voting_delay`.
        fn voting_delay(self: @GovernorComponent::ComponentState<ContractState>) -> u64 {
            VOTING_DELAY
        }

        /// See `GovernorComponent::GovernorSettingsTrait::voting_period`.
        fn voting_period(self: @GovernorComponent::ComponentState<ContractState>) -> u64 {
            VOTING_PERIOD
        }

        /// See `GovernorComponent::GovernorSettingsTrait::proposal_threshold`.
        fn proposal_threshold(self: @GovernorComponent::ComponentState<ContractState>) -> u256 {
            PROPOSAL_THRESHOLD
        }
    }
}

#[starknet::contract]
#[with_components(
    Governor, GovernorVotesQuorumFraction, GovernorCountingSimple, GovernorCoreExecution, SRC5,
)]
pub mod GovernorQuorumFractionMock {
    use openzeppelin_governance::governor::DefaultConfig;
    use openzeppelin_utils::cryptography::snip12::SNIP12Metadata;
    use starknet::ContractAddress;

    pub const VOTING_DELAY: u64 = 86400; // 1 day
    pub const VOTING_PERIOD: u64 = 604800; // 1 week
    pub const PROPOSAL_THRESHOLD: u256 = 10;
    pub const QUORUM_NUMERATOR: u256 = 600; // 60%

    // Governor
    #[abi(embed_v0)]
    impl GovernorImpl = GovernorComponent::GovernorImpl<ContractState>;

    // Extensions external
    #[abi(embed_v0)]
    impl QuorumFractionImpl =
        GovernorVotesQuorumFractionComponent::QuorumFractionImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, votes_token: ContractAddress) {
        self.governor.initializer();
        self.governor_votes_quorum_fraction.initializer(votes_token, QUORUM_NUMERATOR);
    }

    //
    // SNIP12 Metadata
    //

    pub impl SNIP12MetadataImpl of SNIP12Metadata {
        fn name() -> felt252 {
            'DAPP_NAME'
        }

        fn version() -> felt252 {
            'DAPP_VERSION'
        }
    }

    //
    // Locally implemented extensions
    //

    pub impl GovernorSettings of GovernorComponent::GovernorSettingsTrait<ContractState> {
        /// See `GovernorComponent::GovernorSettingsTrait::voting_delay`.
        fn voting_delay(self: @GovernorComponent::ComponentState<ContractState>) -> u64 {
            VOTING_DELAY
        }

        /// See `GovernorComponent::GovernorSettingsTrait::voting_period`.
        fn voting_period(self: @GovernorComponent::ComponentState<ContractState>) -> u64 {
            VOTING_PERIOD
        }

        /// See `GovernorComponent::GovernorSettingsTrait::proposal_threshold`.
        fn proposal_threshold(self: @GovernorComponent::ComponentState<ContractState>) -> u256 {
            PROPOSAL_THRESHOLD
        }
    }
}

#[starknet::contract]
#[with_components(
    Governor,
    GovernorVotes,
    GovernorSettings,
    GovernorCountingSimple,
    GovernorTimelockExecution,
    SRC5,
)]
pub mod GovernorTimelockedMock {
    use openzeppelin_governance::governor::DefaultConfig;
    use openzeppelin_utils::cryptography::snip12::SNIP12Metadata;
    use starknet::ContractAddress;

    pub const VOTING_DELAY: u64 = 86400; // 1 day
    pub const VOTING_PERIOD: u64 = 604800; // 1 week
    pub const PROPOSAL_THRESHOLD: u256 = 10;
    pub const QUORUM: u256 = 100_000_000;

    // Governor
    #[abi(embed_v0)]
    impl GovernorImpl = GovernorComponent::GovernorImpl<ContractState>;

    // Extensions external
    #[abi(embed_v0)]
    impl VotesTokenImpl = GovernorVotesComponent::VotesTokenImpl<ContractState>;
    #[abi(embed_v0)]
    impl GovernorSettingsAdminImpl =
        GovernorSettingsComponent::GovernorSettingsAdminImpl<ContractState>;
    #[abi(embed_v0)]
    impl TimelockedImpl =
        GovernorTimelockExecutionComponent::TimelockedImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState, votes_token: ContractAddress, timelock_controller: ContractAddress,
    ) {
        self.governor.initializer();
        self.governor_votes.initializer(votes_token);
        self.governor_settings.initializer(VOTING_DELAY, VOTING_PERIOD, PROPOSAL_THRESHOLD);
        self.governor_timelock_execution.initializer(timelock_controller);
    }

    //
    // SNIP12 Metadata
    //

    pub impl SNIP12MetadataImpl of SNIP12Metadata {
        fn name() -> felt252 {
            'DAPP_NAME'
        }

        fn version() -> felt252 {
            'DAPP_VERSION'
        }
    }

    //
    // Locally implemented extensions
    //

    impl GovernorQuorum of GovernorComponent::GovernorQuorumTrait<ContractState> {
        /// See `GovernorComponent::GovernorQuorumTrait::quorum`.
        fn quorum(self: @GovernorComponent::ComponentState<ContractState>, timepoint: u64) -> u256 {
            QUORUM
        }
    }

    #[abi(per_item)]
    #[generate_trait]
    impl ExternalImpl of ExternalTrait {
        #[external(v0)]
        fn cancel_operations(
            ref self: ContractState, proposal_id: felt252, description_hash: felt252,
        ) {
            self.governor.cancel_operations(proposal_id, description_hash);
        }

        #[external(v0)]
        fn timelock_salt(ref self: ContractState, description_hash: felt252) -> felt252 {
            self.governor_timelock_execution.timelock_salt(description_hash)
        }
    }
}

#[starknet::interface]
pub trait CancelOperations<TContractState> {
    fn cancel_operations(ref self: TContractState, proposal_id: felt252, description_hash: felt252);
}

#[starknet::interface]
pub trait TimelockSalt<TContractState> {
    fn timelock_salt(ref self: TContractState, description_hash: felt252) -> felt252;
}
