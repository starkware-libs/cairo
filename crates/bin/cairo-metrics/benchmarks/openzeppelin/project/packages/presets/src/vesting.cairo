// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (presets/src/vesting.cairo)

#[starknet::contract]
pub mod VestingWallet {
    use openzeppelin_access::ownable::OwnableComponent;
    use openzeppelin_finance::vesting::{LinearVestingSchedule, VestingComponent};
    use starknet::ContractAddress;

    component!(path: OwnableComponent, storage: ownable, event: OwnableEvent);
    component!(path: VestingComponent, storage: vesting, event: VestingEvent);

    // Ownable Mixin
    #[abi(embed_v0)]
    impl OwnableMixinImpl = OwnableComponent::OwnableMixinImpl<ContractState>;
    impl OwnableInternalImpl = OwnableComponent::InternalImpl<ContractState>;

    // Vesting
    #[abi(embed_v0)]
    impl VestingImpl = VestingComponent::VestingImpl<ContractState>;
    impl VestingInternalImpl = VestingComponent::InternalImpl<ContractState>;

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub ownable: OwnableComponent::Storage,
        #[substorage(v0)]
        pub vesting: VestingComponent::Storage,
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        #[flat]
        OwnableEvent: OwnableComponent::Event,
        #[flat]
        VestingEvent: VestingComponent::Event,
    }

    /// Initializes the vesting component by setting the vesting `start`, `duration` and
    /// `cliff_duration`.
    /// Assigns `beneficiary` as the contract owner and the vesting beneficiary.
    ///
    /// Requirements:
    ///
    /// - `cliff_duration` must be less than or equal to `duration`.
    #[constructor]
    fn constructor(
        ref self: ContractState,
        beneficiary: ContractAddress,
        start: u64,
        duration: u64,
        cliff_duration: u64,
    ) {
        self.ownable.initializer(beneficiary);
        self.vesting.initializer(start, duration, cliff_duration);
    }
}
