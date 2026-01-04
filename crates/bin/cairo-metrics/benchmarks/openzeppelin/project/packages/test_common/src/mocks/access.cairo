#[starknet::contract]
#[with_components(AccessControl, SRC5)]
pub mod DualCaseAccessControlMock {
    use openzeppelin_access::accesscontrol::DEFAULT_ADMIN_ROLE;
    use starknet::ContractAddress;

    // AccessControlMixin
    #[abi(embed_v0)]
    impl AccessControlMixinImpl =
        AccessControlComponent::AccessControlMixinImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, admin: ContractAddress) {
        self.access_control.initializer();
        self.access_control._grant_role(DEFAULT_ADMIN_ROLE, admin);
    }
}

#[starknet::contract]
#[with_components(Ownable)]
pub mod DualCaseOwnableMock {
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl OwnableMixinImpl = OwnableComponent::OwnableMixinImpl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, owner: ContractAddress) {
        self.ownable.initializer(owner);
    }
}

#[starknet::contract]
#[with_components(Ownable)]
pub mod DualCaseTwoStepOwnableMock {
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl OwnableTwoStepMixinImpl =
        OwnableComponent::OwnableTwoStepMixinImpl<ContractState>;

    #[storage]
    pub struct Storage {}


    #[constructor]
    fn constructor(ref self: ContractState, owner: ContractAddress) {
        self.ownable.initializer(owner);
    }
}
