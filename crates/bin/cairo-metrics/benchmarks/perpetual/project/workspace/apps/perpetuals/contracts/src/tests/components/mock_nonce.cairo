#[starknet::contract]
pub mod NonceMock {
    use openzeppelin::access::accesscontrol::AccessControlComponent;
    use openzeppelin::introspection::src5::SRC5Component;
    use perpetuals::core::components::operator_nonce::OperatorNonceComponent;
    use starknet::ContractAddress;
    use starkware_utils::components::roles::RolesComponent;
    use starkware_utils::components::roles::RolesComponent::InternalTrait as RolesInternal;

    component!(path: OperatorNonceComponent, storage: nonce, event: NonceEvent);
    component!(path: RolesComponent, storage: roles, event: RolesEvent);
    component!(path: AccessControlComponent, storage: accesscontrol, event: AccessControlEvent);
    component!(path: SRC5Component, storage: src5, event: SRC5Event);


    #[abi(embed_v0)]
    impl OperatorNonceImpl =
        OperatorNonceComponent::OperatorNonceImpl<ContractState>;

    #[abi(embed_v0)]
    impl RolesImpl = RolesComponent::RolesImpl<ContractState>;

    #[storage]
    pub struct Storage {
        #[substorage(v0)]
        pub nonce: OperatorNonceComponent::Storage,
        #[substorage(v0)]
        pub roles: RolesComponent::Storage,
        #[substorage(v0)]
        accesscontrol: AccessControlComponent::Storage,
        #[substorage(v0)]
        src5: SRC5Component::Storage,
    }

    #[constructor]
    pub fn constructor(ref self: ContractState, governance_admin: ContractAddress) {
        self.roles.initialize(:governance_admin);
    }

    #[event]
    #[derive(Drop, starknet::Event)]
    enum Event {
        #[flat]
        NonceEvent: OperatorNonceComponent::Event,
        #[flat]
        RolesEvent: RolesComponent::Event,
        #[flat]
        AccessControlEvent: AccessControlComponent::Event,
        #[flat]
        SRC5Event: SRC5Component::Event,
    }
}
