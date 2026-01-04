#[starknet::contract]
#[with_components(ERC2981, SRC5)]
pub mod ERC2981Mock {
    use openzeppelin_token::common::erc2981::DefaultConfig;
    use starknet::ContractAddress;

    #[abi(embed_v0)]
    impl ERC2981Impl = ERC2981Component::ERC2981Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC2981InfoImpl = ERC2981Component::ERC2981InfoImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        owner: ContractAddress,
        default_receiver: ContractAddress,
        default_royalty_fraction: u128,
    ) {
        self.erc2981.initializer(default_receiver, default_royalty_fraction);
    }
}

#[starknet::contract]
#[with_components(ERC2981, SRC5, Ownable)]
pub mod ERC2981OwnableMock {
    use openzeppelin_token::common::erc2981::DefaultConfig;
    use starknet::ContractAddress;

    // ERC2981
    #[abi(embed_v0)]
    impl ERC2981Impl = ERC2981Component::ERC2981Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC2981InfoImpl = ERC2981Component::ERC2981InfoImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC2981AdminOwnableImpl =
        ERC2981Component::ERC2981AdminOwnableImpl<ContractState>;

    // Ownable
    #[abi(embed_v0)]
    impl OwnableImpl = OwnableComponent::OwnableImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        owner: ContractAddress,
        default_receiver: ContractAddress,
        default_royalty_fraction: u128,
    ) {
        self.erc2981.initializer(default_receiver, default_royalty_fraction);
        self.ownable.initializer(owner);
    }
}

#[starknet::contract]
#[with_components(ERC2981, SRC5, AccessControl)]
pub mod ERC2981AccessControlMock {
    use openzeppelin_access::accesscontrol::DEFAULT_ADMIN_ROLE;
    use openzeppelin_token::common::erc2981::DefaultConfig;
    use openzeppelin_token::common::erc2981::ERC2981Component::ROYALTY_ADMIN_ROLE;
    use starknet::ContractAddress;

    // ERC2981
    #[abi(embed_v0)]
    impl ERC2981Impl = ERC2981Component::ERC2981Impl<ContractState>;
    #[abi(embed_v0)]
    impl ERC2981InfoImpl = ERC2981Component::ERC2981InfoImpl<ContractState>;
    #[abi(embed_v0)]
    impl ERC2981AdminAccessControlImpl =
        ERC2981Component::ERC2981AdminAccessControlImpl<ContractState>;

    // AccessControl
    #[abi(embed_v0)]
    impl AccessControlImpl =
        AccessControlComponent::AccessControlImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        owner: ContractAddress,
        default_receiver: ContractAddress,
        default_royalty_fraction: u128,
    ) {
        self.erc2981.initializer(default_receiver, default_royalty_fraction);
        self.access_control.initializer();
        self.access_control._grant_role(DEFAULT_ADMIN_ROLE, owner);
        self.access_control._grant_role(ROYALTY_ADMIN_ROLE, owner);
    }
}
