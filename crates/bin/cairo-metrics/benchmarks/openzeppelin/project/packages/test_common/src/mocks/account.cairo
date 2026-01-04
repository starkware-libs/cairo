#[starknet::contract(account)]
#[with_components(Account, SRC5)]
pub mod DualCaseAccountMock {
    // Account
    #[abi(embed_v0)]
    impl SRC6Impl = AccountComponent::SRC6Impl<ContractState>;
    #[abi(embed_v0)]
    impl SRC6CamelOnlyImpl = AccountComponent::SRC6CamelOnlyImpl<ContractState>;
    #[abi(embed_v0)]
    impl DeclarerImpl = AccountComponent::DeclarerImpl<ContractState>;
    #[abi(embed_v0)]
    impl DeployableImpl = AccountComponent::DeployableImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, public_key: felt252) {
        self.account.initializer(public_key);
    }
}

#[starknet::contract(account)]
#[with_components(Account, SRC5)]
pub mod SnakeAccountMock {
    // Account
    #[abi(embed_v0)]
    impl SRC6Impl = AccountComponent::SRC6Impl<ContractState>;
    #[abi(embed_v0)]
    impl PublicKeyImpl = AccountComponent::PublicKeyImpl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}


    #[constructor]
    fn constructor(ref self: ContractState, public_key: felt252) {
        self.account.initializer(public_key);
    }
}

#[starknet::contract(account)]
#[with_components(EthAccount, SRC5)]
pub mod DualCaseEthAccountMock {
    use openzeppelin_account::interface::EthPublicKey;


    #[abi(embed_v0)]
    impl SRC6Impl = EthAccountComponent::SRC6Impl<ContractState>;
    #[abi(embed_v0)]
    impl SRC6CamelOnlyImpl = EthAccountComponent::SRC6CamelOnlyImpl<ContractState>;
    #[abi(embed_v0)]
    impl DeclarerImpl = EthAccountComponent::DeclarerImpl<ContractState>;
    #[abi(embed_v0)]
    impl DeployableImpl = EthAccountComponent::DeployableImpl<ContractState>;

    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, public_key: EthPublicKey) {
        self.eth_account.initializer(public_key);
    }
}

#[starknet::contract(account)]
#[with_components(EthAccount, SRC5)]
pub mod SnakeEthAccountMock {
    use openzeppelin_account::interface::EthPublicKey;

    #[abi(embed_v0)]
    impl SRC6Impl = EthAccountComponent::SRC6Impl<ContractState>;
    #[abi(embed_v0)]
    impl PublicKeyImpl = EthAccountComponent::PublicKeyImpl<ContractState>;

    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, public_key: EthPublicKey) {
        self.eth_account.initializer(public_key);
    }
}
