#[starknet::contract(account)]
#[with_components(Account, SRC9, SRC5)]
pub mod SRC9AccountMock {
    use starknet::storage::StoragePointerWriteAccess;

    // Account
    #[abi(embed_v0)]
    impl SRC6Impl = AccountComponent::SRC6Impl<ContractState>;
    #[abi(embed_v0)]
    impl PublicKeyImpl = AccountComponent::PublicKeyImpl<ContractState>;

    // SRC9
    #[abi(embed_v0)]
    impl OutsideExecutionV2Impl =
        SRC9Component::OutsideExecutionV2Impl<ContractState>;

    // SRC5
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {
        pub value: felt252,
    }

    #[constructor]
    fn constructor(ref self: ContractState, public_key: felt252) {
        self.account.initializer(public_key);
        self.src9.initializer();
    }

    #[abi(per_item)]
    #[generate_trait]
    impl ExternalImpl of ExternalTrait {
        #[external(v0)]
        fn set_value(ref self: ContractState, value: felt252, panic: bool) {
            self.account.assert_only_self();
            if panic {
                panic!("Some error");
            }
            self.value.write(value);
        }
    }
}
