#[starknet::contract]
#[with_components(Nonces)]
pub mod NoncesMock {
    #[abi(embed_v0)]
    impl NoncesImpl = NoncesComponent::NoncesImpl<ContractState>;

    #[storage]
    pub struct Storage {}
}
