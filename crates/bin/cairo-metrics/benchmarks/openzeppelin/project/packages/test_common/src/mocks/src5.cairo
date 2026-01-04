#[starknet::contract]
#[with_components(SRC5)]
pub mod SRC5Mock {
    #[abi(embed_v0)]
    impl SRC5Impl = SRC5Component::SRC5Impl<ContractState>;

    #[storage]
    pub struct Storage {}
}
