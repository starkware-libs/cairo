#[starknet::contract]
mod c1 {
    use starknet::storage::Map;

    #[starknet::interface]
    trait IMy<T> {
        fn a(self: @T);
    }

    #[storage]
    struct Storage {
        v1: Map<felt252, (u32, u32)>,
    }

    #[abi(embed_v0)]
    impl My of IMy<ContractState> {
        fn a(self: @ContractState) {
            let (_one, _two) = self.v1.read(0);
        }
    }
}
