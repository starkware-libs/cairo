use openzeppelin::introspection::first;
use openzeppelin::introspection::interface;

#[starknet::contract]
mod SRC5 {
    mod F;
    mod G;

    use A;

    use openzeppelin::introspection::first;
    use openzeppelin::introspection::interface;
    use openzeppelin::introspection::{interface, AB};
    use starknet::ArrayTrait;

    #[storage]
    struct Storage {
        supported_interfaces: LegacyMap<felt252, bool>
    }

    mod A {}

    #[abi(embed_v0)]
    impl SRC5Impl of interface::ISRC5<ContractState> {
        fn supports_interface(self: @ContractState, interface_id: felt252) -> bool {
            true
        }
    }

    mod Inner {
        use B;
        use C;
    }
}
