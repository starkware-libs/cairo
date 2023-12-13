use openzeppelin::introspection::interface;
use openzeppelin::introspection::first;

#[starknet::contract]
mod SRC5 {
    use openzeppelin::introspection::interface;
    use openzeppelin::introspection::{interface, AB};

    #[storage]
    struct Storage {
        supported_interfaces: LegacyMap<felt252, bool>
    }

    use openzeppelin::introspection::first;

    mod A {}
    mod G;
    mod F;

    #[abi(embed_v0)]
    impl SRC5Impl of interface::ISRC5<ContractState> {
        fn supports_interface(self: @ContractState, interface_id: felt252) -> bool {
            true
        }
    }

    use A;
    use starknet::ArrayTrait;

    mod Inner {
        use C;
        use B;
    }
}
