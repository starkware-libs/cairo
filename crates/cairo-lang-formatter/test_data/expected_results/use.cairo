use openzeppelin::introspection::first;
use openzeppelin::introspection::interface;


#[starknet::contract]
mod SRC5 {
    use A;

    use openzeppelin::introspection::first;
    use openzeppelin::introspection::interface;
    use starknet::ArrayTrait;
    mod A {}

    mod Inner {
        use B;
        use C;
    }

    #[storage]
    struct Storage {
        supported_interfaces: LegacyMap<felt252, bool>
    }

    #[external(v0)]
    impl SRC5Impl of interface::ISRC5<ContractState> {
        fn supports_interface(self: @ContractState, interface_id: felt252) -> bool {
            true
        }
    }
}
