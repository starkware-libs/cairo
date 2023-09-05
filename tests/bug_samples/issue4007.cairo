#[starknet::interface]
trait ISomeInterface<TContractState> {
    fn assert_valid(self: @TContractState);
}

#[starknet::contract]
mod lama_test {
    use super::ISomeInterface;

    #[storage]
    struct Storage {}

    #[external(v0)]
    impl SomeInterfaceImpl of ISomeInterface<ContractState> {
        fn assert_valid(self: @ContractState) {}
    }
}

#[test]
#[should_panic(expected: ('CLASS_HASH_NOT_DECLARED',))]
#[available_gas(2000000)]
fn lama() {
    let check_plugin_type = ISomeInterfaceLibraryDispatcher { class_hash: 32.try_into().unwrap() }
        .assert_valid();
}
