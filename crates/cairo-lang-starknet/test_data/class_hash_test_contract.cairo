// A minimal contract whose external entry point returns the contract's own class hash via the
// generated `__class_hash__::class_hash()` accessor. Used to exercise the two-pass class-hash
// injection (see `compile_test::class_hash_injection_compiles`).
#[starknet::contract]
mod class_hash_test_contract {
    #[storage]
    struct Storage {}

    #[external(v0)]
    fn get_class_hash(self: @ContractState) -> starknet::ClassHash {
        __class_hash__::class_hash()
    }
}
