// Demonstrates STATIC interface forwarding via the compile-time class-hash feature: `static_proxy`
// forwards the `ICounterContract` interface to `counter_contract`'s class using the generated
// `counter_contract::__class_hash__::ForwardingClassHashImpl` (the contract's own class hash,
// injected at Sierra generation). Unlike the dynamic `proxy` contract, it stores no class hash and
// needs no constructor. Exercised by `compile_test::static_forwarding_compiles`.
#[starknet::interface]
pub trait ICounterContract<TContractState> {
    fn increase_counter(ref self: TContractState, amount: u128);
    fn get_counter(self: @TContractState) -> u128;
}

/// The implementation contract whose class is forwarded to.
#[starknet::contract]
pub mod counter_contract {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    struct Storage {
        counter: u128,
    }

    #[abi(embed_v0)]
    impl ICounterImpl of super::ICounterContract<ContractState> {
        fn increase_counter(ref self: ContractState, amount: u128) {
            self.counter.write(self.counter.read() + amount);
        }

        fn get_counter(self: @ContractState) -> u128 {
            self.counter.read()
        }
    }
}

/// A static proxy that forwards every `ICounterContract` call as a library call to
/// `counter_contract`'s class, using its compile-time-injected class hash. No storage, no
/// constructor.
#[starknet::contract]
#[feature("forward-impl")]
pub mod static_proxy {
    #[storage]
    struct Storage {}

    // Statically point the forwarding impl at `counter_contract`'s class.
    impl CounterClassHash =
        super::counter_contract::__class_hash__::ForwardingClassHashImpl<ContractState>;

    #[abi(embed_v0)]
    impl ForwardedImpl = super::ICounterContractForwardImpl<ContractState>;
}
