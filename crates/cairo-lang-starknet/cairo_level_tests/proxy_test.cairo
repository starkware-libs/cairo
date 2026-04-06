use starknet::syscalls::deploy_syscall;
use crate::contracts::proxy::{
    ICounterContract, ICounterContractDispatcher, ICounterContractDispatcherTrait, proxy, ICounterContractForwardImpl,
};

/// A minimal counter contract used as the proxy implementation.
/// Exposes the same entry points as `ICounterContract`.
#[starknet::contract]
mod counter_contract {
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

        fn decrease_counter(ref self: ContractState, amount: u128) {
            self.counter.write(self.counter.read() - amount);
        }

        fn get_counter(self: @ContractState) -> u128 {
            self.counter.read()
        }
    }
}

/// Tests that the proxy's embedded entry points forward calls to the implementation class
/// via library calls.
#[test]
fn test_proxy_entry_points() {
    let mut dispatcher = ICounterContractDispatcher { contract_address: deploy_proxy() };

    assert_eq!(dispatcher.get_counter(), 0);
    dispatcher.increase_counter(10);
    assert_eq!(dispatcher.get_counter(), 10);
    dispatcher.decrease_counter(3);
    assert_eq!(dispatcher.get_counter(), 7);
}

impl ForwardedImpl = ICounterContractForwardImpl<proxy::ContractState>;

/// Tests that `ICounterContractForwardImpl` satisfies the `ICounterContract` trait bound,
/// allowing the proxy's `ContractState` to be used wherever `ICounterContract<T>` is required.
#[test]
fn test_forward_impl_as_trait() {
    // Switch to the proxy's context so contract_state_for_testing reads its storage.
    starknet::testing::set_contract_address(deploy_proxy());
    let mut state = proxy::contract_state_for_testing();

    // Use state as ICounterContract through the forward_impl trait bound.
    assert_eq!(state.get_counter(), 0);
    state.increase_counter(10);
    assert_eq!(state.get_counter(), 10);
    state.decrease_counter(3);
    assert_eq!(state.get_counter(), 7);
}

/// Deploys the proxy contract pointing to the counter contract.
fn deploy_proxy() -> starknet::ContractAddress {
    let (addr, _) = deploy_syscall(
        proxy::TEST_CLASS_HASH, 0, [counter_contract::TEST_CLASS_HASH.into()].span(), false,
    )
        .unwrap();
    addr
}
