use starknet::SyscallResultTrait;
use starknet::syscalls::{deploy_syscall, get_block_hash_syscall};

#[starknet::interface]
trait IContract<T> {
    fn foo(ref self: T, a: u128) -> u128;
}

#[starknet::contract]
mod contract_a {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};

    #[storage]
    struct Storage {
        value: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        self.value.write(value);
    }

    #[external(v0)]
    fn foo(ref self: ContractState, a: u128) -> u128 {
        let value = self.value.read();
        self.value.write(a);
        value
    }

    #[generate_trait]
    pub impl MyImpl of MyTrait {
        fn internal_func(self: @ContractState) -> u128 {
            5
        }
    }
}
use contract_a::MyTrait;
#[test]
fn test_internal_func() {
    let mut contract_state = contract_a::contract_state_for_testing();
    contract_state.internal_func();
}

#[test]
fn test_flow() {
    // Set up.
    let (address0, _) = deploy_syscall(contract_a::TEST_CLASS_HASH, 0, [100].span(), false)
        .unwrap();
    let mut contract0 = IContractDispatcher { contract_address: address0 };
    let (address1, _) = deploy_syscall(contract_a::TEST_CLASS_HASH, 0, [200].span(), false)
        .unwrap();
    let mut contract1 = IContractDispatcher { contract_address: address1 };

    // Interact.
    assert_eq!(contract0.foo(300), 100);
    assert_eq!(contract1.foo(300), 200);
    assert_eq!(contract0.foo(300), 300);
    assert_eq!(contract1.foo(300), 300);

    // Library calls.
    let mut library = IContractLibraryDispatcher { class_hash: contract_a::TEST_CLASS_HASH };
    assert_eq!(library.foo(300), 0);
}

#[test]
#[feature("safe_dispatcher")]
fn test_flow_safe_dispatcher() {
    // Set up.
    let (contract_address, _) = deploy_syscall(contract_a::TEST_CLASS_HASH, 0, [100].span(), false)
        .unwrap();
    let mut contract = IContractSafeDispatcher { contract_address };

    // Interact.
    assert_eq!(contract.foo(300), Ok(100));

    // Library calls.
    let mut library = IContractSafeLibraryDispatcher { class_hash: contract_a::TEST_CLASS_HASH };
    assert_eq!(library.foo(300), Ok(0));
}

// If the test is failing do to gas usage changes, update the gas limit by taking `test_flow` test
// gas usage and add about 110000.
#[test]
#[available_gas(826400)]
#[should_panic(expected: ('Out of gas', 'ENTRYPOINT_FAILED'))]
fn test_flow_out_of_gas() {
    // Calling the `test_flow` test but a low gas limit.
    test_flow();
}

#[test]
fn test_class_hash_not_found() {
    assert_eq!(
        deploy_syscall(5.try_into().unwrap(), 0, [100].span(), false),
        Err(array!['CLASS_HASH_NOT_FOUND']),
    );
}

#[test]
#[should_panic(expected: ('CONTRACT_NOT_DEPLOYED', 'ENTRYPOINT_FAILED'))]
fn test_contract_not_deployed() {
    let mut contract = IContractDispatcher { contract_address: 5.try_into().unwrap() };
    contract.foo(10);
}

#[starknet::contract]
mod contract_failed_constructor {
    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        core::panic_with_felt252('Failure');
    }
}

#[test]
fn test_failed_constructor() {
    // Set up.
    let mut calldata = array![];
    calldata.append(100);
    let mut err = deploy_syscall(
        contract_failed_constructor::TEST_CLASS_HASH, 0, calldata.span(), false,
    )
        .unwrap_err();
    assert_eq!(err.pop_front().unwrap(), 'Failure');
    assert_eq!(err.pop_front().unwrap(), 'CONSTRUCTOR_FAILED');
}

#[starknet::contract]
mod contract_failed_entrypoint {
    #[storage]
    struct Storage {}

    #[external(v0)]
    fn foo(ref self: ContractState, value: u128) {
        core::panic_with_felt252('Failure');
    }
}

#[test]
fn test_non_empty_calldata_nonexistent_constructor() {
    let mut err = deploy_syscall(
        contract_failed_entrypoint::TEST_CLASS_HASH, 0, array![100].span(), false,
    )
        .unwrap_err();
    assert_eq!(err.pop_front().unwrap(), 'INVALID_CALLDATA_LEN');
}

#[test]
#[should_panic(expected: ('Failure', 'ENTRYPOINT_FAILED'))]
fn test_entrypoint_failed() {
    let (address0, _) = deploy_syscall(
        contract_failed_entrypoint::TEST_CLASS_HASH, 0, array![].span(), false,
    )
        .unwrap();
    let mut contract = IContractDispatcher { contract_address: address0 };
    contract.foo(300);
}

#[test]
#[should_panic(expected: ('GET_BLOCK_HASH_NOT_SET',))]
fn test_get_block_hash() {
    get_block_hash_syscall(0).unwrap_syscall();
}
