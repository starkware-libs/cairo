use starknet::class_hash::{ClassHash, class_hash_const};
use starknet::contract_address::contract_address_const;
use starknet::syscalls::{deploy_syscall, get_class_hash_at_syscall};

#[starknet::interface]
trait IWithReplace<TContractState> {
    fn replace(ref self: TContractState, class_hash: ClassHash);
}

#[starknet::contract]
mod contract_a {
    use starknet::SyscallResultTrait;
    use starknet::class_hash::ClassHash;
    use starknet::storage::StoragePointerWriteAccess;

    #[storage]
    struct Storage {
        value: u128,
    }

    #[constructor]
    fn constructor(ref self: ContractState, value: u128) {
        self.value.write(value);
    }

    #[abi(embed_v0)]
    impl IWithReplaceImpl of super::IWithReplace<ContractState> {
        fn replace(ref self: ContractState, class_hash: ClassHash) {
            starknet::syscalls::replace_class_syscall(class_hash).unwrap_syscall();
        }
    }
}

#[starknet::interface]
trait IWithFoo<TContractState> {
    fn foo(ref self: TContractState) -> u128;
}

#[starknet::contract]
mod contract_b {
    use starknet::storage::StoragePointerReadAccess;
    #[storage]
    struct Storage {
        value: u128,
    }

    #[external(v0)]
    fn foo(ref self: ContractState) -> u128 {
        self.value.read()
    }
}

#[test]
fn test_replace_flow() {
    // Deploy ContractA with 100 in the storage.
    let (address0, _) = deploy_syscall(
        class_hash: contract_a::TEST_CLASS_HASH.try_into().unwrap(),
        contract_address_salt: 0,
        calldata: [100].span(),
        deploy_from_zero: false,
    )
        .unwrap();

    // Replace its class hash to Class B.
    let mut contract0 = IWithReplaceDispatcher { contract_address: address0 };
    contract0.replace(contract_b::TEST_CLASS_HASH.try_into().unwrap());

    // Read the previously stored value.
    let mut contract1 = IWithFooDispatcher { contract_address: address0 };
    assert_eq!(contract1.foo(), 100);
}

#[test]
#[available_gas(30000000)]
#[should_panic(expected: ('CLASS_HASH_NOT_FOUND', 'ENTRYPOINT_FAILED'))]
fn test_cannot_replace_with_non_existing_class_hash() {
    // Deploy ContractA with 100 in the storage.
    let (address0, _) = deploy_syscall(
        class_hash: contract_a::TEST_CLASS_HASH.try_into().unwrap(),
        contract_address_salt: 0,
        calldata: [100].span(),
        deploy_from_zero: false,
    )
        .unwrap();

    // Replace its class hash to Class B.
    let mut contract0 = IWithReplaceDispatcher { contract_address: address0 };
    contract0.replace('not a valid class hash'.try_into().unwrap());
}

#[test]
fn test_class_hash_at_syscall() {
    let a_class_hash = class_hash_const::<contract_a::TEST_CLASS_HASH>();
    // Deploy ContractA with 100 in the storage.
    let (address0, _) = deploy_syscall(
        class_hash: a_class_hash,
        contract_address_salt: 0,
        calldata: [100].span(),
        deploy_from_zero: false,
    )
        .unwrap();

    assert_eq!(get_class_hash_at_syscall(address0), Ok(a_class_hash));
    // Replace its class hash to Class B.
    let b_class_hash = class_hash_const::<contract_b::TEST_CLASS_HASH>();
    IWithReplaceDispatcher { contract_address: address0 }.replace(b_class_hash);
    assert_eq!(get_class_hash_at_syscall(address0), Ok(b_class_hash));
}

#[test]
fn test_class_hash_at_syscall_undeployed_contract() {
    assert_eq!(
        get_class_hash_at_syscall(contract_address_const::<123456>()), Ok(class_hash_const::<0>()),
    );
}
