use starknet::deployment::DeploymentParams;
use starknet::syscalls::deploy_syscall;

#[starknet::interface]
trait IValue<TContractState> {
    fn get_value(self: @TContractState) -> felt252;
    fn set_value(ref self: TContractState, value: felt252);
}

#[starknet::contract]
mod self_caller {
    use starknet::storage::{StoragePointerReadAccess, StoragePointerWriteAccess};
    use super::{IValueDispatcher, IValueDispatcherTrait};
    #[storage]
    struct Storage {
        value: felt252,
    }

    #[constructor]
    fn constructor(ref self: ContractState) {
        IValueDispatcher { contract_address: starknet::get_contract_address() }.set_value(1);
    }

    #[abi(embed_v0)]
    impl ValueImpl of super::IValue<ContractState> {
        fn get_value(self: @ContractState) -> felt252 {
            self.value.read()
        }

        fn set_value(ref self: ContractState, value: felt252) {
            self.value.write(value);
        }
    }
}

#[starknet::contract]
pub mod advanced {
    #[storage]
    struct Storage {}

    #[constructor]
    fn constructor(
        ref self: ContractState,
        arr: Array<Array<felt252>>,
        one: u8,
        two: i16,
        three: ByteArray,
        four: (bool, u32),
        five: u256,
        six: felt252,
    ) {}
}

#[test]
fn test_deploy_in_construct() {
    let (contract_address, _) = deploy_syscall(self_caller::TEST_CLASS_HASH, 0, [].span(), false)
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}


#[test]
fn test_redeploy_in_construct() {
    assert!(deploy_syscall(self_caller::TEST_CLASS_HASH, 0, [].span(), false).is_ok());
    assert!(
        deploy_syscall(
            self_caller::TEST_CLASS_HASH, 0, [].span(), false,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}

#[test]
fn test_typed_redeploy_default() {
    assert!(self_caller::deploy_for_test(self_caller::TEST_CLASS_HASH, Default::default()).is_ok());
    assert!(
        deploy_syscall(
            self_caller::TEST_CLASS_HASH, 0, [].span(), false,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}

#[test]
fn test_typed_redeploy_with_params() {
    let deployment_params = DeploymentParams { salt: 42, deploy_from_zero: true };
    assert!(self_caller::deploy_for_test(self_caller::TEST_CLASS_HASH, deployment_params).is_ok());
    assert!(
        deploy_syscall(
            self_caller::TEST_CLASS_HASH, 42, [].span(), true,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}

#[test]
fn test_typed_deploy_default_with_complex_args() {
    let arr: Array<Array<felt252>> = array![array![1, 2, 3], array![4, 5]];
    let one: u8 = 42;
    let two: i16 = -123;
    let three: ByteArray = "Hello, Starknet!";
    let four: (bool, u32) = (true, 456);
    let five: u256 = 0x123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0;
    let six: felt252 = 123;

    let mut calldata = array![];
    arr.serialize(ref calldata);
    one.serialize(ref calldata);
    two.serialize(ref calldata);
    three.serialize(ref calldata);
    four.serialize(ref calldata);
    five.serialize(ref calldata);
    six.serialize(ref calldata);

    assert!(
        advanced::deploy_for_test(
            advanced::TEST_CLASS_HASH, Default::default(), arr, one, two, three, four, five, six,
        )
            .is_ok(),
    );

    assert!(
        deploy_syscall(
            advanced::TEST_CLASS_HASH, 0, calldata.span(), false,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}
