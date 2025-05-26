use starknet::syscalls::deploy_syscall;
use starknet::deployment::DeploymentParams;

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
fn test_typed_deploy_default() {
    let (contract_address, _) = self_caller::deploy(self_caller::TEST_CLASS_HASH, Default::default())
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}

#[test]
fn test_typed_redeploy_default() {
    assert!(self_caller::deploy(self_caller::TEST_CLASS_HASH, Default::default()).is_ok());
    assert!(self_caller::deploy(self_caller::TEST_CLASS_HASH, Default::default()) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}

#[test]
fn test_typed_deploy_with_params() {
    let deployment_params = DeploymentParams {
        salt: 42,
        deploy_from_zero: true,
    };
    let (contract_address, _) = self_caller::deploy(self_caller::TEST_CLASS_HASH, deployment_params)
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}

#[test]
fn test_typed_redeploy_with_params() {
    let deployment_params = DeploymentParams {
        salt: 42,
        deploy_from_zero: true,
    };
    assert!(self_caller::deploy(self_caller::TEST_CLASS_HASH, deployment_params).is_ok());
    assert!(self_caller::deploy(self_caller::TEST_CLASS_HASH, deployment_params) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}
