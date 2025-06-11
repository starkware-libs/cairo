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
    let (contract_address, _) = self_caller::deploy_for_test(
        self_caller::TEST_CLASS_HASH, Default::default(),
    )
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}

#[test]
fn test_typed_redeploy_default() {
    assert!(self_caller::deploy_for_test(self_caller::TEST_CLASS_HASH, Default::default()).is_ok());
    assert!(
        self_caller::deploy_for_test(
            self_caller::TEST_CLASS_HASH, Default::default(),
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}

#[test]
fn test_typed_deploy_with_params() {
    let deployment_params = DeploymentParams { salt: 42, deploy_from_zero: true };
    let (contract_address, _) = self_caller::deploy_for_test(
        self_caller::TEST_CLASS_HASH, deployment_params,
    )
        .expect('deployment failed');
    assert!(IValueDispatcher { contract_address }.get_value() == 1);
}

#[test]
fn test_typed_redeploy_with_params() {
    let deployment_params = DeploymentParams { salt: 42, deploy_from_zero: true };
    assert!(self_caller::deploy_for_test(self_caller::TEST_CLASS_HASH, deployment_params).is_ok());
    assert!(
        self_caller::deploy_for_test(
            self_caller::TEST_CLASS_HASH, deployment_params,
        ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
    );
}


mod test_erc20 {
    use starknet::ContractAddress;
    use starknet::deployment::DeploymentParams;
    use crate::contracts::erc20::{IERC20Dispatcher, IERC20DispatcherTrait, erc_20};

    const name: felt252 = 'strk';
    const symbol: felt252 = 'STRK';
    const decimals: u8 = 42;
    const supply: u256 = 256;

    #[test]
    fn test_typed_deploy_default_with_args() {
        let recipient: ContractAddress = 'John Doe'.try_into().unwrap();

        let (contract_address, _) = erc_20::deploy_for_test(
            erc_20::TEST_CLASS_HASH, Default::default(), name, symbol, decimals, supply, recipient,
        )
            .expect('deployment failed');
        assert!(IERC20Dispatcher { contract_address }.get_name() == name);
    }

    #[test]
    fn test_typed_redeploy_default_with_args() {
        let recipient: ContractAddress = 'John Doe'.try_into().unwrap();

        assert!(
            erc_20::deploy_for_test(
                erc_20::TEST_CLASS_HASH,
                Default::default(),
                name,
                symbol,
                decimals,
                supply,
                recipient,
            )
                .is_ok(),
        );
        assert!(
            erc_20::deploy_for_test(
                erc_20::TEST_CLASS_HASH,
                Default::default(),
                name,
                symbol,
                decimals,
                supply,
                recipient,
            ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
        );
    }

    #[test]
    fn test_typed_deploy_with_params_and_args() {
        let recipient: ContractAddress = 'John Doe'.try_into().unwrap();

        let deployment_params = DeploymentParams { salt: 42, deploy_from_zero: true };
        let (contract_address, _) = erc_20::deploy_for_test(
            erc_20::TEST_CLASS_HASH, deployment_params, name, symbol, decimals, supply, recipient,
        )
            .expect('deployment failed');
        assert!(IERC20Dispatcher { contract_address }.get_name() == name);
    }

    #[test]
    fn test_typed_redeploy_with_params_and_args() {
        let recipient: ContractAddress = 'John Doe'.try_into().unwrap();

        let deployment_params_0 = DeploymentParams { salt: 0, deploy_from_zero: true };
        let deployment_params_1 = DeploymentParams { salt: 1, deploy_from_zero: true };

        assert!(
            erc_20::deploy_for_test(
                erc_20::TEST_CLASS_HASH,
                deployment_params_0,
                name,
                symbol,
                decimals,
                supply,
                recipient,
            )
                .is_ok(),
        );
        assert!(
            erc_20::deploy_for_test(
                erc_20::TEST_CLASS_HASH,
                deployment_params_1,
                name,
                symbol,
                decimals,
                supply,
                recipient,
            )
                .is_ok(),
        );
        assert!(
            erc_20::deploy_for_test(
                erc_20::TEST_CLASS_HASH,
                deployment_params_0,
                name,
                symbol,
                decimals,
                supply,
                recipient,
            ) == Err(array!['CONTRACT_ALREADY_DEPLOYED']),
        );
    }
}
