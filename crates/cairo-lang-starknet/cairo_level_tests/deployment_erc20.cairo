use crate::contracts::erc20::{IERC20Dispatcher, IERC20DispatcherTrait
, erc_20};
use starknet::ContractAddress;
use starknet::deployment::DeploymentParams;

const name: felt252 = 'strk';
const symbol: felt252 = 'STRK';
const decimals: u8 = 18;
const supply: u256 = 256;

#[test]
fn test_typed_deploy_default() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    let (contract_address, _) = erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, Default::default(), name, symbol, decimals, supply, recipient)
        .expect('deployment failed');
    assert!(IERC20Dispatcher { contract_address }.get_name() == name);
}

#[test]
fn test_typed_redeploy_default() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    assert!(erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, Default::default(), name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, Default::default(), name, symbol, decimals, supply, recipient) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}

#[test]
fn test_typed_deploy_with_params() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    let deployment_params = DeploymentParams {
        salt: 42,
        deploy_from_zero: true,
    };
    let (contract_address, _) = erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, deployment_params, name, symbol, decimals, supply, recipient)
        .expect('deployment failed');
    assert!(IERC20Dispatcher { contract_address }.get_name() == name);
}

#[test]
fn test_typed_redeploy_with_params() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    let deployment_params_0 = DeploymentParams {
        salt: 0,
        deploy_from_zero: true,
    };
    let deployment_params_1 = DeploymentParams {
        salt: 1,
        deploy_from_zero: true,
    };

    assert!(erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, deployment_params_0, name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, deployment_params_1, name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy_for_test(erc_20::TEST_CLASS_HASH, deployment_params_0, name, symbol, decimals, supply, recipient) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}
