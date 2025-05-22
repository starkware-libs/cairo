use crate::contracts::erc20::{IERC20Dispatcher, IERC20DispatcherTrait
, erc_20};
use starknet::ContractAddress;
use starknet::deployment::{DeploymentParams, DeploymentParamsImpl, DeploymentParamsTrait};

const name: felt252 = 'strk';
const symbol: felt252 = 'STRK';
const decimals: u8 = 18;
const supply: u256 = 256;

#[test]
fn test_typed_deploy_default() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();
    let deployment_params = DeploymentParamsTrait::new(erc_20::TEST_CLASS_HASH);

    let (contract_address, _) = erc_20::deploy(deployment_params, name, symbol, decimals, supply, recipient)
        .expect('deployment failed');
    assert!(IERC20Dispatcher { contract_address }.get_name() == name);
}

#[test]
fn test_typed_redeploy_default() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();
    let deployment_params = DeploymentParamsTrait::new(erc_20::TEST_CLASS_HASH);

    assert!(erc_20::deploy(deployment_params, name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy(deployment_params, name, symbol, decimals, supply, recipient) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}

#[test]
fn test_typed_deploy_with_params() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    let deployment_params = DeploymentParams {
        class_hash: erc_20::TEST_CLASS_HASH,
        salt: 0,
        deploy_from_zero: true,
    };
    let (contract_address, _) = erc_20::deploy(deployment_params, name, symbol, decimals, supply, recipient)
        .expect('deployment failed');
    assert!(IERC20Dispatcher { contract_address }.get_name() == name);
}

#[test]
fn test_typed_redeploy_with_params() {
    let recipient: ContractAddress = 'Kamil'.try_into().unwrap();

    let deployment_params_0 = DeploymentParams {
        class_hash: erc_20::TEST_CLASS_HASH,
        salt: 0,
        deploy_from_zero: true,
    };
    let deployment_params_1 = DeploymentParams {
        class_hash: erc_20::TEST_CLASS_HASH,
        salt: 1,
        deploy_from_zero: true,
    };

    assert!(erc_20::deploy(deployment_params_0, name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy(deployment_params_1, name, symbol, decimals, supply, recipient).is_ok());
    assert!(erc_20::deploy(deployment_params_0, name, symbol, decimals, supply, recipient) == Err(array!['CONTRACT_ALREADY_DEPLOYED']));
}