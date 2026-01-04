use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{CALLER, NAME, RECIPIENT, SALT, SUPPLY, SYMBOL};
use openzeppelin_testing::{EventSpyExt, EventSpyQueue as EventSpy, spy_events};
use openzeppelin_token::erc20::interface::{IERC20Dispatcher, IERC20DispatcherTrait};
use openzeppelin_utils::deployments::{DeployerInfo, calculate_contract_address_from_udc};
use openzeppelin_utils::interfaces::{
    UniversalDeployerABIDispatcher, UniversalDeployerABIDispatcherTrait,
};
use openzeppelin_utils::serde::SerializedAppend;
use snforge_std::start_cheat_caller_address;
use starknet::{ClassHash, ContractAddress};
use crate::universal_deployer::UniversalDeployer;
use crate::universal_deployer::UniversalDeployer::ContractDeployed;

fn ERC20_CLASS_HASH() -> ClassHash {
    utils::declare_class("DualCaseERC20Mock").class_hash
}

fn ERC20_CALLDATA() -> Span<felt252> {
    let mut calldata = array![];
    calldata.append_serde(NAME());
    calldata.append_serde(SYMBOL());
    calldata.append_serde(SUPPLY);
    calldata.append_serde(RECIPIENT);
    calldata.span()
}

fn deploy_udc() -> UniversalDeployerABIDispatcher {
    let mut calldata = array![];

    let address = utils::declare_and_deploy("UniversalDeployer", calldata);
    UniversalDeployerABIDispatcher { contract_address: address }
}

#[test]
fn test_deploy_from_zero() {
    test_deploy_from_zero_internal(false);
}

#[test]
fn test_deploy_from_zero_camel_case() {
    test_deploy_from_zero_internal(true);
}

fn test_deploy_from_zero_internal(camel_case: bool) {
    let udc = deploy_udc();
    let caller = CALLER;

    // Deploy args
    let erc20_class_hash = ERC20_CLASS_HASH();
    let salt = SALT;
    let not_from_zero = false;
    let erc20_calldata = ERC20_CALLDATA();

    let mut spy = spy_events();
    start_cheat_caller_address(udc.contract_address, caller);

    // Check address
    let expected_addr = calculate_contract_address_from_udc(
        salt, erc20_class_hash, erc20_calldata, Option::None,
    );
    let deployed_addr = if camel_case {
        udc.deployContract(erc20_class_hash, salt, not_from_zero, erc20_calldata)
    } else {
        udc.deploy_contract(erc20_class_hash, salt, not_from_zero, erc20_calldata)
    };
    assert_eq!(expected_addr, deployed_addr);

    // Drop ERC20 event, check deploy event
    spy.drop_event();
    spy
        .assert_only_event_contract_deployed(
            udc.contract_address,
            deployed_addr,
            caller,
            not_from_zero,
            erc20_class_hash,
            erc20_calldata,
            salt,
        );

    // Check deployment
    let erc20 = IERC20Dispatcher { contract_address: deployed_addr };
    let total_supply = erc20.total_supply();
    assert_eq!(total_supply, SUPPLY);
}

#[test]
fn test_deploy_not_from_zero() {
    test_deploy_not_from_zero_internal(false);
}

#[test]
fn test_deploy_not_from_zero_camel_case() {
    test_deploy_not_from_zero_internal(true);
}

fn test_deploy_not_from_zero_internal(camel_case: bool) {
    let udc = deploy_udc();
    let caller = CALLER;

    // Deploy args
    let erc20_class_hash = ERC20_CLASS_HASH();
    let salt = SALT;
    let not_from_zero = true;
    let erc20_calldata = ERC20_CALLDATA();

    let mut spy = spy_events();
    start_cheat_caller_address(udc.contract_address, caller);

    // Check address
    let expected_addr = calculate_contract_address_from_udc(
        salt,
        erc20_class_hash,
        erc20_calldata,
        Option::Some(DeployerInfo { caller_address: caller, udc_address: udc.contract_address }),
    );
    let deployed_addr = if camel_case {
        udc.deployContract(erc20_class_hash, salt, not_from_zero, erc20_calldata)
    } else {
        udc.deploy_contract(erc20_class_hash, salt, not_from_zero, erc20_calldata)
    };
    assert_eq!(expected_addr, deployed_addr);

    // Drop ERC20 event, check deploy event
    spy.drop_event();
    spy
        .assert_only_event_contract_deployed(
            udc.contract_address,
            deployed_addr,
            caller,
            not_from_zero,
            erc20_class_hash,
            erc20_calldata,
            salt,
        );

    // Check deployment
    let erc20 = IERC20Dispatcher { contract_address: deployed_addr };
    let total_supply = erc20.total_supply();
    assert_eq!(total_supply, SUPPLY);
}

//
// Helpers
//

#[generate_trait]
impl UniversalDeployerHelpersImpl of UniversalDeployerSpyHelpers {
    fn assert_only_event_contract_deployed(
        ref self: EventSpy,
        contract: ContractAddress,
        address: ContractAddress,
        deployer: ContractAddress,
        not_from_zero: bool,
        class_hash: ClassHash,
        calldata: Span<felt252>,
        salt: felt252,
    ) {
        let expected = UniversalDeployer::Event::ContractDeployed(
            ContractDeployed { address, deployer, not_from_zero, class_hash, calldata, salt },
        );
        self.assert_only_event(contract, expected);
    }
}
