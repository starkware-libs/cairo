use openzeppelin_test_common::mocks::upgrades::{
    IUpgradesV1Dispatcher, IUpgradesV1DispatcherTrait, IUpgradesV1SafeDispatcher,
    IUpgradesV1SafeDispatcherTrait, IUpgradesV2Dispatcher, IUpgradesV2DispatcherTrait,
};
use openzeppelin_test_common::upgrades::UpgradeableSpyHelpers;
use openzeppelin_testing as utils;
use openzeppelin_testing::constants::{CLASS_HASH_ZERO, FELT_VALUE as VALUE};
use openzeppelin_testing::{declare_class, deploy, spy_events};
use snforge_std::ContractClass;

//
// Setup
//

fn setup_test() -> (IUpgradesV1Dispatcher, ContractClass) {
    let v1_class = declare_class("UpgradesV1");
    let v2_class = declare_class("UpgradesV2");
    let v1_contract_address = deploy(v1_class, array![]);
    let v1 = IUpgradesV1Dispatcher { contract_address: v1_contract_address };
    (v1, v2_class)
}

//
// upgrade
//

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_with_class_hash_zero() {
    let (v1, _) = setup_test();
    v1.upgrade(CLASS_HASH_ZERO);
}

#[test]
fn test_upgraded_event() {
    let (v1, v2_class) = setup_test();
    let mut spy = spy_events();

    v1.upgrade(v2_class.class_hash);

    spy.assert_only_event_upgraded(v1.contract_address, v2_class.class_hash);
}

#[test]
fn test_new_selector_after_upgrade() {
    let (v1, v2_class) = setup_test();

    v1.upgrade(v2_class.class_hash);
    let v2 = IUpgradesV2Dispatcher { contract_address: v1.contract_address };

    v2.set_value2(VALUE);
    assert_eq!(v2.get_value2(), VALUE);
}

#[test]
fn test_state_persists_after_upgrade() {
    let (v1, v2_class) = setup_test();

    v1.set_value(VALUE);

    v1.upgrade(v2_class.class_hash);
    let v2 = IUpgradesV2Dispatcher { contract_address: v1.contract_address };

    assert_eq!(v2.get_value(), VALUE);
}

#[test]
fn test_remove_selector_passes_in_v1() {
    let (v1, _) = setup_test();

    v1.remove_selector();
}

#[test]
#[feature("safe_dispatcher")]
fn test_remove_selector_fails_in_v2() {
    let (v1, v2_class) = setup_test();

    v1.upgrade(v2_class.class_hash);

    // We use the v1 dispatcher because `remove_selector` is not in v2 interface
    let safe_dispatcher = IUpgradesV1SafeDispatcher { contract_address: v1.contract_address };
    let mut result = safe_dispatcher.remove_selector();
    utils::assert_entrypoint_not_found_error(
        result, selector!("remove_selector"), v1.contract_address,
    );
}

//
// upgrade_and_call
//

#[test]
#[should_panic(expected: 'Class hash cannot be zero')]
fn test_upgrade_and_call_with_class_hash_zero() {
    let (v1, _) = setup_test();
    let calldata = array![VALUE];
    v1.upgrade_and_call(CLASS_HASH_ZERO, selector!("set_value2"), calldata.span());
}

#[test]
fn test_upgrade_and_call_with_new_selector() {
    let (v1, v2_class) = setup_test();
    let mut spy = spy_events();

    let calldata = array![VALUE];
    let new_selector = selector!("set_value2");
    v1.upgrade_and_call(v2_class.class_hash, new_selector, calldata.span());
    spy.assert_only_event_upgraded(v1.contract_address, v2_class.class_hash);

    let v2 = IUpgradesV2Dispatcher { contract_address: v1.contract_address };
    assert_eq!(v2.get_value2(), VALUE);
}

#[test]
fn test_upgrade_and_call_with_return_value() {
    let (v1, v2_class) = setup_test();

    // Set value to get with upgrade_and_call
    v1.set_value(VALUE);

    let calldata = array![];
    let selector = selector!("get_value");
    let call_res = v1.upgrade_and_call(v2_class.class_hash, selector, calldata.span());

    assert_eq!(call_res.len(), 1, "Return span should include one value");
    assert_eq!(*call_res.at(0), VALUE);
}

#[test]
fn test_upgrade_and_call_with_no_return_value() {
    let (v1, v2_class) = setup_test();

    let calldata = array![VALUE];
    let selector = selector!("set_value");
    let call_res = v1.upgrade_and_call(v2_class.class_hash, selector, calldata.span());

    assert_eq!(call_res.len(), 0);
}

#[test]
#[should_panic(expected: 'ENTRYPOINT_NOT_FOUND')]
fn test_upgrade_and_call_with_removed_selector() {
    let (v1, v2_class) = setup_test();
    let removed_selector = selector!("remove_selector");
    let calldata = array![];

    // We use the v1 dispatcher because `remove_selector` is not in v2 interface
    v1.upgrade_and_call(v2_class.class_hash, removed_selector, calldata.span());
}
