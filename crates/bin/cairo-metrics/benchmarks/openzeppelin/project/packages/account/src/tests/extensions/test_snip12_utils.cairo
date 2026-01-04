use openzeppelin_testing::constants::{OWNER, RECIPIENT};
use snforge_std::start_cheat_chain_id_global;
use starknet::account::Call;
use crate::extensions::src9::interface::OutsideExecution;
use crate::extensions::src9::snip12_utils::{
    CALL_TYPE_HASH, CallStructHash, OUTSIDE_EXECUTION_TYPE_HASH, OutsideExecutionStructHash,
};

#[test]
fn test_outside_execution_type_hash() {
    let expected = selector!(
        "\"OutsideExecution\"(\"Caller\":\"ContractAddress\",\"Nonce\":\"felt\",\"Execute After\":\"u128\",\"Execute Before\":\"u128\",\"Calls\":\"Call*\")\"Call\"(\"To\":\"ContractAddress\",\"Selector\":\"selector\",\"Calldata\":\"felt*\")",
    );
    assert_eq!(OUTSIDE_EXECUTION_TYPE_HASH, expected);
}

#[test]
fn test_call_type_hash() {
    let expected = selector!(
        "\"Call\"(\"To\":\"ContractAddress\",\"Selector\":\"selector\",\"Calldata\":\"felt*\")",
    );
    assert_eq!(CALL_TYPE_HASH, expected);
}

#[test]
fn test_call_struct_hash_generation() {
    start_cheat_chain_id_global('SN_TEST');

    let to = RECIPIENT;
    let selector = selector!("selector");
    let calldata = array![1, 2, 3, 4, 5].span();
    let call = Call { to, selector, calldata };

    let hash = call.hash_struct();
    // This hash was computed using starknet js sdk from the following values:
    //
    // - to: 'RECIPIENT'
    // - selector: 'selector'
    // - calldata: [1, 2, 3, 4, 5]
    // - revision: '1'
    let expected_hash = 0x4ae967778b464ac19d41ebe243aae57e06fab783c88e60612d48cd671d3b78d;
    assert_eq!(hash, expected_hash);
}

#[test]
fn test_outside_execution_struct_hash_generation() {
    start_cheat_chain_id_global('SN_TEST');

    // Call
    let to = RECIPIENT;
    let selector = selector!("selector");
    let calldata = array![1, 2, 3, 4, 5].span();
    let call = Call { to, selector, calldata };

    // OutsideExecution
    let caller = OWNER;
    let nonce = 5;
    let execute_after = 10;
    let execute_before = 20;
    let calls = array![call].span();
    let outside_execution = OutsideExecution {
        caller, nonce, execute_after, execute_before, calls,
    };

    let hash = outside_execution.hash_struct();
    // This hash was computed using starknet js sdk from the following values:
    //
    // OutsideExecution:
    // - caller: 'OWNER'
    // - nonce: 5
    // - execute_after: 10
    // - execute_before: 20
    // - calls: [Call]
    // - revision: '1'
    //
    // Call:
    // - to: 'RECIPIENT'
    // - selector: 'selector'
    // - calldata: [1, 2, 3, 4, 5]
    let expected_hash = 0x6eaa7a55ee940f04bf2a784c616ecc43864042fa2c61ad8243512e20eb368d;
    assert_eq!(hash, expected_hash);
}
