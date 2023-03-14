extern fn roll(address: felt, caller_address: felt) -> Result::<(), felt> nopanic;

extern fn warp(blk_timestamp: felt, target_contract_address: felt) -> Result::<(), felt> nopanic;

extern fn start_prank(
    caller_address: felt, target_contract_address: felt
) -> Result::<(), felt> nopanic;

extern fn declare(contract: felt) -> Result::<felt, felt> nopanic;

extern fn invoke(
    contract_address: felt, entry_point_selector: felt, calldata: Array::<felt>
) -> Result::<(), felt> nopanic;

extern fn mock_call(
    contract_address: felt, entry_point_selector: felt, response: Array::<felt>
) -> Result::<(), felt> nopanic;

struct PreparedContract {
    address: felt,
    class_hash: felt,
    constructor_calldata: Array::<felt>,
}
// returns deployed `contract_address`
extern fn deploy(
    prepared_contract_address: felt,
    prepared_class_hash: felt,
    prepared_constructor_calldata: Array::<felt>
) -> Result::<felt, felt> nopanic;

fn deploy_wrapper(prepared_contract: PreparedContract) -> Result::<felt, felt> nopanic {
    let PreparedContract{address, class_hash, constructor_calldata } = prepared_contract;
    deploy(address, class_hash, constructor_calldata)
}
