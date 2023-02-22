extern fn roll(address: felt, caller_address: felt) -> Result::<(), felt> nopanic;

extern fn warp(blk_timestamp: felt, target_contract_address: felt) -> Result::<(), felt> nopanic;

extern fn start_prank(caller_address: felt, target_contract_address: felt) -> Result::<(), felt> nopanic;

extern fn declare(contract: felt) -> Result::<felt, felt> nopanic;
