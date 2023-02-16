extern fn cheat_roll(address: felt, caller_address: felt) -> Result::<(), felt> nopanic;

fn roll(address: felt, caller_address: felt) -> () {
    match cheat_roll(address, caller_address) {
        Result::Ok(()) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(ref data, x);
            panic(data)
        },
    }
}

extern fn start_prank(caller_address: felt, target_contract_address: Option::<felt>) -> Result::<(), felt> nopanic;

extern fn declare(contract: felt) -> Result::<felt, felt> nopanic;
