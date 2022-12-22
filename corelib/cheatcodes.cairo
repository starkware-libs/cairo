extern func cheat_roll(address: felt, caller_address: felt) -> Result::<(), felt> nopanic;

func roll(address: felt, caller_address: felt) -> () {
    match cheat_roll(address, caller_address) {
        Result::Ok(()) => (),
        Result::Err(x) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, x);
            panic(data)
        },
    }
}
