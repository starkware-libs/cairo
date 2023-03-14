fn run_tests() {
    assert(bool::True(()), 1);
}

fn assert(cond: bool, err_code: felt252) {
    if cond {} else {
        let mut data = array_new::<felt252>();
        array_append::<felt252>(ref data, err_code);
        panic(data);
    }
}
