fn run_tests() {
    assert(bool::True(()), 1);
}

fn assert(cond: bool, err_code: felt) {
    if cond {} else {
        let mut data = queue_new::<felt>();
        queue_append::<felt>(ref data, err_code);
        panic(data);
    }
}
