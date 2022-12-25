fn run_tests() {
    assert(bool::True(()), 1);
}

fn assert(cond: bool, err_code: felt) {
    if cond {
    } else {
        let mut data = array_new::<felt>();
        array_append::<felt>(data, err_code);
        panic(data);
    }
}
