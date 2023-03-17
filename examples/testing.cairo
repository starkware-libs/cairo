use array::ArrayTrait;

fn run_tests() {
    assert(bool::True(()), 1);
}

fn assert(cond: bool, err_code: felt252) {
    if cond {} else {
        let mut data = ArrayTrait::new();
        data.append(err_code);
        panic(data);
    }
}
