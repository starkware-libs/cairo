use queue::QueueTrait;

#[contract]
mod TestContract {
    use queue::QueueTrait;
    use traits::Into;

    struct Storage {
        value: felt,
        mapping: LegacyMap::<u128, bool>,
        large_mapping: LegacyMap::<u256, u256>,
    }

    #[view]
    fn get_plus_2(a: felt) -> felt {
        a + 2
    }

    #[view]
    fn get_appended_queue(q: Queue::<felt>) -> Queue::<felt> {
        // `mut` is currently not allowed in the signature.
        let mut q = q;
        let elem = q.len().into();
        q.append(elem);
        q
    }

    #[external]
    fn set_value(a: felt) {
        value::write(a);
    }

    #[view]
    fn get_value() -> felt {
        value::read()
    }

    #[external]
    fn insert(key: u128) {
        mapping::write(key, true)
    }

    #[external]
    fn remove(key: u128) {
        mapping::write(key, false)
    }

    #[view]
    fn contains(key: u128) -> bool {
        mapping::read(key)
    }

    #[external]
    fn set_large(key: u256, value: u256) {
        large_mapping::write(key, value)
    }

    #[view]
    fn get_large(key: u256) -> u256 {
        large_mapping::read(key)
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    let calldata = QueueTrait::new();
    TestContract::__external::get_plus_2(calldata);
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    let mut calldata = QueueTrait::new();
    calldata.append(1);
    calldata.append(2);
    TestContract::__external::get_plus_2(QueueTrait::new());
}

fn single_element_queue(value: felt) -> Queue::<felt> {
    let mut q = QueueTrait::new();
    q.append(value);
    q
}

fn pop_and_compare(ref q: Queue::<felt>, value: felt, err: felt) {
    match q.pop_front() {
        Option::Some(x) => {
            assert(x == value, err);
        },
        Option::None(_) => {
            panic(single_element_queue('Got empty result data'))
        },
    };
}

fn assert_empty(mut q: Queue::<felt>) {
    assert(q.is_empty(), 'Queue not empty');
}

#[test]
#[available_gas(20000)]
fn test_wrapper_valid_args() {
    let mut retdata = TestContract::__external::get_plus_2(single_element_queue(1));
    pop_and_compare(ref retdata, 3, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(5000)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    TestContract::__external::get_plus_2(single_element_queue(1));
}

#[test]
#[available_gas(200000)]
fn test_wrapper_queue_arg_and_output() {
    let mut calldata = QueueTrait::new();
    calldata.append(1);
    calldata.append(2);
    let mut retdata = TestContract::__external::get_appended_queue(calldata);
    pop_and_compare(ref retdata, 2, 'Wrong length');
    pop_and_compare(ref retdata, 2, 'Wrong original value');
    pop_and_compare(ref retdata, 1, 'Wrong added value');
    assert_empty(retdata);
}

#[test]
#[available_gas(200000)]
fn read_first_value() {
    let mut retdata = TestContract::__external::get_value(QueueTrait::new());
    pop_and_compare(ref retdata, 0, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(300000)]
fn write_read_value() {
    assert_empty(TestContract::__external::set_value(single_element_queue(4)));
    let mut retdata = TestContract::__external::get_value(QueueTrait::new());
    pop_and_compare(ref retdata, 4, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(200000)]
fn empty_start() {
    let mut retdata = TestContract::__external::contains(single_element_queue(4));
    pop_and_compare(ref retdata, 0, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(300000)]
fn contains_added() {
    assert_empty(TestContract::__external::insert(single_element_queue(4)));
    let mut retdata = TestContract::__external::contains(single_element_queue(4));
    pop_and_compare(ref retdata, 1, 'Wrong result');
    assert_empty(retdata);
    let mut retdata = TestContract::__external::contains(single_element_queue(5));
    pop_and_compare(ref retdata, 0, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(300000)]
fn not_contains_removed() {
    assert_empty(TestContract::__external::insert(single_element_queue(4)));
    assert_empty(TestContract::__external::remove(single_element_queue(4)));
    let mut retdata = TestContract::__external::contains(single_element_queue(4));
    pop_and_compare(ref retdata, 0, 'Wrong result');
    assert_empty(retdata);
}

fn single_u256_queue(value: u256) -> Queue::<felt> {
    let mut q = QueueTrait::new();
    serde::Serde::serialize(ref q, value);
    q
}

fn pop_u256(ref q: Queue::<felt>) -> u256 {
    match serde::Serde::deserialize(ref q) {
        Option::Some(x) => x,
        Option::None(_) => {
            panic(single_element_queue('Got empty result data'))
        },
    }
}

#[test]
#[available_gas(300000)]
fn read_large_first_value() {
    let mut retdata = TestContract::__external::get_large(
        single_u256_queue(u256 { low: 1_u128, high: 2_u128 })
    );
    let value = pop_u256(ref retdata);
    assert_empty(retdata);
    assert(value.low == 0_u128, 'bad low');
    assert(value.high == 0_u128, 'bad high');
}

#[test]
#[available_gas(300000)]
fn write_read_large_value() {
    let mut args = QueueTrait::new();
    serde::Serde::serialize(ref args, u256 { low: 1_u128, high: 2_u128 });
    serde::Serde::serialize(ref args, u256 { low: 3_u128, high: 4_u128 });
    let mut retdata = TestContract::__external::set_large(args);
    assert_empty(retdata);
    let mut retdata = TestContract::__external::get_large(
        single_u256_queue(u256 { low: 1_u128, high: 2_u128 })
    );
    let value = pop_u256(ref retdata);
    assert_empty(retdata);
    assert(value.low == 3_u128, 'bad low');
    assert(value.high == 4_u128, 'bad high');
}
