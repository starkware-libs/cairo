#[contract]
mod TestContract {
    struct Storage { value: felt }

    #[view]
    fn get_plus_2(a: felt) -> felt {
        a + 2
    }

    #[view]
    fn get_appended_array(arr: Array::<felt>) -> Array::<felt> {
        // `mut` is currently not allowed in the signature.
        let mut arr = arr;
        let elem = u128_to_felt(array_len::<felt>(arr));
        array_append::<felt>(arr, elem);
        arr
    }

    #[external]
    fn set_value(a: felt) {
        super::value::write(a);
    }

    #[view]
    fn get_value() -> felt {
        super::value::read()
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    let calldata = array_new::<felt>();
    TestContract::__external::get_plus_2(calldata);
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    let mut calldata = array_new::<felt>();
    array_append::<felt>(calldata, 1);
    array_append::<felt>(calldata, 2);
    TestContract::__external::get_plus_2(array_new::<felt>());
}

fn single_element_arr(value: felt) -> Array::<felt> {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, value);
    arr
}

fn pop_and_compare(ref arr: Array::<felt>, value: felt, err: felt) {
    match array_pop_front::<felt>(arr) {
        Option::Some(x) => {
            assert(x == value, err);
        },
        Option::None(_) => {
            panic(single_element_arr('Got empty result data'))
        },
    };
}

fn assert_empty(mut arr: Array::<felt>) {
    assert(array_len::<felt>(arr) == 0_u128, 'Array not empty');
}

#[test]
#[available_gas(20000)]
fn test_wrapper_valid_args() {
    let mut retdata = TestContract::__external::get_plus_2(single_element_arr(1));
    pop_and_compare(retdata, 3, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(200)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    TestContract::__external::get_plus_2(single_element_arr(1));
}

#[test]
#[available_gas(200000)]
fn test_wrapper_array_arg_and_output() {
    let mut calldata = array_new::<felt>();
    array_append::<felt>(calldata, 1);
    array_append::<felt>(calldata, 2);
    let mut retdata = TestContract::__external::get_appended_array(calldata);
    pop_and_compare(retdata, 2, 'Wrong length');
    pop_and_compare(retdata, 2, 'Wrong original value');
    pop_and_compare(retdata, 1, 'Wrong added value');
    assert_empty(retdata);
}

#[test]
#[available_gas(20000)]
fn read_first_value() {
    let mut retdata = TestContract::__external::get_value(array_new::<felt>());
    pop_and_compare(retdata, 0, 'Wrong result');
    assert_empty(retdata);
}

#[test]
#[available_gas(20000)]
fn write_read_value() {
    assert_empty(TestContract::__external::set_value(single_element_arr(4)));
    let mut retdata = TestContract::__external::get_value(array_new::<felt>());
    pop_and_compare(retdata, 4, 'Wrong result');
    assert_empty(retdata);
}
