#[contract]
mod TestContract {
    struct Storage { value: felt }

    #[external]
    fn test(a: felt) -> felt {
        a + 2
    }

    #[external]
    fn set_value(a: felt) {
        super::value::write(a);
    }

    #[external]
    fn get_value() -> felt {
        super::value::read()
    }
}

#[test]
#[should_panic]
fn test_wrapper_not_enough_args() {
    let calldata = array_new::<felt>();
    TestContract::__external::test(calldata);
}

#[test]
#[should_panic]
fn test_wrapper_too_many_enough_args() {
    let mut calldata = array_new::<felt>();
    array_append::<felt>(calldata, 1);
    array_append::<felt>(calldata, 2);
    TestContract::__external::test(array_new::<felt>());
}

fn single_element_arr(value: felt) -> Array::<felt> {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, value);
    arr
}

fn unpack_single_result(mut retdata: Array::<felt>) -> felt {
    let x = match array_pop_front::<felt>(retdata) {
        Option::Some(x) => {
            x
        },
        Option::None(_) => {
            panic(single_element_arr('Got empty result data'))
        },
    };
    assert(array_len::<felt>(retdata) == 0_u128, 'Got too long result data');
    x
}

fn unpack_no_results(mut retdata: Array::<felt>) {
    assert(array_len::<felt>(retdata) == 0_u128, 'Got too long result data');
}

#[test]
#[available_gas(20000)]
fn test_wrapper_valid_args() {
    assert(
        unpack_single_result(
            __generated__TestContract::__external::test(single_element_arr(1))
        ) == 3,
        'Wrong result'
    );
}

#[test]
#[available_gas(200)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    assert(
        unpack_single_result(
            __generated__TestContract::__external::test(single_element_arr(1))
        ) == 3,
        'Wrong result'
    );
}

#[test]
#[available_gas(20000)]
fn read_first_value() {
    assert(
        unpack_single_result(
            __generated__TestContract::__external::get_value(array_new::<felt>())
        ) == 0,
        'Wrong result'
    );
}

#[test]
#[available_gas(20000)]
fn write_read_value() {
    unpack_no_results(__generated__TestContract::__external::set_value(single_element_arr(4)));
    assert(
        unpack_single_result(
            __generated__TestContract::__external::get_value(array_new::<felt>())
        ) == 4,
        'Wrong result'
    );
}
