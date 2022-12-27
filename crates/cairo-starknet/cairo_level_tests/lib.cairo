#[contract]
mod TestContract {
    struct Storage { }

    #[external]
    fn test(a: felt) -> felt {
        a + 2
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

fn run_wrapper_valid_args() {
    let mut calldata = array_new::<felt>();
    array_append::<felt>(calldata, 1);
    let mut retdata = TestContract::__external::test(calldata);
    match array_pop_front::<felt>(retdata) {
        Option::Some(x) => {
            assert(x == 3, 'Wrong result');
        },
        Option::None(_) => {
            assert(false, 'Got empty result data');
        },
    }
    assert(array_len::<felt>(retdata) == 0_u128, 'Got too long result data');
}

#[test]
#[available_gas(20000)]
fn test_wrapper_valid_args() {
    run_wrapper_valid_args()
}

#[test]
#[available_gas(200)]
#[should_panic]
fn test_wrapper_valid_args_out_of_gas() {
    run_wrapper_valid_args()
}
