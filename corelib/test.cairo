#[test]
#[should_panic]
func test_assert_false() {
    assert(false, 1);
}

#[test]
func test_assert_true() {
    assert(true, 1);
}

#[test]
func test_bool_operators() {
    assert(true == true, 1);
    assert(false == false, 1);
    assert(!true == false, 1);
    assert(!false == true, 1);
    assert(true != false, 1);
    assert(false != true, 1);
    assert(!(false & false), 1);
    assert(!(true & false), 1);
    assert(!(false & true), 1);
    assert(true & true, 1);
    assert(!(false | false), 1);
    assert(true | false, 1);
    assert(false | true, 1);
    assert(true | true, 1);
}

#[test]
func test_felt_operators() {
    assert(1 + 3 == 4, 1);
    assert(3 + 6 == 9, 1);
    assert(3 - 1 == 2, 1);
    assert(1231 - 231 == 1000, 1);
    assert(1 * 3 == 3, 1);
    assert(3 * 6 == 18, 1);
    assert(1 < 4, 1);
    assert(1 <= 4, 1);
    assert(!(4 < 4), 1);
    assert(4 <= 4, 1);
    assert(5 > 2, 1);
    assert(5 >= 2, 1);
    assert(!(3 > 3), 1);
    assert(3 >= 3, 1);
}

#[test]
func test_u128_operators() {
    assert(1_u128 == 1_u128, 1);
    assert(!(1_u128 == 2_u128), 1);
    assert(1_u128 + 3_u128 == 4_u128, 1);
    assert(3_u128 + 6_u128 == 9_u128, 1);
    assert(3_u128 - 1_u128 == 2_u128, 1);
    assert(1231_u128 - 231_u128 == 1000_u128, 1);
    assert(1_u128 * 3_u128 == 3_u128, 1);
    assert(2_u128 * 4_u128 == 8_u128, 1);
    assert(8_u128 / 2_u128 == 4_u128, 1);
    assert(8_u128 % 2_u128 == 0_u128, 1);
    assert(7_u128 / 3_u128 == 2_u128, 1);
    assert(7_u128 % 3_u128 == 1_u128, 1);
    assert(1_u128 < 4_u128, 1);
    assert(1_u128 <= 4_u128, 1);
    assert(!(4_u128 < 4_u128), 1);
    assert(4_u128 <= 4_u128, 1);
    assert(5_u128 > 2_u128, 1);
    assert(5_u128 >= 2_u128, 1);
    assert(!(3_u128 > 3_u128), 1);
    assert(3_u128 >= 3_u128, 1);
}

func pow_2_127() -> u128 {
    0x80000000000000000000000000000000_u128
}

func pow_2_64() -> u128 {
    0x10000000000000000_u128
}

#[test]
#[should_panic]
func test_u128_sub_overflow_1() {
    0_u128 - 1_u128;
}

#[test]
#[should_panic]
func test_u128_sub_overflow_2() {
    0_u128 - 3_u128;
}

#[test]
#[should_panic]
func test_u128_sub_overflow_3() {
    1_u128 - 3_u128;
}

#[test]
#[should_panic]
func test_u128_sub_overflow_4() {
    100_u128 - 1000_u128;
}

#[test]
#[should_panic]
func test_u128_add_overflow_1() {
    pow_2_127() + pow_2_127();
}

#[test]
#[should_panic]
func test_u128_add_overflow_2() {
    (pow_2_127() + 12_u128) + pow_2_127();
}

#[test]
#[should_panic]
func test_u128_mul_overflow_1() {
    pow_2_64() * pow_2_64();
}

#[test]
#[should_panic]
func test_u128_mul_overflow_2() {
    (pow_2_64() + 1_u128) * pow_2_64();
}

#[test]
#[should_panic]
func test_u128_mul_overflow_3() {
    2_u128 * pow_2_127();
}

#[test]
#[should_panic]
func test_u128_div_by_0() {
    2_u128 / 0_u128;
}

#[test]
#[should_panic]
func test_u128_mod_by_0() {
    2_u128 % 0_u128;
}

// TODO(orizi): Remove when u256 literals are supported.
func as_u256(high: u128, low: u128) -> u256 {
    u256 { low, high }
}

#[test]
func test_u256_from_felt() {
    assert(u256_from_felt(1) == as_u256(0_u128, 1_u128), 1);
    assert(
        u256_from_felt(170141183460469231731687303715884105728 * 2) == as_u256(1_u128, 0_u128), 1
    );
}

// TODO(orizi): Use u256 literals when supported.
#[test]
func test_u256_operators() {
    assert(as_u256(1_u128, 1_u128) + as_u256(3_u128, 2_u128) == as_u256(4_u128, 3_u128), 1);
    assert(
        as_u256(1_u128, pow_2_127()) + as_u256(3_u128, pow_2_127()) == as_u256(5_u128, 0_u128), 1
    );
    assert(as_u256(4_u128, 3_u128) - as_u256(1_u128, 1_u128) == as_u256(3_u128, 2_u128), 1);
    assert(
        as_u256(5_u128, 0_u128) - as_u256(1_u128, pow_2_127()) == as_u256(3_u128, pow_2_127()), 1
    );
    assert(as_u256(4_u128, 3_u128) * as_u256(0_u128, 1_u128) == as_u256(4_u128, 3_u128), 1);
    assert(as_u256(4_u128, 3_u128) * as_u256(0_u128, 2_u128) == as_u256(8_u128, 6_u128), 1);
}

#[test]
#[should_panic]
func test_u256_add_overflow() {
    as_u256(pow_2_127(), 1_u128) + as_u256(pow_2_127(), 1_u128);
}

#[test]
#[should_panic]
func test_u256_sub_overflow() {
    as_u256(1_u128, 1_u128) - as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
func test_u256_mul_overflow_1() {
    as_u256(1_u128, 1_u128) * as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
func test_u256_mul_overflow_2() {
    as_u256(0_u128, pow_2_127()) * as_u256(2_u128, 0_u128);
}

// TODO(orizi): Switch to operators and literals when added.
func test_array_helper(idx: u128) -> felt {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, 10);
    array_append::<felt>(arr, 11);
    array_append::<felt>(arr, 12);
    match array_at::<felt>(arr, idx) {
        Option::Some(x) => x,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, u128_to_felt(idx));
            panic(data)
        },
    }
}

#[test]
func test_array() {
    assert(test_array_helper(0_u128) == 10, 1);
    assert(test_array_helper(1_u128) == 11, 1);
    assert(test_array_helper(2_u128) == 12, 1);
}

#[test]
#[should_panic]
func test_array_out_of_bound_1() {
    test_array_helper(3_u128);
}

#[test]
#[should_panic]
func test_array_out_of_bound_2() {
    test_array_helper(11_u128);
}
