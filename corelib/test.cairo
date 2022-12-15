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

// TODO(orizi): Use uint128 literals when supported.
#[test]
func test_uint128_operators() {
    assert(1_uint128 + 3_uint128 == 4_uint128, 1);
    assert(3_uint128 + 6_uint128 == 9_uint128, 1);
    assert(3_uint128 - 1_uint128 == 2_uint128, 1);
    assert(1231_uint128 - 231_uint128 == 1000_uint128, 1);
    assert(1_uint128 * 3_uint128 == 3_uint128, 1);
    assert(2_uint128 * 4_uint128 == 8_uint128, 1);
    assert(8_uint128 / 2_uint128 == 4_uint128, 1);
    assert(8_uint128 % 2_uint128 == 0_uint128, 1);
    assert(7_uint128 / 3_uint128 == 2_uint128, 1);
    assert(7_uint128 % 3_uint128 == 1_uint128, 1);
    assert(1_uint128 < 4_uint128, 1);
    assert(1_uint128 <= 4_uint128, 1);
    assert(!(4_uint128 < 4_uint128), 1);
    assert(4_uint128 <= 4_uint128, 1);
    assert(5_uint128 > 2_uint128, 1);
    assert(5_uint128 >= 2_uint128, 1);
    assert(!(3_uint128 > 3_uint128), 1);
    assert(3_uint128 >= 3_uint128, 1);
}

func pow_2_127() -> uint128 {
    0x80000000000000000000000000000000_uint128
}

func pow_2_64() -> uint128 {
    0x10000000000000000_uint128
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_1() {
    0_uint128 - 1_uint128;
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_2() {
    0_uint128 - 3_uint128;
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_3() {
    1_uint128 - 3_uint128;
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_4() {
    100_uint128 - 1000_uint128;
}

#[test]
#[should_panic]
func test_uint128_add_overflow_1() {
    pow_2_127() + pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_add_overflow_2() {
    (pow_2_127() + 12_uint128) + pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_1() {
    pow_2_64() * pow_2_64();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_2() {
    (pow_2_64() + 1_uint128) * pow_2_64();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_3() {
    2_uint128 * pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_div_by_0() {
    2_uint128 / 0_uint128;
}

#[test]
#[should_panic]
func test_uint128_mod_by_0() {
    2_uint128 % 0_uint128;
}

// TODO(orizi): Remove when uint256 literals are supported.
func as_uint256(high: uint128, low: uint128) -> uint256 {
    uint256 { low, high }
}

#[test]
func test_uint256_from_felt() {
    assert(uint256_from_felt(1) == as_uint256(0_uint128, 1_uint128), 1);
    assert(
        uint256_from_felt(
            170141183460469231731687303715884105728 * 2
        ) == as_uint256(1_uint128, 0_uint128),
        1
    );
}

// TODO(orizi): Use uint256 literals when supported.
#[test]
func test_uint256_operators() {
    assert(
        as_uint256(1_uint128, 1_uint128)
            + as_uint256(3_uint128, 2_uint128) == as_uint256(4_uint128, 3_uint128),
        1
    );
    assert(
        as_uint256(1_uint128, pow_2_127())
            + as_uint256(3_uint128, pow_2_127()) == as_uint256(5_uint128, 0_uint128),
        1
    );
    assert(
        as_uint256(4_uint128, 3_uint128)
            - as_uint256(1_uint128, 1_uint128) == as_uint256(3_uint128, 2_uint128),
        1
    );
    assert(
        as_uint256(5_uint128, 0_uint128)
            - as_uint256(1_uint128, pow_2_127()) == as_uint256(3_uint128, pow_2_127()),
        1
    );
    assert(
        as_uint256(4_uint128, 3_uint128)
            * as_uint256(0_uint128, 1_uint128) == as_uint256(4_uint128, 3_uint128),
        1
    );
    assert(
        as_uint256(4_uint128, 3_uint128)
            * as_uint256(0_uint128, 2_uint128) == as_uint256(8_uint128, 6_uint128),
        1
    );
}

#[test]
#[should_panic]
func test_uint256_add_overflow() {
    as_uint256(pow_2_127(), 1_uint128) + as_uint256(pow_2_127(), 1_uint128);
}

#[test]
#[should_panic]
func test_uint256_sub_overflow() {
    as_uint256(1_uint128, 1_uint128) - as_uint256(1_uint128, 2_uint128);
}

#[test]
#[should_panic]
func test_uint256_mul_overflow_1() {
    as_uint256(1_uint128, 1_uint128) * as_uint256(1_uint128, 2_uint128);
}

#[test]
#[should_panic]
func test_uint256_mul_overflow_2() {
    as_uint256(0_uint128, pow_2_127()) * as_uint256(2_uint128, 0_uint128);
}

// TODO(orizi): Switch to operators and literals when added.
func test_array_helper(idx: uint128) -> felt {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, 10);
    array_append::<felt>(arr, 11);
    array_append::<felt>(arr, 12);
    match array_at::<felt>(arr, idx) {
        Option::Some(x) => x,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, uint128_to_felt(idx));
            panic(data)
        },
    }
}

#[test]
func test_array() {
    assert(test_array_helper(0_uint128) == 10, 1);
    assert(test_array_helper(1_uint128) == 11, 1);
    assert(test_array_helper(2_uint128) == 12, 1);
}

#[test]
#[should_panic]
func test_array_out_of_bound_1() {
    test_array_helper(3_uint128);
}

#[test]
#[should_panic]
func test_array_out_of_bound_2() {
    test_array_helper(11_uint128);
}
