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
    let u0 = uint128_const::<0>();
    let u1 = uint128_const::<1>();
    let u2 = uint128_const::<2>();
    let u3 = uint128_const::<3>();
    let u4 = uint128_const::<4>();
    let u5 = uint128_const::<5>();
    let u6 = uint128_const::<6>();
    let u7 = uint128_const::<7>();
    let u8 = uint128_const::<8>();
    let u9 = uint128_const::<9>();
    let u231 = uint128_const::<231>();
    let u1000 = uint128_const::<1000>();
    let u1231 = uint128_const::<1231>();
    assert(u1 + u3 == u4, 1);
    assert(u3 + u6 == u9, 1);
    assert(u3 - u1 == u2, 1);
    assert(u1231 - u231 == u1000, 1);
    assert(u1 * u3 == u3, 1);
    assert(u2 * u4 == u8, 1);
    assert(u8 / u2 == u4, 1);
    assert(u8 % u2 == u0, 1);
    assert(u7 / u3 == u2, 1);
    assert(u7 % u3 == u1, 1);
    assert(u1 < u4, 1);
    assert(u1 <= u4, 1);
    assert(!(u4 < u4), 1);
    assert(u4 <= u4, 1);
    assert(u5 > u2, 1);
    assert(u5 >= u2, 1);
    assert(!(u3 > u3), 1);
    assert(u3 >= u3, 1);
}

func pow_2_127() -> uint128 {
    uint128_const::<170141183460469231731687303715884105728>()
}

func pow_2_64() -> uint128 {
    uint128_const::<18446744073709551616>()
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_1() {
    uint128_const::<0>() - uint128_const::<1>();
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_2() {
    uint128_const::<0>() - uint128_const::<3>();
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_3() {
    uint128_const::<1>() - uint128_const::<3>();
}

#[test]
#[should_panic]
func test_uint128_sub_overflow_4() {
    uint128_const::<100>() - uint128_const::<1000>();
}

#[test]
#[should_panic]
func test_uint128_add_overflow_1() {
    pow_2_127() + pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_add_overflow_2() {
    (pow_2_127() + uint128_const::<12>()) + pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_1() {
    pow_2_64() * pow_2_64();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_2() {
    (pow_2_64() + uint128_const::<1>()) * pow_2_64();
}

#[test]
#[should_panic]
func test_uint128_mul_overflow_3() {
    uint128_const::<2>() * pow_2_127();
}

#[test]
#[should_panic]
func test_uint128_div_by_0() {
    uint128_const::<2>() / uint128_const::<0>();
}

#[test]
#[should_panic]
func test_uint128_mod_by_0() {
    uint128_const::<2>() % uint128_const::<0>();
}

// TODO(orizi): Remove when uint256 literals are supported.
func as_uint256(high: uint128, low: uint128) -> uint256 {
    uint256 { low, high }
}

#[test]
func test_uint256_from_felt() {
    assert(uint256_from_felt(1) == as_uint256(uint128_const::<0>(), uint128_const::<1>()), 1);
    assert(
        uint256_from_felt(
            170141183460469231731687303715884105728 * 2
        ) == as_uint256(uint128_const::<1>(), uint128_const::<0>()),
        1
    );
}

// TODO(orizi): Use uint256 literals when supported.
#[test]
func test_uint256_operators() {
    assert(
        as_uint256(uint128_const::<1>(), uint128_const::<1>())
            + as_uint256(
                uint128_const::<3>(), uint128_const::<2>()
            ) == as_uint256(uint128_const::<4>(), uint128_const::<3>()),
        1
    );
    assert(
        as_uint256(uint128_const::<1>(), pow_2_127())
            + as_uint256(
                uint128_const::<3>(), pow_2_127()
            ) == as_uint256(uint128_const::<5>(), uint128_const::<0>()),
        1
    );
    assert(
        as_uint256(uint128_const::<4>(), uint128_const::<3>())
            - as_uint256(
                uint128_const::<1>(), uint128_const::<1>()
            ) == as_uint256(uint128_const::<3>(), uint128_const::<2>()),
        1
    );
    assert(
        as_uint256(uint128_const::<5>(), uint128_const::<0>())
            - as_uint256(
                uint128_const::<1>(), pow_2_127()
            ) == as_uint256(uint128_const::<3>(), pow_2_127()),
        1
    );
    assert(
        as_uint256(uint128_const::<4>(), uint128_const::<3>())
            * as_uint256(
                uint128_const::<0>(), uint128_const::<1>()
            ) == as_uint256(uint128_const::<4>(), uint128_const::<3>()),
        1
    );
    assert(
        as_uint256(uint128_const::<4>(), uint128_const::<3>())
            * as_uint256(
                uint128_const::<0>(), uint128_const::<2>()
            ) == as_uint256(uint128_const::<8>(), uint128_const::<6>()),
        1
    );
}

#[test]
#[should_panic]
func test_uint256_add_overflow() {
    as_uint256(pow_2_127(), uint128_const::<1>()) + as_uint256(pow_2_127(), uint128_const::<1>());
}

#[test]
#[should_panic]
func test_uint256_sub_overflow() {
    as_uint256(uint128_const::<1>(), uint128_const::<1>())
        - as_uint256(uint128_const::<1>(), uint128_const::<2>());
}

#[test]
#[should_panic]
func test_uint256_mul_overflow_1() {
    as_uint256(uint128_const::<1>(), uint128_const::<1>())
        * as_uint256(uint128_const::<1>(), uint128_const::<2>());
}

#[test]
#[should_panic]
func test_uint256_mul_overflow_2() {
    as_uint256(uint128_const::<0>(), pow_2_127())
        * as_uint256(uint128_const::<2>(), uint128_const::<0>());
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
    assert(test_array_helper(uint128_const::<0>()) == 10, 1);
    assert(test_array_helper(uint128_const::<1>()) == 11, 1);
    assert(test_array_helper(uint128_const::<2>()) == 12, 1);
}

#[test]
#[should_panic]
func test_array_out_of_bound_1() {
    test_array_helper(uint128_const::<3>());
}

#[test]
#[should_panic]
func test_array_out_of_bound_2() {
    test_array_helper(uint128_const::<11>());
}
