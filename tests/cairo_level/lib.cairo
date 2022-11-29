// TODO(orizi): Split tests into modules.

func run_tests() -> felt {
    let test_count = 0;
    bool_tests(test_count);
    felt_tests(test_count);
    uint128_calc_tests(test_count);
    uint128_cmp_tests(test_count);
    test_count
}

func bool_tests(ref test_count: felt) {
    assert_and_count(test_count, true);
    assert_and_count(test_count, ! false);
    assert_and_count(test_count, true == true);
    assert_and_count(test_count, false == false);
    assert_and_count(test_count, ! (true == false));
    assert_and_count(test_count, ! (false == true));
    assert_and_count(test_count, ! (false & false));
    assert_and_count(test_count, ! (true & false));
    assert_and_count(test_count, ! (false & true));
    assert_and_count(test_count, true & true);
    assert_and_count(test_count, ! (false | false));
    assert_and_count(test_count, true | false);
    assert_and_count(test_count, false | true);
    assert_and_count(test_count, true | true);
}

func felt_tests(ref test_count: felt) {
    assert_and_count(test_count, 1 + 3 == 4);
    assert_and_count(test_count, 3 + 6 == 9);
    assert_and_count(test_count, 3 - 1 == 2);
    assert_and_count(test_count, 1231 - 231 == 1000);
    assert_and_count(test_count, 1 * 3 == 3);
    assert_and_count(test_count, 3 * 6 == 18);
    assert_and_count(test_count, 1 < 4);
    assert_and_count(test_count, 1 <= 4);
    assert_and_count(test_count, ! (4 < 4));
    assert_and_count(test_count, 4 <= 4);
    assert_and_count(test_count, 5 > 2);
    assert_and_count(test_count, 5 >= 2);
    assert_and_count(test_count, ! (3 > 3));
    assert_and_count(test_count, 3 >= 3);
}

func uint128_calc_tests(ref test_count: felt) {
    let u1 = uint128_from_felt_panicable(1);
    let u2 = uint128_from_felt_panicable(2);
    let u3 = uint128_from_felt_panicable(3);
    let u4 = uint128_from_felt_panicable(4);
    let u6 = uint128_from_felt_panicable(6);
    let u9 = uint128_from_felt_panicable(9);
    let u231 = uint128_from_felt_panicable(231);
    let u1000 = uint128_from_felt_panicable(1000);
    let u1231 = uint128_from_felt_panicable(1231);
    assert_and_count(test_count, u1 + u3 == u4);
    assert_and_count(test_count, u3 + u6 == u9);
    assert_and_count(test_count, u3 - u1 == u2);
    assert_and_count(test_count, u1231 - u231 == u1000);
}

func uint128_cmp_tests(ref test_count: felt) {
    let u1 = uint128_from_felt_panicable(1);
    let u2 = uint128_from_felt_panicable(2);
    let u3 = uint128_from_felt_panicable(3);
    let u4 = uint128_from_felt_panicable(4);
    let u5 = uint128_from_felt_panicable(5);
    assert_and_count(test_count, u1 < u4);
    assert_and_count(test_count, u1 <= u4);
    assert_and_count(test_count, ! (u4 < u4));
    assert_and_count(test_count, u4 <= u4);
    assert_and_count(test_count, u5 > u2);
    assert_and_count(test_count, u5 >= u2);
    assert_and_count(test_count, ! (u3 > u3));
    assert_and_count(test_count, u3 >= u3);
}

func assert_and_count(ref test_count: felt, cond: bool) {
    assert(cond, 1);
    test_count = test_count + 1;
}
