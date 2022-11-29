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
    let t = bool::True(());
    let f = bool::False(());
    assert_and_count(test_count, t);
    assert_and_count(test_count, bool_not(f));
    assert_and_count(test_count, t == t);
    assert_and_count(test_count, f == f);
    assert_and_count(test_count, bool_not(t == f));
    assert_and_count(test_count, bool_not(f == t));
    assert_and_count(test_count, bool_not(f & f));
    assert_and_count(test_count, bool_not(t & f));
    assert_and_count(test_count, bool_not(f & t));
    assert_and_count(test_count, t & t);
    assert_and_count(test_count, bool_not(f | f));
    assert_and_count(test_count, t | f);
    assert_and_count(test_count, f | t);
    assert_and_count(test_count, t | t);
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
    assert_and_count(test_count, bool_not(4 < 4));
    assert_and_count(test_count, 4 <= 4);
    assert_and_count(test_count, 5 > 2);
    assert_and_count(test_count, 5 >= 2);
    assert_and_count(test_count, bool_not(3 > 3));
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
    assert_and_count(test_count, bool_not(u4 < u4));
    assert_and_count(test_count, u4 <= u4);
    assert_and_count(test_count, u5 > u2);
    assert_and_count(test_count, u5 >= u2);
    assert_and_count(test_count, bool_not(u3 > u3));
    assert_and_count(test_count, u3 >= u3);
}

func assert_and_count(ref test_count: felt, cond: bool) {
    assert(cond, 1);
    test_count = test_count + 1;
}
