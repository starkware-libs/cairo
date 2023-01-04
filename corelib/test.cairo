#[test]
#[should_panic]
fn test_assert_false() {
    assert(false, 'assert(false)');
}

#[test]
fn test_assert_true() {
    assert(true, 'assert(true)');
}

#[test]
fn test_bool_operators() {
    assert(true == true, 't == t');
    assert(false == false, 'f == f');
    assert(!true == false, '!t == f');
    assert(!false == true, '!f == t');
    assert(true != false, 't != f');
    assert(false != true, 'f != t');
    assert(!(false & false), '!(f & f)');
    assert(!(true & false), '!(t & f)');
    assert(!(false & true), '!(f & t)');
    assert(true & true, 't & t');
    assert(!(false | false), '!(f | f)');
    assert(true | false, 't | f');
    assert(false | true, 'f | t');
    assert(true | true, 't | t');
    assert(!(false ^ false), '!(f ^ f)');
    assert(true ^ false, 't ^ f');
    assert(false ^ true, 'f ^ t');
    assert(!(true ^ true), '!(t ^ t)');
}

#[test]
fn test_ec_operations() {
    let state = ec_init_state();
    // Beta + 2 is a square, and for x = 1 and alpha = 1, x^3 + alpha * x + beta = beta + 2.
    let beta_plus_2 = 3141592653589793238462643383279502884197169399375105820974944592307816406667;
    let beta_p2_root = 2487829544412206244690656897973144572467842667075005257202960243805141046681;
    let p = ec_point_from_felts(1, beta_p2_root);
    let (x, y) = ec_point_unwrap(p);
    assert(x == 1, 'x == 1');
    assert(y == beta_p2_root, 'y is correct');
    let state2 = ec_add_to_state(state, p);
}

#[test]
#[should_panic]
fn test_bad_ec_point_creation() {
    ec_point_from_felts(0, 0);
}

#[test]
fn test_felt_operators() {
    assert(1 + 3 == 4, '1 + 3 == 4');
    assert(3 + 6 == 9, '3 + 6 == 9');
    assert(3 - 1 == 2, '3 - 1 == 2');
    assert(1231 - 231 == 1000, '1231-231=1000');
    assert(1 * 3 == 3, '1 * 3 == 3');
    assert(3 * 6 == 18, '3 * 6 == 18');
    assert(1 < 4, '1 < 4');
    assert(1 <= 4, '1 <= 4');
    assert(!(4 < 4), '!(4 < 4)');
    assert(4 <= 4, '4 <= 4');
    assert(5 > 2, '5 > 2');
    assert(5 >= 2, '5 >= 2');
    assert(!(3 > 3), '!(3 > 3)');
    assert(3 >= 3, '3 >= 3');
}

#[test]
fn test_u128_operators() {
    assert(1_u128 == 1_u128, '1 == 1');
    assert(!(1_u128 == 2_u128), '!(1 == 2)');
    assert(1_u128 + 3_u128 == 4_u128, '1 + 3 == 4');
    assert(3_u128 + 6_u128 == 9_u128, '3 + 6 == 9');
    assert(3_u128 - 1_u128 == 2_u128, '3 - 1 == 2');
    assert(1231_u128 - 231_u128 == 1000_u128, '1231-231=1000');
    assert(1_u128 * 3_u128 == 3_u128, '1 * 3 == 3');
    assert(2_u128 * 4_u128 == 8_u128, '2 * 4 == 8');
    assert(8_u128 / 2_u128 == 4_u128, '8 / 2 == 4');
    assert(8_u128 % 2_u128 == 0_u128, '8 % 2 == 0');
    assert(7_u128 / 3_u128 == 2_u128, '7 / 3 == 2');
    assert(7_u128 % 3_u128 == 1_u128, '7 % 3 == 1');
    assert(1_u128 < 4_u128, '1 < 4');
    assert(1_u128 <= 4_u128, '1 <= 4');
    assert(!(4_u128 < 4_u128), '!(4 < 4)');
    assert(4_u128 <= 4_u128, '4 <= 4');
    assert(5_u128 > 2_u128, '5 > 2');
    assert(5_u128 >= 2_u128, '5 >= 2');
    assert(!(3_u128 > 3_u128), '!(3 > 3)');
    assert(3_u128 >= 3_u128, '3 >= 3');
    assert((1_u128 | 2_u128) == 3_u128, '1 | 2 == 3');
    assert((1_u128 & 2_u128) == 0_u128, '1 & 2 == 0');
    assert((1_u128 ^ 2_u128) == 3_u128, '1 ^ 2 == 3');
    assert((2_u128 | 2_u128) == 2_u128, '2 | 2 == 2');
    assert((2_u128 & 2_u128) == 2_u128, '2 & 2 == 2');
    assert((2_u128 & 3_u128) == 2_u128, '2 & 3 == 2');
    assert((3_u128 ^ 6_u128) == 5_u128, '3 ^ 6 == 5');
}

fn pow_2_127() -> u128 {
    0x80000000000000000000000000000000_u128
}

fn pow_2_64() -> u128 {
    0x10000000000000000_u128
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_1() {
    0_u128 - 1_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_2() {
    0_u128 - 3_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_3() {
    1_u128 - 3_u128;
}

#[test]
#[should_panic]
fn test_u128_sub_overflow_4() {
    100_u128 - 1000_u128;
}

#[test]
#[should_panic]
fn test_u128_add_overflow_1() {
    pow_2_127() + pow_2_127();
}

#[test]
#[should_panic]
fn test_u128_add_overflow_2() {
    (pow_2_127() + 12_u128) + pow_2_127();
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_1() {
    pow_2_64() * pow_2_64();
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_2() {
    (pow_2_64() + 1_u128) * pow_2_64();
}

#[test]
#[should_panic]
fn test_u128_mul_overflow_3() {
    2_u128 * pow_2_127();
}

#[test]
#[should_panic]
fn test_u128_div_by_0() {
    2_u128 / 0_u128;
}

#[test]
#[should_panic]
fn test_u128_mod_by_0() {
    2_u128 % 0_u128;
}

// TODO(orizi): Remove when u256 literals are supported.
fn as_u256(high: u128, low: u128) -> u256 {
    u256 { low, high }
}

#[test]
fn test_u256_from_felt() {
    assert(u256_from_felt(1) == as_u256(0_u128, 1_u128), 'into 1');
    assert(
        u256_from_felt(170141183460469231731687303715884105728 * 2) == as_u256(1_u128, 0_u128),
        'into 2**128'
    );
}

// TODO(orizi): Use u256 literals when supported.
#[test]
fn test_u256_operators() {
    let max_u128 = 0xffffffffffffffffffffffffffffffff_u128;
    assert(as_u256(1_u128, 1_u128) + as_u256(3_u128, 2_u128) == as_u256(4_u128, 3_u128), 'no OF');
    assert(
        as_u256(1_u128, pow_2_127()) + as_u256(3_u128, pow_2_127()) == as_u256(5_u128, 0_u128),
        'basic OF'
    );
    assert(as_u256(4_u128, 3_u128) - as_u256(1_u128, 1_u128) == as_u256(3_u128, 2_u128), 'no UF');
    assert(
        as_u256(5_u128, 0_u128) - as_u256(1_u128, pow_2_127()) == as_u256(3_u128, pow_2_127()),
        'basic UF'
    );
    assert(
        as_u256(4_u128, 3_u128) * as_u256(0_u128, 1_u128) == as_u256(4_u128, 3_u128), 'mul by 1'
    );
    assert(
        as_u256(4_u128, 3_u128) * as_u256(0_u128, 2_u128) == as_u256(8_u128, 6_u128), 'mul by 2'
    );
    assert(
        as_u256(0_u128, pow_2_127()) * as_u256(0_u128, 2_u128) == as_u256(1_u128, 0_u128),
        'basic mul OF'
    );
    assert(
        as_u256(0_u128, max_u128)
            * as_u256(
                0_u128, max_u128
            ) == as_u256(0xfffffffffffffffffffffffffffffffe_u128, 1_u128),
        'max_u128 * max_u128'
    );
    assert(
        as_u256(0_u128, max_u128) * as_u256(0_u128, 1_u128) == as_u256(0_u128, max_u128),
        'max_u128 * 1'
    );
    assert(
        as_u256(0_u128, 1_u128) * as_u256(0_u128, max_u128) == as_u256(0_u128, max_u128),
        '1 * max_u128'
    );
    assert(
        (as_u256(1_u128, 2_u128) | as_u256(2_u128, 2_u128)) == as_u256(3_u128, 2_u128),
        '1.2|2.2==3.2'
    );
    assert(
        (as_u256(2_u128, 1_u128) | as_u256(2_u128, 2_u128)) == as_u256(2_u128, 3_u128),
        '2.1|2.2==2.3'
    );
    assert(
        (as_u256(2_u128, 2_u128) | as_u256(1_u128, 2_u128)) == as_u256(3_u128, 2_u128),
        '2.2|1.2==3.2'
    );
    assert(
        (as_u256(2_u128, 2_u128) | as_u256(2_u128, 1_u128)) == as_u256(2_u128, 3_u128),
        '2.2|2.1==2.3'
    );
    assert(
        (as_u256(1_u128, 2_u128) & as_u256(2_u128, 2_u128)) == as_u256(0_u128, 2_u128),
        '1.2&2.2==0.2'
    );
    assert(
        (as_u256(2_u128, 1_u128) & as_u256(2_u128, 2_u128)) == as_u256(2_u128, 0_u128),
        '2.1&2.2==2.0'
    );
    assert(
        (as_u256(2_u128, 2_u128) & as_u256(1_u128, 2_u128)) == as_u256(0_u128, 2_u128),
        '2.2&1.2==0.2'
    );
    assert(
        (as_u256(2_u128, 2_u128) & as_u256(2_u128, 1_u128)) == as_u256(2_u128, 0_u128),
        '2.2&2.1==2.0'
    );
    assert(
        (as_u256(1_u128, 2_u128) ^ as_u256(2_u128, 2_u128)) == as_u256(3_u128, 0_u128),
        '1.2^2.2==3.0'
    );
    assert(
        (as_u256(2_u128, 1_u128) ^ as_u256(2_u128, 2_u128)) == as_u256(0_u128, 3_u128),
        '2.1^2.2==0.3'
    );
    assert(
        (as_u256(2_u128, 2_u128) ^ as_u256(1_u128, 2_u128)) == as_u256(3_u128, 0_u128),
        '2.2^1.2==3.0'
    );
    assert(
        (as_u256(2_u128, 2_u128) ^ as_u256(2_u128, 1_u128)) == as_u256(0_u128, 3_u128),
        '2.2^2.1==0.3'
    );
    assert(as_u256(1_u128, 2_u128) < as_u256(2_u128, 2_u128), '1.2<2.2');
    assert(as_u256(2_u128, 1_u128) < as_u256(2_u128, 2_u128), '2.1<2.2');
    assert(!(as_u256(2_u128, 2_u128) < as_u256(1_u128, 2_u128)), '2.2<1.2');
    assert(!(as_u256(2_u128, 2_u128) < as_u256(2_u128, 1_u128)), '2.2<2.1');
    assert(!(as_u256(2_u128, 2_u128) < as_u256(2_u128, 2_u128)), '2.2<2.2');
    assert(as_u256(1_u128, 2_u128) <= as_u256(2_u128, 2_u128), '1.2<=2.2');
    assert(as_u256(2_u128, 1_u128) <= as_u256(2_u128, 2_u128), '2.1<=2.2');
    assert(!(as_u256(2_u128, 2_u128) <= as_u256(1_u128, 2_u128)), '2.2<=1.2');
    assert(!(as_u256(2_u128, 2_u128) <= as_u256(2_u128, 1_u128)), '2.2<=2.1');
    assert(as_u256(2_u128, 2_u128) <= as_u256(2_u128, 2_u128), '2.2<=2.2');
    assert(!(as_u256(1_u128, 2_u128) > as_u256(2_u128, 2_u128)), '1.2>2.2');
    assert(!(as_u256(2_u128, 1_u128) > as_u256(2_u128, 2_u128)), '2.1>2.2');
    assert(as_u256(2_u128, 2_u128) > as_u256(1_u128, 2_u128), '2.2>1.2');
    assert(as_u256(2_u128, 2_u128) > as_u256(2_u128, 1_u128), '2.2>2.1');
    assert(!(as_u256(2_u128, 2_u128) > as_u256(2_u128, 2_u128)), '2.2>2.2');
    assert(!(as_u256(1_u128, 2_u128) >= as_u256(2_u128, 2_u128)), '1.2>=2.2');
    assert(!(as_u256(2_u128, 1_u128) >= as_u256(2_u128, 2_u128)), '2.1>=2.2');
    assert(as_u256(2_u128, 2_u128) >= as_u256(1_u128, 2_u128), '2.2>=1.2');
    assert(as_u256(2_u128, 2_u128) >= as_u256(2_u128, 1_u128), '2.2>=2.1');
    assert(as_u256(2_u128, 2_u128) >= as_u256(2_u128, 2_u128), '2.2>=2.2');
}

#[test]
#[should_panic]
fn test_u256_add_overflow() {
    as_u256(pow_2_127(), 1_u128) + as_u256(pow_2_127(), 1_u128);
}

#[test]
#[should_panic]
fn test_u256_sub_overflow() {
    as_u256(1_u128, 1_u128) - as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_1() {
    as_u256(1_u128, 1_u128) * as_u256(1_u128, 2_u128);
}

#[test]
#[should_panic]
fn test_u256_mul_overflow_2() {
    as_u256(0_u128, pow_2_127()) * as_u256(2_u128, 0_u128);
}

// TODO(orizi): Switch to operators and literals when added.
fn test_array_helper(idx: u128) -> felt {
    let mut arr = array_new::<felt>();
    array_append::<felt>(arr, 10);
    array_append::<felt>(arr, 11);
    array_append::<felt>(arr, 12);
    match array_at::<felt>(arr, idx) {
        Option::Some(x) => x,
        Option::None(()) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'array index OOB');
            panic(data)
        },
    }
}

#[test]
fn test_array() {
    assert(test_array_helper(0_u128) == 10, 'array[0] == 10');
    assert(test_array_helper(1_u128) == 11, 'array[1] == 11');
    assert(test_array_helper(2_u128) == 12, 'array[2] == 12');
}

#[test]
#[should_panic]
fn test_array_out_of_bound_1() {
    test_array_helper(3_u128);
}

#[test]
#[should_panic]
fn test_array_out_of_bound_2() {
    test_array_helper(11_u128);
}
