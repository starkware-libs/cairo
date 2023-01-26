use array::ArrayTrait;
use dict::DictFeltToTrait;
use option::OptionTrait;
use option::OptionTraitImpl;

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

impl OptionEcPointCopy of Copy::<Option::<EcPoint>>;

#[test]
fn test_ec_operations() {
    // Beta + 2 is a square, and for x = 1 and alpha = 1, x^3 + alpha * x + beta = beta + 2.
    let beta_p2_root = 2487829544412206244690656897973144572467842667075005257202960243805141046681;
    let p = ec_point_from_x(1).unwrap();
    let (x, y) = ec_point_unwrap(p);
    assert(x == 1, 'x != 1');
    assert(y == beta_p2_root | y == -beta_p2_root, 'y is wrong');

    let mut state = ec_state_init();
    ec_state_add(ref state, p);
    let q = ec_state_finalize(state).expect('zero point');
    let (qx, qy) = ec_point_unwrap(q);
    assert(qx == x, 'bad finalize x');
    assert(qy == y, 'bad finalize y');

    // Try doing the same thing with the EC op builtin.
    let mut state = ec_state_init();
    ec_state_add_mul(ref state, 1, p);
    let q3 = ec_state_finalize(state).expect('zero point');
    let (qx, qy) = ec_point_unwrap(q3);
    assert(qx == x, 'bad EC op x');
    assert(qy == y, 'bad EC op y');

    // Try computing `p + p` using the ec_mul function.
    let double_p = ec_mul(p, 2);
    let (double_x, double_y) = ec_point_unwrap(double_p.unwrap());
    let expected_double_y =
        3572434102142093425782752266058856056057826477682467661647843687948039943621;
    assert(
        double_x == 75984168971785666410219869038140038216102669781812169677875295511117260233,
        'bad double x'
    );
    assert(double_y == expected_double_y | double_y == -expected_double_y, 'bad double y');

    // Compute `2p - p`.
    let (sub_x, sub_y) = ec_point_unwrap((double_p - Option::Some(p)).unwrap());
    assert(sub_x == x, 'bad x for 2p - p');
    assert(sub_y == y, 'bad y for 2p - p');

    // Compute `p - p`.
    assert((Option::Some(p) - Option::Some(p)).is_none(), 'p - p did not return 0.');

    // Compute `(-p) - p`.
    let (sub2_x, sub2_y) = ec_point_unwrap((Option::Some(ec_neg(p)) - Option::Some(p)).unwrap());
    assert(sub2_x == double_x, 'bad x for (-p) - p');
    assert(sub2_y == -double_y, 'bad y for (-p) - p');
}

#[test]
#[should_panic]
fn test_bad_ec_point_creation() {
    ec_point_new(0, 0);
}

#[test]
fn test_ec_point_finalization_zero() {
    let state = ec_state_init();
    let point_at_infinity = ec_state_finalize(state);
    assert(point_at_infinity.is_none(), 'Wrong point');
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
fn test_u8_operators() {
    assert(1_u8 == 1_u8, '1 == 1');
    assert(1_u8 != 2_u8, '1 != 2');
    assert(1_u8 + 3_u8 == 4_u8, '1 + 3 == 4');
    assert(3_u8 + 6_u8 == 9_u8, '3 + 6 == 9');
    assert(3_u8 - 1_u8 == 2_u8, '3 - 1 == 2');
    assert(231_u8 - 131_u8 == 100_u8, '231-131=100');
    assert(1_u8 < 4_u8, '1 < 4');
    assert(1_u8 <= 4_u8, '1 <= 4');
    assert(!(4_u8 < 4_u8), '!(4 < 4)');
    assert(4_u8 <= 4_u8, '4 <= 4');
    assert(5_u8 > 2_u8, '5 > 2');
    assert(5_u8 >= 2_u8, '5 >= 2');
    assert(!(3_u8 > 3_u8), '!(3 > 3)');
    assert(3_u8 >= 3_u8, '3 >= 3');
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_1() {
    0_u8 - 1_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_2() {
    0_u8 - 3_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_3() {
    1_u8 - 3_u8;
}

#[test]
#[should_panic]
fn test_u8_sub_overflow_4() {
    100_u8 - 250_u8;
}

#[test]
#[should_panic]
fn test_u8_add_overflow_1() {
    128_u8 + 128_u8;
}

#[test]
#[should_panic]
fn test_u8_add_overflow_2() {
    200_u8 + 60_u8;
}


#[test]
fn test_u64_operators() {
    assert(1_u64 == 1_u64, '1 == 1');
    assert(1_u64 != 2_u64, '1 != 2');
    assert(1_u64 + 3_u64 == 4_u64, '1 + 3 == 4');
    assert(3_u64 + 6_u64 == 9_u64, '3 + 6 == 9');
    assert(3_u64 - 1_u64 == 2_u64, '3 - 1 == 2');
    assert(231_u64 - 131_u64 == 100_u64, '231-131=100');
    assert(1_u64 < 4_u64, '1 < 4');
    assert(1_u64 <= 4_u64, '1 <= 4');
    assert(!(4_u64 < 4_u64), '!(4 < 4)');
    assert(4_u64 <= 4_u64, '4 <= 4');
    assert(5_u64 > 2_u64, '5 > 2');
    assert(5_u64 >= 2_u64, '5 >= 2');
    assert(!(3_u64 > 3_u64), '!(3 > 3)');
    assert(3_u64 >= 3_u64, '3 >= 3');
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_1() {
    0_u64 - 1_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_2() {
    0_u64 - 3_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_3() {
    1_u64 - 3_u64;
}

#[test]
#[should_panic]
fn test_u64_sub_overflow_4() {
    100_u64 - 250_u64;
}

#[test]
#[should_panic]
fn test_u64_add_overflow_1() {
    0x8000000000000000_u64 + 0x8000000000000000_u64;
}

#[test]
#[should_panic]
fn test_u64_add_overflow_2() {
    0x9000000000000000_u64 + 0x8000000000000001_u64;
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
            * as_u256(0_u128, max_u128) == as_u256(0xfffffffffffffffffffffffffffffffe_u128, 1_u128),
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
    let mut arr = ArrayTrait::new();
    arr.append(10);
    arr.append(11);
    arr.append(12);
    array_at(ref arr, idx)
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

#[test]
fn test_dict_new() -> DictFeltTo::<felt> {
    DictFeltToTrait::new()
}

#[test]
fn test_dict_default_val() {
    let mut dict = DictFeltToTrait::new();
    let default_val = dict.get(0);
    let squashed_dict = dict.squash();
    assert(default_val == 0, 'default_val == 0');
}

// TODO(Gil): Assert before the squash when drop will autosquash the dict.
#[test]
fn test_dict_write_read() {
    let mut dict = DictFeltToTrait::new();
    dict.insert(10, 110);
    dict.insert(11, 111);
    let val10 = dict.get(10);
    let val11 = dict.get(11);
    let val12 = dict.get(12);
    let squashed_dict = dict.squash();
    assert(val10 == 110, 'dict[10] == 110');
    assert(val11 == 111, 'dict[11] == 111');
    assert(val12 == 0, 'default_val == 0');
}

#[test]
fn test_box_unbox_felts() {
    let x = 10;
    let boxed_x = into_box::<felt>(x);
    let y = 11;
    let boxed_y = into_box::<felt>(y);
    assert(unbox::<felt>(boxed_x) == 10, 'x == 10');
    assert(unbox::<felt>(boxed_y) == 11, 'y == 11');
}


// Test objects of size>1.
#[test]
fn test_box_unbox_u256() {
    let x = as_u256(1_u128, 0_u128);
    let boxed_x = into_box::<u256>(x);
    let y = as_u256(1_u128, 1_u128);
    let boxed_y = into_box::<u256>(y);
    assert(unbox::<u256>(boxed_x) == as_u256(1_u128, 0_u128), 'unbox u256 x');
    assert(unbox::<u256>(boxed_y) == as_u256(1_u128, 1_u128), 'unbox u256 y');
}
