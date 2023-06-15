use test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_bool_operators() {
    assert_eq(@true, @true, 't != t');
    assert_eq(@false, @false, 'f != f');
    assert_eq(@!true, @false, '!t != f');
    assert_eq(@!false, @true, '!f != t');
    assert_ne(@true, @false, 't == f');
    assert_ne(@false, @true, 'f == t');
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
fn test_bool_casts() {
    assert_eq(false.into(), 0, 'false as felt252');
    assert_eq(true.into(), 1, 'true as felt252');
    assert_eq(0_felt252.try_into().unwrap(), false, 'felt252 as false');
    assert_eq(1_felt252.try_into().unwrap(), true, 'felt252 as true');
    assert_eq(2_felt252.try_into().unwrap(), (), felt252 as non-bool');
}
