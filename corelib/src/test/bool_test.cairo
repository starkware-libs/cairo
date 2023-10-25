use core::test::test_utils::{assert_eq, assert_ne};

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
fn test_bool_conversion() {
    assert_eq(@false.into(), @0, 'f.into() != 0');
    assert_eq(@true.into(), @1, 'f.into() != 1');
}
