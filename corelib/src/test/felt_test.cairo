use core::test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_felt252_operators() {
    assert_eq(@(1 + 3), @4, '1 + 3 == 4');
    assert_eq(@(3 + 6), @9, '3 + 6 == 9');
    assert_eq(@(3 - 1), @2, '3 - 1 == 2');
    assert_eq(@(1231 - 231), @1000, '1231-231=1000');
    assert_eq(@(1 * 3), @3, '1 * 3 == 3');
    assert_eq(@(3 * 6), @18, '3 * 6 == 18');
    assert_eq(@(-3), @(1 - 4), '-3 == 1 - 4');
}

#[test]
fn test_felt252_clone() {
    let felt252_snap = @2;
    let felt252_clone = felt252_snap.clone();
    assert_eq(@felt252_clone, @2, 'felt252_clone == 2');
}
