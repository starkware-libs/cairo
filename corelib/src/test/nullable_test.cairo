use core::test::test_utils::{assert_eq, assert_ne};
use core::nullable::null;

#[test]
fn test_nullable_felt252s() {
    let x = 10;
    let nullable_x = NullableTrait::new(x);
    assert(!nullable_x.is_null(), 'nullable_x.is_null() true');
    assert_eq(@nullable_x.deref(), @x, '*&x != x');
    let y = 11;
    let nullable_y = NullableTrait::new(y);
    assert(!nullable_y.is_null(), 'nullable_y.is_null() true');
    assert_eq(@nullable_y.deref(), @y, '*&y != y');
    let null: Nullable<felt252> = null();
    assert(null.is_null(), 'null.is_null() false');
}

// Testing `u256` as a test for objects of size larger than 1.
#[test]
fn test_nullable_u256() {
    let x: u256 = 10;
    let nullable_x = NullableTrait::new(x);
    assert(!nullable_x.is_null(), 'nullable_x.is_null() true');
    assert_eq(@nullable_x.deref(), @x, '*&x != x');
    let y: u256 = 11;
    let nullable_y = NullableTrait::new(y);
    assert(!nullable_y.is_null(), 'nullable_y.is_null() true');
    assert_eq(@nullable_y.deref(), @y, '*&y != y');
    let null: Nullable<u256> = null();
    assert(null.is_null(), 'null.is_null() false');
}
