use test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_nullable_felt252s() {
    let x = 10;
    let nullable_x = NullableTrait::new(x);
    assert(!nullable_x.is_null(), 'nullable_x.is_null() true');
    assert_eq(@nullable_x.deref(), @x, 'x != 10');
    let y = 11;
    let nullable_y = NullableTrait::new(y);
    assert(!nullable_y.is_null(), 'nullable_y.is_null() true');
    assert_eq(@nullable_y.deref(), @y, 'y != 11');
    let null: Nullable<felt252> = null();
    assert(null.is_null(), 'null.is_null() false');
}

// Test objects of size>1.
#[test]
fn test_nullable_u256() {
    let x: u256 = 10;
    let nullable_x = NullableTrait::new(x);
    assert(!nullable_x.is_null(), 'nullable_x.is_null() true');
    assert_eq(@nullable_x.deref(), @x, 'x != 10');
    let y: u256 = 11;
    let nullable_y = NullableTrait::new(y);
    assert(!nullable_y.is_null(), 'nullable_y.is_null() true');
    assert_eq(@nullable_y.deref(), @y, 'y != 11');
    let null: Nullable<u256> = null();
    assert(null.is_null(), 'null.is_null() false');
}
