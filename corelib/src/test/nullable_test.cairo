use core::nullable::null;

#[test]
fn test_nullable_felt252s() {
    let x = 10;
    let nullable_x = NullableTrait::new(x);
    assert!(!nullable_x.is_null());
    assert_eq!(nullable_x.deref(), x);
    let y = 11;
    let nullable_y = NullableTrait::new(y);
    assert!(!nullable_y.is_null());
    assert_eq!(nullable_y.deref(), y);
    let null: Nullable<felt252> = null();
    assert!(null.is_null());
}

// Testing `u256` as a test for objects of size larger than 1.
#[test]
fn test_nullable_u256() {
    let x: u256 = 10;
    let nullable_x = NullableTrait::new(x);
    assert!(!nullable_x.is_null());
    assert_eq!(nullable_x.deref(), x);
    let y: u256 = 11;
    let nullable_y = NullableTrait::new(y);
    assert!(!nullable_y.is_null());
    assert_eq!(nullable_y.deref(), y);
    let null: Nullable<u256> = null();
    assert!(null.is_null());
}
