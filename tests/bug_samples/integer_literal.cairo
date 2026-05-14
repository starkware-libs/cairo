/// Unsuffixed literals unify across an array when only one element pins the type.
#[test]
fn test_unification_from_single_suffixed_element() {
    let arr: Array<u32> = array![10, 20, 30_u32];
    assert_eq!(arr.len(), 3);
    assert_eq!(*arr[0], 10_u32);
    assert_eq!(*arr[2], 30_u32);
}

/// Unsuffixed literals default to felt252 when no other constraint pins them.
#[test]
fn test_default_to_felt252() {
    let x = 5;
    let y: felt252 = x;
    assert_eq!(y, 5);
}

/// Unsuffixed literals satisfy `Drop` and `Copy` directly — no `NumericLiteral` impl is searched.
#[test]
fn test_copy_and_drop_on_unsuffixed_literals() {
    let x = 7_u32;
    let a = x;
    let b = x;
    assert_eq!(a + b, 14);
}
