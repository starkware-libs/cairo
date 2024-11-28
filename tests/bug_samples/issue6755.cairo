#[test]
fn const_box_test() {
    let value: Nullable<u32> = NullableTrait::new(10);
    let null_value: Nullable<u32> = Default::default();
    let y = null_value.deref_or(1);
    assert!(y == 1);
    if value.is_null() {} else {}
}
