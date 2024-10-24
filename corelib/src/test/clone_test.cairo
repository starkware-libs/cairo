#[test]
fn test_tuple_clone() {
    let tup = (array![10_u32, 11, 12], array![20_u64, 21, 22], array![30, 31, 32]);
    assert!(tup.clone() == tup);
}

#[test]
fn test_fixed_sized_array_clone() {
    let arr = [array![10, 11, 12], array![20, 21, 22], array![30, 31, 32]];
    assert!(arr.clone() == arr);
}
