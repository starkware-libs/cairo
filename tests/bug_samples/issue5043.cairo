#[test]
fn array_of_u256s() {
    let arr = array![1_u256, 2_u256, 3_u256];
    assert!(arr[0].low == @1);
}
