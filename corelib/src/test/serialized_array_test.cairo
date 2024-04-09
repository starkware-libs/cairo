use core::test::test_utils::assert_eq;

#[derive(Drop, Serde)]
struct Test {
    a: u16,
    b: u256,
    c: bool,
}

#[test]
fn test_simple_serialized_array() {
    let arr = serialized_array![10, 11, 12_u256];
    assert_eq(arr[0], @10, 'array[0] != 10');
    assert_eq(arr[1], @11, 'array[1] != 11');
    assert_eq(arr[2], @12, 'array[2] != 12');
    assert_eq(arr[3], @0, 'array[3] != 0');
}

#[test]
fn test_complex_serialized_array() {
    let arr = serialized_array![Test { a: 10, b: 11, c: true }, array![1, 100_u256], 12_u256];
    assert_eq(arr[0], @10, 'arr[0] != 10');
    assert_eq(arr[1], @11, 'arr[1] != 11');
    assert_eq(arr[2], @0, 'arr[2] != 0');
    assert_eq(arr[3], @1, 'arr[3] != 1');
    assert_eq(arr[4], @2, 'arr[4] != 2');
    assert_eq(arr[5], @1, 'arr[5] != 1');
    assert_eq(arr[6], @0, 'arr[6] != 0');
    assert_eq(arr[7], @100, 'arr[7] != 100');
    assert_eq(arr[8], @0, 'arr[8] != 0');
    assert_eq(arr[9], @12, 'arr[9] != 12');
}
