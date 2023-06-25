use array::{ArrayTrait, SpanTrait};
use box::BoxTrait;
use clone::Clone;
use option::OptionTrait;
use test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_array() {
    // TODO(Gil): Fix the inference of literals.
    let arr: Array<felt252> = array![10, 11, 12];
    assert_eq(arr[0], @10, 'array[0] != 10');
    assert_eq(arr[1], @11, 'array[1] != 11');
    assert_eq(arr[2], @12, 'array[2] != 12');
}

#[test]
#[should_panic]
fn test_array_out_of_bound_1() {
    let arr: Array<felt252> = array![10, 11, 12];
    arr[3];
}

#[test]
#[should_panic]
fn test_array_out_of_bound_2() {
    let arr: Array<felt252> = array![10, 11, 12];
    arr[11];
}

#[test]
#[available_gas(100000)]
fn test_array_clone() {
    let felt252_snap_array: @Array<felt252> = @array![10, 11, 12];
    let felt252_snap_array_clone = felt252_snap_array.clone();
    assert_eq(@felt252_snap_array_clone.len(), @3, 'array len != 3');
    assert_eq(felt252_snap_array_clone[0], @10, 'array[0] != 10');
    assert_eq(felt252_snap_array_clone[1], @11, 'array[1] != 11');
    assert_eq(felt252_snap_array_clone[2], @12, 'array[2] != 12');
}

#[test]
fn test_span() {
    let mut span: Span<felt252> = array![10, 11, 12].span();
    assert_eq(@span.len(), @3, 'Unexpected span length.');
    assert_eq(span.get(0).unwrap().unbox(), @10, 'Unexpected element');
    assert_eq(span.pop_front().unwrap(), @10, 'Unexpected element');
    assert_eq(@span.len(), @2, 'Unexpected span length.');
    assert_eq(span[1], @12, 'Unexpected element');
    assert_eq(span.pop_back().unwrap(), @12, 'Unexpected element');
    assert_eq(@span.len(), @1, 'Unexpected span length.');
}

#[test]
fn test_slice() {
    let span: Span<felt252> = array![10, 11, 12].span();
    assert_eq(@span.slice(0, 3).len(), @3, 'Unexpected span length.');
    assert_eq(span.slice(0, 3)[0], @10, 'Unexpected Element.');
    assert_eq(@span.slice(0, 2).len(), @2, 'Unexpected span length.');
    assert_eq(span.slice(0, 2)[0], @10, 'Unexpected Element.');
    assert_eq(@span.slice(0, 1).len(), @1, 'Unexpected span length.');
    assert_eq(span.slice(0, 1)[0], @10, 'Unexpected Element.');
    assert_eq(@span.slice(0, 0).len(), @0, 'Unexpected span length.');
    assert_eq(@span.slice(1, 2).len(), @2, 'Unexpected span length.');
    assert_eq(span.slice(1, 2)[0], @11, 'Unexpected Element.');
    assert_eq(@span.slice(1, 1).len(), @1, 'Unexpected span length.');
    assert_eq(span.slice(1, 1)[0], @11, 'Unexpected Element.');
    assert_eq(@span.slice(1, 0).len(), @0, 'Unexpected span length.');
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_1() {
    array![10_felt252, 11, 12].span().slice(3, 1);
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_2() {
    array![10_felt252, 11, 12].span().slice(0, 4);
}
