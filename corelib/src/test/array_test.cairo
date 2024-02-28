use core::test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_array() {
    let arr = array![10, 11, 12];
    assert_eq(arr[0], @10, 'array[0] != 10');
    assert_eq(arr[1], @11, 'array[1] != 11');
    assert_eq(arr[2], @12, 'array[2] != 12');
}

#[test]
#[should_panic]
fn test_array_out_of_bound_1() {
    let arr = array![10, 11, 12];
    arr[3];
}

#[test]
#[should_panic]
fn test_array_out_of_bound_2() {
    let arr = array![10, 11, 12];
    arr[11];
}

#[test]
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
    let mut span = array![10, 11, 12].span();
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
    let span = array![10, 11, 12].span();
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
    array![10, 11, 12].span().slice(3, 1);
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_2() {
    array![10, 11, 12].span().slice(0, 4);
}

#[test]
fn test_equality() {
    let arr1 = array![];
    let arr2 = array![10, 11, 12];
    let arr3 = array![10, 11, 13];
    let arr4 = array![10, 11];
    let arr5 = array![10, 11, 12, 13];

    assert(arr1 == arr1, 'arr1 != arr1');
    assert(arr2 == arr2, 'arr2 != arr2');
    assert(arr3 == arr3, 'arr3 != arr3');
    assert(arr4 == arr4, 'arr4 != arr4');
    assert(arr5 == arr5, 'arr5 != arr5');

    assert(arr1 != arr2, 'arr1 == arr2');
    assert(arr1 != arr3, 'arr1 == arr3');
    assert(arr1 != arr4, 'arr1 == arr4');
    assert(arr1 != arr5, 'arr1 == arr5');
    assert(arr2 != arr3, 'arr2 == arr3');
    assert(arr2 != arr4, 'arr2 == arr4');
    assert(arr2 != arr5, 'arr2 == arr5');
    assert(arr3 != arr4, 'arr3 == arr4');
    assert(arr3 != arr5, 'arr3 == arr5');
    assert(arr4 != arr5, 'arr4 == arr5');
}

#[test]
fn test_append() {
    let mut arr = array![10, 11, 12];
    arr.append(13);
    assert(arr.len() == 4, 'Unexpected length');
    assert_eq(arr[3], @13, 'Unexpected element');
}

#[test]
fn test_append_span() {
    let mut arr = array![10, 11, 12];
    arr.append_span(arr.span());
    assert(arr.len() == 6, 'Unexpected length');
    assert_eq(arr[3], @10, 'Unexpected element');
    assert_eq(arr[4], @11, 'Unexpected element');
    assert_eq(arr[5], @12, 'Unexpected element');
}

mod felt252_span_from_tuple {
    pub extern fn span_from_tuple<T>(struct_like: Box<@T>) -> @Array<felt252> nopanic;
}

mod tuple_span_from_tuple {
    pub extern fn span_from_tuple<T>(
        struct_like: Box<@T>
    ) -> @Array<(felt252, felt252, felt252)> nopanic;
}

#[test]
fn test_felt252_span_from_tuple() {
    let span = felt252_span_from_tuple::span_from_tuple(BoxTrait::new(@(10, 20, 30)));
    assert!(*span[0] == 10);
    assert!(*span[1] == 20);
    assert!(*span[2] == 30);
}

#[test]
fn test_tuple_span_from_tuple() {
    let multi_tuple = ((10, 20, 30), (40, 50, 60), (70, 80, 90));
    let span = tuple_span_from_tuple::span_from_tuple(BoxTrait::new(@multi_tuple));
    assert!(*span[0] == (10, 20, 30));
    assert!(*span[1] == (40, 50, 60));
    assert!(*span[2] == (70, 80, 90));
}

#[test]
fn test_fixed_size_array() {
    let arr = [10, 11, 12];
    let [x, y, z] = arr;
    assert_eq!(x, 10);
    assert_eq!(y, 11);
    assert_eq!(z, 12);
}
