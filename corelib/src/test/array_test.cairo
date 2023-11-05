use core::test::test_utils::assert_ne;

#[test]
fn test_array() {
    let arr = array![10, 11, 12];
    assert_eq!(*arr[0], 10);
    assert_eq!(*arr[1], 11);
    assert_eq!(*arr[2], 12);
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
    assert_eq!(felt252_snap_array_clone.len(), 3);
    assert_eq!(*felt252_snap_array_clone[0], 10);
    assert_eq!(*felt252_snap_array_clone[1], 11);
    assert_eq!(*felt252_snap_array_clone[2], 12);
}

#[test]
fn test_span() {
    let mut span = array![10, 11, 12].span();
    assert_eq!(span.len(), 3);
    assert_eq!(*span.get(0).unwrap().unbox(), 10);
    assert_eq!(*span.pop_front().unwrap(), 10);
    assert_eq!(span.len(), 2);
    assert_eq!(*span[1], 12);
    assert_eq!(*span.pop_back().unwrap(), 12);
    assert_eq!(span.len(), 1);
}

#[test]
fn test_slice() {
    let span = array![10, 11, 12].span();
    assert_eq!(span.slice(0, 3).len(), 3);
    assert_eq!(*span.slice(0, 3)[0], 10);
    assert_eq!(span.slice(0, 2).len(), 2);
    assert_eq!(*span.slice(0, 2)[0], 10);
    assert_eq!(span.slice(0, 1).len(), 1);
    assert_eq!(*span.slice(0, 1)[0], 10);
    assert_eq!(span.slice(0, 0).len(), 0);
    assert_eq!(span.slice(1, 2).len(), 2);
    assert_eq!(*span.slice(1, 2)[0], 11);
    assert_eq!(span.slice(1, 1).len(), 1);
    assert_eq!(*span.slice(1, 1)[0], 11);
    assert_eq!(span.slice(1, 0).len(), 0);
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

    assert!(arr1 == arr1);
    assert!(arr2 == arr2);
    assert!(arr3 == arr3);
    assert!(arr4 == arr4);
    assert!(arr5 == arr5);

    assert!(arr1 != arr2);
    assert!(arr1 != arr3);
    assert!(arr1 != arr4);
    assert!(arr1 != arr5);
    assert!(arr2 != arr3);
    assert!(arr2 != arr4);
    assert!(arr2 != arr5);
    assert!(arr3 != arr4);
    assert!(arr3 != arr5);
    assert!(arr4 != arr5);
}

#[test]
fn test_append() {
    let mut arr = array![10, 11, 12];
    arr.append(13);
    assert!(arr.len() == 4);
    assert_eq!(*arr[3], 13, "Unexpected element");
}

#[test]
fn test_append_span() {
    let mut arr = array![10, 11, 12];
    arr.append_span(arr.span());
    assert!(arr.len() == 6);
    assert_eq!(*arr[3], 10, "Unexpected element");
    assert_eq!(*arr[4], 11, "Unexpected element");
    assert_eq!(*arr[5], 12, "Unexpected element");
}
