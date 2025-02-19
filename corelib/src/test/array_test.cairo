use crate::test::test_utils::assert_eq;
use crate::iter::Extend;

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
    let span = [10, 11, 12].span();
    assert_eq!(span.slice(0, 3), span);
    assert_eq!(span.slice(0, 2), [10, 11].span());
    assert_eq!(span.slice(0, 1), [10].span());
    assert!(span.slice(0, 0).is_empty());
    assert_eq!(span.slice(1, 2), [11, 12].span());
    assert_eq!(span.slice(1, 1), [11].span());
    assert!(span.slice(1, 0).is_empty());
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_1() {
    [10, 11, 12].span().slice(3, 1);
}

#[test]
#[should_panic]
fn test_slice_out_of_bound_2() {
    [10, 11, 12].span().slice(0, 4);
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

#[test]
fn test_felt252_span_from_tuple() {
    let span: Span<felt252> = [10, 20, 30].span();
    assert!(*span[0] == 10);
    assert!(*span[1] == 20);
    assert!(*span[2] == 30);
}

#[test]
fn test_tuple_span_from_tuple() {
    let span: Span<(felt252, felt252, felt252)> = [(10, 20, 30), (40, 50, 60), (70, 80, 90)].span();
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

fn consume<const N: usize>(_arr: [felt252; N]) {}

#[test]
fn test_fixed_size_array_copy() {
    let arr = [10, 11, 12];
    consume(arr);
    consume(arr);
}

/// Helper for tests removing the box wrapping a fixed sized array wrapped by an option.
fn debox<T, const SIZE: usize>(value: Option<@Box<[T; SIZE]>>) -> Option<@[T; SIZE]> {
    Some(value?.as_snapshot().unbox())
}

#[test]
fn test_span_into_fixed_size_array() {
    assert!(debox::<felt252, 2>([10, 11, 12].span().try_into()).is_none());
    assert!(debox::<felt252, 3>([10, 11, 12].span().try_into()) == Some(@[10, 11, 12]));
    assert!(debox::<felt252, 4>([10, 11, 12].span().try_into()).is_none());
    assert!(debox::<u256, 2>([10, 11, 12].span().try_into()).is_none());
    assert!(debox::<u256, 3>([10, 11, 12].span().try_into()) == Some(@[10, 11, 12]));
    assert!(debox::<u256, 4>([10, 11, 12].span().try_into()).is_none());
    assert!(debox::<felt252, 1>([].span().try_into()).is_none());
    assert!(debox::<felt252, 0>([].span().try_into()) == Some(@[]));
}

#[test]
fn test_span_multi_pop() {
    let mut span = array![10, 11, 12, 13].span();
    assert!(span.multi_pop_front::<5>().is_none());
    assert!(debox(span.multi_pop_front::<4>()) == Some(@[10, 11, 12, 13]));
    let mut span = array![10, 11, 12, 13].span();
    assert!(debox(span.multi_pop_front::<3>()) == Some(@[10, 11, 12]));
    let mut span = array![10, 11, 12, 13].span();
    assert!(span.multi_pop_back::<5>().is_none());
    assert!(debox(span.multi_pop_back::<4>()) == Some(@[10, 11, 12, 13]));
    let mut span = array![10, 11, 12, 13].span();
    assert!(debox(span.multi_pop_back::<3>()) == Some(@[11, 12, 13]));
}

#[test]
fn test_span_iterator() {
    let mut iter = array![10, 11, 12, 13_felt252].span().into_iter();
    let mut i = 10;
    while let Some(value) = iter.next() {
        assert_eq!(value, @i);
        i += 1;
    }
}

#[test]
fn test_array_iterator() {
    let mut iter = array![10, 11, 12, 13].into_iter();
    let mut i = 10;
    while let Some(value) = iter.next() {
        assert_eq!(value, i);
        i += 1;
    }
}

#[test]
fn test_snapshot_fixed_size_array_iterator() {
    let mut iter = (@[10_usize, 11, 12, 13]).into_iter();
    assert_eq!(iter.next(), Some(@10));
    assert_eq!(iter.next(), Some(@11));
    assert_eq!(iter.next(), Some(@12));
    assert_eq!(iter.next(), Some(@13));
    assert!(iter.next().is_none());
}

#[test]
fn test_empty_snapshot_fixed_size_array_iterator() {
    let mut input: [usize; 0] = [];
    let mut iter = (@input).into_iter();
    assert!(iter.next().is_none());
}

fn test_snapshot_array_into_iter() {
    let mut iter = (@array![1, 2, 3, 4, 5]).into_iter();
    assert_eq!(iter.next(), Some(@1));
    assert_eq!(iter.next(), Some(@2));
    assert_eq!(iter.next(), Some(@3));
    assert_eq!(iter.next(), Some(@4));
    assert_eq!(iter.next(), Some(@5));
    assert!(iter.next().is_none());
}

#[test]
fn test_snapshot_span_into_iter() {
    let mut iter = (@(array![1, 2, 3, 4, 5].span())).into_iter();
    assert_eq!(iter.next(), Some(@1));
    assert_eq!(iter.next(), Some(@2));
    assert_eq!(iter.next(), Some(@3));
    assert_eq!(iter.next(), Some(@4));
    assert_eq!(iter.next(), Some(@5));
    assert!(iter.next().is_none());
}

#[test]
fn test_array_from_iterator() {
    assert_eq!(FromIterator::from_iter(0..5_u32), array![0, 1, 2, 3, 4]);
}

#[test]
fn test_array_from_collect() {
    assert_eq!((0..5_u32).into_iter().collect(), array![0, 1, 2, 3, 4]);
}

#[test]
fn test_array_extend() {
    let mut arr = array![1_u32, 2, 3];
    arr.extend(4..6_u32);
    assert_eq!(arr, array![1, 2, 3, 4, 5]);
}

#[test]
fn test_array_into_span() {
    assert_eq!(array![1, 2, 3].span(), array![1, 2, 3].into())
}

#[test]
fn test_span_into_array() {
    assert_eq!(array![1, 2, 3], array![1, 2, 3].span().into());
}

#[test]
fn test_array_snap_into_span() {
    assert_eq!(array![1, 2, 3].span(), (@array![1, 2, 3]).into());
}

#[test]
fn test_span_into_array_snap() {
    assert_eq!(@array![1, 2, 3], array![1, 2, 3].span().into());
}
#[test]
fn nested_for_loop() {
    let mat = array![array![1, 2], array![3, 4], array![5, 6]];
    let mut result = 0;
    for arr in mat {
        for i in arr {
            result += i;
        }
    }
    assert_eq!(result, 21);
}
