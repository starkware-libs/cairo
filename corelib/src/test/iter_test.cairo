#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iter_adapter_collect() {
    let arr: Array<u32> = (0..3_u32).into_iter().collect();

    assert_eq!(*arr[0], 0);
    assert_eq!(*arr[1], 1);
    assert_eq!(*arr[2], 2);
}
