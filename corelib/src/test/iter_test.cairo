#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iter_adapter_fold() {
    let mut iter = array![1, 2, 3].into_iter();
    let sum = iter.fold(0, |acc, x| acc + x);

    assert_eq!(sum, 6);
}
