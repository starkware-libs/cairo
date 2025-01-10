#[test]
fn test_iter_adapter_map() {
    let a = array![1, 2, 3];
    let mut iter = a.into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}
