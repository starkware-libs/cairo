#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iterator_zip() {
    let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6].into_iter());

    assert_eq!(iter.next(), Option::Some((1, 4)));
    assert_eq!(iter.next(), Option::Some((2, 5)));
    assert_eq!(iter.next(), Option::Some((3, 6)));
    assert_eq!(iter.next(), Option::None);

    // Nested zips
    let mut iter = array![1, 2, 3]
        .into_iter()
        .zip(array![4, 5, 6].into_iter())
        .zip(array![7, 8, 9].into_iter());

    assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
    assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
    assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
    assert_eq!(iter.next(), Option::None);
}
