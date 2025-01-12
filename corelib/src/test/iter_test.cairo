use core::iter::zip;

#[test]
fn test_iterator_zip() {
    let mut iter = zip(array![1, 2, 3], array![4, 5, 6]);

    assert_eq!(iter.next(), Option::Some((1, 4)));
    assert_eq!(iter.next(), Option::Some((2, 5)));
    assert_eq!(iter.next(), Option::Some((3, 6)));
    assert_eq!(iter.next(), Option::None);

    // Nested zips are also possible:
    let mut iter = zip(zip(array![1, 2, 3], array![4, 5, 6]), array![7, 8, 9]);

    assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
    assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
    assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
    assert_eq!(iter.next(), Option::None);
}

