use core::iter::zip;

#[test]
fn test_iter_adapter_map() {
    let a = array![1, 2, 3];
    let mut iter = a.into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iterator_zip() {
    let xs = array![1, 2, 3];
    let ys = array![4, 5, 6];

    let mut iter = zip(xs, ys);

    assert_eq!(iter.next(), Option::Some((1, 4)));
    assert_eq!(iter.next(), Option::Some((2, 5)));
    assert_eq!(iter.next(), Option::Some((3, 6)));
    assert_eq!(iter.next(), Option::None);

    // Nested zips are also possible:
    let xs = array![1, 2, 3];
    let ys = array![4, 5, 6];
    let zs = array![7, 8, 9];

    let mut iter = zip(zip(xs, ys), zs);

    assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
    assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
    assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
    assert_eq!(iter.next(), Option::None);
}
