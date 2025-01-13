use core::ops::Range;

#[test]
fn test_iterator_chain_different_types() {
    let a: Array<u8> = array![7, 8, 9];
    let b: Range<u8> = 0..5;

    let mut iter = a.into_iter().chain(b.into_iter());

    assert_eq!(iter.next(), Option::Some(7));
    assert_eq!(iter.next(), Option::Some(8));
    assert_eq!(iter.next(), Option::Some(9));
    assert_eq!(iter.next(), Option::Some(0));
    assert_eq!(iter.next(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(3));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iterator_chain_without_into_iter() {
    let a = array![1, 2, 3];
    let b = array![4, 5, 6];

    let mut iter = a.into_iter().chain(b);

    assert_eq!(iter.next(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(3));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(5));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}
