use core::iter::chain;

#[test]
fn test_iterator_chain() {
    let a = array![1, 2, 3];
    let b = array![4, 5, 6];

    let mut iter = chain(a, b);

    assert_eq!(iter.next(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(3));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(5));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}
