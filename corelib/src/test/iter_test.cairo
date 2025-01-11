#[test]
fn test_iterator_enumerate() {
    let a = array!['a', 'b', 'c'];

    let mut iter = a.into_iter().enumerate();

    assert_eq!(iter.next(), Option::Some((0, 'a')));
    assert_eq!(iter.next(), Option::Some((1, 'b')));
    assert_eq!(iter.next(), Option::Some((2, 'c')));
    assert_eq!(iter.next(), Option::None);
}
