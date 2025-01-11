#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

fn test_iterator_enumerate() {
    let mut iter = array!['a', 'b', 'c'].into_iter().enumerate();

    assert_eq!(iter.next(), Option::Some((0, 'a')));
    assert_eq!(iter.next(), Option::Some((1, 'b')));
    assert_eq!(iter.next(), Option::Some((2, 'c')));
    assert_eq!(iter.next(), Option::None);
}
