#[test]
fn test_advance_by() {
    let mut iter = array![1_u8, 2, 3, 4].into_iter();

    assert_eq!(iter.advance_by(2), Result::Ok(()));
    assert_eq!(iter.next(), Option::Some(3));
    assert_eq!(iter.advance_by(0), Result::Ok(()));
    assert_eq!(iter.advance_by(100), Result::Err(99));
}

#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(4));
    assert_eq!(iter.next(), Option::Some(6));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iterator_enumerate() {
    let mut iter = array!['a', 'b', 'c'].into_iter().enumerate();

    assert_eq!(iter.next(), Option::Some((0, 'a')));
    assert_eq!(iter.next(), Option::Some((1, 'b')));
    assert_eq!(iter.next(), Option::Some((2, 'c')));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iter_adapter_fold() {
    let mut iter = array![1, 2, 3].into_iter();
    let sum = iter.fold(0, |acc, x| acc + x);

    assert_eq!(sum, 6);
}
