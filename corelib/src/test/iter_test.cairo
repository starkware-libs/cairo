#[test]
fn test_iter_count() {
    let mut empty_iter = ArrayTrait::<usize>::new().into_iter();
    let count = empty_iter.count();
    assert_eq!(count, 0);

    let mut iter = array![1, 2, 3].into_iter();
    let count = iter.count();

    assert_eq!(count, 3);
}

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
fn test_iterator_zip() {
    let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]);

    assert_eq!(iter.next(), Option::Some((1, 4)));
    assert_eq!(iter.next(), Option::Some((2, 5)));
    assert_eq!(iter.next(), Option::Some((3, 6)));
    assert_eq!(iter.next(), Option::None);

    // Nested zips
    let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]).zip(array![7, 8, 9]);

    assert_eq!(iter.next(), Option::Some(((1, 4), 7)));
    assert_eq!(iter.next(), Option::Some(((2, 5), 8)));
    assert_eq!(iter.next(), Option::Some(((3, 6), 9)));
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iter_adapter_fold() {
    let mut iter = array![1, 2, 3].into_iter();
    let sum = iter.fold(0, |acc, x| acc + x);

    assert_eq!(sum, 6);
}

#[test]
fn test_iter_adapter_collect() {
    assert_eq!((0..3_u32).into_iter().collect(), array![0, 1, 2]);
}

#[test]
fn test_iter_find() {
    let mut iter = array![1, 2, 3].into_iter();
    assert_eq!(iter.find(|x| x == 2), Option::Some(2));
    assert_eq!(iter.find(|x| x == 5), Option::None);

    let mut iter = array![1, 2, 3].into_iter();
    assert_eq!(iter.find(|x| x == 2), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(3));
}
