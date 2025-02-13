use core::iter::PeekableTrait;

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
fn test_iter_last() {
    assert_eq!(array![1_u8, 2, 3].into_iter().last(), Option::Some(3));
    assert_eq!(array![].into_iter().last(), Option::<u8>::None);
}

#[test]
fn test_advance_by() {
    let mut iter = array![1_u8, 2, 3, 4].into_iter();

    assert_eq!(iter.advance_by(2), Ok(()));
    assert_eq!(iter.next(), Some(3));
    assert_eq!(iter.advance_by(0), Ok(()));
    assert_eq!(iter.advance_by(100), Err(99));
}

#[test]
fn test_iter_nth() {
    let mut iter = array![1_u8, 2, 3].into_iter();

    assert_eq!(iter.nth(1), Some(2));
    assert_eq!(iter.nth(0), Some(3));
    assert_eq!(iter.nth(0), None);
}

#[test]
fn test_iter_adapter_map() {
    let mut iter = array![1, 2, 3].into_iter().map(|x| 2 * x);

    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), Some(4));
    assert_eq!(iter.next(), Some(6));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iterator_enumerate() {
    let mut iter = array!['a', 'b', 'c'].into_iter().enumerate();

    assert_eq!(iter.next(), Some((0, 'a')));
    assert_eq!(iter.next(), Some((1, 'b')));
    assert_eq!(iter.next(), Some((2, 'c')));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iterator_zip() {
    let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]);

    assert_eq!(iter.next(), Some((1, 4)));
    assert_eq!(iter.next(), Some((2, 5)));
    assert_eq!(iter.next(), Some((3, 6)));
    assert_eq!(iter.next(), None);

    // Nested zips
    let mut iter = array![1, 2, 3].into_iter().zip(array![4, 5, 6]).zip(array![7, 8, 9]);

    assert_eq!(iter.next(), Some(((1, 4), 7)));
    assert_eq!(iter.next(), Some(((2, 5), 8)));
    assert_eq!(iter.next(), Some(((3, 6), 9)));
    assert_eq!(iter.next(), None);
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
fn test_iter_adapter_peekable() {
    let mut iter = (1..4_u8).into_iter().peekable();

    // peek() lets us see one step into the future
    assert_eq!(iter.peek(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(2));

    // The iterator does not advance even if we `peek` multiple times
    assert_eq!(iter.peek(), Option::Some(3));
    assert_eq!(iter.peek(), Option::Some(3));
    assert_eq!(iter.next(), Option::Some(3));

    // After the iterator is finished, so is `peek()`
    assert_eq!(iter.peek(), Option::None);
    assert_eq!(iter.next(), Option::None);
}

#[test]
fn test_iter_adapter_take() {
    assert_eq!((1_u8..=10).into_iter().take(4).collect(), array![1, 2, 3, 4]);
    assert_eq!((1_u8..=10).into_iter().take(0).collect(), array![]);
}

#[test]
fn test_iter_adapter_take_next() {
    let mut iter = (1_u8..=10).into_iter().take(2);
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
    assert_eq!(iter.next(), None);
}

#[test]
fn test_iter_adapter_take_nth() {
    // Test when n > requested nth
    let mut iter = (1_u8..=10).into_iter().take(8);
    assert_eq!(iter.nth(5), Some(6));
    assert_eq!(iter.nth(1), Some(8));
    assert_eq!(iter.nth(0), None);

    // Test when n > 0 but not enough elements
    let mut iter = (1_u8..=10).into_iter().take(5);
    assert_eq!(iter.nth(15), None);
    assert_eq!(iter.next(), None);

    // Test when n = 0
    let mut iter = (1_u8..=3).into_iter().take(0);
    assert_eq!(iter.nth(0), None);
}

#[test]
fn test_iter_adapter_take_advance_by() {
    let mut iter = (1_u8..=10).into_iter().take(8);
    // self.n >= n and inner iterator succeeds
    assert_eq!(iter.advance_by(7), Ok(()));
    assert_eq!(iter.next(), Some(8));
    assert_eq!(iter.next(), None);

    // self.n >= n but inner iterator fails
    let mut iter = (1_u8..=3).into_iter().take(10);
    assert_eq!(iter.advance_by(5), Err(2));

    // self.n < n and inner iterator succeeds
    let mut iter = (1_u8..=10).into_iter().take(8);
    assert_eq!(iter.advance_by(9), Err(1));
    assert_eq!(iter.next(), None);

    let mut iter = (1_u8..=2).into_iter().take(10);
    // self.n < n and inner iterator fails
    assert_eq!(iter.advance_by(20), Err(18));
    assert_eq!(iter.next(), None);

    // self.n = 0
    let mut iter = (1_u8..=10).into_iter().take(8);
    assert_eq!(iter.advance_by(0), Ok(()));
    assert_eq!(iter.next(), Some(1));
    assert_eq!(iter.next(), Some(2));
}

#[test]
fn test_iter_accum_sum() {
    assert_eq!(array![1, 2, 3].into_iter().sum(), 6);
    assert_eq!(array![].into_iter().sum(), 0);
}

#[test]
fn test_iter_accum_product() {
    assert_eq!((1_usize..=0).into_iter().product(), 1);
    assert_eq!((1_usize..=1).into_iter().product(), 1);
    assert_eq!((1_usize..=5).into_iter().product(), 120);
}

#[test]
fn test_iter_find() {
    let mut iter = array![1, 2, 3].into_iter();
    assert_eq!(iter.find(|x| *x == 2), Option::Some(2));
    assert_eq!(iter.find(|x| *x == 5), Option::None);

    let mut iter = array![1, 2, 3].into_iter();
    assert_eq!(iter.find(|x| *x == 2), Option::Some(2));
    assert_eq!(iter.next(), Option::Some(3));
}

#[test]
fn test_iter_adapter_filter() {
    let mut iter = array![0_u32, 1, 2].into_iter().filter(|x| *x > 0);
    assert_eq!(iter.next(), Option::Some(1));
    assert_eq!(iter.next(), Option::Some(2));
    assert_eq!(iter.next(), Option::None);
}
