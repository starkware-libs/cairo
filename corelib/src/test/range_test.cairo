use core::ops::RangeTrait;

#[test]
fn test_range_is_empty() {
    assert!(!(3_u8..5_u8).is_empty());
    assert!((3_u8..3_u8).is_empty());
    assert!((3_u8..2_u8).is_empty());
}

#[test]
fn test_range_contains() {
    assert!(!(3_u8..5).contains(@2));
    assert!((3_u8..5).contains(@3));
    assert!((3_u8..5).contains(@4));
    assert!(!(3_u8..5).contains(@5));

    assert!(!(3_u8..3).contains(@3));
    assert!(!(3_u8..2).contains(@3));
}

#[test]
fn test_range_format() {
    assert!(format!("{:?}", 1..5) == "1..5");
}

#[test]
fn test_range_inclusive_iterator_working() {
    let x = (1_usize..=3);
    let mut iter = x.into_iter();
    assert!(iter.next() == Option::Some(1));
    assert!(iter.next() == Option::Some(2));
    assert!(iter.next() == Option::Some(3));
    assert!(iter.next() == Option::None);
}
