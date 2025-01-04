use core::ops::RangeTrait;

#[test]
fn test_range_is_empty() {
    assert!(!(3_u8..5_u8).is_empty());
    assert!((3_u8..3_u8).is_empty());
    assert!((3_u8..2_u8).is_empty());
}

#[test]
fn test_range_format() {
    assert!(format!("{:?}", 1..5) == "1..5");
}

#[test]
fn test_range_inclusive_iterator() {
    let mut iter = (1_usize..=3).into_iter();
    assert!(iter.next() == Option::Some(1));
    assert!(iter.next() == Option::Some(2));
    assert!(iter.next() == Option::Some(3));
    assert!(iter.next() == Option::None);
}

#[test]
fn test_range_inclusive_iterator_range_end() {
    let mut iter = (253_u8..=255).into_iter();
    assert!(iter.next() == Option::Some(253));
    assert!(iter.next() == Option::Some(254));
    assert!(iter.next() == Option::Some(255));
    assert!(iter.next() == Option::None);
}

#[test]
fn test_range_inclusive_empty_ranges() {
    let mut iter = (255_u8..=125).into_iter();
    assert!(iter.next() == Option::None);
    let mut iter = (255_u8..=0).into_iter();
    assert!(iter.next() == Option::None);
}
