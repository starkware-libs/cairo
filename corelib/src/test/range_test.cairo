use core::ops::{Bound, RangeBounds, RangeTrait};

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
fn test_range_bounds() {
    assert!((3_u8..5).start_bound() == Bound::Included(@3));
    assert!((3_u8..5).end_bound() == Bound::Excluded(@5));

    assert!(!(3_u8..5).contains(@2));
    assert!((3_u8..5).contains(@4));
    assert!(!(3_u8..5).contains(@5));
}
