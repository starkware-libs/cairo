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
