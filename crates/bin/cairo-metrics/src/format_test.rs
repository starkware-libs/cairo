use crate::format::format_duration;

#[test]
fn test_format_duration() {
    assert_eq!(format_duration(500), "500 ns");
    assert_eq!(format_duration(1_500), "1.5 \u{00b5}s");
    assert_eq!(format_duration(100_000), "100.0 \u{00b5}s");
    assert_eq!(format_duration(1_000_000), "1.0 ms");
    assert_eq!(format_duration(100_000_000), "100.0 ms");
    assert_eq!(format_duration(1_500_000_000), "1.50 s");
}
