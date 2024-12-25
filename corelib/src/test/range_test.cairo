#[test]
fn test_range_format() {
    assert(format!("{:?}", 1..5) == "1..5", 'bad range debug formatting');
}
