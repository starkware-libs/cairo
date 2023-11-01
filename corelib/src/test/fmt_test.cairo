#[test]
fn test_format() {
    // With a ByteArray.
    let ba: ByteArray = "hello";
    assert(format!("{}", ba) == ba, 'bad formatting');

    // With a felt252.
    assert(format!("{}", 97_felt252) == "97", 'bad formatting');

    // With an integer.
    assert(format!("{}", 97_usize) == "97", 'bad formatting');
}
