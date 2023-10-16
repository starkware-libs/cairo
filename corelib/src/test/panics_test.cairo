#[test]
#[should_panic(expected: "")]
fn test_panic_with_byte_array_empty() {
    let ba: ByteArray = Default::default();
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "error")]
fn test_panic_with_byte_array_short() {
    let ba: ByteArray = "error";
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "long error with more than 31 characters")]
fn test_panic_with_byte_array_long() {
    let ba: ByteArray = "long error with more than 31 characters";
    panics::panic_with_byte_array(@ba);
}
