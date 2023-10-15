#[test]
// TODO(yuval): Allow strings in `should_panic`.
#[should_panic(
    expected: (
        0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, // BYTE_ARRAY_MAGIC
        0, // data len
        0, // pending_word
        0 // pending_word_len
    )
)]
fn test_panic_with_byte_array_empty() {
    let ba: ByteArray = Default::default();
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(
    expected: (
        0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, // BYTE_ARRAY_MAGIC
        0, // data len
        0x6572726f72, // pending_word
        5 // pending_word_len
    )
)]
fn test_panic_with_byte_array_short() {
    let ba: ByteArray = "error";
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(
    expected: (
        0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3, // BYTE_ARRAY_MAGIC
        1, // data len
        0x6c6f6e67206572726f722077697468206d6f7265207468616e203331206368, // data
        0x6172616374657273, // pending_word
        8 // pending_word_len
    )
)]
fn test_panic_with_byte_array_long() {
    let ba: ByteArray = "long error with more than 31 characters";
    panics::panic_with_byte_array(@ba);
}
