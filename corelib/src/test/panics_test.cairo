use panics::BYTE_ARRAY_PANIC_MAGIC;

#[test]
#[should_panic(expected: 'short_string')]
fn test_panic_with_short_string() {
    panic_with_felt252('short_string');
}

#[test]
#[should_panic(expected: 1)]
fn test_panic_with_felt252() {
    panic_with_felt252(1);
}

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

#[test]
#[should_panic(expected: "long err\0r with more than 31 characters")]
fn test_panic_with_byte_array_null_in_full_word() {
    let mut ba: ByteArray = "long err";
    ba.append_byte(0);
    let ba2: ByteArray = "r with more than 31 characters";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "long error with more than 31 character\0s")]
fn test_panic_with_byte_array_null_in_pending() {
    let mut ba: ByteArray = "long error with more than 31 character";
    ba.append_byte(0);
    let ba2: ByteArray = "s";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: "\0error")]
fn test_panic_with_byte_array_null_in_beginning() {
    let mut ba: ByteArray = "";
    ba.append_byte(0);
    let ba2: ByteArray = "error";
    ba.append(@ba2);
    panics::panic_with_byte_array(@ba);
}

#[test]
#[should_panic(expected: ("error", 3, "hello", 5, 'short_string'))]
fn test_panic_with_stacked_errors() {
    let mut error = array![];
    let ba: ByteArray = "error";
    error.append(BYTE_ARRAY_PANIC_MAGIC);
    ba.serialize(ref error);

    error.append(3);

    let ba: ByteArray = "hello";
    error.append(BYTE_ARRAY_PANIC_MAGIC);
    ba.serialize(ref error);

    error.append(5);
    error.append('short_string');

    panic(error);
}
