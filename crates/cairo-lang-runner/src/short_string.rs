use cairo_felt::Felt252;

/// Converts a bigint representing a felt252 to a Cairo short-string.
pub fn as_cairo_short_string(value: &Felt252) -> Option<String> {
    let mut as_string = String::default();
    let mut is_end = false;
    for byte in value.to_bytes_be() {
        if byte == 0 {
            is_end = true;
        } else if is_end || !byte.is_ascii() {
            return None;
        } else {
            as_string.push(byte as char);
        }
    }
    Some(as_string)
}

/// Converts a bigint representing a felt252 to a Cairo short-string of the given length.
/// Nulls are allowed and length must be <= 31.
pub fn as_cairo_short_string_ex(value: &Felt252, length: usize) -> Option<String> {
    if length == 0 {
        return Some("".to_string());
    }
    assert!(length <= 31, "A short string can't be longer than 31 bytes.");

    let bytes = value.to_bytes_be();
    let bytes_len = bytes.len();
    assert!(
        bytes_len <= length,
        "Value has more bytes than expected. Actual: {bytes_len}, expected: {length}."
    );

    let mut as_string = "".to_string();
    for byte in bytes {
        if byte == 0 {
            as_string.push_str(r#"\0"#);
        } else if byte.is_ascii() {
            as_string.push(byte as char);
        } else {
            return None;
        }
    }

    // `to_bytes_be` misses starting nulls. Prepend them as needed.
    let missing_nulls = length - bytes_len;
    as_string.insert_str(0, &r#"\0"#.repeat(missing_nulls));

    Some(as_string)
}
