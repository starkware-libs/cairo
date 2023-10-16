use cairo_felt::Felt252;

/// Converts a bigint representing a felt252 to a Cairo short-string.
pub fn as_cairo_short_string(value: &Felt252) -> Option<String> {
    as_cairo_short_string_ex(value, None)
}

/// Converts a bigint representing a felt252 to a Cairo short-string.
/// `expected_length` controls the number of expected bytes in the result.
/// If set, it implies that nulls are allowed in the beginning/middle of the string.
pub fn as_cairo_short_string_ex(value: &Felt252, expected_length: Option<usize>) -> Option<String> {
    let mut as_string = String::default();
    let mut is_end = false;
    let bytes = value.to_bytes_be();
    let bytes_len = bytes.len();
    for byte in bytes {
        if byte == 0 {
            if expected_length.is_some() {
                // Nulls are allowed.
                as_string.push_str(r#"\0"#);
            } else {
                is_end = true;
            }
        } else if is_end || !byte.is_ascii() {
            return None;
        } else {
            as_string.push(byte as char);
        }
    }
    // `to_bytes_be` misses starting nulls. Prepend them as needed.
    if let Some(expected_length) = expected_length {
        let missing_nulls = expected_length - bytes_len;
        as_string.insert_str(0, &"\\0".repeat(missing_nulls));
    }

    Some(as_string)
}
