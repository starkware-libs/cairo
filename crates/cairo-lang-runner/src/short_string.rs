use num_traits::Zero;
use starknet_types_core::felt::Felt;

/// Converts a bigint representing a felt252 to a Cairo short-string.
pub fn as_cairo_short_string(value: &Felt) -> Option<String> {
    let mut as_string = String::default();
    let mut is_end = false;
    for byte in value.to_bytes_be().into_iter().skip_while(|b| *b == 0) {
        if byte == 0 {
            is_end = true;
        } else if is_end {
            return None;
        } else if byte.is_ascii_graphic() || byte.is_ascii_whitespace() {
            as_string.push(byte as char);
        } else {
            return None;
        }
    }
    Some(as_string)
}

/// Converts a bigint representing a felt252 to a Cairo short-string of the given length.
/// Nulls are allowed and length must be <= 31.
pub fn as_cairo_short_string_ex(value: &Felt, length: usize) -> Option<String> {
    if length == 0 {
        return if value.is_zero() { Some("".to_string()) } else { None };
    }
    if length > 31 {
        // A short string can't be longer than 31 bytes.
        return None;
    }

    let bytes = value.to_bytes_be();

    let mut as_string = "".to_string();
    for (i, byte) in bytes.into_iter().skip_while(|b| *b == 0).enumerate() {
        if i == length {
            return None;
        }

        if byte == 0 {
            as_string.push_str(r"\0");
        } else if byte.is_ascii_graphic() || byte.is_ascii_whitespace() {
            as_string.push(byte as char);
        } else {
            as_string.push_str(format!(r"\x{:02x}", byte).as_str());
        }
    }

    // `to_bytes_be` misses starting nulls. Prepend them as needed.
    let missing_nulls = length.saturating_sub(as_string.len());
    if missing_nulls > 0 {
        as_string.insert_str(0, &r"\0".repeat(missing_nulls));
    }

    Some(as_string)
}
