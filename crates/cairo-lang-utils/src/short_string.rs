use num_bigint::BigInt;

/// Converts a bigint representing a felt to a Cairo short-string.
pub fn as_cairo_short_string(value: &BigInt) -> Option<String> {
    let mut as_string = String::default();
    let mut is_end = false;
    for byte in value.to_bytes_be().1 {
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
