/// Casts a usize to an i16 if there is no overflow.
/// Panics on overflow.
pub fn usize_as_i16(n: usize) -> i16 {
    i16::try_from(n).unwrap_or_else(|_| panic!("Cast from usize to i16 failed: {n}"))
}
