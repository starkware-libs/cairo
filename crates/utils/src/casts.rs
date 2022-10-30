/// Casts a usize to an i16 if there is no overflow.
/// Panics on overflow.
pub fn usize_as_i16(n: usize) -> i16 {
    i16::try_from(n).expect(format!("cast from usize to i16 failed: {n}").as_str())
}
