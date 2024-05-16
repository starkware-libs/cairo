pub const BYTE_ARRAY_MAGIC: &str =
    "46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3";
pub const BYTES_IN_WORD: usize = 31;
use num_traits::Num;
use starknet_types_core::felt::Felt as Felt252;

pub fn felt_from_str(val: &str, radix: u32) -> Felt252 {
    Felt252::from(num_bigint::BigInt::from_str_radix(val, radix).expect("Couldn't parse bytes"))
}

#[macro_export]
macro_rules! felt_str {
    ($val:expr) => {
        cairo_lang_utils::byte_array::felt_from_str($val, 10_u32)
    };
    ($val:expr, $opt:expr) => {
        cairo_lang_utils::byte_array::felt_from_str($val, $opt as u32)
    };
}
