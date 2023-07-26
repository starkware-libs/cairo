#[cfg(all(not(feature = "std"), feature = "serde"))]
use alloc::format;
#[cfg(feature = "serde")]
use core::ops::Neg;
#[cfg(feature = "parity-scale-codec")]
use core::str::FromStr;

#[cfg(feature = "serde")]
use num_bigint::BigInt;
#[cfg(feature = "serde")]
use num_traits::Num;
#[cfg(feature = "parity-scale-codec")]
use parity_scale_codec::{Decode, Encode};
#[cfg(feature = "serde")]
use test_case::test_case;

#[cfg(any(feature = "serde", feature = "parity-scale-codec"))]
use crate::bigint::BigIntAsHex;

#[test_case("800000000000011000000000000000000000000000000000000000000000001", true; "positive")]
#[test_case("800000000000011000000000000000000000000000000000000000000000001", false; "negative")]
#[test_case("0", false; "zero")]
#[cfg(feature = "serde")]
fn test_bigint_serde(s: &str, is_negative: bool) {
    let mut num = BigIntAsHex { value: BigInt::from_str_radix(s, 16).unwrap() };
    if is_negative {
        num = num.value.neg().into();
    }

    let serialized = serde_json::to_string_pretty(&num).unwrap();
    assert_eq!(serialized, format!("\"{}0x{}\"", if is_negative { "-" } else { "" }, s));

    assert_eq!(num, serde_json::from_str(&serialized).unwrap())
}

#[test]
#[cfg(feature = "parity-scale-codec")]
fn encode_bigint() {
    let bigint = BigIntAsHex {
        value: num_bigint::BigInt::from_str(
            "3618502788666131106986593281521497120414687020801267626233049500247285301248",
        )
        .unwrap(),
    };
    let encoding = bigint.encode();
    let decoded = BigIntAsHex::decode(&mut encoding.as_slice()).unwrap();
    assert_eq!(bigint, decoded);
}
