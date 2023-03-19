use std::ops::Neg;

use crate::bigint::deserialize_big_int;
use crate::bigint::serialize_big_int;
use num_bigint::BigInt;
use num_traits::Num;
use serde::{Deserialize, Serialize};
use test_log::test;

// A wrapper for BigUint that serializes as hex.
#[derive(Default, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct BigIntAsHex {
    /// A field element that encodes the signature of the called function.
    #[serde(serialize_with = "serialize_big_int", deserialize_with = "deserialize_big_int")]
    pub value: BigInt,
}

#[test]
fn test_bigint_serde() {
    let num = BigIntAsHex {
        value: BigInt::from_str_radix(
            "800000000000011000000000000000000000000000000000000000000000001",
            16,
        )
        .unwrap()
        .neg(),
    };

    let serialized = serde_json::to_string_pretty(&num).unwrap();
    assert_eq!(
        serialized,
        "\"-0x800000000000011000000000000000000000000000000000000000000000001\""
    );

    assert_eq!(num, serde_json::from_str(&serialized).unwrap())
}
