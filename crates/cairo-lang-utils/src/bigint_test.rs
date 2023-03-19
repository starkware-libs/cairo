use std::ops::Neg;

use num_bigint::BigInt;
use num_traits::Num;
use test_log::test;

use crate::bigint::BigIntAsHex;

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
