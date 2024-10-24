use crate::bigint::BigIntAsHex;

#[test]
fn encode_bigint() {
    use core::str::FromStr;

    use parity_scale_codec::{Decode, Encode};

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
