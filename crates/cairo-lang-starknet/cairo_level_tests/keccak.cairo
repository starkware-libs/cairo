/// Returns the selector for the given `ByteArray`.
fn selector(ba: ByteArray) -> felt252 {
    let value = core::keccak::compute_keccak_byte_array(@ba);
    u256 {
        low: core::integer::u128_byte_reverse(value.high),
        high: core::integer::u128_byte_reverse(value.low) & 0x3ffffffffffffffffffffffffffffff,
    }
        .try_into()
        .unwrap()
}

#[test]
fn test_keccak_byte_array() {
    assert_eq!(selector(""), selector!(""));
    assert_eq!(selector("0123456789abedef"), selector!("0123456789abedef"));
    assert_eq!(selector("hello-world"), selector!("hello-world"));
}
