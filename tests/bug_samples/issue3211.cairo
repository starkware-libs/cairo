impl U256TryIntoU64 of TryInto<u256, u64> {
    #[inline(always)]
    fn try_into(self: u256) -> Option<u64> {
        if (self.high == 0) {
            self.low.try_into()
        } else {
            None
        }
    }
}

#[test]
fn test_u256_tryinto_u64() {
    let a = u256 { low: 64, high: 0 };
    let b: u64 = U256TryIntoU64::try_into(a).unwrap();
    assert_eq!(b, 64);
}
