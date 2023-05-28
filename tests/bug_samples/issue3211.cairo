use core::integer::u128;
use core::integer::Felt252TryIntoU128;
use traits::{Into, TryInto};
use option::OptionTrait;

impl U256TryIntoU64 of TryInto<u256, u64> {
    #[inline(always)]
    fn try_into(self: u256) -> Option<u64> {
        if (self.high == 0) {
            self.low.try_into()
        } else {
            Option::None(())
        }
    }
}

#[test]
fn test_u256_tryinto_u64() {
    let a = u256 { low: 64, high: 0 };
    let b: u64 = a.try_into().unwrap();
    assert(b == 64, 'b conv');
}
