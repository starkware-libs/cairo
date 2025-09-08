use core::option::OptionTrait;
use core::traits::TryInto;

const NONZERO_U8: Option<NonZero<u8>> = 5_u8.try_into();
const ZERO_U8: Option<NonZero<u8>> = 0_u8.try_into();

const NONZERO_U32: Option<NonZero<u32>> = 42_u32.try_into();
const ZERO_U32: Option<NonZero<u32>> = 0_u32.try_into();

const NONZERO_FELT: Option<NonZero<felt252>> = 123_felt252.try_into();
const ZERO_FELT: Option<NonZero<felt252>> = 0_felt252.try_into();

fn main() {
    // Test that non-zero values work
    assert!(NONZERO_U8.is_some());
    assert!(NONZERO_U32.is_some());
    assert!(NONZERO_FELT.is_some());

    // Test that zero values return None
    assert!(ZERO_U8.is_none());
    assert!(ZERO_U32.is_none());
    assert!(ZERO_FELT.is_none());
}
