use core::num::traits::BitSize;

#[test]
fn test_bit_size() {
    assert!(BitSize::<u8>::bits() == 8);
    assert!(BitSize::<u16>::bits() == 16);
    assert!(BitSize::<u32>::bits() == 32);
    assert!(BitSize::<u64>::bits() == 64);
    assert!(BitSize::<u128>::bits() == 128);
    assert!(BitSize::<u256>::bits() == 256);
    assert!(BitSize::<i8>::bits() == 8);
    assert!(BitSize::<i16>::bits() == 16);
    assert!(BitSize::<i32>::bits() == 32);
    assert!(BitSize::<i64>::bits() == 64);
    assert!(BitSize::<i128>::bits() == 128);
    assert!(BitSize::<bytes31>::bits() == 248);
}
