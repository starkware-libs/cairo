#[feature("generic-divrem")]
use crate::traits::{DivRem, DivRemGeneric};
use crate::zeroable::NonZero;


// -----------------------------------------------------------------------------
// 1. Bridge-driven generic division: u32 / u32
// -----------------------------------------------------------------------------
#[test]
fn generic_u32() {
    let lhs: u32 = 27;
    let rhs: NonZero<u32> = 4_u32.try_into().unwrap();

    let (q, r) = DivRemGeneric::<u32, u32>::div_rem(lhs, rhs);

    assert_eq!(q, 6); // 27 / 4  == 6
    assert_eq!(r, 3); // 27 % 4  == 3
}

// -----------------------------------------------------------------------------
// 2. Legacy API still works
// -----------------------------------------------------------------------------
#[test]
fn legacy_u32() {
    let lhs: u32 = 27;
    let rhs: NonZero<u32> = 4_u32.try_into().unwrap();

    let (q, r) = DivRem::<u32>::div_rem(lhs, rhs);

    assert_eq!(q, 6);
    assert_eq!(r, 3);
}

#[test]
fn generic_u256_by_u128() {
    let lhs: u256 = 20;
    let rhs: NonZero<u128> = 6_u128.try_into().unwrap();

    let (q, r) = DivRemGeneric::<u256, u128>::div_rem(lhs, rhs);

    assert_eq!(q, 3_u256);
    assert_eq!(r, 2_u128);
}

// Local impl so the test can call DivRemGeneric<u256,u128>.
impl U256ByU128DivRem of DivRemGeneric<u256, u128> {
    type Quotient = u256;
    type Remainder = u128;

    fn div_rem(lhs: u256, rhs: NonZero<u128>) -> (u256, u128) {
        // Reuse the old symmetric trait on (u256,u256).
        let rhs_u128: u128 = rhs.into();
        let rhs_u256: u256 = rhs_u128.into();
        let rhs_u256_nz: NonZero<u256> = rhs_u256.try_into().unwrap();
        let (q, r_u256) = DivRem::<u256>::div_rem(lhs, rhs_u256_nz);
        (q, r_u256.try_into().unwrap())
    }
}
