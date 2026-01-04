use core::num::traits::{One, Pow, Zero};
use perpetuals::core::types::balance::Balance;
use starkware_utils::math::utils::mul_wide_and_div;
use starkware_utils::signature::stark::{PublicKey, Signature};


// 2^28
pub const PRICE_SCALE: u64 = 2_u64.pow(28);

// 2^56
const MAX_PRICE: u64 = 2_u64.pow(56);

// Oracle always sign the price with 18 decimal places.
pub const ORACLE_SCALE: u128 = 10_u128.pow(18);

// StarkNet Perps scale is with 6 decimal places.
// The value here is 10^6 and it must correspond to the quantum of the collateral so the minimal
// collateral unit is 10^-6 USD.
pub const SN_PERPS_SCALE: u128 = 10_u128.pow(6);

// The ratio between the StarkNet Perps scale and the Oracle scale.
const ORACLE_SCALE_SN_PERPS_RATIO: u128 = ORACLE_SCALE / SN_PERPS_SCALE;

const MAX_PRICE_ERROR: felt252 = 'Value must be < 2^56';

#[derive(Copy, Debug, Default, Drop, PartialEq, Serde, starknet::Store)]
/// Price is the price of a synthetic asset in the Perps system.
/// The price is the price of the minimal unit of the asset in 10^-6 USD.
pub struct Price {
    // Unsigned 28-bit fixed point decimal precision.
    // 28-bit for the integer part and 28-bit for the fractional part.
    value: u64,
}

pub impl U64IntoPrice of Into<u64, Price> {
    fn into(self: u64) -> Price {
        Price { value: self }
    }
}

#[derive(Copy, Debug, Drop, Serde)]
pub struct SignedPrice {
    pub signature: Signature,
    pub signer_public_key: PublicKey,
    pub timestamp: u32,
    pub oracle_price: u128,
}

impl PricePartialOrd of PartialOrd<Price> {
    fn lt(lhs: Price, rhs: Price) -> bool {
        lhs.value < rhs.value
    }
}


pub trait PriceMulTrait<T> {
    /// The result type of the multiplication.
    type Target;
    fn mul(self: @Price, rhs: T) -> Self::Target;
}


impl PriceMulU32 of PriceMulTrait<u32> {
    type Target = u128;
    fn mul(self: @Price, rhs: u32) -> Self::Target {
        mul_wide_and_div(*self.value, rhs.into(), PRICE_SCALE.try_into().unwrap())
            .expect('Price mul overflow')
            .into()
    }
}

impl PriceMulBalance of PriceMulTrait<Balance> {
    type Target = i128;
    fn mul(self: @Price, rhs: Balance) -> Self::Target {
        let price: i128 = (*self.value).try_into().unwrap();
        let balance: i128 = rhs.into();
        let intermediate: i128 = price * balance;
        return intermediate / PRICE_SCALE.into();
    }
}

#[generate_trait]
pub impl PriceImpl of PriceTrait {
    fn new(value: u64) -> Price {
        let value = value * PRICE_SCALE;
        assert(value < MAX_PRICE, MAX_PRICE_ERROR);
        Price { value }
    }

    fn value(self: @Price) -> u64 {
        *self.value
    }
}

pub fn convert_oracle_to_perps_price(oracle_price: u128, resolution_factor: u64) -> Price {
    let mut converted_price = oracle_price * PRICE_SCALE.into();
    converted_price /= resolution_factor.into();
    converted_price /= ORACLE_SCALE_SN_PERPS_RATIO;
    let value = converted_price.try_into().expect(MAX_PRICE_ERROR);
    assert(value < MAX_PRICE, MAX_PRICE_ERROR);
    Price { value }
}

pub impl PriceZeroImpl of Zero<Price> {
    fn zero() -> Price {
        PriceTrait::new(value: 0)
    }

    fn is_zero(self: @Price) -> bool {
        self.value.is_zero()
    }

    fn is_non_zero(self: @Price) -> bool {
        self.value.is_non_zero()
    }
}

pub impl PriceOneImpl of One<Price> {
    fn one() -> Price {
        PriceTrait::new(value: 1)
    }

    fn is_one(self: @Price) -> bool {
        *self.value == PRICE_SCALE
    }

    fn is_non_one(self: @Price) -> bool {
        !self.value.is_one()
    }
}

#[cfg(test)]
mod tests {
    use core::num::traits::Zero;
    use perpetuals::core::types::balance::BalanceTrait;
    use super::*;


    #[test]
    fn test_new_price() {
        let price = PriceTrait::new(100);
        assert!(price.value == 100 * PRICE_SCALE);
    }

    #[test]
    #[should_panic(expected: 'Value must be < 2^56')]
    fn test_new_price_over_limit() {
        let _price = PriceTrait::new(PRICE_SCALE);
    }

    #[test]
    fn test_price_mul_u32() {
        let price = PriceTrait::new(value: 100);
        let result = price.mul(2_u32);
        assert!(result == 200_u128);
    }

    #[test]
    fn test_price_mul_balance() {
        let price = PriceTrait::new(value: 100);
        let balance = BalanceTrait::new(value: 2);
        let result = price.mul(balance);
        assert!(result == 200);
    }

    #[test]
    fn test_price_zero() {
        let price: Price = Zero::zero();
        assert!(price.value == 0);
    }

    #[test]
    fn test_price_is_zero() {
        let price: Price = Zero::zero();
        assert!(price.is_zero());
    }

    #[test]
    fn test_price_is_non_zero() {
        let price = PriceTrait::new(value: 100);
        assert!(price.is_non_zero());
    }

    #[test]
    fn test_price_balance_mul_handles_max_values_balance_and_price() {
        let price = Price { value: MAX_PRICE };
        let balance = BalanceTrait::new(value: 9223372036854775807); // Maximum i64 value
        let result = price.mul(balance);
        assert!(result.is_non_zero());
    }
}
