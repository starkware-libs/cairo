use core::num::traits::{Pow, Zero};
use core::panics::panic_with_byte_array;
use perpetuals::core::errors::invalid_funding_rate_err;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::balance::{Balance, BalanceTrait};
use perpetuals::core::types::price::{Price, PriceMulTrait};
use starkware_utils::math::utils::mul_wide_and_floor_div;

pub const FUNDING_SCALE: i64 = 2_i64.pow(32);

const MAX_FUNDING_ERROR: felt252 = 'Value must be < 2^64';
const MIN_FUNDING_ERROR: felt252 = 'Value must be > -2^64';

#[derive(Copy, Debug, Drop, PartialEq, starknet::Store, Serde)]
/// `FundingIndex` represents a global funding rate tracker for each synthetic asset in the system.
/// It's used to calculate funding payments between long and short position holders.
/// To optimize performance, positions are only updated with the latest funding index when their
/// owners execute transactions. The system then calculates the accumulated funding payment since
/// the last interaction and adjusts the position's collateral balance accordingly.
/// After each update, the current funding index is cached for each synthetic asset in the position.
pub struct FundingIndex {
    /// Signed 64-bit fixed-point number:
    /// 1 sign bit, 31-bits integer part, 32-bits fractional part.
    /// Represents values as: actual_value = stored_value / 2**32.
    pub value: i64,
}

pub impl I64IntoFundingIndex of Into<i64, FundingIndex> {
    fn into(self: i64) -> FundingIndex {
        FundingIndex { value: self }
    }
}


pub trait FundingIndexMulTrait {
    /// Multiply the funding index with a balance.
    /// The funding is calculated as: funding = funding_index * balance / 2^32.
    fn mul(self: @FundingIndex, balance: Balance) -> Balance;
}

impl FundingIndexMul of FundingIndexMulTrait {
    fn mul(self: @FundingIndex, balance: Balance) -> Balance {
        let result = mul_wide_and_floor_div(*self.value, balance.into(), FUNDING_SCALE)
            .expect('Funding index mul overflow');
        BalanceTrait::new(result.try_into().unwrap())
    }
}

impl FundingIndexDefault of Default<FundingIndex> {
    #[inline]
    fn default() -> FundingIndex {
        FundingIndex { value: 0 }
    }
}

/// Calculate the funding payment for a position with the given balance.
///
/// The funding payment is calculated as:
///   funding_payment = (old_funding_index - new_funding_index) * balance / 2^32
///
/// When the funding index increases over time:
/// - Long positions (positive balance) pay funding (negative result)
/// - Short positions (negative balance) receive funding (positive result)
///
/// When the funding index decreases over time:
/// - Long positions receive funding (positive result)
/// - Short positions pay funding (negative result)
///
/// This is why we subtract the new index from the old index in the calculation,
/// to ensure the correct direction of payment based on position type.
pub fn calculate_funding(
    old_funding_index: FundingIndex, new_funding_index: FundingIndex, balance: Balance,
) -> Balance {
    (old_funding_index - new_funding_index).mul(balance)
}

#[derive(Copy, Drop, starknet::Store, Serde)]
pub struct FundingTick {
    pub asset_id: AssetId,
    pub funding_index: FundingIndex,
}

pub impl FundingIndexZero of Zero<FundingIndex> {
    fn zero() -> FundingIndex {
        FundingIndex { value: 0 }
    }
    fn is_zero(self: @FundingIndex) -> bool {
        self.value.is_zero()
    }
    fn is_non_zero(self: @FundingIndex) -> bool {
        self.value.is_non_zero()
    }
}

impl FundingIndexSubImpl of Sub<FundingIndex> {
    fn sub(lhs: FundingIndex, rhs: FundingIndex) -> FundingIndex {
        FundingIndex { value: lhs.value - rhs.value }
    }
}

impl FundingIndexIntoImpl of Into<FundingIndex, i64> {
    fn into(self: FundingIndex) -> i64 {
        self.value
    }
}

/// Validates the funding rate by ensuring that the index difference is bounded by the max funding
/// rate.
///
/// The max funding rate represents the rate of change **per second**, so it is multiplied by
/// `time_diff`.
/// Additionally, since the index includes the synthetic price,
/// the formula also multiplies by `synthetic_price`.
///
/// Formula:
/// `index_diff <= max_funding_rate * time_diff * synthetic_price`
pub fn validate_funding_rate(
    synthetic_id: AssetId,
    // index_diff scale is the same as the `FUNDING_SCALE` (2^32).
    index_diff: u64,
    // max_funding_rate scale is the same as the `FUNDING_SCALE` (2^32).
    max_funding_rate: u32,
    time_diff: u64,
    synthetic_price: Price,
) {
    if (index_diff.into() > synthetic_price.mul(rhs: max_funding_rate) * time_diff.into()) {
        let err = invalid_funding_rate_err(:synthetic_id);
        panic_with_byte_array(err: @err);
    }
}


#[cfg(test)]
mod tests {
    use core::num::traits::zero::Zero;
    use super::{BalanceTrait, FUNDING_SCALE, FundingIndex, FundingIndexMulTrait};

    #[test]
    fn test_zero() {
        let index: FundingIndex = Zero::zero();
        assert!(index.value == 0);
    }
    #[test]
    fn test_is_zero() {
        let index: FundingIndex = Zero::zero();
        assert!(index.is_zero());
        assert!(!index.is_non_zero());
    }
    #[test]
    fn test_is_non_zero() {
        let index = FundingIndex { value: 1 };
        assert!(!index.is_zero());
        assert!(index.is_non_zero());
    }

    #[test]
    fn test_funding_mul() {
        /// Test the case that the funding is half.
        let index = FundingIndex { value: FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 500_000_000);

        /// Test the case that the funding is negative half.
        let index = FundingIndex { value: -FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -500_000_000);

        /// Test the case that the funding is half and the balance is negative.
        let index = FundingIndex { value: FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(-1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -500_000_000);

        /// Test the case that the funding is negative half and the balance is negative.
        let index = FundingIndex { value: -FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(-1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 500_000_000);

        /// Test the case that the funding is half and the balance is odd.
        let index = FundingIndex { value: FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(1_000_000_001);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 500_000_000);

        /// Test the case that the funding is negative half and the balance is odd.
        let index = FundingIndex { value: -FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(1_000_000_001);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -500_000_001);

        /// Test the case that the funding is half and the balance is negative and odd.
        let index = FundingIndex { value: FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(-1_000_000_001);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -500_000_001);

        /// Test the case that the funding is negative half and the balance is negative and odd.
        let index = FundingIndex { value: -FUNDING_SCALE / 2 };
        let balance = BalanceTrait::new(-1_000_000_001);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 500_000_000);

        /// Test the case that the funding is 1.
        let index = FundingIndex { value: FUNDING_SCALE };
        let balance = BalanceTrait::new(1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 1_000_000_000);

        /// Test the case that the balance is negative one.
        let index = FundingIndex { value: FUNDING_SCALE };
        let balance = BalanceTrait::new(-1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -1_000_000_000);

        /// Test the case that the funding is negative.
        let index = FundingIndex { value: -FUNDING_SCALE };
        let balance = BalanceTrait::new(1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -1_000_000_000);

        /// Test the case that the funding is negative and the balance is negative.
        let index = FundingIndex { value: -FUNDING_SCALE };
        let balance = BalanceTrait::new(-1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 1_000_000_000);

        /// Test the case that the funding is 0.
        let index = FundingIndex { value: 0 };
        let balance = BalanceTrait::new(1_000_000_000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 0);

        /// Test the case that the balance is 0.
        let index = FundingIndex { value: FUNDING_SCALE };
        let balance = BalanceTrait::new(0);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 0);

        // positive funding, positive balance
        let index = FundingIndex { value: 38654705 };
        let balance = BalanceTrait::new(225000000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 2024999);

        // positive funding, negative balance
        let index = FundingIndex { value: 38654705 };
        let balance = BalanceTrait::new(-225000000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -2025000);

        // negative funding, negative balance
        let index = FundingIndex { value: -38654705 };
        let balance = BalanceTrait::new(-225000000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, 2024999);

        // negative funding, positive balance
        let index = FundingIndex { value: -38654705 };
        let balance = BalanceTrait::new(225000000);
        let result: i64 = index.mul(balance).into();
        assert_eq!(result, -2025000);
    }
}
