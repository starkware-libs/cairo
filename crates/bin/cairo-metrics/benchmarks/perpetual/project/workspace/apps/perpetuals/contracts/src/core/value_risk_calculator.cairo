use core::num::traits::{One, Zero};
use core::panics::panic_with_byte_array;
use perpetuals::core::errors::{
    position_not_deleveragable, position_not_fair_deleverage, position_not_healthy_nor_healthier,
    position_not_liquidatable,
};
use perpetuals::core::types::asset::synthetic::AssetBalanceInfo;
use perpetuals::core::types::balance::{Balance, BalanceDiff};
use perpetuals::core::types::position::{
    AssetEnrichedPositionDiff, PositionDiffEnriched, PositionId,
};
use perpetuals::core::types::price::{Price, PriceMulTrait};
use perpetuals::core::types::risk_factor::RiskFactorMulTrait;
use starkware_utils::math::abs::Abs;
use starkware_utils::math::fraction::FractionTrait;

/// This is equivalent to 1e-6 USD as the everything is in units of the smallest collateral asset.
const EPSILON: i128 = 1_i128;


/// Represents the state of a position based on its total value and total risk.
/// - A position is **Deleveragable** (and also **Liquidatable**) if its total value is negative.
/// - A position is **Liquidatable** if its total value is less than its total risk.
/// - Otherwise, the position is considered **Healthy**.clear
#[derive(Copy, Drop, Debug, PartialEq, Serde)]
pub enum PositionState {
    Healthy,
    Liquidatable,
    Deleveragable,
}

/// The total value and total risk of a position.
#[derive(Copy, Debug, Drop, Serde)]
pub struct PositionTVTR {
    pub total_value: i128,
    pub total_risk: u128,
}

/// The change in terms of total value and total risk of a position.
#[derive(Copy, Debug, Drop, Serde)]
pub struct TVTRChange {
    pub before: PositionTVTR,
    pub after: PositionTVTR,
}


/// Returns the state of a position based on its total value and total risk.
fn get_position_state(position_tvtr: PositionTVTR) -> PositionState {
    if position_tvtr.total_value < 0 {
        PositionState::Deleveragable
    } else if position_tvtr.total_value.abs() < position_tvtr.total_risk {
        // We apply abs() to total_value to be able to compare it with total_risk which is unsigned.
        // At this point, we've already ensured total_value is >= 0.
        PositionState::Liquidatable
    } else {
        PositionState::Healthy
    }
}

/// The position is fair if the total_value divided by the total_risk is the almost before and after
/// the change - the before_ratio needs to be between after_ratio-epsilon and after ratio.
fn is_fair_deleverage(before: PositionTVTR, after: PositionTVTR) -> bool {
    if after.total_risk == 0 {
        return after.total_value == 0;
    }
    let before_ratio = FractionTrait::new(
        numerator: before.total_value, denominator: before.total_risk,
    );
    let after_ratio = FractionTrait::new(
        numerator: after.total_value, denominator: after.total_risk,
    );
    let after_minus_epsilon_ratio = FractionTrait::new(
        numerator: after.total_value - EPSILON, denominator: after.total_risk,
    );
    after_minus_epsilon_ratio <= before_ratio && before_ratio <= after_ratio
}

/// Returns the state of a position.
pub fn evaluate_position(
    unchanged_assets: Span<AssetBalanceInfo>, collateral_balance: Balance,
) -> PositionState {
    let tvtr = calculate_position_tvtr(:unchanged_assets, :collateral_balance);
    get_position_state(position_tvtr: tvtr)
}

pub fn assert_healthy_or_healthier(position_id: PositionId, tvtr: TVTRChange) {
    let position_state_after_change = get_position_state(position_tvtr: tvtr.after);
    if position_state_after_change == PositionState::Healthy {
        // If the position is healthy we can return.
        return;
    }

    if tvtr.before.total_risk.is_zero() || tvtr.after.total_risk.is_zero() {
        panic_with_byte_array(@position_not_healthy_nor_healthier(:position_id, :tvtr));
    }

    /// This is checked only when the after is not healthy:
    /// The position is healthier if the total_value divided by the total_risk
    /// is equal or higher after the change and the total_risk is lower.
    /// Formal definition:
    /// total_value_after / total_risk_after >= total_value_before / total_risk_before
    /// AND total_risk_after < total_risk_before.
    if tvtr.after.total_risk >= tvtr.before.total_risk {
        panic_with_byte_array(@position_not_healthy_nor_healthier(:position_id, :tvtr));
    }
    let before_ratio = FractionTrait::new(tvtr.before.total_value, tvtr.before.total_risk);
    let after_ratio = FractionTrait::new(tvtr.after.total_value, tvtr.after.total_risk);

    if (after_ratio < before_ratio) {
        let err = position_not_healthy_nor_healthier(:position_id, :tvtr);
        panic_with_byte_array(err: @err);
    }
}

pub fn liquidated_position_validations(
    position_id: PositionId,
    unchanged_assets: Span<AssetBalanceInfo>,
    position_diff_enriched: PositionDiffEnriched,
) {
    let tvtr_before = calculate_position_tvtr_before(:unchanged_assets, :position_diff_enriched);
    let tvtr = calculate_position_tvtr_change(
        :tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
    );
    let position_state_before_change = get_position_state(position_tvtr: tvtr.before);

    // Validate that the position isn't healthy before the change.
    let condition = position_state_before_change == PositionState::Liquidatable
        || position_state_before_change == PositionState::Deleveragable;
    if (!condition) {
        let err = position_not_liquidatable(:position_id, :tvtr);
        panic_with_byte_array(err: @err);
    }
    assert_healthy_or_healthier(:position_id, :tvtr);
}

pub fn deleveraged_position_validations(
    position_id: PositionId,
    unchanged_assets: Span<AssetBalanceInfo>,
    position_diff_enriched: PositionDiffEnriched,
) {
    let tvtr_before = calculate_position_tvtr_before(:unchanged_assets, :position_diff_enriched);
    let tvtr = calculate_position_tvtr_change(
        :tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
    );
    let position_state_before_change = get_position_state(position_tvtr: tvtr.before);

    if (position_state_before_change != PositionState::Deleveragable) {
        let err = position_not_deleveragable(:position_id, :tvtr);
        panic_with_byte_array(err: @err);
    }

    assert_healthy_or_healthier(:position_id, :tvtr);
    if (!is_fair_deleverage(before: tvtr.before, after: tvtr.after)) {
        let err = position_not_fair_deleverage(:position_id, :tvtr);
        panic_with_byte_array(err: @err);
    }
}

pub fn calculate_position_tvtr(
    unchanged_assets: Span<AssetBalanceInfo>, collateral_balance: Balance,
) -> PositionTVTR {
    let position_diff_enriched = PositionDiffEnriched {
        collateral_enriched: BalanceDiff { before: collateral_balance, after: collateral_balance },
        asset_diff_enriched: Option::None,
    };
    calculate_position_tvtr_before(:unchanged_assets, :position_diff_enriched)
}

/// Calculates the total value and total risk change for a position, taking into account both
/// unchanged assets and position changes (collateral and synthetic assets).
///
/// # Arguments
///
/// * `tvtr_before` - The total value and total risk before the change
/// * `position_diff_enriched` - Changes in collateral and synthetic assets for the position
///
/// # Returns
///
/// * `TVTRChange` - Contains the total value and total risk before and after the changes
///
/// # Logic Flow
/// 1. Calculates value and risk changes for synthetic assets
/// 2. Calculates value and risk changes for collateral assets
/// 3. Combines all calculations into final before/after totals
pub fn calculate_position_tvtr_change(
    tvtr_before: PositionTVTR, synthetic_enriched_position_diff: AssetEnrichedPositionDiff,
) -> TVTRChange {
    let mut total_value_after = tvtr_before.total_value;
    let mut total_risk_after = tvtr_before.total_risk;

    if let Option::Some(asset_diff) = synthetic_enriched_position_diff.asset_diff_enriched {
        // asset_value is in units of 10^-6 USD.
        let asset_value_before = asset_diff.price.mul(rhs: asset_diff.balance_before);
        let asset_value_after = asset_diff.price.mul(rhs: asset_diff.balance_after);
        let asset_risk_before = asset_diff.risk_factor_before.mul(asset_value_before.abs());
        let asset_risk_after = asset_diff.risk_factor_after.mul(asset_value_after.abs());

        total_value_after = total_value_after + asset_value_after - asset_value_before;
        total_risk_after = total_risk_after + asset_risk_after - asset_risk_before;
    }

    // Collateral price is always "One" in Perps - "One" is 10^-6 USD which means 2^28 same as the
    // PRICE_SCALE.
    let price: Price = One::one();

    // asset_value is in units of 10^-6 USD.
    total_value_after += price.mul(rhs: synthetic_enriched_position_diff.collateral_diff);

    TVTRChange {
        before: tvtr_before,
        after: PositionTVTR { total_value: total_value_after, total_risk: total_risk_after },
    }
}

pub fn calculate_position_tvtr_before(
    unchanged_assets: Span<AssetBalanceInfo>, position_diff_enriched: PositionDiffEnriched,
) -> PositionTVTR {
    let mut total_value = 0_i128;
    let mut total_risk = 0_u128;
    for synthetic in unchanged_assets {
        // asset_value is in units of 10^-6 USD.
        let asset_value: i128 = (*synthetic.price).mul(rhs: *synthetic.balance);
        total_value += asset_value;
        total_risk += (*synthetic.risk_factor).mul(asset_value.abs());
    }

    if let Option::Some(asset_diff) = position_diff_enriched.asset_diff_enriched {
        // asset_value is in units of 10^-6 USD.
        let asset_value_before = asset_diff.price.mul(rhs: asset_diff.balance_before);
        total_value += asset_value_before;
        total_risk += asset_diff.risk_factor_before.mul(asset_value_before.abs());
    }

    // Collateral price is always "One" in Perps - "One" is 10^-6 USD which means 2^28 same as the
    // PRICE_SCALE.
    let price: Price = One::one();
    total_value += price.mul(rhs: position_diff_enriched.collateral_enriched.before);

    PositionTVTR { total_value: total_value, total_risk: total_risk }
}

#[cfg(test)]
mod tests {
    use perpetuals::core::types::asset::synthetic::{AssetBalanceDiffEnriched, AssetBalanceInfo};
    use perpetuals::core::types::asset::{AssetId, AssetIdTrait};
    use perpetuals::core::types::balance::BalanceTrait;
    use perpetuals::core::types::funding::FundingIndex;
    use perpetuals::core::types::price::{Price, PriceTrait};
    use perpetuals::core::types::risk_factor::{RiskFactor, RiskFactorTrait};
    use super::*;


    /// Prices
    fn PRICE_1() -> Price {
        PriceTrait::new(value: 900_u64)
    }
    fn PRICE_2() -> Price {
        PriceTrait::new(value: 1000_u64)
    }
    fn PRICE_3() -> Price {
        PriceTrait::new(value: 500_u64)
    }
    fn PRICE_4() -> Price {
        PriceTrait::new(value: 100_u64)
    }
    fn PRICE_5() -> Price {
        PriceTrait::new(value: 2000_u64)
    }

    /// Risk factors
    fn RISK_FACTOR_1() -> RiskFactor {
        RiskFactorTrait::new(500)
    }
    fn RISK_FACTOR_2() -> RiskFactor {
        RiskFactorTrait::new(900)
    }
    fn RISK_FACTOR_3() -> RiskFactor {
        RiskFactorTrait::new(100)
    }
    fn RISK_FACTOR_4() -> RiskFactor {
        RiskFactorTrait::new(700)
    }
    fn RISK_FACTOR_5() -> RiskFactor {
        RiskFactorTrait::new(0)
    }

    /// Assets IDs
    fn SYNTHETIC_ASSET_ID_1() -> AssetId {
        AssetIdTrait::new(value: selector!("SYNTHETIC_ASSET_ID_1"))
    }
    fn SYNTHETIC_ASSET_ID_2() -> AssetId {
        AssetIdTrait::new(value: selector!("SYNTHETIC_ASSET_ID_2"))
    }
    fn SYNTHETIC_ASSET_ID_3() -> AssetId {
        AssetIdTrait::new(value: selector!("SYNTHETIC_ASSET_ID_3"))
    }
    fn SYNTHETIC_ASSET_ID_4() -> AssetId {
        AssetIdTrait::new(value: selector!("SYNTHETIC_ASSET_ID_4"))
    }
    fn SYNTHETIC_ASSET_ID_5() -> AssetId {
        AssetIdTrait::new(value: selector!("SYNTHETIC_ASSET_ID_5"))
    }

    #[test]
    fn test_calculate_position_tvtr_change_basic_case() {
        // Create a position with a single asset entry.
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: 60),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let position_data = array![].span();
        let asset_diff = AssetBalanceDiffEnriched {
            asset_id: asset.id,
            balance_before: asset.balance,
            balance_after: BalanceTrait::new(value: 80),
            price: asset.price,
            risk_factor_before: RISK_FACTOR_1(),
            risk_factor_after: RISK_FACTOR_1(),
        };
        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: Default::default(), asset_diff_enriched: Option::Some(asset_diff),
        };
        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: position_data, position_diff_enriched: position_diff_enriched,
        );
        let position_tvtr_change = calculate_position_tvtr_change(
            tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
        );

        /// Ensures `total_value` before the change is `54,000`, calculated as `balance_before *
        /// price`
        /// (`60 * 900`).
        assert!(position_tvtr_change.before.total_value == 54_000);

        /// Ensures `total_risk` before the change is `27,000`, calculated as `abs(balance_before) *
        /// price * risk_factor` (`abs(60) * 900 * 0.5`).
        assert!(position_tvtr_change.before.total_risk == 27_000);

        /// Ensures `total_value` after the change is `72,000`, calculated as `balance_after *
        /// price`
        /// (`80 * 900`).
        assert!(position_tvtr_change.after.total_value == 72_000);

        /// Ensures `total_risk` after the change is `36,000`, calculated as `abs(balance_after) *
        /// price * risk_factor` (`abs(80) * 900 * 0.5`).
        assert!(position_tvtr_change.after.total_risk == 36_000);
    }

    /// Test the `calculate_position_tvtr_change` function for the case where the balance is
    /// negative.
    ///
    /// This test verifies the correctness of total value and total risk calculations before and
    /// after a position change in a scenario with one asset entry and one asset diff, where the
    /// balance is negative.
    #[test]
    fn test_calculate_position_tvtr_change_negative_balance() {
        // Create a position with a single asset entry.
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: -60),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let position_data = array![].span();
        let asset_diff = AssetBalanceDiffEnriched {
            asset_id: asset.id,
            balance_before: asset.balance,
            balance_after: BalanceTrait::new(value: 20),
            price: asset.price,
            risk_factor_before: RISK_FACTOR_1(),
            risk_factor_after: RISK_FACTOR_1(),
        };
        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: Default::default(), asset_diff_enriched: Option::Some(asset_diff),
        };

        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: position_data, position_diff_enriched: position_diff_enriched,
        );
        let position_tvtr_change = calculate_position_tvtr_change(
            tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
        );

        /// Ensures `total_value` before the change is `-54,000`, calculated as `balance_before *
        /// price`
        /// (`-60 * 900`).
        assert!(position_tvtr_change.before.total_value == -54_000);

        /// Ensures `total_risk` before the change is `27,000`, calculated as `abs(balance_before) *
        /// price * risk_factor` (`abs(-60) * 900 * 0.5`).
        assert!(position_tvtr_change.before.total_risk == 27_000);

        /// Ensures `total_value` after the change is `-18,000`, calculated as `balance_after *
        /// price`
        /// (`20 * 900`).
        assert!(position_tvtr_change.after.total_value == 18_000);

        /// Ensures `total_risk` after the change is `9,000`, calculated as `abs(balance_after) *
        /// price * risk_factor` (`abs(20) * 900 * 0.5`).
        assert!(position_tvtr_change.after.total_risk == 9_000);
    }

    /// Test the `calculate_position_tvtr_change` function for the case where there are multiple
    /// assets
    ///
    /// This test verifies the correctness of total value and total risk calculations before and
    /// after a position change in a scenario with multiple assets and a single asset diff.
    #[test]
    fn test_calculate_position_tvtr_change_multiple_assets() {
        // Create a position with multiple assets.
        let asset_1 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: 60),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let asset_2 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_2(),
            balance: BalanceTrait::new(value: 40),
            price: PRICE_2(),
            risk_factor: RISK_FACTOR_2(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let asset_3 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_3(),
            balance: BalanceTrait::new(value: 20),
            price: PRICE_3(),
            risk_factor: RISK_FACTOR_3(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let asset_4 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_4(),
            balance: BalanceTrait::new(value: 10),
            price: PRICE_4(),
            risk_factor: RISK_FACTOR_4(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let asset_5 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_5(),
            balance: BalanceTrait::new(value: 5),
            price: PRICE_5(),
            risk_factor: RISK_FACTOR_5(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let position_data = array![asset_2, asset_3, asset_4, asset_5].span();

        // Create a position diff with two assets diff.
        let asset_diff_1 = AssetBalanceDiffEnriched {
            asset_id: asset_1.id,
            balance_before: asset_1.balance,
            balance_after: BalanceTrait::new(value: 80),
            price: asset_1.price,
            risk_factor_before: RISK_FACTOR_1(),
            risk_factor_after: RISK_FACTOR_1(),
        };

        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: Default::default(),
            asset_diff_enriched: Option::Some(asset_diff_1),
        };

        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: position_data, position_diff_enriched: position_diff_enriched,
        );
        let position_tvtr_change = calculate_position_tvtr_change(
            tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
        );

        /// Ensures `total_value` before the change is `121,500`, calculated as `balance_1_before *
        /// price + balance_2_before * price + balance_3_before * price + balance_4_before * price +
        /// balance_5_before * price` (`60 * 900 + 40 * 1000 + 20 * 500 + 10 * 100 + 5 * 2000`).
        assert!(position_tvtr_change.before.total_value == 115_000);

        /// Ensures `total_risk` before the change is `57,500`, calculated as `abs(balance_1_before)
        /// *
        /// price * risk_factor_1 + abs(balance_2_before) * price * risk_factor_2 +
        /// abs(balance_3_before) *
        /// price * risk_factor_3 + abs(balance_4_before) * price * risk_factor_4 +
        /// abs(balance_5_before) *
        /// price * risk_factor_5` (`abs(60) * 900 * 0.5 + abs(40) * 1000 * 0.9 + abs(20) * 500 *
        /// 0.1 +
        /// abs(10) * 100 * 0.7 + abs(5) * 2000 * 0`).
        assert!(position_tvtr_change.before.total_risk == 64_700);

        /// Ensures `total_value` after the change is `139,500`, calculated as `balance_1_after *
        /// price + balance_2_after * price` + balance_3_after * price + balance_4_after * price +
        /// balance_5_after * price` (`80 * 900 + 40 * 1000 + 20 * 500 + 10 * 100 + 5 * 2000`).
        /// The balance of the other assets remains the same, so balance_2_after = 40,
        /// balance_3_after = 20, balance_4_after = 10, balance_5_after = 5.
        assert!(position_tvtr_change.after.total_value == 133_000);

        /// Ensures `total_risk` after the change is `66,500`, calculated as `abs(balance_1_after) *
        /// price * risk_factor_1 + abs(balance_2_after) * price * risk_factor_2 +
        /// abs(balance_3_after) * price * risk_factor_3 + abs(balance_4_after) * price *
        /// risk_factor_4 + abs(balance_5_after) * price * risk_factor_5` (`abs(80) * 900 * 0.5 +
        /// abs(40) * 1000 * 0.9 + abs(20) * 500 * 0.1 + abs(10) * 100 * 0.7 + abs(5) * 2000 *
        /// 0`).
        /// The balance of the other assets remains the same, so balance_2_after = 40,
        /// balance_3_after = 20, balance_4_after = 10, balance_5_after = 5.
        assert!(position_tvtr_change.after.total_risk == 73_700);
    }

    /// Test the `calculate_position_tvtr_change` function for the case where the diff is empty.
    #[test]
    fn test_calculate_position_tvtr_empty_diff() {
        // Create a position with a single asset entry.
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: 60),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 38654705 },
        };
        let position_data = array![asset].span();

        // Create an empty position diff.
        let position_diff_enriched = Default::default();

        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: position_data, position_diff_enriched: position_diff_enriched,
        );
        let position_tvtr_change = calculate_position_tvtr_change(
            tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
        );

        /// Ensures `total_value` before the change is `54,000`, calculated as `balance_before *
        /// price`
        /// (`60 * 900`).
        assert!(position_tvtr_change.before.total_value == 54_000);

        /// Ensures `total_risk` before the change is `27,000`, calculated as `abs(balance_before) *
        /// price * risk_factor` (`abs(60) * 900 * 0.5`).
        assert!(position_tvtr_change.before.total_risk == 27_000);

        /// Ensures `total_value` after the change is `54,000`, calculated as `balance_before *
        /// price`
        /// (`60 * 900`).
        assert!(position_tvtr_change.after.total_value == 54_000);

        /// Ensures `total_risk` after the change is `27,000`, calculated as `abs(balance_before) *
        /// price * risk_factor` (`abs(60) * 900 * 0.5`).
        assert!(position_tvtr_change.after.total_risk == 27_000);
    }


    /// Test the `calculate_position_tvtr_change` function for the case where the position is empty,
    /// and no diff is provided.
    #[test]
    fn test_calculate_position_tvtr_empty_position_and_diff() {
        // Create an empty position.
        let position_data = array![].span();

        // Create an empty position diff.
        let position_diff_enriched = Default::default();

        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: position_data, position_diff_enriched: position_diff_enriched,
        );
        let position_tvtr_change = calculate_position_tvtr_change(
            tvtr_before, synthetic_enriched_position_diff: position_diff_enriched.into(),
        );

        /// Ensures `total_value` before the change is `0`.
        assert!(position_tvtr_change.before.total_value == 0);

        /// Ensures `total_risk` before the change is `0`.
        assert!(position_tvtr_change.before.total_risk == 0);

        /// Ensures `total_value` after the change is `0`.
        assert!(position_tvtr_change.after.total_value == 0);

        /// Ensures `total_risk` after the change is `0`.
        assert!(position_tvtr_change.after.total_risk == 0);
    }


    /// Test the `evaluate_position` function for the case where the position is empty.
    #[test]
    fn test_evaluate_position_empty_position_and_empty_diff() {
        // Create an empty position.

        let evaluated_position = evaluate_position(
            unchanged_assets: array![].span(), collateral_balance: Zero::zero(),
        );
        assert!(evaluated_position == PositionState::Healthy);
    }

    #[test]
    fn test_basic_calculate_position_tvtr_before() {
        let balance = 50;
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: balance),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let unchanged_assets = array![asset].span();
        let position_diff_enriched = Default::default();
        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: unchanged_assets, position_diff_enriched: position_diff_enriched,
        );
        //tvtr_before.total_value = balance * PRICE_1() = 50 * 900 = 45_000
        //tvtr_before.total_risk = balance * PRICE_1() * RISK_FACTOR_1() = 50 * 900 *
        //0.5 = 22_500
        assert!(tvtr_before.total_value == 45_000);
        assert!(tvtr_before.total_risk == 22_500);
    }

    #[test]
    fn test_calculate_position_tvtr_before_with_multiple_assets() {
        let balance_1 = 50;
        let balance_2 = 100;
        let balance_3 = 150;
        let collateral_balance_before = 50;
        let asset_1 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: balance_1),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 0 },
        };

        let asset_2 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_2(),
            balance: BalanceTrait::new(value: balance_2),
            price: PRICE_2(),
            risk_factor: RISK_FACTOR_2(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let asset_3 = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_3(),
            balance: BalanceTrait::new(value: balance_3),
            price: PRICE_3(),
            risk_factor: RISK_FACTOR_3(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let unchanged_assets = array![asset_1, asset_2].span();
        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: BalanceDiff {
                before: BalanceTrait::new(value: collateral_balance_before),
                after: BalanceTrait::new(value: 0),
            },
            asset_diff_enriched: Option::Some(
                AssetBalanceDiffEnriched {
                    asset_id: asset_3.id,
                    balance_before: asset_3.balance,
                    balance_after: BalanceTrait::new(value: 0),
                    price: asset_3.price,
                    risk_factor_before: RISK_FACTOR_3(),
                    risk_factor_after: RISK_FACTOR_3(),
                },
            ),
        };
        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: unchanged_assets, position_diff_enriched: position_diff_enriched,
        );
        //tvtr_before.total_value = balance_1 * PRICE_1() + balance_2 * PRICE_2()
        // + balance_3 * PRICE_3()  + collateral_balance_before  = 50 * 900 + 100 * 1000  + 150 *
        // 500
        //+ 50 = 220_050
        //tvtr_before.total_risk = abs(balance_1) * PRICE_1() * RISK_FACTOR_1() +
        //abs(balance_2) * PRICE_2() * RISK_FACTOR_2() + abs(balance_3) *
        //PRICE_3() * RISK_FACTOR_3() = abs(50) * 900 * 0.5 + abs(100) * 1000 * 0.9
        //+ abs(150) * 500 * 0.1 = 120_000
        assert!(tvtr_before.total_value == 220_050);
        assert!(tvtr_before.total_risk == 120_000);
    }

    #[test]
    fn test_calculate_position_tvtr_before_no_unchanged_assets() {
        let collateral_balance_before = 50;
        let synthetic_balance_before = 200;
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: synthetic_balance_before),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let unchanged_assets = array![].span();
        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: BalanceDiff {
                before: BalanceTrait::new(value: collateral_balance_before),
                after: BalanceTrait::new(value: 0),
            },
            asset_diff_enriched: Option::Some(
                AssetBalanceDiffEnriched {
                    asset_id: asset.id,
                    balance_before: asset.balance,
                    balance_after: BalanceTrait::new(value: 0),
                    price: asset.price,
                    risk_factor_before: RISK_FACTOR_1(),
                    risk_factor_after: RISK_FACTOR_1(),
                },
            ),
        };
        let tvtr_before = calculate_position_tvtr_before(
            unchanged_assets: unchanged_assets, position_diff_enriched: position_diff_enriched,
        );
        //tvtr_before.total_value = asset.balance * asset.price + collateral_balance_before
        //= 200 * 900  + 50  = 180_050
        //tvtr_before.total_risk = abs(asset.balance) * asset.price * RISK_FACTOR_1()
        //= abs(200) * 900 * 0.5 = 90_000
        assert!(tvtr_before.total_value == 180_050);
        assert!(tvtr_before.total_risk == 90_000);
    }

    #[test]
    fn test_calculate_position_tvtr_before_no_assets() {
        let unchanged_assets = array![].span();
        let position_diff_enriched = Default::default();
        let tvtr_before = calculate_position_tvtr_before(
            :unchanged_assets, :position_diff_enriched,
        );
        assert!(tvtr_before.total_value == 0);
        assert!(tvtr_before.total_risk == 0);
    }

    #[test]
    fn test_calculate_position_tvtr_before_negative_balance() {
        let balance = -50;
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: balance),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let unchanged_assets = array![asset].span();
        let position_diff_enriched = Default::default();
        let tvtr_before = calculate_position_tvtr_before(
            :unchanged_assets, :position_diff_enriched,
        );
        assert!(tvtr_before.total_value == -45_000);
        assert!(tvtr_before.total_risk == 22_500);
    }

    #[test]
    fn test_calculate_position_tvtr_before_negative_balance_in_diff() {
        let balance = -50;
        let asset = AssetBalanceInfo {
            id: SYNTHETIC_ASSET_ID_1(),
            balance: BalanceTrait::new(value: balance),
            price: PRICE_1(),
            risk_factor: RISK_FACTOR_1(),
            cached_funding_index: FundingIndex { value: 0 },
        };
        let unchanged_assets = array![].span();
        let position_diff_enriched = PositionDiffEnriched {
            collateral_enriched: Default::default(),
            asset_diff_enriched: Option::Some(
                AssetBalanceDiffEnriched {
                    asset_id: asset.id,
                    balance_before: asset.balance,
                    balance_after: BalanceTrait::new(value: 0),
                    price: asset.price,
                    risk_factor_before: RISK_FACTOR_1(),
                    risk_factor_after: RISK_FACTOR_1(),
                },
            ),
        };
        let tvtr_before = calculate_position_tvtr_before(
            :unchanged_assets, :position_diff_enriched,
        );
        //tvtr_before.total_value = asset.balance * asset.price = -50 * 900 = -45_000
        //tvtr_before.total_risk = abs(asset.balance) * asset.price * RISK_FACTOR_1()
        //= abs(-50) * 900 * 0.5 = 22_500
        assert!(tvtr_before.total_value == -45_000);
        assert!(tvtr_before.total_risk == 22_500);
    }
}
