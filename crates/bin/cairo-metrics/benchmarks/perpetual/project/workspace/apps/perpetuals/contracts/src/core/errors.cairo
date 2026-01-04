use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use perpetuals::core::value_risk_calculator::TVTRChange;

pub const AMOUNT_OVERFLOW: felt252 = 'AMOUNT_OVERFLOW';
pub const ASSET_ID_NOT_COLLATERAL: felt252 = 'QUOTE_ASSET_ID_NOT_COLLATERAL';
pub const CANT_TRADE_WITH_FEE_POSITION: felt252 = 'CANT_TRADE_WITH_FEE_POSITION';
pub const CANT_LIQUIDATE_IF_POSITION: felt252 = 'CANT_LIQUIDATE_IF_POSITION';
pub const DIFFERENT_BASE_ASSET_IDS: felt252 = 'DIFFERENT_BASE_ASSET_IDS';
pub const INVALID_ACTUAL_BASE_SIGN: felt252 = 'INVALID_ACTUAL_BASE_SIGN';
pub const INVALID_ACTUAL_QUOTE_SIGN: felt252 = 'INVALID_ACTUAL_QUOTE_SIGN';
pub const INVALID_AMOUNT_SIGN: felt252 = 'INVALID_AMOUNT_SIGN';
pub const INVALID_BASE_CHANGE: felt252 = 'INVALID_BASE_CHANGE';
pub const INVALID_QUOTE_AMOUNT_SIGN: felt252 = 'INVALID_QUOTE_AMOUNT_SIGN';
pub const INVALID_QUOTE_FEE_AMOUNT: felt252 = 'INVALID_QUOTE_FEE_AMOUNT';
pub const INVALID_SAME_POSITIONS: felt252 = 'INVALID_SAME_POSITIONS';
pub const INVALID_WITHDRAW_COLLATERAL: felt252 = 'INVALID_WITHDRAW_COLLATERAL';
pub const INVALID_ZERO_AMOUNT: felt252 = 'INVALID_ZERO_AMOUNT';
pub const INVALID_ZERO_TIMEOUT: felt252 = 'INVALID_ZERO_TIMEOUT';
pub const FORCED_WAIT_REQUIRED: felt252 = 'FORCED_WAIT_REQUIRED';
pub const SIGNED_TX_EXPIRED: felt252 = 'SIGNED_TX_EXPIRED';
pub const SYNTHETIC_IS_ACTIVE: felt252 = 'SYNTHETIC_IS_ACTIVE';
pub const TRADE_ASSET_NOT_SYNTHETIC: felt252 = 'TRADE_ASSET_NOT_SYNTHETIC';
pub const TRANSFER_FAILED: felt252 = 'TRANSFER_FAILED';
pub const SAME_BASE_QUOTE_ASSET_IDS: felt252 = 'SAME_BASE_QUOTE_ASSET_IDS';
pub const ORDER_IS_NOT_EXPIRED: felt252 = 'ORDER_IS_NOT_EXPIRED';
pub const LENGTH_MISMATCH: felt252 = 'LENGTH_MISMATCH';

pub fn fulfillment_exceeded_err(position_id: PositionId) -> ByteArray {
    format!("FULFILLMENT_EXCEEDED position_id: {:?}", position_id)
}

pub fn illegal_base_to_quote_ratio_err(position_id: PositionId) -> ByteArray {
    format!("ILLEGAL_BASE_TO_QUOTE_RATIO position_id: {:?}", position_id)
}

pub fn illegal_fee_to_quote_ratio_err(position_id: PositionId) -> ByteArray {
    format!("ILLEGAL_FEE_TO_QUOTE_RATIO position_id: {:?}", position_id)
}

pub fn invalid_funding_rate_err(synthetic_id: AssetId) -> ByteArray {
    format!("INVALID_FUNDING_RATE synthetic_id: {:?}", synthetic_id)
}

pub fn order_expired_err(position_id: PositionId) -> ByteArray {
    format!("ORDER_EXPIRED position_id: {:?}", position_id)
}

pub fn position_not_deleveragable(position_id: PositionId, tvtr: TVTRChange) -> ByteArray {
    format!(
        "POSITION_IS_NOT_DELEVERAGABLE position_id: {:?} TV before {:?}, TR before {:?}, TV after {:?}, TR after {:?}",
        position_id,
        tvtr.before.total_value,
        tvtr.before.total_risk,
        tvtr.after.total_value,
        tvtr.after.total_risk,
    )
}

pub fn position_not_fair_deleverage(position_id: PositionId, tvtr: TVTRChange) -> ByteArray {
    format!(
        "POSITION_IS_NOT_FAIR_DELEVERAGE position_id: {:?} TV before {:?}, TR before {:?}, TV after {:?}, TR after {:?}",
        position_id,
        tvtr.before.total_value,
        tvtr.before.total_risk,
        tvtr.after.total_value,
        tvtr.after.total_risk,
    )
}

pub fn position_not_healthy_nor_healthier(position_id: PositionId, tvtr: TVTRChange) -> ByteArray {
    format!(
        "POSITION_NOT_HEALTHY_NOR_HEALTHIER position_id: {:?} TV before {:?}, TR before {:?}, TV after {:?}, TR after {:?}",
        position_id,
        tvtr.before.total_value,
        tvtr.before.total_risk,
        tvtr.after.total_value,
        tvtr.after.total_risk,
    )
}

pub fn position_not_liquidatable(position_id: PositionId, tvtr: TVTRChange) -> ByteArray {
    format!(
        "POSITION_IS_NOT_LIQUIDATABLE position_id: {:?} TV before {:?}, TR before {:?}, TV after {:?}, TR after {:?}",
        position_id,
        tvtr.before.total_value,
        tvtr.before.total_risk,
        tvtr.after.total_value,
        tvtr.after.total_risk,
    )
}
