use core::hash::{HashStateExTrait, HashStateTrait};
use core::panics::panic_with_byte_array;
use core::poseidon::PoseidonTrait;
use openzeppelin::utils::snip12::StructHash;
use perpetuals::core::errors::{illegal_base_to_quote_ratio_err, illegal_fee_to_quote_ratio_err};
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starkware_utils::math::abs::Abs;
use starkware_utils::math::fraction::FractionTrait;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::Timestamp;

pub const VERSION: u8 = 0;

pub trait ValidateableOrderTrait<T> {
    fn position_id(self: @T) -> PositionId;
    // The base asset
    fn base_asset_id(self: @T) -> AssetId;
    // The amount of the base asset to be bought or sold.
    fn base_amount(self: @T) -> i64;
    // The quote asset.
    fn quote_asset_id(self: @T) -> AssetId;
    // The amount of the quote asset to be paid or received.
    fn quote_amount(self: @T) -> i64;
    // The fee asset.
    fn fee_asset_id(self: @T) -> AssetId;
    // The amount of the fee asset to be paid.
    fn fee_amount(self: @T) -> u64;
    // The expiration time of the order.
    fn expiration(self: @T) -> Timestamp;

    fn validate_against_actual_amounts(
        self: @T, actual_amount_base: i64, actual_amount_quote: i64, actual_fee: u64,
    ) {
        let order_base_amount = Self::base_amount(:self);
        let order_quote_amount = Self::quote_amount(:self);
        let order_fee_amount = Self::fee_amount(:self);
        let position = Self::position_id(:self);

        let order_base_to_quote_ratio = FractionTrait::new(
            numerator: (order_base_amount).into(), denominator: (order_quote_amount).abs().into(),
        );
        let actual_base_to_quote_ratio = FractionTrait::new(
            numerator: actual_amount_base.into(), denominator: actual_amount_quote.abs().into(),
        );
        if (order_base_to_quote_ratio > actual_base_to_quote_ratio) {
            let err = illegal_base_to_quote_ratio_err(position);
            panic_with_byte_array(err: @err);
        }

        // Validating the fee-to-quote ratio enables increasing in both the user's quote and the
        // operator's fee.
        let actual_fee_to_quote_ratio = FractionTrait::new(
            numerator: actual_fee.into(), denominator: actual_amount_quote.abs().into(),
        );
        let order_fee_to_quote_ratio = FractionTrait::new(
            numerator: (order_fee_amount).into(), denominator: (order_quote_amount).abs().into(),
        );

        if (actual_fee_to_quote_ratio > order_fee_to_quote_ratio) {
            let err = illegal_fee_to_quote_ratio_err(position);
            panic_with_byte_array(err: @err);
        }
    }
}

#[derive(Copy, Drop, Hash, Serde)]
// An order to buy or sell an asset for a collateral asset.
// The base amount and quote amount have opposite signs.
pub(crate) struct LimitOrder {
    pub source_position: PositionId,
    pub receive_position: PositionId,
    // The asset to be bought or sold.
    pub base_asset_id: AssetId,
    // The amount of the asset to be bought or sold.
    pub base_amount: i64,
    // The collateral asset.
    pub quote_asset_id: AssetId,
    // The amount of the collateral asset to be paid or received.
    pub quote_amount: i64,
    // The collateral asset.
    pub fee_asset_id: AssetId,
    // The amount of the collateral asset to be paid.
    pub fee_amount: u64,
    // The expiration time of the order.
    pub expiration: Timestamp,
    // A random value to make each order unique.
    pub salt: felt252,
}

pub impl LimitOrderValidationTraitImpl of ValidateableOrderTrait<LimitOrder> {
    fn position_id(self: @LimitOrder) -> PositionId {
        *self.source_position
    }
    fn base_asset_id(self: @LimitOrder) -> AssetId {
        *self.base_asset_id
    }
    fn base_amount(self: @LimitOrder) -> i64 {
        *self.base_amount
    }
    fn quote_asset_id(self: @LimitOrder) -> AssetId {
        *self.quote_asset_id
    }
    fn quote_amount(self: @LimitOrder) -> i64 {
        *self.quote_amount
    }
    fn fee_asset_id(self: @LimitOrder) -> AssetId {
        *self.fee_asset_id
    }
    fn fee_amount(self: @LimitOrder) -> u64 {
        *self.fee_amount
    }
    fn expiration(self: @LimitOrder) -> Timestamp {
        *self.expiration
    }
}

/// selector!(
///   "\"Order\"(
///    \"source_position\":\"PositionId\",
///    \"receive_position\":\"PositionId\",
///    \"base_asset_id\":\"AssetId\",
///    \"base_amount\":\"i64\",
///    \"quote_asset_id\":\"AssetId\",
///    \"quote_amount\":\"i64\",
///    \"fee_asset_id\":\"AssetId\",
///    \"fee_amount\":\"u64\",
///    \"expiration\":\"Timestamp\",
///    \"salt\":\"felt\"
///    )
///    \"PositionId\"(
///    \"value\":\"u32\"
///    )"
///    \"AssetId\"(
///    \"value\":\"felt\"
///    )"
///    \"Timestamp\"(
///    \"seconds\":\"u64\"
///    )
/// );

const LIMIT_ORDER_TYPE_HASH: HashType =
    0x03c79b3b5997e78a29ab2fb5e8bc8244f222c5e01ae914c10f956bd0f805199a;


pub impl LimitOrderStructHashImpl of StructHash<LimitOrder> {
    fn hash_struct(self: @LimitOrder) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(LIMIT_ORDER_TYPE_HASH).update_with(*self).finalize()
    }
}

#[derive(Copy, Drop, Hash, Serde)]
// An order to buy or sell a synthetic asset for a collateral asset.
// The base amount and quote amount have opposite signs.
pub struct Order {
    pub position_id: PositionId,
    // The synthetic asset to be bought or sold.
    pub base_asset_id: AssetId,
    // The amount of the synthetic asset to be bought or sold.
    pub base_amount: i64,
    // The collateral asset.
    pub quote_asset_id: AssetId,
    // The amount of the collateral asset to be paid or received.
    pub quote_amount: i64,
    // The collateral asset.
    pub fee_asset_id: AssetId,
    // The amount of the collateral asset to be paid.
    pub fee_amount: u64,
    // The expiration time of the order.
    pub expiration: Timestamp,
    // A random value to make each order unique.
    pub salt: felt252,
}

impl OrderValidationTraitImpl of ValidateableOrderTrait<Order> {
    fn position_id(self: @Order) -> PositionId {
        *self.position_id
    }
    fn base_asset_id(self: @Order) -> AssetId {
        *self.base_asset_id
    }
    fn base_amount(self: @Order) -> i64 {
        *self.base_amount
    }
    fn quote_asset_id(self: @Order) -> AssetId {
        *self.quote_asset_id
    }
    fn quote_amount(self: @Order) -> i64 {
        *self.quote_amount
    }
    fn fee_asset_id(self: @Order) -> AssetId {
        *self.fee_asset_id
    }
    fn fee_amount(self: @Order) -> u64 {
        *self.fee_amount
    }
    fn expiration(self: @Order) -> Timestamp {
        *self.expiration
    }
}

/// selector!(
///   "\"Order\"(
///    \"position_id\":\"PositionId\",
///    \"base_asset_id\":\"AssetId\",
///    \"base_amount\":\"i64\",
///    \"quote_asset_id\":\"AssetId\",
///    \"quote_amount\":\"i64\",
///    \"fee_asset_id\":\"AssetId\",
///    \"fee_amount\":\"u64\",
///    \"expiration\":\"Timestamp\",
///    \"salt\":\"felt\"
///    )
///    \"PositionId\"(
///    \"value\":\"u32\"
///    )"
///    \"AssetId\"(
///    \"value\":\"felt\"
///    )"
///    \"Timestamp\"(
///    \"seconds\":\"u64\"
///    )
/// );

const ORDER_TYPE_HASH: HashType = 0x36da8d51815527cabfaa9c982f564c80fa7429616739306036f1f9b608dd112;

impl OrderStructHashImpl of StructHash<Order> {
    fn hash_struct(self: @Order) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(ORDER_TYPE_HASH).update_with(*self).finalize()
    }
}

#[derive(Copy, Drop, Hash, Serde)]
pub struct ForcedTrade {
    pub order_a: Order,
    pub order_b: Order,
}

/// selector!(
///   "\"ForcedTrade\"(
///    \"order_a\":\"Order\",
///    \"order_b\":\"Order\",
///    )
///   "\"Order\"(
///    \"position_id\":\"PositionId\",
///    \"base_asset_id\":\"AssetId\",
///    \"base_amount\":\"i64\",
///    \"quote_asset_id\":\"AssetId\",
///    \"quote_amount\":\"i64\",
///    \"fee_asset_id\":\"AssetId\",
///    \"fee_amount\":\"u64\",
///    \"expiration\":\"Timestamp\",
///    \"salt\":\"felt\"
///    )
///    \"PositionId\"(
///    \"value\":\"u32\"
///    )"
///    \"AssetId\"(
///    \"value\":\"felt\"
///    )"
/// );

const FORCED_TRADE_TYPE_HASH: HashType =
    0x172f9174139c4be3f21b0cd36200c84be2f8e782f181422a6ca51370756dac5;

impl ForcedTradeStructHashImpl of StructHash<ForcedTrade> {
    fn hash_struct(self: @ForcedTrade) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(FORCED_TRADE_TYPE_HASH).update_with(*self).finalize()
    }
}


#[cfg(test)]
mod tests {
    use starkware_utils::math::utils::to_base_16_string;
    use super::{FORCED_TRADE_TYPE_HASH, LIMIT_ORDER_TYPE_HASH, ORDER_TYPE_HASH};

    #[test]
    fn test_order_type_hash() {
        let expected = selector!(
            "\"Order\"(\"position_id\":\"felt\",\"base_asset_id\":\"AssetId\",\"base_amount\":\"i64\",\"quote_asset_id\":\"AssetId\",\"quote_amount\":\"i64\",\"fee_asset_id\":\"AssetId\",\"fee_amount\":\"u64\",\"expiration\":\"Timestamp\",\"salt\":\"felt\")\"PositionId\"(\"value\":\"u32\")\"AssetId\"(\"value\":\"felt\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert_eq!(to_base_16_string(ORDER_TYPE_HASH), to_base_16_string(expected));
    }
    #[test]
    fn test_limit_order_type_hash() {
        let expected = selector!(
            "\"LimitOrder\"(\"source_position\":\"PositionId\",\"receive_position\":\"PositionId\",\"base_asset_id\":\"AssetId\",\"base_amount\":\"i64\",\"quote_asset_id\":\"AssetId\",\"quote_amount\":\"i64\",\"fee_asset_id\":\"AssetId\",\"fee_amount\":\"u64\",\"expiration\":\"Timestamp\",\"salt\":\"felt\")\"PositionId\"(\"value\":\"u32\")\"AssetId\"(\"value\":\"felt\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert_eq!(to_base_16_string(LIMIT_ORDER_TYPE_HASH), to_base_16_string(expected));
    }
    #[test]
    fn test_forced_trade_type_hash() {
        let expected = selector!(
            "\"ForcedTrade\"(\"order_a\":\"Order\",\"order_b\":\"Order\")\"Order\"(\"position_id\":\"PositionId\",\"base_asset_id\":\"AssetId\",\"base_amount\":\"i64\",\"quote_asset_id\":\"AssetId\",\"quote_amount\":\"i64\",\"fee_asset_id\":\"AssetId\",\"fee_amount\":\"u64\",\"expiration\":\"Timestamp\",\"salt\":\"felt\")\"PositionId\"(\"value\":\"u32\")\"AssetId\"(\"value\":\"felt\")",
        );
        assert_eq!(to_base_16_string(FORCED_TRADE_TYPE_HASH), to_base_16_string(expected));
    }
}
