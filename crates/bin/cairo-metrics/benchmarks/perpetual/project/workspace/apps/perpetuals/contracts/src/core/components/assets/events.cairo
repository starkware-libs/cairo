use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::funding::FundingIndex;
use perpetuals::core::types::price::Price;
use starknet::ContractAddress;
use starkware_utils::signature::stark::PublicKey;

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct OracleAdded {
    #[key]
    pub asset_id: AssetId,
    pub asset_name: felt252,
    #[key]
    pub oracle_public_key: PublicKey,
    pub oracle_name: felt252,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct SyntheticAdded {
    #[key]
    pub asset_id: AssetId,
    pub risk_factor_tiers: Span<u16>,
    pub risk_factor_first_tier_boundary: u128,
    pub risk_factor_tier_size: u128,
    pub resolution_factor: u64,
    pub quorum: u8,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct SyntheticChanged {
    #[key]
    pub asset_id: AssetId,
    pub risk_factor_tiers: Span<u16>,
    pub risk_factor_first_tier_boundary: u128,
    pub risk_factor_tier_size: u128,
    pub resolution_factor: u64,
    pub quorum: u8,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct SpotAssetAdded {
    #[key]
    pub asset_id: AssetId,
    pub risk_factor_tiers: Span<u16>,
    pub risk_factor_first_tier_boundary: u128,
    pub risk_factor_tier_size: u128,
    pub resolution_factor: u64,
    pub quorum: u8,
    pub contract_address: ContractAddress,
    pub quantum: u64,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct AssetActivated {
    #[key]
    pub asset_id: AssetId,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct SyntheticAssetDeactivated {
    #[key]
    pub asset_id: AssetId,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct FundingTick {
    #[key]
    pub asset_id: AssetId,
    pub funding_index: FundingIndex,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct PriceTick {
    #[key]
    pub asset_id: AssetId,
    pub price: Price,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct OracleRemoved {
    #[key]
    pub asset_id: AssetId,
    #[key]
    pub oracle_public_key: PublicKey,
}

#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct AssetQuorumUpdated {
    #[key]
    pub asset_id: AssetId,
    pub new_quorum: u8,
    pub old_quorum: u8,
}
