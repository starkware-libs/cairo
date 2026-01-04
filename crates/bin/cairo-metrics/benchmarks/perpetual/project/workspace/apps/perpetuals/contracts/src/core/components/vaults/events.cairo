use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;


#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct VaultOpened {
    #[key]
    pub position_id: PositionId,
    #[key]
    pub asset_id: AssetId,
}


#[derive(Debug, Drop, PartialEq, starknet::Event)]
pub struct InvestInVault {
    #[key]
    pub vault_position_id: PositionId,
    #[key]
    pub investing_position_id: PositionId,
    #[key]
    pub receiving_position_id: PositionId,
    #[key]
    pub vault_asset_id: AssetId,
    #[key]
    pub invested_asset_id: AssetId,
    pub shares_received: u64,
    pub user_investment: u64,
    pub correlation_id: felt252,
}
