use perpetuals::core::types::asset::AssetId;

#[derive(Copy, Debug, Default, Drop, Hash, PartialEq, Serde, starknet::Store)]
pub struct VaultConfig {
    pub version: u8,
    pub asset_id: AssetId,
    pub position_id: u32,
}
