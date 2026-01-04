use starkware_utils::time::time::Timestamp;


const VERSION: u8 = 1;


#[derive(Copy, Drop, Serde, starknet::Store)]
pub struct VaultData {
    version: u8,
    pub total_shares: u64,
    pub total_value: i128,
    pub last_total_value_update: Timestamp,
    pub status: VaultStatus,
}

#[derive(Copy, Debug, Drop, PartialEq, Serde, starknet::Store)]
pub enum VaultStatus {
    #[default]
    NON_EXISTENT,
    ACTIVE,
    INACTIVE,
}

