use core::num::traits::Zero;
use perpetuals::core::types::asset::{AssetId, AssetStatus};
use perpetuals::core::types::balance::Balance;
use perpetuals::core::types::funding::FundingIndex;
use perpetuals::core::types::price::Price;
use perpetuals::core::types::risk_factor::RiskFactor;
use starknet::storage::StoragePointer0Offset;
use starknet::storage_access::storage_address_from_base_and_offset;
use starknet::syscalls::storage_read_syscall;
use starknet::{ContractAddress, SyscallResultTrait};
use starkware_utils::time::time::Timestamp;


const VERSION: u8 = 1;

#[derive(Copy, Debug, Drop, PartialEq, Serde, starknet::Store)]
pub enum AssetType {
    #[default]
    SYNTHETIC,
    SPOT_COLLATERAL,
    VAULT_SHARE_COLLATERAL,
}

pub impl Felt252TryIntoAssetType of TryInto<felt252, AssetType> {
    fn try_into(self: felt252) -> Option<AssetType> {
        match self {
            0 => Option::Some(AssetType::SYNTHETIC),
            1 => Option::Some(AssetType::SPOT_COLLATERAL),
            2 => Option::Some(AssetType::VAULT_SHARE_COLLATERAL),
            _ => Option::None,
        }
    }
}

#[derive(Copy, Drop, Serde, starknet::Store)]
pub struct AssetConfig {
    version: u8,
    // Configurable
    pub status: AssetStatus, // V1
    pub risk_factor_first_tier_boundary: u128, // V1
    pub risk_factor_tier_size: u128, // V1
    pub quorum: u8, // V1
    // Smallest unit of a synthetic asset in the system.
    pub resolution_factor: u64, // V1
    pub quantum: u64, // V2
    pub token_contract: Option<ContractAddress>, // V2
    pub asset_type: AssetType // V2
}


#[derive(Copy, Drop, Serde, starknet::Store)]
pub struct TimelyData {
    version: u8,
    pub price: Price,
    pub last_price_update: Timestamp,
    pub funding_index: FundingIndex,
}

impl TimelyDataDefault of Default<TimelyData> {
    fn default() -> TimelyData {
        TimelyData {
            version: VERSION,
            price: Zero::zero(),
            last_price_update: Zero::zero(),
            funding_index: Zero::zero(),
        }
    }
}

#[derive(Copy, Debug, Drop, Serde, PartialEq)]
pub struct AssetBalanceInfo {
    pub id: AssetId,
    pub balance: Balance,
    pub price: Price,
    pub risk_factor: RiskFactor,
    pub cached_funding_index: FundingIndex,
}

#[derive(Copy, Debug, Default, Drop, Serde)]
pub struct AssetBalanceDiffEnriched {
    pub asset_id: AssetId,
    pub balance_before: Balance,
    pub balance_after: Balance,
    pub price: Price,
    pub risk_factor_before: RiskFactor,
    pub risk_factor_after: RiskFactor,
}

#[generate_trait]
pub impl SyntheticImpl of SyntheticTrait {
    fn synthetic(
        status: AssetStatus,
        risk_factor_first_tier_boundary: u128,
        risk_factor_tier_size: u128,
        quorum: u8,
        resolution_factor: u64,
    ) -> AssetConfig {
        AssetConfig {
            version: VERSION,
            status,
            risk_factor_first_tier_boundary,
            risk_factor_tier_size,
            quorum,
            resolution_factor,
            quantum: 0,
            token_contract: None,
            asset_type: AssetType::SYNTHETIC,
        }
    }

    fn spot(
        status: AssetStatus,
        risk_factor_first_tier_boundary: u128,
        risk_factor_tier_size: u128,
        quorum: u8,
        resolution_factor: u64,
        quantum: u64,
        token_contract: ContractAddress,
    ) -> AssetConfig {
        AssetConfig {
            version: VERSION,
            status,
            risk_factor_first_tier_boundary,
            risk_factor_tier_size,
            quorum,
            resolution_factor,
            quantum: quantum,
            token_contract: Some(token_contract),
            asset_type: AssetType::SPOT_COLLATERAL,
        }
    }

    fn vault_share(
        status: AssetStatus,
        risk_factor_first_tier_boundary: u128,
        risk_factor_tier_size: u128,
        quorum: u8,
        resolution_factor: u64,
        quantum: u64,
        token_contract: ContractAddress,
    ) -> AssetConfig {
        AssetConfig {
            version: VERSION,
            status,
            risk_factor_first_tier_boundary,
            risk_factor_tier_size,
            quorum,
            resolution_factor,
            quantum: quantum,
            token_contract: Some(token_contract),
            asset_type: AssetType::VAULT_SHARE_COLLATERAL,
        }
    }

    fn timely_data(
        price: Price, last_price_update: Timestamp, funding_index: FundingIndex,
    ) -> TimelyData {
        TimelyData { version: VERSION, price, last_price_update, funding_index }
    }

    /// Reads the Option<TimelyData> from the storage.
    /// The offset is used to read specific fields of the struct.
    #[inline]
    fn read(
        entry: StoragePointer0Offset<Option<TimelyData>>, offset: OptionTimelyDataOffset,
    ) -> felt252 {
        storage_read_syscall(
            0,
            storage_address_from_base_and_offset(entry.__storage_pointer_address__, offset.into()),
        )
            .unwrap_syscall()
    }

    /// Reads the Option<AssetConfig> from the storage.
    /// The offset is used to read specific fields of the struct.
    #[inline]
    fn read_config(
        entry: StoragePointer0Offset<Option<AssetConfig>>, offset: OptionAssetConfigOffset,
    ) -> felt252 {
        storage_read_syscall(
            0,
            storage_address_from_base_and_offset(entry.__storage_pointer_address__, offset.into()),
        )
            .unwrap_syscall()
    }

    /// Reads the variant of the Option<TimelyData>.
    /// The variant mark if the Option is Some or None.
    #[inline]
    fn read_variant(entry: StoragePointer0Offset<Option<TimelyData>>) -> felt252 {
        Self::read(entry, OptionTimelyDataOffset::VARIANT)
    }

    /// Returns true if the Option is Some, false if None.
    /// At the storage 0 indicates None, 1 indicates Some.
    #[inline]
    fn is_some(entry: StoragePointer0Offset<Option<TimelyData>>) -> bool {
        let variant = Self::read_variant(entry);
        variant == 1
    }

    /// Returns true if the Option is None, false if Some.
    /// At the storage 0 indicates None, 1 indicates Some.
    #[inline]
    fn is_none(entry: StoragePointer0Offset<Option<TimelyData>>) -> bool {
        let variant = Self::read_variant(entry);
        variant == 0
    }

    /// Reads the price from the Option<TimelyData>.
    /// This function does not check if the Option is Some or None.
    fn at_price(entry: StoragePointer0Offset<Option<TimelyData>>) -> Price {
        let price = Self::read(entry, OptionTimelyDataOffset::PRICE);
        let price: u64 = price.try_into().unwrap();
        price.into()
    }

    /// Reads the funding index from the Option<TimelyData>.
    /// This function does not check if the Option is Some or None.
    fn at_funding_index(entry: StoragePointer0Offset<Option<TimelyData>>) -> FundingIndex {
        let funding_index = Self::read(entry, OptionTimelyDataOffset::FUNDING_INDEX);
        let funding_index: i64 = funding_index.try_into().unwrap();
        funding_index.into()
    }

    /// Returns true if the Option is Some, false if None.
    /// At the storage, 0 indicates None, 1 indicates Some.
    #[inline]
    fn is_some_config(entry: StoragePointer0Offset<Option<AssetConfig>>) -> bool {
        let variant = Self::read_config(entry, OptionAssetConfigOffset::VARIANT);
        variant == 1
    }

    /// Reads the asset type from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    #[inline]
    fn at_asset_type(entry: StoragePointer0Offset<Option<AssetConfig>>) -> AssetType {
        let asset_type = Self::read_config(entry, OptionAssetConfigOffset::ASSET_TYPE);
        asset_type.try_into().unwrap()
    }

    /// Reads the asset type from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    #[inline]
    fn at_asset_status(entry: StoragePointer0Offset<Option<AssetConfig>>) -> AssetStatus {
        let asset_status = Self::read_config(entry, OptionAssetConfigOffset::STATUS);
        asset_status.try_into().unwrap()
    }

    /// Reads the asset type from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    #[inline]
    fn at_quantum(entry: StoragePointer0Offset<Option<AssetConfig>>) -> u64 {
        let quantum = Self::read_config(entry, OptionAssetConfigOffset::QUANTUM);
        quantum.try_into().unwrap()
    }

    /// Reads the asset type from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    #[inline]
    fn at_token_contract(entry: StoragePointer0Offset<Option<AssetConfig>>) -> ContractAddress {
        assert(
            Self::read_config(entry, OptionAssetConfigOffset::TOKEN_CONTRACT_VARIANT) == 1,
            'EXPECTED_TOKEN_ADDRESS',
        );
        let token_contract = Self::read_config(entry, OptionAssetConfigOffset::TOKEN_CONTRACT);
        token_contract.try_into().unwrap()
    }

    /// Gets the price from the Option<TimelyData>.
    /// Returns None if the Option is None.
    fn get_price(entry: StoragePointer0Offset<Option<TimelyData>>) -> Option<Price> {
        if Self::is_none(entry) {
            return Option::None;
        }
        Option::Some(Self::at_price(entry))
    }

    /// Gets the funding index from the Option<TimelyData>.
    /// Returns None if the Option is None.
    fn get_funding_index(entry: StoragePointer0Offset<Option<TimelyData>>) -> Option<FundingIndex> {
        if Self::is_none(entry) {
            return Option::None;
        }
        Option::Some(Self::at_funding_index(entry))
    }

    /// Gets the asset type from the Option<AssetConfig>.
    /// Returns None if the Option is None.
    fn get_asset_type(entry: StoragePointer0Offset<Option<AssetConfig>>) -> Option<AssetType> {
        if Self::is_some_config(entry) {
            Option::Some(Self::at_asset_type(entry))
        } else {
            Option::None
        }
    }

    /// Reads the risk factor first tier boundary from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    fn at_risk_factor_first_tier_boundary(
        entry: StoragePointer0Offset<Option<AssetConfig>>,
    ) -> u128 {
        let value = Self::read_config(
            entry, OptionAssetConfigOffset::RISK_FACTOR_FIRST_TIER_BOUNDARY,
        );
        let value: u128 = value.try_into().unwrap();
        value
    }

    /// Reads the risk factor tier size from the Option<AssetConfig>.
    /// This function does not check if the Option is Some or None.
    fn at_risk_factor_tier_size(entry: StoragePointer0Offset<Option<AssetConfig>>) -> u128 {
        let value = Self::read_config(entry, OptionAssetConfigOffset::RISK_FACTOR_TIER_SIZE);
        let value: u128 = value.try_into().unwrap();
        value
    }
}

/// In the storage, the Option<TimelyData> is stored as a struct with the following layout:
/// - variant: u8 (1 for Some, 0 for None)
/// - version: u8
/// - price: u64
/// - last_price_update: u64
/// - funding_index: i64
/// The offsets are used to read specific fields of the struct.
#[derive(Copy, Drop, Debug, PartialEq, Serde)]
pub enum OptionTimelyDataOffset {
    VARIANT,
    VERSION,
    PRICE,
    LAST_PRICE_UPDATE,
    FUNDING_INDEX,
}


/// Convert the enum to u8 for storage access.
pub impl OptionTimelyDataOffsetIntoU8 of Into<OptionTimelyDataOffset, u8> {
    fn into(self: OptionTimelyDataOffset) -> u8 {
        match self {
            OptionTimelyDataOffset::VARIANT => 0_u8,
            OptionTimelyDataOffset::VERSION => 1_u8,
            OptionTimelyDataOffset::PRICE => 2_u8,
            OptionTimelyDataOffset::LAST_PRICE_UPDATE => 3_u8,
            OptionTimelyDataOffset::FUNDING_INDEX => 4_u8,
        }
    }
}

#[derive(Copy, Drop, Debug, PartialEq, Serde)]
pub enum OptionAssetConfigOffset {
    VARIANT,
    VERSION,
    STATUS,
    RISK_FACTOR_FIRST_TIER_BOUNDARY,
    RISK_FACTOR_TIER_SIZE,
    QUORUM,
    RESOLUTION_FACTOR,
    QUANTUM,
    TOKEN_CONTRACT_VARIANT,
    TOKEN_CONTRACT,
    ASSET_TYPE,
}

pub impl OptionAssetConfigOffsetIntoU8 of Into<OptionAssetConfigOffset, u8> {
    fn into(self: OptionAssetConfigOffset) -> u8 {
        match self {
            OptionAssetConfigOffset::VARIANT => 0_u8,
            OptionAssetConfigOffset::VERSION => 1_u8,
            OptionAssetConfigOffset::STATUS => 2_u8,
            OptionAssetConfigOffset::RISK_FACTOR_FIRST_TIER_BOUNDARY => 3_u8,
            OptionAssetConfigOffset::RISK_FACTOR_TIER_SIZE => 4_u8,
            OptionAssetConfigOffset::QUORUM => 5_u8,
            OptionAssetConfigOffset::RESOLUTION_FACTOR => 6_u8,
            OptionAssetConfigOffset::QUANTUM => 7_u8,
            OptionAssetConfigOffset::TOKEN_CONTRACT_VARIANT => 8_u8,
            OptionAssetConfigOffset::TOKEN_CONTRACT => 9_u8,
            OptionAssetConfigOffset::ASSET_TYPE => 10_u8,
        }
    }
}
