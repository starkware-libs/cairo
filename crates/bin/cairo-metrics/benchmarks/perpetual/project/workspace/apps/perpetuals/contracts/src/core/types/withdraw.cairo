use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin::utils::snip12::StructHash;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::Timestamp;

#[derive(Copy, Drop, Hash, Serde, Debug)]
pub struct WithdrawArgs {
    pub recipient: ContractAddress,
    pub position_id: PositionId,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    pub salt: felt252,
}

/// selector!(
///   "\"WithdrawArgs\"(
///    \"recipient\":\"ContractAddress\",
///    \"position_id\":\"PositionId\",
///    \"collateral_id\":\"AssetId\",
///    \"amount\":\"u64\",
///    \"expiration\":\"Timestamp\"
///    \"salt\":\"felt\",
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
const WITHDRAW_ARGS_TYPE_HASH: HashType =
    0x250a5fa378e8b771654bd43dcb34844534f9d1e29e16b14760d7936ea7f4b1d;

impl WithdrawArgsStructHashImpl of StructHash<WithdrawArgs> {
    fn hash_struct(self: @WithdrawArgs) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(WITHDRAW_ARGS_TYPE_HASH).update_with(*self).finalize()
    }
}

#[derive(Copy, Drop, Hash, Serde, Debug)]
pub struct ForcedWithdrawArgs {
    pub withdraw_args_hash: HashType,
}

/// selector!(
///   "\"ForcedWithdrawArgs\"(
///    \"withdraw_args_hash\":\"HashType\",
///    )
/// );
const FORCED_WITHDRAW_ARGS_TYPE_HASH: HashType =
    0x9178e6792dcaaa6e712c83d5a34ff15f5c6a8158887dfb66d7f0956f557b0e;

impl ForcedWithdrawArgsStructHashImpl of StructHash<ForcedWithdrawArgs> {
    fn hash_struct(self: @ForcedWithdrawArgs) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(FORCED_WITHDRAW_ARGS_TYPE_HASH).update_with(*self).finalize()
    }
}


#[cfg(test)]
mod tests {
    use perpetuals::core::types::asset::AssetIdTrait;
    use starkware_utils::math::utils::to_base_16_string;
    use super::*;

    #[test]
    fn test_withdraw_type_hash() {
        let expected = selector!(
            "\"WithdrawArgs\"(\"recipient\":\"ContractAddress\",\"position_id\":\"PositionId\",\"collateral_id\":\"AssetId\",\"amount\":\"u64\",\"expiration\":\"Timestamp\",\"salt\":\"felt\")\"PositionId\"(\"value\":\"u32\")\"AssetId\"(\"value\":\"felt\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert_eq!(to_base_16_string(WITHDRAW_ARGS_TYPE_HASH), to_base_16_string(expected));
    }

    #[test]
    fn test_forced_withdraw_type_hash() {
        let expected = selector!("\"ForcedWithdrawArgs\"(\"withdraw_args_hash\":\"HashType\")");
        assert_eq!(to_base_16_string(FORCED_WITHDRAW_ARGS_TYPE_HASH), to_base_16_string(expected));
    }

    #[test]
    fn test_withdraw_hash_struct() {
        let withdraw_args = WithdrawArgs {
            position_id: PositionId { value: 1_u32 },
            salt: 123,
            expiration: Timestamp { seconds: 5 },
            collateral_id: AssetIdTrait::new(4),
            amount: 1000,
            recipient: 0x019ec96d4aea6fdc6f0b5f393fec3f186aefa8f0b8356f43d07b921ff48aa5da
                .try_into()
                .unwrap(),
        };
        let withdraw_args_hash = withdraw_args.hash_struct();
        assert_eq!(
            to_base_16_string(withdraw_args_hash),
            "0x04c22f625c59651e1219c60d03055f11f5dc23959929de35861548d86c0bc4ec",
        );

        let forced_withdraw_args_hash = ForcedWithdrawArgs { withdraw_args_hash }.hash_struct();
        assert_eq!(
            to_base_16_string(forced_withdraw_args_hash),
            "0x0731c220cc5dd5083cd9010e28d1bcf84061b1f86dffaf1406901cad99fa05a3",
        );
    }
}
