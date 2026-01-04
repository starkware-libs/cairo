use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin::utils::snip12::StructHash;
use perpetuals::core::types::asset::AssetId;
use perpetuals::core::types::position::PositionId;
use starkware_utils::signature::stark::HashType;
use starkware_utils::time::time::Timestamp;

#[derive(Copy, Drop, Hash, Serde)]
pub struct TransferArgs {
    pub recipient: PositionId,
    pub position_id: PositionId,
    pub collateral_id: AssetId,
    pub amount: u64,
    pub expiration: Timestamp,
    pub salt: felt252,
}


/// selector!(
///   "\"TransferArgs\"(
///    \"recipient\":\"PositionId\",
///    \"position_id\":\"PositionId\",
///    \"collateral_id\":\"AssetId\"
///    \"amount\":\"u64\"
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
const TRANSFER_ARGS_TYPE_HASH: HashType =
    0x1db88e2709fdf2c59e651d141c3296a42b209ce770871b40413ea109846a3b4;

impl StructHashImpl of StructHash<TransferArgs> {
    fn hash_struct(self: @TransferArgs) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(TRANSFER_ARGS_TYPE_HASH).update_with(*self).finalize()
    }
}

#[cfg(test)]
mod tests {
    use starkware_utils::math::utils::to_base_16_string;
    use super::TRANSFER_ARGS_TYPE_HASH;

    #[test]
    fn test_transfer_type_hash() {
        let expected = selector!(
            "\"TransferArgs\"(\"recipient\":\"PositionId\",\"position_id\":\"PositionId\",\"collateral_id\":\"AssetId\",\"amount\":\"u64\",\"expiration\":\"Timestamp\",\"salt\":\"felt\")\"PositionId\"(\"value\":\"u32\")\"AssetId\"(\"value\":\"felt\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert!(to_base_16_string(TRANSFER_ARGS_TYPE_HASH) == to_base_16_string(expected));
    }
}

