use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin::utils::snip12::StructHash;
use perpetuals::core::types::position::PositionId;
use starknet::ContractAddress;
use starkware_utils::signature::stark::{HashType, PublicKey};
use starkware_utils::time::time::Timestamp;

#[derive(Copy, Drop, Hash, Serde)]
pub struct SetOwnerAccountArgs {
    pub position_id: PositionId,
    pub public_key: PublicKey,
    pub new_owner_account: ContractAddress,
    pub expiration: Timestamp,
}


/// selector!(
///   "\"SetOwnerAccountArgs\"(
///    \"position_id\":\"PositionId\",
///    \"public_key\":\"felt\",
///    \"new_owner_account\":\"ContractAddress\",
///    \"expiration\":\"Timestamp\"
///    )
///    \"PositionId\"(
///    \"value\":\"u32\"
///    )"
///    \"Timestamp\"(
///    \"seconds\":\"u64\"
///    )
/// );
const SET_OWNER_ACCOUNT_ARGS_HASH: HashType =
    0x02c897e00cdbfcfefe21b980feb2bf084673bba0020c809eeecd810c2cf97cfd;

impl StructHashImpl of StructHash<SetOwnerAccountArgs> {
    fn hash_struct(self: @SetOwnerAccountArgs) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(SET_OWNER_ACCOUNT_ARGS_HASH).update_with(*self).finalize()
    }
}

#[cfg(test)]
mod tests {
    use starkware_utils::math::utils::to_base_16_string;
    use super::SET_OWNER_ACCOUNT_ARGS_HASH;

    #[test]
    fn test_set_owner_account_type_hash() {
        let expected = selector!(
            "\"SetOwnerAccountArgs\"(\"position_id\":\"PositionId\",\"public_key\":\"felt\",\"new_owner_account\":\"ContractAddress\",\"expiration\":\"Timestamp\")\"PositionId\"(\"value\":\"u32\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert!(to_base_16_string(SET_OWNER_ACCOUNT_ARGS_HASH) == to_base_16_string(expected));
    }
}
