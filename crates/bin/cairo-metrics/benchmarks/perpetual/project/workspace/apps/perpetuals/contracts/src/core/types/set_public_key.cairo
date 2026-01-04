use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin::utils::snip12::StructHash;
use perpetuals::core::types::position::PositionId;
use starkware_utils::signature::stark::{HashType, PublicKey};
use starkware_utils::time::time::Timestamp;

#[derive(Copy, Drop, Hash, Serde)]
pub struct SetPublicKeyArgs {
    pub position_id: PositionId,
    pub old_public_key: PublicKey,
    pub new_public_key: PublicKey,
    pub expiration: Timestamp,
}


/// selector!(
///   "\"SetPublicKeyArgs\"(
///    \"position_id\":\"PositionId\",
///    \"old_public_key\":\"felt\",
///    \"new_public_key\":\"felt\",
///    \"expiration\":\"Timestamp\"
///    )
///    \"PositionId\"(
///    \"value\":\"u32\"
///    )"
///    \"Timestamp\"(
///    \"seconds\":\"u64\"
///    )
/// );
const SET_PUBLIC_KEY_ARGS_HASH: HashType =
    0x95737230c7eeb47c10a450cdb69cfe565a1f0da2bc7402a701cda82be14e36;

impl StructHashImpl of StructHash<SetPublicKeyArgs> {
    fn hash_struct(self: @SetPublicKeyArgs) -> HashType {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(SET_PUBLIC_KEY_ARGS_HASH).update_with(*self).finalize()
    }
}

#[cfg(test)]
mod tests {
    use starkware_utils::math::utils::to_base_16_string;
    use super::SET_PUBLIC_KEY_ARGS_HASH;

    #[test]
    fn test_update_position_public_key_type_hash() {
        let expected = selector!(
            "\"SetPublicKeyArgs\"(\"position_id\":\"PositionId\",\"old_public_key\":\"felt\",\"new_public_key\":\"felt\",\"expiration\":\"Timestamp\")\"PositionId\"(\"value\":\"u32\")\"Timestamp\"(\"seconds\":\"u64\")",
        );
        assert!(to_base_16_string(SET_PUBLIC_KEY_ARGS_HASH) == to_base_16_string(expected));
    }
}
