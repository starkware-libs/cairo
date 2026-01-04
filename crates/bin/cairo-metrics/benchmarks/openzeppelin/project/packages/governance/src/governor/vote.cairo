// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/governor/vote.cairo)

use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin_utils::cryptography::snip12::{SNIP12HashSpanImpl, StructHash};
use starknet::ContractAddress;

// sn_keccak(
//      "\"Vote\"(\"verifying_contract\":\"ContractAddress\",
//      \"nonce\":\"felt\",
//      \"proposal_id\":\"felt\",
//      \"support\":\"u128\",
//      \"voter\":\"ContractAddress\")"
// )
//
// Since there's no u8 type in SNIP-12, we use u128 for `support` in the type hash generation.
pub const VOTE_TYPE_HASH: felt252 =
    0x19a625949c5c367200d9ca91e845fe9c5f3c7f04735d97d91f3a6cb4cb30b81;

#[derive(Copy, Drop, Hash)]
pub struct Vote {
    pub verifying_contract: ContractAddress,
    pub nonce: felt252,
    pub proposal_id: felt252,
    pub support: u8,
    pub voter: ContractAddress,
}

impl VoteStructHashImpl of StructHash<Vote> {
    fn hash_struct(self: @Vote) -> felt252 {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(VOTE_TYPE_HASH).update_with(*self).finalize()
    }
}

// sn_keccak(
//      "\"VoteWithReasonAndParams\"(\"verifying_contract\":\"ContractAddress\",
//      \"nonce\":\"felt\",
//      \"proposal_id\":\"felt\",
//      \"support\":\"u128\",
//      \"voter\":\"ContractAddress\",
//      \"reason_hash\":\"felt\",
//      \"params\":\"felt*\")"
// )
//
// Since there's no u8 type in SNIP-12, we use u128 for `support` in the type hash generation.
pub const VOTE_WITH_REASON_AND_PARAMS_TYPE_HASH: felt252 =
    0x1f4ccab7220d6a3c0c1cbc1008bfcb3f6fbdb361dd14fd017fabd229e0cf94b;

#[derive(Copy, Drop)]
pub struct VoteWithReasonAndParams {
    pub verifying_contract: ContractAddress,
    pub nonce: felt252,
    pub proposal_id: felt252,
    pub support: u8,
    pub voter: ContractAddress,
    pub reason_hash: felt252,
    pub params: Span<felt252>,
}

impl VoteWithReasonAndParamsStructHashImpl of StructHash<VoteWithReasonAndParams> {
    fn hash_struct(self: @VoteWithReasonAndParams) -> felt252 {
        let hash_state = PoseidonTrait::new();
        hash_state
            .update_with(VOTE_WITH_REASON_AND_PARAMS_TYPE_HASH)
            .update_with(*self.verifying_contract)
            .update_with(*self.nonce)
            .update_with(*self.proposal_id)
            .update_with(*self.support)
            .update_with(*self.voter)
            .update_with(*self.reason_hash)
            .update_with(*self.params)
            .finalize()
    }
}

#[cfg(test)]
mod tests {
    use super::{VOTE_TYPE_HASH, VOTE_WITH_REASON_AND_PARAMS_TYPE_HASH};

    #[test]
    fn test_vote_type_hash() {
        let expected = selector!(
            "\"Vote\"(\"verifying_contract\":\"ContractAddress\",\"nonce\":\"felt\",\"proposal_id\":\"felt\",\"support\":\"u128\",\"voter\":\"ContractAddress\")",
        );
        assert_eq!(VOTE_TYPE_HASH, expected);
    }

    #[test]
    fn test_vote_with_reason_and_params_type_hash() {
        let expected = selector!(
            "\"VoteWithReasonAndParams\"(\"verifying_contract\":\"ContractAddress\",\"nonce\":\"felt\",\"proposal_id\":\"felt\",\"support\":\"u128\",\"voter\":\"ContractAddress\",\"reason_hash\":\"felt\",\"params\":\"felt*\")",
        );
        assert_eq!(VOTE_WITH_REASON_AND_PARAMS_TYPE_HASH, expected);
    }
}
