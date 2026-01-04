// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (governance/src/votes/delegation.cairo)

use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin_utils::cryptography::snip12::StructHash;
use starknet::ContractAddress;

// sn_keccak(
//  "\"Delegation\"(\"verifying_contract\":\"ContractAddress\",
//  \"delegatee\":\"ContractAddress\",\"nonce\":\"felt\",\"expiry\":\"u128\")"
// )
//
// Since there's no u64 type in SNIP-12, we use u128 for `expiry` in the type hash generation.
pub const DELEGATION_TYPE_HASH: felt252 =
    0x29444c6b338056aafe1802658864510c2651f5cd23897cb7b7edafe5b126d4f;

#[derive(Copy, Drop, Hash)]
pub struct Delegation {
    pub verifying_contract: ContractAddress,
    pub delegatee: ContractAddress,
    pub nonce: felt252,
    pub expiry: u64,
}

impl StructHashImpl of StructHash<Delegation> {
    fn hash_struct(self: @Delegation) -> felt252 {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(DELEGATION_TYPE_HASH).update_with(*self).finalize()
    }
}
