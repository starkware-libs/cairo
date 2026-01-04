// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (account/src/extensions/src9/snip12_utils.cairo)

use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::{PoseidonTrait, poseidon_hash_span};
use openzeppelin_utils::cryptography::snip12::StructHash;
use starknet::account::Call;
use crate::extensions::src9::OutsideExecution;

// sn_keccak(
//   "\"OutsideExecution\"(\"Caller\":\"ContractAddress\",\"Nonce\":\"felt\",
//   \"Execute After\":\"u128\",\"Execute Before\":\"u128\",\"Calls\":\"Call*\")
//   \"Call\"(\"To\":\"ContractAddress\",
//   \"Selector\":\"selector\",\"Calldata\":\"felt*\")"
// )
pub const OUTSIDE_EXECUTION_TYPE_HASH: felt252 =
    0x312b56c05a7965066ddbda31c016d8d05afc305071c0ca3cdc2192c3c2f1f0f;

// sn_keccak("\"Call\"(\"To\":\"ContractAddress\",\"Selector\":\"selector\",\"Calldata\":\"felt*\")");
pub const CALL_TYPE_HASH: felt252 =
    0x3635c7f2a7ba93844c0d064e18e487f35ab90f7c39d00f186a781fc3f0c2ca9;

pub impl CallStructHash of StructHash<Call> {
    fn hash_struct(self: @Call) -> felt252 {
        let hash_state = PoseidonTrait::new();
        hash_state
            .update_with(CALL_TYPE_HASH)
            .update_with(*self.to)
            .update_with(*self.selector)
            .update_with(poseidon_hash_span(*self.calldata))
            .finalize()
    }
}

pub impl OutsideExecutionStructHash of StructHash<OutsideExecution> {
    fn hash_struct(self: @OutsideExecution) -> felt252 {
        let mut calls_span = *self.calls;
        let mut hashed_calls = array![];

        for call in calls_span {
            hashed_calls.append(call.hash_struct());
        }

        let hash_state = PoseidonTrait::new();
        hash_state
            .update_with(OUTSIDE_EXECUTION_TYPE_HASH)
            .update_with(*self.caller)
            .update_with(*self.nonce)
            .update_with(*self.execute_after)
            .update_with(*self.execute_before)
            .update_with(poseidon_hash_span(hashed_calls.span()))
            .finalize()
    }
}
