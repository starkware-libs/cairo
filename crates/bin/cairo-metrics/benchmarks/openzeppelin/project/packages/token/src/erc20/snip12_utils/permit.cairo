// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (token/src/erc20/snip12_utils/permit.cairo)

use core::hash::{HashStateExTrait, HashStateTrait};
use core::poseidon::PoseidonTrait;
use openzeppelin_utils::cryptography::snip12::StructHash;
use starknet::ContractAddress;

#[derive(Copy, Drop, Hash)]
pub struct Permit {
    pub token: ContractAddress,
    pub spender: ContractAddress,
    pub amount: u256,
    pub nonce: felt252,
    pub deadline: u64,
}

// Since there's no u64 type in SNIP-12, the type used for `deadline` parameter is u128
// selector!(
//   "\"Permit\"(
//     \"token\":\"ContractAddress\",
//     \"spender\":\"ContractAddress\",
//     \"amount\":\"u256\",
//     \"nonce\":\"felt\",
//     \"deadline\":\"u128\"
//   )\"u256\"(
//     \"low\":\"u128\",
//     \"high\":\"u128\"
//   )"
// );
pub const PERMIT_TYPE_HASH: felt252 =
    0x3210d4fa611411fccef9213585061fbd533f82d1ea2bd3c1b3b6b589448d5cf;

impl StructHashImpl of StructHash<Permit> {
    fn hash_struct(self: @Permit) -> felt252 {
        PoseidonTrait::new().update_with(PERMIT_TYPE_HASH).update_with(*self).finalize()
    }
}
