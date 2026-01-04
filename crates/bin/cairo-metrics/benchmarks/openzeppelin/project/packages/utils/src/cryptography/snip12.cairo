// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/cryptography/snip12.cairo)

use core::hash::{Hash, HashStateExTrait, HashStateTrait};
use core::poseidon::{HashState, PoseidonTrait};
use starknet::{ContractAddress, get_tx_info};

// selector!(
//   "\"StarknetDomain\"(
//    \"name\":\"shortstring\",
//    \"version\":\"shortstring\",
//    \"chainId\":\"shortstring\",
//    \"revision\":\"shortstring\"
//   )"
// );
pub const STARKNET_DOMAIN_TYPE_HASH: felt252 =
    0x1ff2f602e42168014d405a94f75e8a93d640751d71d16311266e140d8b0a210;

/// Generic Starknet domain separator representation as defined in SNIP-12.
#[derive(Drop, Copy, Hash)]
pub struct StarknetDomain {
    pub name: felt252,
    pub version: felt252,
    pub chain_id: felt252,
    pub revision: felt252,
}

/// Trait for calculating the hash of a struct.
pub trait StructHash<T> {
    fn hash_struct(self: @T) -> felt252;
}

/// Trait for calculating the hash of a message given the passed `signer`.
pub trait OffchainMessageHash<T> {
    fn get_message_hash(self: @T, signer: ContractAddress) -> felt252;
}

/// Implementation of `StructHash` that calculates the Poseidon hash of type `StarknetDomain`.
pub impl StructHashStarknetDomainImpl of StructHash<StarknetDomain> {
    fn hash_struct(self: @StarknetDomain) -> felt252 {
        let hash_state = PoseidonTrait::new();
        hash_state.update_with(STARKNET_DOMAIN_TYPE_HASH).update_with(*self).finalize()
    }
}

pub trait SNIP12Metadata {
    /// Returns the name of the dapp.
    fn name() -> felt252;

    /// Returns the version of the dapp.
    fn version() -> felt252;
}

/// Implementation of OffchainMessageHash that calculates the Poseidon hash of the message.
///
/// The hash state hashes the following in order:
///
/// - 'StarkNet Message' short string.
/// - Starknet domain struct hash.
/// - `signer` of the message.
/// - Hashed struct of the message.
pub(crate) impl OffchainMessageHashImpl<
    T, +StructHash<T>, impl metadata: SNIP12Metadata,
> of OffchainMessageHash<T> {
    fn get_message_hash(self: @T, signer: ContractAddress) -> felt252 {
        let domain = StarknetDomain {
            name: metadata::name(),
            version: metadata::version(),
            chain_id: get_tx_info().unbox().chain_id,
            revision: 1,
        };
        let mut state = PoseidonTrait::new();
        state = state.update_with('StarkNet Message');
        state = state.update_with(domain.hash_struct());
        state = state.update_with(signer);
        state = state.update_with(self.hash_struct());
        state.finalize()
    }
}

/// Hash trait implementation for a span of elements according to SNIP-12.
pub impl SNIP12HashSpanImpl<T, +Copy<T>, +Hash<T, HashState>> of Hash<Span<T>, HashState> {
    fn update_state(mut state: HashState, value: Span<T>) -> HashState {
        let mut inner_state = PoseidonTrait::new();
        for elem in value {
            inner_state = inner_state.update_with(*elem);
        }
        state.update_with(inner_state.finalize())
    }
}
