// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (merkle_tree/src/hashes.cairo)

use core::hash::HashStateTrait;
use core::pedersen::PedersenTrait;
use core::poseidon::PoseidonTrait;
use core::traits::PartialOrd;

/// Computes a commutative hash of a sorted pair of felt252 values.
///
/// This is usually implemented as an extension of a non-commutative hash function, like
/// Pedersen or Poseidon, returning the hash of the concatenation of the two values by first
/// sorting them.
///
/// Frequently used when working with merkle proofs.
pub trait CommutativeHasher {
    fn commutative_hash(a: felt252, b: felt252) -> felt252;
}

/// Computes the Pedersen commutative hash of a sorted pair of felt252 values.
pub impl PedersenCHasher of CommutativeHasher {
    /// Computes the Pedersen hash by chaining the two values
    /// with the length, sorting the pair first.
    fn commutative_hash(a: felt252, b: felt252) -> felt252 {
        let hash_state = PedersenTrait::new(0);
        if a < b {
            hash_state.update(a).update(b).update(2).finalize()
        } else {
            hash_state.update(b).update(a).update(2).finalize()
        }
    }
}

/// Computes the Poseidon commutative hash of a sorted pair of felt252 values.
pub impl PoseidonCHasher of CommutativeHasher {
    /// Computes the Poseidon hash of the concatenation of two values, sorting the pair first.
    fn commutative_hash(a: felt252, b: felt252) -> felt252 {
        let hash_state = PoseidonTrait::new();
        if a < b {
            hash_state.update(a).update(b).finalize()
        } else {
            hash_state.update(b).update(a).finalize()
        }
    }
}

/// PartialOrd implementation for felt252 comparisons.
/// This is used in the CommutativeHasher impls.
impl Felt252AsIntPartialOrd of PartialOrd<felt252> {
    #[inline(always)]
    fn lt(lhs: felt252, rhs: felt252) -> bool {
        let lhs: u256 = lhs.into();
        lhs < rhs.into()
    }
}
