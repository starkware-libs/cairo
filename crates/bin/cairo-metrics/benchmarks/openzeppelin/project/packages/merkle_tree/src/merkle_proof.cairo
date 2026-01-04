// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (merkle_tree/src/merkle_proof.cairo)

/// These functions deal with verification of Merkle Tree proofs.
///
/// WARNING: You should avoid using leaf values that are two felt252 long prior to
/// hashing, or use a different hash function for hashing leaves and pre-images.
/// This is because the concatenation of a sorted pair of internal nodes in
/// the Merkle tree could be reinterpreted as a leaf value.
///
/// NOTE: This library supports proof verification for merkle trees built using
/// custom _commutative_ hashing functions (i.e. `H(a, b) == H(b, a)`). Proving
/// leaf inclusion in trees built using non-commutative hashing functions requires
/// additional logic that is not supported by this library.

use crate::hashes::{CommutativeHasher, PedersenCHasher, PoseidonCHasher};

/// Returns true if a `leaf` can be proved to be a part of a Merkle tree
/// defined by `root`. For this, a `proof` must be provided, containing
/// sibling hashes on the branch from the leaf to the root of the tree. Each
/// pair of leaves and each pair of pre-images are assumed to be sorted.
pub fn verify<impl Hasher: CommutativeHasher>(
    proof: Span<felt252>, root: felt252, leaf: felt252,
) -> bool {
    process_proof::<Hasher>(proof, leaf) == root
}

/// Version of `verify` using Pedersen as the hashing function.
pub fn verify_pedersen(proof: Span<felt252>, root: felt252, leaf: felt252) -> bool {
    verify::<PedersenCHasher>(proof, root, leaf)
}

/// Version of `verify` using Poseidon as the hashing function.
pub fn verify_poseidon(proof: Span<felt252>, root: felt252, leaf: felt252) -> bool {
    verify::<PoseidonCHasher>(proof, root, leaf)
}

/// Returns the rebuilt hash obtained by traversing a Merkle tree up
/// from `leaf` using `proof`. A `proof` is valid if and only if the rebuilt
/// hash matches the root of the tree. When processing the proof, the pairs
/// of leaves & pre-images are assumed to be sorted.
pub fn process_proof<impl Hasher: CommutativeHasher>(
    proof: Span<felt252>, leaf: felt252,
) -> felt252 {
    let mut computed_hash = leaf;
    for hash in proof {
        computed_hash = Hasher::commutative_hash(computed_hash, *hash);
    }
    computed_hash
}

/// Returns true if the `leaves` can be simultaneously proven to be a part of a Merkle tree defined
/// by `root`, according to `proof` and `proof_flags` as described in `process_multi_proof`.
///
/// CAUTION: Not all Merkle trees admit multiproofs. See `process_multi_proof` for details.
///
/// NOTE: Consider the case where `root == proof.at(0) && leaves.len() == 0`
/// as it will return `true`.
///
/// The `leaves` must be validated independently. See `process_multi_proof`.
pub fn verify_multi_proof<impl Hasher: CommutativeHasher>(
    proof: Span<felt252>, proof_flags: Span<bool>, root: felt252, leaves: Span<felt252>,
) -> bool {
    process_multi_proof::<Hasher>(proof, proof_flags, leaves) == root
}

/// Returns the root of a tree reconstructed from `leaves` and sibling nodes in `proof`. The
/// reconstruction proceeds by incrementally reconstructing all inner nodes by combining a
/// leaf/inner node with either another leaf/inner node or a proof sibling node, depending on
/// whether each `proof_flags` item is true or false respectively.
///
/// CAUTION: Not all Merkle trees admit multiproofs. To use multiproofs, it is sufficient to ensure
/// that: 1) the tree is complete (but not necessarily perfect), 2) the leaves to be proven are in
/// the opposite order than they are in the tree (i.e., as seen from right to left starting at the
/// deepest layer and continuing at the next layer).
///
/// NOTE: The _empty set_ (i.e. the case where `proof.len() == 1 && leaves.len() == 0`) is
/// considered a no-op, and therefore a valid multiproof (i.e. it returns `proof.at(0)`). Consider
/// disallowing this case if you're not validating the leaves elsewhere.
pub fn process_multi_proof<impl Hasher: CommutativeHasher>(
    proof: Span<felt252>, proof_flags: Span<bool>, leaves: Span<felt252>,
) -> felt252 {
    // This function rebuilds the root hash by traversing the tree up from the leaves. The root is
    // rebuilt by consuming and producing values on a queue. The queue starts with the `leaves`
    // span, then goes onto the `hashes` span. At the end of the process, the last hash in the
    // `hashes` span should contain the root of the Merkle tree.
    let leaves_len = leaves.len();
    let proof_flags_len = proof_flags.len();

    // Check proof validity.
    if (leaves_len + proof.len() != proof_flags_len + 1) {
        #[allow(panic)]
        panic!("MerkleProof: invalid multi proof");
    }

    // The x_pos values are "pointers" to the next value to consume in each array.
    // By incrementing the value, we simulate a queue's pop operation.
    let mut hashes = array![];
    let mut leaf_pos = 0;
    let mut hash_pos = 0;
    let mut proof_pos = 0;

    // At each step, we compute the next hash using two values:
    // 1. A value from the "main queue". If not all leaves have been consumed, we get the next leaf,
    // otherwise we get the next hash.
    // 2. Depending on the flag, either another value from the "main queue" (merging branches) or an
    // element from the `proof` array.
    for i in 0..proof_flags_len {
        let a = if leaf_pos < leaves_len {
            leaf_pos += 1;
            leaves.at(leaf_pos - 1)
        } else {
            hash_pos += 1;
            hashes.at(hash_pos - 1)
        };

        let b = if *proof_flags.at(i) {
            if leaf_pos < leaves_len {
                leaf_pos += 1;
                leaves.at(leaf_pos - 1)
            } else {
                hash_pos += 1;
                hashes.at(hash_pos - 1)
            }
        } else {
            proof_pos += 1;
            proof.at(proof_pos - 1)
        };

        hashes.append(Hasher::commutative_hash(*a, *b));
    }

    let root = if proof_flags_len > 0 {
        hashes.at(proof_flags_len - 1)
    } else if leaves_len > 0 {
        // If `proof_flags_len` is zero, and `leaves_len` is greater than zero,
        // then `leaves_len` can only be 1, because of the proof validity check.
        leaves.at(0)
    } else {
        proof.at(0)
    };

    *root
}
