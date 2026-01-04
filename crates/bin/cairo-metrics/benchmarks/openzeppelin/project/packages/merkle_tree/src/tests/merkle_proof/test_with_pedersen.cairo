use core::hash::{HashStateExTrait, HashStateTrait};
use core::pedersen::{PedersenTrait, pedersen};
use crate::hashes::PedersenCHasher;
use crate::merkle_proof::{
    process_multi_proof, process_proof, verify, verify_multi_proof, verify_pedersen,
};
use super::common::{LEAVES, Leaf};

// `ROOT`, `PROOF`, and `MULTI_PROOF` were computed using @ericnordelo/strk-merkle-tree
const ROOT: felt252 = 0x02a40717603180fa52f40a55508cd360d301840f3e502665cf0132ef412911de;
const PROOF: [felt252; 2] = [
    0x044fdc540a81d0189ed30b49d64136f9e8bd499c942ba170404ef0b9406e524c,
    0x02b0ee474cf2ab27501e54a661d17ac1dc162571c111fe2455d09fe23471099e,
];
const MULTI_PROOF: [felt252; 2] = [
    0x044fdc540a81d0189ed30b49d64136f9e8bd499c942ba170404ef0b9406e524c,
    0x05fb6a626bb2c1e12fc2d6fa7f218ec06928ba5febf4d5677c2c5060827e383b,
];

//
// verify
//

#[test]
fn test_valid_merkle_proof() {
    let leaves = LEAVES();
    let hash = leaf_hash(*leaves.at(0));
    let proof = PROOF.span();

    assert_eq!(process_proof::<PedersenCHasher>(proof, hash), ROOT);
    assert!(verify::<PedersenCHasher>(proof, ROOT, hash));
    assert!(verify_pedersen(proof, ROOT, hash));

    // For demonstration, it is also possible to create valid
    // proofs for certain values *NOT* in elements:
    let no_such_leaf = PedersenCHasher::commutative_hash(hash, *proof.at(0));
    let second_proof = [0x02b0ee474cf2ab27501e54a661d17ac1dc162571c111fe2455d09fe23471099e].span();

    assert_eq!(process_proof::<PedersenCHasher>(second_proof, no_such_leaf), ROOT);
    assert!(verify::<PedersenCHasher>(second_proof, ROOT, no_such_leaf));
    assert!(verify_pedersen(second_proof, ROOT, no_such_leaf));
}

#[test]
fn test_invalid_merkle_proof() {
    let leaves = LEAVES();
    let hash = leaf_hash(*leaves.at(0));
    let invalid_proof = [
        0x044fdc540a81d0189ed30b49d64136f9e8bd499c942ba170404ef0b9406e524c, 'invalid',
    ]
        .span();

    assert!(process_proof::<PedersenCHasher>(invalid_proof, hash) != ROOT);
    assert!(!verify::<PedersenCHasher>(invalid_proof, ROOT, hash));
    assert!(!verify_pedersen(invalid_proof, ROOT, hash));
}

//
// verify_multi_proof
//

#[test]
fn test_valid_merkle_multi_proof() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();
    let proof_flags = [false, false, true].span();

    assert_eq!(process_multi_proof::<PedersenCHasher>(proof, proof_flags, leaves_to_prove), ROOT);
    assert!(verify_multi_proof::<PedersenCHasher>(proof, proof_flags, ROOT, leaves_to_prove));
}

#[test]
fn test_invalid_merkle_multi_proof() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let invalid_proof = [
        0x044fdc540a81d0189ed30b49d64136f9e8bd499c942ba170404ef0b9406e524c, 'invalid',
    ]
        .span();
    let proof_flags = [false, false, true].span();

    assert!(
        process_multi_proof::<PedersenCHasher>(invalid_proof, proof_flags, leaves_to_prove) != ROOT,
    );
    assert!(
        !verify_multi_proof::<PedersenCHasher>(invalid_proof, proof_flags, ROOT, leaves_to_prove),
    );
}

#[test]
fn test_invalid_merkle_multi_proof_flags() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();
    let proof_flags = [false, true, false].span();

    assert!(process_multi_proof::<PedersenCHasher>(proof, proof_flags, leaves_to_prove) != ROOT);
    assert!(!verify_multi_proof::<PedersenCHasher>(proof, proof_flags, ROOT, leaves_to_prove));
}

#[test]
#[should_panic(expected: "MerkleProof: invalid multi proof")]
fn test_process_multi_proof_invalid_len_proof_flags_panics() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();

    // `proof_flags.len()` is not equal to `proof.len() + leaves_to_prove.len() + 1`
    let proof_flags = [true, false].span();

    process_multi_proof::<PedersenCHasher>(proof, proof_flags, leaves_to_prove);
}

#[test]
#[should_panic(expected: "MerkleProof: invalid multi proof")]
fn test_verify_multi_proof_invalid_len_proof_flags_panics() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();

    // `proof_flags.len()` is not equal to `proof.len() + leaves_to_prove.len() + 1`
    let proof_flags = [true, false].span();

    verify_multi_proof::<PedersenCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
}

#[test]
#[should_panic(expected: 'Index out of bounds')]
fn test_process_multi_proof_flags_extra_leaves_expected() {
    let leaves = LEAVES();
    let leaves_to_prove = [
        leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1)), leaf_hash(*leaves.at(2)),
    ]
        .span();
    let proof = MULTI_PROOF.span();

    // For each true one leaf is expected
    let proof_flags = [true, true, true, true].span();

    process_multi_proof::<PedersenCHasher>(proof, proof_flags, leaves_to_prove);
}

#[test]
#[should_panic(expected: 'Index out of bounds')]
fn test_process_multi_proof_flags_extra_proofs_expected() {
    let leaves = LEAVES();
    let leaves_to_prove = [
        leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1)), leaf_hash(*leaves.at(2)),
    ]
        .span();
    let proof = MULTI_PROOF.span();

    // For each false one proof is expected
    let proof_flags = [true, false, false, false].span();

    process_multi_proof::<PedersenCHasher>(proof, proof_flags, leaves_to_prove);
}

#[test]
#[should_panic(expected: 'Index out of bounds')]
fn test_verify_multi_proof_flags_extra_leaves_expected() {
    let leaves = LEAVES();
    let leaves_to_prove = [
        leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1)), leaf_hash(*leaves.at(2)),
    ]
        .span();
    let proof = MULTI_PROOF.span();

    // For each true one leaf is expected
    let proof_flags = [true, true, true, true].span();

    verify_multi_proof::<PedersenCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
}

#[test]
#[should_panic(expected: 'Index out of bounds')]
fn test_verify_multi_proof_flags_extra_proofs_expected() {
    let leaves = LEAVES();
    let leaves_to_prove = [
        leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1)), leaf_hash(*leaves.at(2)),
    ]
        .span();
    let proof = MULTI_PROOF.span();

    // For each false one proof is expected
    let proof_flags = [true, false, false, false].span();

    verify_multi_proof::<PedersenCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
}

//
// Helpers
//

fn leaf_hash(leaf: Leaf) -> felt252 {
    let hash_state = PedersenTrait::new(0);
    pedersen(0, hash_state.update_with(leaf).update(2).finalize())
}
