use core::poseidon::poseidon_hash_span;
use crate::hashes::PoseidonCHasher;
use crate::merkle_proof::{
    process_multi_proof, process_proof, verify, verify_multi_proof, verify_poseidon,
};
use super::common::{LEAVES, Leaf};

// `ROOT`, `PROOF`, and `MULTI_PROOF` were computed using @ericnordelo/strk-merkle-tree
const ROOT: felt252 = 0x013f43fdca44b32f5334414b385b46aa1016d0172a1f066eab4cc93636426fcc;
const PROOF: [felt252; 2] = [
    0x05b151ebb9201ce27c56a70f5d0571ccfb9d9d62f12b8ccab7801ba87ec21a2f,
    0x2b7d689bd2ff488fd06dfb8eb22f5cdaba1e5d9698d3fabff2f1801852dbb2,
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

    assert_eq!(process_proof::<PoseidonCHasher>(proof, hash), ROOT);
    assert!(verify::<PoseidonCHasher>(proof, ROOT, hash));
    assert!(verify_poseidon(proof, ROOT, hash));

    // For demonstration, it is also possible to create valid
    // proofs for certain values *NOT* in elements:
    let no_such_leaf = PoseidonCHasher::commutative_hash(hash, *proof.at(0));
    let second_proof = [0x2b7d689bd2ff488fd06dfb8eb22f5cdaba1e5d9698d3fabff2f1801852dbb2].span();

    assert_eq!(process_proof::<PoseidonCHasher>(second_proof, no_such_leaf), ROOT);
    assert!(verify::<PoseidonCHasher>(second_proof, ROOT, no_such_leaf));
    assert!(verify_poseidon(second_proof, ROOT, no_such_leaf));
}

#[test]
fn test_invalid_merkle_proof() {
    let leaves = LEAVES();
    let hash = leaf_hash(*leaves.at(0));
    let invalid_proof = [
        0x044fdc540a81d0189ed30b49d64136f9e8bd499c942ba170404ef0b9406e524c, 'invalid',
    ]
        .span();

    assert!(process_proof::<PoseidonCHasher>(invalid_proof, hash) != ROOT);
    assert!(!verify::<PoseidonCHasher>(invalid_proof, ROOT, hash));
    assert!(!verify_poseidon(invalid_proof, ROOT, hash));
}

//
// verify_multi_proof
//

#[test]
fn test_valid_merkle_multi_proof() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = [0x2b7d689bd2ff488fd06dfb8eb22f5cdaba1e5d9698d3fabff2f1801852dbb2].span();
    let proof_flags = [true, false].span();

    assert_eq!(process_multi_proof::<PoseidonCHasher>(proof, proof_flags, leaves_to_prove), ROOT);
    assert!(verify_multi_proof::<PoseidonCHasher>(proof, proof_flags, ROOT, leaves_to_prove));
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
        process_multi_proof::<PoseidonCHasher>(invalid_proof, proof_flags, leaves_to_prove) != ROOT,
    );
    assert!(
        !verify_multi_proof::<PoseidonCHasher>(invalid_proof, proof_flags, ROOT, leaves_to_prove),
    );
}

#[test]
fn test_invalid_merkle_multi_proof_flags() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();
    let proof_flags = [false, true, false].span();

    assert!(process_multi_proof::<PoseidonCHasher>(proof, proof_flags, leaves_to_prove) != ROOT);
    assert!(!verify_multi_proof::<PoseidonCHasher>(proof, proof_flags, ROOT, leaves_to_prove));
}

#[test]
#[should_panic(expected: "MerkleProof: invalid multi proof")]
fn test_process_multi_proof_invalid_len_proof_flags_panics() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();

    // `proof_flags.len()` is not equal to `proof.len() + leaves_to_prove.len() + 1`
    let proof_flags = [true, false].span();

    process_multi_proof::<PoseidonCHasher>(proof, proof_flags, leaves_to_prove);
}

#[test]
#[should_panic(expected: "MerkleProof: invalid multi proof")]
fn test_verify_multi_proof_invalid_len_proof_flags_panics() {
    let leaves = LEAVES();
    let leaves_to_prove = [leaf_hash(*leaves.at(0)), leaf_hash(*leaves.at(1))].span();
    let proof = MULTI_PROOF.span();

    // `proof_flags.len()` is not equal to `proof.len() + leaves_to_prove.len() + 1`
    let proof_flags = [true, false].span();

    verify_multi_proof::<PoseidonCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
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

    process_multi_proof::<PoseidonCHasher>(proof, proof_flags, leaves_to_prove);
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

    process_multi_proof::<PoseidonCHasher>(proof, proof_flags, leaves_to_prove);
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

    verify_multi_proof::<PoseidonCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
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

    verify_multi_proof::<PoseidonCHasher>(proof, proof_flags, ROOT, leaves_to_prove);
}

//
// Helpers
//

fn leaf_hash(leaf: Leaf) -> felt252 {
    poseidon_hash_span(
        [poseidon_hash_span([leaf.address.into(), leaf.amount.into()].span())].span(),
    )
}
