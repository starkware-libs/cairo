use core::hash::HashStateTrait;
use core::pedersen::PedersenTrait;
use core::poseidon::poseidon_hash_span;
use crate::hashes::{PedersenCHasher, PoseidonCHasher};

#[test]
fn test_pedersen_commutative_hash_is_commutative() {
    let a = 'a';
    let b = 'b';
    let hash = PedersenCHasher::commutative_hash(a, b);
    assert_eq!(hash, PedersenCHasher::commutative_hash(b, a));
}

#[test]
fn test_pedersen_commutative_hash_smaller_first() {
    let a = 'a';
    let b = 'b';

    let hash_state = PedersenTrait::new(0);
    let expected = hash_state.update(a).update(b).update(2).finalize();

    let hash = PedersenCHasher::commutative_hash(b, a);
    assert_eq!(hash, expected);
}

#[test]
fn test_poseidon_commutative_hash_is_commutative() {
    let a = 'a';
    let b = 'b';
    let hash = PoseidonCHasher::commutative_hash(a, b);
    assert_eq!(hash, PoseidonCHasher::commutative_hash(b, a));
}


#[test]
fn test_poseidon_commutative_hash_smaller_first() {
    let a = 'a';
    let b = 'b';

    let hash = PoseidonCHasher::commutative_hash(b, a);
    assert_eq!(hash, poseidon_hash_span([a, b].span()));
}
