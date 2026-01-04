use openzeppelin_test_common::account::SIGNED_TX_DATA as stark_signature_data;
use openzeppelin_test_common::eth_account::SIGNED_TX_DATA as eth_signature_data;
use openzeppelin_testing::constants::{TRANSACTION_HASH, secp256k1, secp256r1, stark};
use openzeppelin_testing::signing::{Secp256r1KeyPair, Secp256r1SerializedSigning};
use snforge_std::signature::secp256r1_curve::Secp256r1CurveSignerImpl;
use starknet::secp256_trait::Secp256Trait;
use starknet::secp256k1::Secp256k1Point;
use starknet::secp256r1::Secp256r1Point;
use crate::interface::P256PublicKey;
use crate::utils::signature::{
    Secp256Signature, is_valid_eth_signature, is_valid_p256_signature, is_valid_stark_signature,
};

//
// is_valid_stark_signature
//

#[test]
fn test_is_valid_stark_signature_good_sig() {
    let key_pair = stark::KEY_PAIR();
    let data = stark_signature_data(key_pair);
    let good_signature = array![data.r, data.s].span();

    let is_valid = is_valid_stark_signature(data.tx_hash, key_pair.public_key, good_signature);
    assert!(is_valid);
}

#[test]
fn test_is_valid_stark_signature_bad_sig() {
    let key_pair = stark::KEY_PAIR();
    let data = stark_signature_data(key_pair);
    let bad_signature = array!['BAD', 'SIGNATURE'].span();

    let is_invalid = !is_valid_stark_signature(data.tx_hash, key_pair.public_key, bad_signature);
    assert!(is_invalid);
}

#[test]
fn test_is_valid_stark_signature_invalid_len_sig() {
    let key_pair = stark::KEY_PAIR();
    let data = stark_signature_data(key_pair);
    let bad_signature = array!['BAD_SIGNATURE'].span();

    let is_invalid = !is_valid_stark_signature(data.tx_hash, key_pair.public_key, bad_signature);
    assert!(is_invalid);
}

//
// is_valid_eth_signature
//

#[test]
fn test_is_valid_eth_signature_good_sig() {
    let key_pair = secp256k1::KEY_PAIR();
    let data = eth_signature_data(key_pair);

    let mut serialized_good_signature = array![];
    data.signature.serialize(ref serialized_good_signature);

    let is_valid = is_valid_eth_signature(
        data.tx_hash, key_pair.public_key, serialized_good_signature.span(),
    );
    assert!(is_valid);
}

#[test]
fn test_is_valid_eth_signature_bad_sig() {
    let key_pair = secp256k1::KEY_PAIR();
    let data = eth_signature_data(key_pair);
    let mut bad_signature = data.signature;

    bad_signature.r += 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_eth_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
fn test_is_valid_eth_signature_invalid_format_sig() {
    let key_pair = secp256k1::KEY_PAIR();
    let data = eth_signature_data(key_pair);
    let mut serialized_bad_signature = array![0x1];

    is_valid_eth_signature(data.tx_hash, key_pair.public_key, serialized_bad_signature.span());
}

#[test]
fn test_eth_signature_r_out_of_range() {
    let key_pair = secp256k1::KEY_PAIR();
    let data = eth_signature_data(key_pair);
    let mut bad_signature = data.signature;

    let curve_size = Secp256Trait::<Secp256k1Point>::get_curve_size();

    bad_signature.r = curve_size + 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_eth_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}

#[test]
fn test_eth_signature_s_out_of_range() {
    let key_pair = secp256k1::KEY_PAIR();
    let data = eth_signature_data(key_pair);
    let mut bad_signature = data.signature;

    let curve_size = Secp256Trait::<Secp256k1Point>::get_curve_size();

    bad_signature.s = curve_size + 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_eth_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}

//
// is_valid_p256_signature
//

#[derive(Drop)]
pub struct SignedTransactionData {
    pub private_key: u256,
    pub public_key: P256PublicKey,
    pub tx_hash: felt252,
    pub signature: Secp256Signature,
}

fn p256_signature_data(key_pair: Secp256r1KeyPair) -> SignedTransactionData {
    let tx_hash = TRANSACTION_HASH;
    let (r, s) = key_pair.sign(tx_hash.into()).unwrap();
    SignedTransactionData {
        private_key: key_pair.secret_key,
        public_key: key_pair.public_key,
        tx_hash,
        signature: Secp256Signature { r, s },
    }
}

#[test]
fn test_is_valid_p256_signature_good_sig() {
    let key_pair = secp256r1::KEY_PAIR();
    let data = p256_signature_data(key_pair);

    let mut serialized_good_signature = array![];
    data.signature.serialize(ref serialized_good_signature);

    let is_valid = is_valid_p256_signature(
        data.tx_hash, key_pair.public_key, serialized_good_signature.span(),
    );
    assert!(is_valid);
}

#[test]
fn test_is_valid_p256_signature_bad_sig() {
    let key_pair = secp256r1::KEY_PAIR();
    let data = p256_signature_data(key_pair);
    let mut bad_signature = data.signature;

    bad_signature.r += 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_p256_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}

#[test]
#[should_panic(expected: 'Signature: Invalid format.')]
fn test_is_valid_p256_signature_invalid_format_sig() {
    let key_pair = secp256r1::KEY_PAIR();
    let data = p256_signature_data(key_pair);
    let mut serialized_bad_signature = array![0x1];

    is_valid_p256_signature(data.tx_hash, key_pair.public_key, serialized_bad_signature.span());
}

#[test]
fn test_p256_signature_r_out_of_range() {
    let key_pair = secp256r1::KEY_PAIR();
    let data = p256_signature_data(key_pair);
    let mut bad_signature = data.signature;

    let curve_size = Secp256Trait::<Secp256r1Point>::get_curve_size();

    bad_signature.r = curve_size + 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_p256_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}

#[test]
fn test_p256_signature_s_out_of_range() {
    let key_pair = secp256r1::KEY_PAIR();
    let data = p256_signature_data(key_pair);
    let mut bad_signature = data.signature;

    let curve_size = Secp256Trait::<Secp256r1Point>::get_curve_size();

    bad_signature.s = curve_size + 1;

    let mut serialized_bad_signature = array![];

    bad_signature.serialize(ref serialized_bad_signature);

    let is_invalid = !is_valid_p256_signature(
        data.tx_hash, key_pair.public_key, serialized_bad_signature.span(),
    );
    assert!(is_invalid);
}
