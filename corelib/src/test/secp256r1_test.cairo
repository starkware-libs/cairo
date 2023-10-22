use starknet::{secp256r1::Secp256r1Impl, SyscallResultTrait};
use starknet::secp256_trait::{recover_public_key, Secp256PointTrait, Signature, is_valid_signature};
use starknet::secp256r1::{Secp256r1Point, Secp256r1PointImpl};
use test::test_utils::assert_eq;

#[test]
#[available_gas(100000000)]
fn test_secp256r1_recover_public_key() {
    let (msg_hash, signature, expected_public_key_x, expected_public_key_y, _) =
        get_message_and_signature();
    let public_key = recover_public_key::<Secp256r1Point>(msg_hash, signature).unwrap();
    let (x, y) = public_key.get_coordinates().unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed 1');
    assert(expected_public_key_y == y, 'recover failed 2');
}


/// Returns a golden valid message hash and its signature, for testing.
fn get_message_and_signature() -> (u256, Signature, u256, u256, Secp256r1Point) {
    // msg = ""
    // public key: (0x04aaec73635726f213fb8a9e64da3b8632e41495a944d0045b522eba7240fad5,
    //              0x0087d9315798aaa3a5ba01775787ced05eaaf7b4e09fc81d6d1aa546e8365d525d)
    let msg_hash = 0xe3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855;
    let r = 0xb292a619339f6e567a305c951c0dcbcc42d16e47f219f9e98e76e09d8770b34a;
    let s = 0x177e60492c5a8242f76f07bfe3661bde59ec2a17ce5bd2dab2abebdf89a62e2;

    let (public_key_x, public_key_y) = (
        0x04aaec73635726f213fb8a9e64da3b8632e41495a944d0045b522eba7240fad5,
        0x0087d9315798aaa3a5ba01775787ced05eaaf7b4e09fc81d6d1aa546e8365d525d
    );

    let public_key = Secp256r1Impl::secp256_ec_new_syscall(public_key_x, public_key_y)
        .unwrap_syscall()
        .unwrap();

    (msg_hash, Signature { r, s, y_parity: true }, public_key_x, public_key_y, public_key)
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature() {
    let (msg_hash, signature, _, _, public_key) = get_message_and_signature();
    let is_valid = is_valid_signature::<
        Secp256r1Point
    >(msg_hash, signature.r, signature.s, public_key);
    assert(is_valid, 'Signature should be valid');
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature_invalid_signature() {
    let (msg_hash, signature, _, _, public_key) = get_message_and_signature();
    let is_valid = is_valid_signature::<
        Secp256r1Point
    >(msg_hash, signature.r + 1, signature.s, public_key);
    assert(!is_valid, 'Signature should be invalid');
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature_overflowing_signature_r() {
    let (msg_hash, mut signature, _, _, public_key) = get_message_and_signature();
    let is_valid = is_valid_signature::<
        Secp256r1Point
    >(msg_hash, Secp256r1Impl::get_curve_size() + 1, signature.s, public_key);
    assert(!is_valid, 'Signature out of range');
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature_overflowing_signature_s() {
    let (msg_hash, mut signature, _, _, public_key) = get_message_and_signature();
    let is_valid = is_valid_signature::<
        Secp256r1Point
    >(msg_hash, signature.r, Secp256r1Impl::get_curve_size() + 1, public_key);
    assert(!is_valid, 'Signature out of range');
}


#[test]
#[available_gas(100_000_000)]
fn test_recover_public_key_y_even() {
    let x: u256 = 0x502a43ce77c6f5c736a82f847fa95f8c2d483fe223b12b91047d83258a958b0f;
    let y: u256 = 0xdb0a2e6710c71ba80afeb3abdf69d306ce729c7704f4ddf2eaaf0b76209fe1b0;
    let r: u256 = 0x7380df4a623c5c2259a5e5f5b225d7265a9e24b3a13c101d1afddcf29e3cf8b2;
    let s: u256 = 0x0d131afacdd17a4ea1b544bb3ade677ff8accbe7830e15b9c225e6031155946a;
    let y_parity = false;
    let message_hash: u256 = 0x28c7fff9aef4847a82cd64280434712a5b49205831b60eea6e70614077e672eb;
    let recovered = recover_public_key::<Secp256r1Point>(message_hash, Signature { r, s, y_parity })
        .unwrap();
    let (recovered_x, recovered_y) = recovered.get_coordinates().unwrap_syscall();

    assert_eq(@recovered_x, @x, 'Signature is not valid');
}
