use starknet::{
    eth_address::U256IntoEthAddress, EthAddress, secp256k1::Secp256k1Impl, SyscallResultTrait
};
use starknet::secp256_trait::{
    Signature, recover_public_key, Secp256PointTrait, signature_from_vrs, is_valid_signature
};
use starknet::secp256k1::{Secp256k1Point, Secp256k1PointImpl};
use starknet::eth_signature::verify_eth_signature;

#[test]
#[available_gas(100000000)]
fn test_secp256k1_recover_public_key() {
    let y_parity = true;
    let (msg_hash, signature, expected_public_key_x, expected_public_key_y, _) =
        get_message_and_signature(
        :y_parity
    );
    let public_key = recover_public_key::<Secp256k1Point>(msg_hash, signature).unwrap();
    let (x, y) = public_key.get_coordinates().unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed 1');
    assert(expected_public_key_y == y, 'recover failed 2');

    let y_parity = false;
    let (msg_hash, signature, expected_public_key_x, expected_public_key_y, _) =
        get_message_and_signature(
        :y_parity
    );
    let public_key = recover_public_key::<Secp256k1Point>(msg_hash, signature).unwrap();
    let (x, y) = public_key.get_coordinates().unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed 3');
    assert(expected_public_key_y == y, 'recover failed 4');
}

#[test]
fn test_signature_from_vrs() {
    let v = 27;
    let r = 1;
    let s = 2;
    let signature = signature_from_vrs(v, r, s);

    assert(signature == Signature { r, s, y_parity: false }, 'Wrong result');
}

/// Returns a golden valid message hash and its signature, for testing.
fn get_message_and_signature(y_parity: bool) -> (u256, Signature, u256, u256, EthAddress) {
    let msg_hash = 0xe888fbb4cf9ae6254f19ba12e6d9af54788f195a6f509ca3e934f78d7a71dd85;
    let r = 0x4c8e4fbc1fbb1dece52185e532812c4f7a5f81cf3ee10044320a0d03b62d3e9a;
    let s = 0x4ac5e5c0c0e8a4871583cc131f35fb49c2b7f60e6a8b84965830658f08f7410c;

    let (public_key_x, public_key_y) = if y_parity {
        (
            0xa9a02d48081294b9bb0d8740d70d3607feb20876964d432846d9b9100b91eefd,
            0x18b410b5523a1431024a6ab766c89fa5d062744c75e49efb9925bf8025a7c09e
        )
    } else {
        (
            0x57a910a2a58ef7d57f452e1f6ea7ee0080789091de946b0ca6e5c6af2c8ff5c8,
            0x249d233d0d21f35db55ce852edbd340d31e92ea4d591886149ca5d89911331ac
        )
    };
    let eth_address = 0x767410c1bb448978bd42b984d7de5970bcaf5c43_u256.into();

    (msg_hash, Signature { r, s, y_parity }, public_key_x, public_key_y, eth_address)
}

#[test]
#[available_gas(100000000)]
fn test_verify_eth_signature() {
    let y_parity = true;
    let (msg_hash, signature, expected_public_key_x, expected_public_key_y, eth_address) =
        get_message_and_signature(
        :y_parity
    );
    verify_eth_signature(:msg_hash, :signature, :eth_address);
}

#[test]
#[should_panic(expected: ('Invalid signature',))]
#[available_gas(100000000)]
fn test_verify_eth_signature_wrong_eth_address() {
    let y_parity = true;
    let (msg_hash, signature, expected_public_key_x, expected_public_key_y, eth_address) =
        get_message_and_signature(
        :y_parity
    );
    let eth_address = (eth_address.into() + 1).try_into().unwrap();
    verify_eth_signature(:msg_hash, :signature, :eth_address);
}

#[test]
#[should_panic(expected: ('Signature out of range',))]
#[available_gas(100000000)]
fn test_verify_eth_signature_overflowing_signature_r() {
    let y_parity = true;
    let (msg_hash, mut signature, expected_public_key_x, expected_public_key_y, eth_address) =
        get_message_and_signature(
        :y_parity
    );
    signature.r = Secp256k1Impl::get_curve_size() + 1;
    verify_eth_signature(:msg_hash, :signature, :eth_address);
}

#[test]
#[should_panic(expected: ('Signature out of range',))]
#[available_gas(100000000)]
fn test_verify_eth_signature_overflowing_signature_s() {
    let y_parity = true;
    let (msg_hash, mut signature, expected_public_key_x, expected_public_key_y, eth_address) =
        get_message_and_signature(
        :y_parity
    );
    signature.s = Secp256k1Impl::get_curve_size() + 1;
    verify_eth_signature(:msg_hash, :signature, :eth_address);
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature() {
    let (msg_hash, signature, public_key_x, public_key_y, _) = get_message_and_signature(false);

    let public_key = Secp256k1Impl::secp256_ec_new_syscall(public_key_x, public_key_y)
        .unwrap_syscall()
        .unwrap();

    let is_valid = is_valid_signature::<
        Secp256k1Point
    >(msg_hash, signature.r, signature.s, public_key);
    assert(is_valid, 'Signature should be valid');
}

#[test]
#[available_gas(100000000)]
fn test_verify_signature_invalid_signature() {
    let (msg_hash, signature, public_key_x, public_key_y, _) = get_message_and_signature(false);

    let public_key = Secp256k1Impl::secp256_ec_new_syscall(public_key_x, public_key_y)
        .unwrap_syscall()
        .unwrap();

    let is_valid = is_valid_signature::<
        Secp256k1Point
    >(msg_hash, signature.r + 1, signature.s, public_key);
    assert(!is_valid, 'Signature should be invalid');
}
