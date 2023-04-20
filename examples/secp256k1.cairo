use starknet::SyscallResultTrait;
use starknet::secp256k1::{
    recover_public_key, recover_public_key_u32, secp256k1_ec_get_coordinates_syscall
};
use option::OptionTrait;

fn test_secp256k1_recover_public_key() {
    let (msg_hash, r, s, y_parity, expected_public_key_x, expected_public_key_y) =
        get_message_and_signature();

    let public_key = recover_public_key(msg_hash, r, s, y_parity).unwrap();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed');
    assert(expected_public_key_y == y, 'recover failed');

    let public_key = recover_public_key(msg_hash, r, s, !y_parity).unwrap();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed');
    assert(expected_public_key_y != y, 'recover failed');
}

fn test_secp256k1_recover_public_key_u32() {
    let (msg_hash, r, s, y_parity, expected_public_key_x, expected_public_key_y) =
        get_message_and_signature();
    let v = if y_parity {
        0_u32
    } else {
        1_u32
    };

    let public_key = recover_public_key_u32(msg_hash, r, s, v).unwrap();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed');
    assert(expected_public_key_y == y, 'recover failed');

    let public_key = recover_public_key_u32(msg_hash, r, s, v + 1).unwrap();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert(expected_public_key_x == x, 'recover failed');
    assert(expected_public_key_y != y, 'recover failed');
}

/// Returns a valid message hash and its signature for testing.
fn get_message_and_signature() -> (u256, u256, u256, bool, u256, u256) {
    // TODO(yg): change golden values to hex.
    let msg_hash = 105178609285114473020851029150595163065281545268023964343634496796339681222021;
    let r = 34627219085299802438030559924718133626325687994345768323532899246965609283226;
    let s = 33820805363461893235047205307311902728990972830565689467719450876427994874124;
    let y_parity = false;
    let public_key_x =
        76723879461488468834578907165327257041939912798324757792382787763854600433405;
    let public_key_y =
        11173656154009898082869059772649372004771963844109677398354639573819310194846;
    (msg_hash, r, s, y_parity, public_key_x, public_key_y)
}
