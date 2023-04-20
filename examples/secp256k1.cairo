// TODO(yg): get golden values.
fn test_secp256k1_recover_public_key() -> felt252 {
    let msg_hash = 1;
    let r = 1;
    let s = 1;
    let y_parity = true;
    let expected_public_key_x = 1;
    let expected_public_key_y = 1;

    let public_key = recover_public_key(msg_hash, r, s, y_parity).unwrap_syscall();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert_eq(expected_public_key_x, x, 'recover failed');
    assert_eq(expected_public_key_y, y, 'recover failed');

    let public_key = recover_public_key(msg_hash, r, s, !y_parity).unwrap_syscall();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert_eq(expected_public_key_x, x, 'recover failed');
    assert_eq(expected_public_key_y, -y, 'recover failed');
}

fn test_secp256k1_recover_public_key_u32() -> felt252 {
    let msg_hash = 1;
    let r = 1;
    let s = 1;
    let v = 0;
    let expected_public_key_x = 1;
    let expected_public_key_y = 1;

    let public_key = recover_public_key(msg_hash, r, s, v).unwrap_syscall();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert_eq(expected_public_key_x, x, 'recover failed');
    assert_eq(expected_public_key_y, y, 'recover failed');

    let public_key = recover_public_key(msg_hash, r, s, v + 1).unwrap_syscall();
    let (x, y) = secp256k1_ec_get_coordinates_syscall(public_key).unwrap_syscall();
    assert_eq(expected_public_key_x, x, 'recover failed');
    assert_eq(expected_public_key_y, -y, 'recover failed');
}
