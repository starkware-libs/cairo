use starknet::SyscallResultTrait;
use core::test::test_utils::{assert_eq, assert_ne};

#[test]
fn test_keccak_syscall() {
    let input = array![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17];
    assert_eq(
        @starknet::syscalls::keccak_syscall(input.span()).unwrap_syscall(),
        @u256 { low: 0xec687be9c50d2218388da73622e8fdd5, high: 0xd2eb808dfba4703c528d145dfe6571af },
        'Wrong hash value'
    );
}

#[test]
fn test_keccak_hash() {
    let res = keccak::keccak_u256s_le_inputs(array![1].span());
    assert_eq(@res.low, @0x587f7cc3722e9654ea3963d5fe8c0748, 'Wrong hash low 1');
    assert_eq(@res.high, @0xa5963aa610cb75ba273817bce5f8c48f, 'Wrong hash high 1');

    let res = keccak::keccak_u256s_be_inputs(array![1].span());
    assert_eq(@res.low, @0x326a7e71fdcdee263b071276522d0eb1, 'Wrong hash low 2');
    assert_eq(@res.high, @0xf60cfab7e2cb9f2d73b0c2fa4a4bf40c, 'Wrong hash high 2');

    let res = keccak::keccak_u256s_le_inputs(array![1, 2, 3, 4].span());
    assert_eq(@res.low, @0x845f8e9f5191367fb5181e74f6eb550d, 'Wrong hash low 3');
    assert_eq(@res.high, @0x17a2126cf7391a26b41c36a687090cc5, 'Wrong hash high 3');

    let res = keccak::keccak_u256s_be_inputs(array![1, 2, 3, 4].span());
    assert_eq(@res.low, @0x6510e6fd534f267a01086462df912739, 'Wrong hash low 4');
    assert_eq(@res.high, @0x2d9982dfaf468a9ddf7101b6323aa9d5, 'Wrong hash high 4');
}

// Same input as in `test_keccak_hash` but as a u64 array.
#[test]
fn test_keccak_u64() {
    let mut input = array![
        0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000
    ];

    let res = keccak::cairo_keccak(ref input, 0, 0);

    assert_eq(@res.low, @0x587f7cc3722e9654ea3963d5fe8c0748, 'Wrong hash low');
    assert_eq(@res.high, @0xa5963aa610cb75ba273817bce5f8c48f, 'Wrong hash high');
}

#[test]
fn test_keccak_u64_full_block() {
    let mut input = array![
        0x0000000000000001,
        0x0000000000000002,
        0x0000000000000003,
        0x0000000000000004,
        0x0000000000000005,
        0x0000000000000006,
        0x0000000000000007,
        0x0000000000000008,
        0x0000000000000009,
        0x000000000000000a,
        0x000000000000000b,
        0x000000000000000c,
        0x000000000000000d,
        0x000000000000000e,
        0x000000000000000f,
        0x0000000000000010,
        0x0000000000000011
    ];

    let res = keccak::cairo_keccak(ref input, 0, 0);

    assert_eq(@res.low, @0x5d291eebae35b254ff50ec1fc57832e8, 'Wrong hash low');
    assert_eq(@res.high, @0x210740d45b1fe2ac908a497ef45509f5, 'Wrong hash high');
}

#[test]
fn test_keccak_u64_full_block_minus_byte() {
    let mut input = array![
        0x0000000000000001,
        0x0000000000000002,
        0x0000000000000003,
        0x0000000000000004,
        0x0000000000000005,
        0x0000000000000006,
        0x0000000000000007,
        0x0000000000000008,
        0x0000000000000009,
        0x000000000000000a,
        0x000000000000000b,
        0x000000000000000c,
        0x000000000000000d,
        0x000000000000000e,
        0x000000000000000f,
        0x0000000000000010,
    ];

    // We must clone the array to be used in the second part, as it's modified by `cairo_keccak`.
    let mut orig_array = input.clone();

    let res = keccak::cairo_keccak(ref input, 0x12000000000011, 7);

    assert_eq(@res.low, @0xbb968836d1704bea541a8d79dcc9067c, 'Wrong hash low 1');
    assert_eq(@res.high, @0xc98592786514c87f1a1a3d567b4dcd75, 'Wrong hash high 1');

    // With "garbage" at the end (note the `aa`), we should get the same result.
    let res = keccak::cairo_keccak(ref orig_array, 0xaa12000000000011, 7);

    assert_eq(@res.low, @0xbb968836d1704bea541a8d79dcc9067c, 'Wrong hash low 2');
    assert_eq(@res.high, @0xc98592786514c87f1a1a3d567b4dcd75, 'Wrong hash high 2');
}

#[test]
fn test_keccak_u64_full_block_minus_word() {
    let mut input = array![
        0x0000000000000001,
        0x0000000000000002,
        0x0000000000000003,
        0x0000000000000004,
        0x0000000000000005,
        0x0000000000000006,
        0x0000000000000007,
        0x0000000000000008,
        0x0000000000000009,
        0x000000000000000a,
        0x000000000000000b,
        0x000000000000000c,
        0x000000000000000d,
        0x000000000000000e,
        0x000000000000000f,
        0x0000000000000010,
    ];

    let res = keccak::cairo_keccak(ref input, 0, 0);

    assert_eq(@res.low, @0xfd9895001ee22e79b59c7997b3618a01, 'Wrong hash low');
    assert_eq(@res.high, @0x6851f2dbbfb3bfadff94db3ad476164f, 'Wrong hash high');
}

#[test]
fn test_keccak_u64_full_block_minus_word_minus_byte() {
    let mut input = array![
        0x0000000000000001,
        0x0000000000000002,
        0x0000000000000003,
        0x0000000000000004,
        0x0000000000000005,
        0x0000000000000006,
        0x0000000000000007,
        0x0000000000000008,
        0x0000000000000009,
        0x000000000000000a,
        0x000000000000000b,
        0x000000000000000c,
        0x000000000000000d,
        0x000000000000000e,
        0x000000000000000f,
    ];

    // We must clone the array to be used in the second part, as it's modified by `cairo_keccak`.
    let mut orig_array = input.clone();

    let res = keccak::cairo_keccak(ref input, 0x11000000000010, 7);

    assert_eq(@res.low, @0x2cc6d33d8630a63c428d9cf38a89568b, 'Wrong hash low');
    assert_eq(@res.high, @0xdd7e11698dc8b37323c854a53abcd330, 'Wrong hash high');

    // With "garbage" at the end (note the `aa`), we should get the same result.
    let res = keccak::cairo_keccak(ref orig_array, 0xaa11000000000010, 7);

    assert_eq(@res.low, @0x2cc6d33d8630a63c428d9cf38a89568b, 'Wrong hash low 2');
    assert_eq(@res.high, @0xdd7e11698dc8b37323c854a53abcd330, 'Wrong hash high 2');
}

#[test]
fn test_keccak_u64_full_block_minus_3_words_minus_4_bytes() {
    let mut input = array![
        0x0000000000000001,
        0x0000000000000002,
        0x0000000000000003,
        0x0000000000000004,
        0x0000000000000005,
        0x0000000000000006,
        0x0000000000000007,
        0x0000000000000008,
        0x0000000000000009,
        0x000000000000000a,
        0x000000000000000b,
        0x000000000000000c,
        0x000000000000000d
    ];

    // We must clone the array to be used in the second part, as it's modified by `cairo_keccak`.
    let mut orig_array = input.clone();

    let res = keccak::cairo_keccak(ref input, 0x11000010, 4);
    assert_eq(@res.low, @0x43ccdbe17ae03b02b308ebe4a23c4cc9, 'Wrong hash low 1');
    assert_eq(@res.high, @0xf3cc56e9bd860f83e3e3bc69919b176a, 'Wrong hash high 1');

    // With "garbage" at the end (note the `aa`s), we should get the same result.
    let res = keccak::cairo_keccak(ref orig_array, 0xaaaaaaaa11000010, 4);
    assert_eq(@res.low, @0x43ccdbe17ae03b02b308ebe4a23c4cc9, 'Wrong hash low 2');
    assert_eq(@res.high, @0xf3cc56e9bd860f83e3e3bc69919b176a, 'Wrong hash high 2');
}
