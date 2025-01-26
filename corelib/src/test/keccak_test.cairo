use crate::keccak;

#[test]
fn test_keccak_syscall() {
    assert_eq!(
        starknet::syscalls::keccak_syscall(
            [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17].span(),
        ),
        Ok(0xd2eb808dfba4703c528d145dfe6571afec687be9c50d2218388da73622e8fdd5),
    );
}

#[test]
fn test_keccak_hash() {
    assert_eq!(
        keccak::keccak_u256s_le_inputs([1].span()),
        0xa5963aa610cb75ba273817bce5f8c48f587f7cc3722e9654ea3963d5fe8c0748,
    );
    assert_eq!(
        keccak::keccak_u256s_be_inputs([1].span()),
        0xf60cfab7e2cb9f2d73b0c2fa4a4bf40c326a7e71fdcdee263b071276522d0eb1,
    );
    assert_eq!(
        keccak::keccak_u256s_le_inputs([1, 2, 3, 4].span()),
        0x17a2126cf7391a26b41c36a687090cc5845f8e9f5191367fb5181e74f6eb550d,
    );
    assert_eq!(
        keccak::keccak_u256s_be_inputs([1, 2, 3, 4].span()),
        0x2d9982dfaf468a9ddf7101b6323aa9d56510e6fd534f267a01086462df912739,
    );
}

// Same input as in `test_keccak_hash` but as a u64 array.
#[test]
fn test_keccak_u64() {
    let mut input = array![
        0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000,
    ];
    assert_eq!(
        keccak::cairo_keccak(ref input, 0, 0),
        0xa5963aa610cb75ba273817bce5f8c48f587f7cc3722e9654ea3963d5fe8c0748,
    );
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
        0x0000000000000011,
    ];
    assert_eq!(
        keccak::cairo_keccak(ref input, 0, 0),
        0x210740d45b1fe2ac908a497ef45509f55d291eebae35b254ff50ec1fc57832e8,
    );
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

    assert_eq!(
        keccak::cairo_keccak(ref input, 0x12000000000011, 7),
        0xc98592786514c87f1a1a3d567b4dcd75bb968836d1704bea541a8d79dcc9067c,
    );

    // With "garbage" at the end (note the `aa`), we should get the same result.
    assert_eq!(
        keccak::cairo_keccak(ref orig_array, 0xaa12000000000011, 7),
        0xc98592786514c87f1a1a3d567b4dcd75bb968836d1704bea541a8d79dcc9067c,
    );
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
    assert_eq!(
        keccak::cairo_keccak(ref input, 0, 0),
        0x6851f2dbbfb3bfadff94db3ad476164ffd9895001ee22e79b59c7997b3618a01,
    );
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

    assert_eq!(
        keccak::cairo_keccak(ref input, 0x11000000000010, 7),
        0xdd7e11698dc8b37323c854a53abcd3302cc6d33d8630a63c428d9cf38a89568b,
    );

    // With "garbage" at the end (note the `aa`), we should get the same result.
    assert_eq!(
        keccak::cairo_keccak(ref orig_array, 0xaa11000000000010, 7),
        0xdd7e11698dc8b37323c854a53abcd3302cc6d33d8630a63c428d9cf38a89568b,
    );
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
        0x000000000000000d,
    ];

    // We must clone the array to be used in the second part, as it's modified by `cairo_keccak`.
    let mut orig_array = input.clone();

    assert_eq!(
        keccak::cairo_keccak(ref input, 0x11000010, 4),
        0xf3cc56e9bd860f83e3e3bc69919b176a43ccdbe17ae03b02b308ebe4a23c4cc9,
    );

    // With "garbage" at the end (note the `aa`s), we should get the same result.
    assert_eq!(
        keccak::cairo_keccak(ref orig_array, 0xaaaaaaaa11000010, 4),
        0xf3cc56e9bd860f83e3e3bc69919b176a43ccdbe17ae03b02b308ebe4a23c4cc9,
    );
}

#[test]
fn test_keccak_byte_array() {
    assert_eq!(
        keccak::compute_keccak_byte_array(@""),
        0x70a4855d04d8fa7b3b2782ca53b600e5c003c7dcb27d7e923c23f7860146d2c5,
    );
    assert_eq!(
        keccak::compute_keccak_byte_array(@"0123456789abedef"),
        0x156c8d1049ee26f4f392bf8260b9e1c5ad5542778f003235f8cf5e0a19520886,
    );
    assert_eq!(
        keccak::compute_keccak_byte_array(@"hello-world"),
        0xd9ba3e823d55e5075f58ee022c16025c3abe8b41b95d7b4651a3cf8422ad1bd4,
    );
}
