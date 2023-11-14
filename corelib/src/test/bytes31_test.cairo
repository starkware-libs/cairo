use core::bytes_31::{split_bytes31, bytes31_const};

const POW_2_248: felt252 = 0x100000000000000000000000000000000000000000000000000000000000000;

#[test]
fn test_at() {
    let b1 = bytes31_const::<0x01>();
    assert_eq!(b1.at(0), 0x01);

    let b17 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f1011>();
    assert_eq!(b17.at(0), 0x11);
    assert_eq!(b17.at(1), 0x10);
    assert_eq!(b17.at(2), 0x0f);
    assert_eq!(b17.at(14), 0x03);
    assert_eq!(b17.at(15), 0x02);
    assert_eq!(b17.at(16), 0x01);

    let b31 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f>();
    assert_eq!(b31.at(0), 0x1f);
    assert_eq!(b31.at(1), 0x1e);
    assert_eq!(b31.at(2), 0x1d);
    assert_eq!(b31.at(14), 0x11);
    assert_eq!(b31.at(15), 0x10);
    assert_eq!(b31.at(16), 0x0f);
    assert_eq!(b31.at(17), 0x0e);
    assert_eq!(b31.at(29), 0x02);
    assert_eq!(b31.at(30), 0x01);
}

// Same as the previous test, but with [] instead of .at()
#[test]
fn test_index_view() {
    let b1 = bytes31_const::<0x01>();
    assert_eq!(b1[0], 0x01);

    let b17 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f1011>();
    assert_eq!(b17[0], 0x11);
    assert_eq!(b17[1], 0x10);
    assert_eq!(b17[2], 0x0f);
    assert_eq!(b17[14], 0x03);
    assert_eq!(b17[15], 0x02);
    assert_eq!(b17[16], 0x01);

    let b31 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f>();
    assert_eq!(b31[0], 0x1f);
    assert_eq!(b31[1], 0x1e);
    assert_eq!(b31[2], 0x1d);
    assert_eq!(b31[14], 0x11);
    assert_eq!(b31[15], 0x10);
    assert(b31[16] == 0x0f, 'wrong byte at index 16');
    assert(b31[17] == 0x0e, 'wrong byte at index 17');
    assert(b31[29] == 0x02, 'wrong byte at index 29');
    assert(b31[30] == 0x01, 'wrong byte at index 30');
}


#[test]
fn test_bytes31_to_from_felt252() {
    let zero_as_bytes31: Option<bytes31> = 0.try_into();
    assert(zero_as_bytes31.is_some(), '0 is not a bytes31');
    let zero_as_felt252 = zero_as_bytes31.unwrap().into();
    assert(zero_as_felt252 == 0_felt252, 'bad cast: 0');

    let one_as_bytes31: Option<bytes31> = 1.try_into();
    assert(one_as_bytes31.is_some(), '1 is not a bytes31');
    let one_as_felt252 = one_as_bytes31.unwrap().into();
    assert(one_as_felt252 == 1_felt252, 'bad cast: 1');

    let max_as_bytes31: Option<bytes31> = (POW_2_248 - 1).try_into();
    assert(max_as_bytes31.is_some(), '2^248 - 1 is not a bytes31');
    let max_as_felt252 = max_as_bytes31.unwrap().into();
    assert(max_as_felt252 == POW_2_248 - 1, 'bad cast: 2^248 - 1');

    let out_of_range: Option<bytes31> = POW_2_248.try_into();
    assert(out_of_range.is_none(), '2^248 is a bytes31');

    let out_of_range: Option<bytes31> = (-1).try_into();
    assert(out_of_range.is_none(), '-1 is a bytes31');
}

#[test]
fn test_u8_into_bytes31() {
    let one_u8 = 1_u8;
    let one_as_bytes31: bytes31 = one_u8.into();
    assert(one_as_bytes31.into() == 1_felt252, 'bad cast: 1');

    let max_u8 = 0xff_u8;
    let max_as_bytes31: bytes31 = max_u8.into();
    assert(max_as_bytes31.into() == 0xff_felt252, 'bad cast: 2^8 - 1');
}

#[test]
fn test_u16_into_bytes31() {
    let one_u16 = 1_u16;
    let one_as_bytes31: bytes31 = one_u16.into();
    assert(one_as_bytes31.into() == 1_felt252, 'bad cast: 1');

    let max_u16 = 0xffff_u16;
    let max_as_bytes31: bytes31 = max_u16.into();
    assert(max_as_bytes31.into() == 0xffff_felt252, 'bad cast: 2^16 - 1');
}

#[test]
fn test_u32_into_bytes31() {
    let one_u32 = 1_u32;
    let one_as_bytes31: bytes31 = one_u32.into();
    assert(one_as_bytes31.into() == 1_felt252, 'bad cast: 1');

    let max_u32 = 0xffffffff_u32;
    let max_as_bytes31: bytes31 = max_u32.into();
    assert(max_as_bytes31.into() == 0xffffffff_felt252, 'bad cast: 2^32 - 1');
}

#[test]
fn test_u64_into_bytes31() {
    let one_u64 = 1_u64;
    let one_as_bytes31: bytes31 = one_u64.into();
    assert(one_as_bytes31.into() == 1_felt252, 'bad cast: 1');

    let max_u64 = 0xffffffffffffffff_u64;
    let max_as_bytes31: bytes31 = max_u64.into();
    assert(max_as_bytes31.into() == 0xffffffffffffffff_felt252, 'bad cast: 2^64 - 1');
}

#[test]
fn test_u128_into_bytes31() {
    let one_u128 = 1_u128;
    let one_as_bytes31: bytes31 = one_u128.into();
    assert(one_as_bytes31.into() == 1_felt252, 'bad cast: 1');

    let max_u128 = 0xffffffffffffffffffffffffffffffff_u128;
    let max_as_bytes31: bytes31 = max_u128.into();
    assert(
        max_as_bytes31.into() == 0xffffffffffffffffffffffffffffffff_felt252, 'bad cast: 2^128 - 1'
    );
}

#[test]
fn test_split_bytes31() {
    let (left, right) = split_bytes31(0x1122, 2, 1);
    assert(left == 0x22, 'bad split (2, 1) left');
    assert(right == 0x11, 'bad split (2, 1) right');

    let x = 0x112233445566778899aabbccddeeff00112233;
    let (left, right) = split_bytes31(x, 19, 0);
    assert(left == 0, 'bad split (19, 0) left');
    assert(right == 0x112233445566778899aabbccddeeff00112233, 'bad split (19, 0) right');

    let (left, right) = split_bytes31(x, 19, 1);
    assert(left == 0x33, 'bad split (19, 1) left');
    assert(right == 0x112233445566778899aabbccddeeff001122, 'bad split (19, 1) right');

    let (left, right) = split_bytes31(x, 19, 15);
    assert(left == 0x5566778899aabbccddeeff00112233, 'bad split (19, 15) left');
    assert(right == 0x11223344, 'bad split (19, 15) right');

    let (left, right) = split_bytes31(x, 19, 16);
    assert(left == 0x445566778899aabbccddeeff00112233, 'bad split (19, 16) left');
    assert(right == 0x112233, 'bad split (19, 16) right');

    let (left, right) = split_bytes31(x, 19, 18);
    assert(left == 0x2233445566778899aabbccddeeff00112233, 'bad split (19, 18) left');
    assert(right == 0x11, 'bad split (19, 18) right');

    let (left, right) = split_bytes31(x, 19, 19);
    assert(left == 0x112233445566778899aabbccddeeff00112233, 'bad split (19, 19) left');
    assert(right == 0, 'bad split (19, 19) right');
}

#[test]
fn test_equality() {
    let b1 = bytes31_const::<0x01>();
    let b2 = bytes31_const::<0x0102>();
    let b3 = bytes31_const::<0x0201>();
    assert(b1 == b1, 'b1 != b1');
    assert(b2 == b2, 'b2 != b2');
    assert(b3 == b3, 'b3 != b3');
    assert(b1 != b2, 'b1 == b2');
    assert(b1 != b3, 'b1 == b3');
    assert(b2 != b3, 'b2 == b3');

    let b4 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f>();
    let b5 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1e>();
    let b6 = bytes31_const::<0x0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e>();

    assert(b4 == b4, 'b4 != b4');
    assert(b5 == b5, 'b5 != b5');
    assert(b6 == b6, 'b6 != b6');
    assert(b4 != b5, 'b4 == b5');
    assert(b4 != b6, 'b4 == b6');
    assert(b5 != b6, 'b5 == b6');
}
