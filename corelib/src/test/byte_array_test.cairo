use array::{ArrayTrait, SpanTrait};
use byte_array::ByteArrayTrait;
use bytes_31::Bytes31IntoFelt252;
use option::OptionTrait;
use traits::Into;

// #[test]
// #[available_gas(1000000)]
// fn test_append_byte() {
//     let mut ba = Default::default();
//     let mut c = 1_u8;
//     loop {
//         if c == 34 {
//             break;
//         }
//         ba.append_byte(c);
//         c += 1;
//     };

//     let mut expected_data: Array<felt252> = Default::default();
//     expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
//     compare_byte_array(@ba, expected_data.span(), 2, 0x2120);
// }

// #[test]
// #[available_gas(1000000)]
// fn test_append_word() {
//     let mut ba = Default::default();

//     ba
//         .append_word(
//             bytes31_const::<0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201>(), 30
//         );
//     let mut expected_data: Array<felt252> = Default::default();
//     compare_byte_array(
//         @ba,
//         expected_data.span(),
//         30,
//         0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201
//     );

//     ba.append_word(bytes31_const::<0x21201f>(), 3);
//     expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
//     compare_byte_array(@ba, expected_data.span(), 2, 0x2120);

//     ba.append_word(bytes31_const::<0x2322>(), 2);
//     compare_byte_array(@ba, expected_data.span(), 4, 0x23222120);

//     // Length is 0, so nothing is actually appended.
//     ba.append_word(bytes31_const::<0xffee>(), 0);
//     compare_byte_array(@ba, expected_data.span(), 4, 0x23222120);

//     ba.append_word(bytes31_const::<0x3e3d3c3b3a393837363534333231302f2e2d2c2b2a292827262524>(), 27);
//     expected_data.append(0x3e3d3c3b3a393837363534333231302f2e2d2c2b2a29282726252423222120);
//     compare_byte_array(@ba, expected_data.span(), 0, 0);

//     ba.append_word(bytes31_const::<0x3f>(), 1);
//     compare_byte_array(@ba, expected_data.span(), 1, 0x3f);
// }

#[test]
#[available_gas(1000000)]
fn test_append() {
    let mut ba1 = test_byte_array_1();
    let ba2 = test_byte_array_2();

    ba1.new_append_outer(@ba2);

    let mut expected_data: Array<felt252> = Default::default();
    expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
    expected_data.append(0x4e4d4c4b4a494847464544434241403f3e3d3c3b3a39383736353433323120);
    compare_byte_array(@ba1, expected_data.span(), 2, 0x504f);
}

// #[test]
// #[available_gas(1000000)]
// fn test_concat() {
//     let ba1 = test_byte_array_1();
//     let ba2 = test_byte_array_2();

//     let ba3 = ByteArrayTrait::concat(@ba1, @ba2);

//     let mut expected_data: Array<felt252> = Default::default();
//     expected_data.append(0x1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201);
//     expected_data.append(0x4e4d4c4b4a494847464544434241403f3e3d3c3b3a39383736353433323120);
//     compare_byte_array(@ba3, expected_data.span(), 2, 0x504f);
// }

use debug::PrintTrait;
fn compare_byte_array(
    mut ba: @ByteArray, mut data: Span<felt252>, pending_word_len: u8, pending_word: felt252
) {
    assert(ba.data.len() == data.len(), 'wrong data len');
    let mut ba_data = ba.data.span();

    loop {
        match ba_data.pop_front() {
            Option::Some(x) => {
                assert((*x).into() == *data.pop_front().unwrap(), 'wrong data');
            },
            Option::None(_) => {
                break;
            }
        };
    };

    assert(*ba.pending_word_len == pending_word_len, 'wrong pending_word_len');
    assert((*ba.pending_word).into() == pending_word, 'wrong pending_word');
}

fn test_byte_array_1() -> ByteArray {
    let mut ba1 = Default::default();
    ba1
        .append_word(
            bytes31_const::<0x1e1d1c1b1a191817161514131211100f0e0d0c0b0a090807060504030201>(), 30
        );
    ba1.append_word(bytes31_const::<0x201f>(), 2);
    ba1
}

fn test_byte_array_2() -> ByteArray {
    let mut ba2 = Default::default();
    ba2
        .append_word(
            bytes31_const::<0x4e4d4c4b4a494847464544434241403f3e3d3c3b3a393837363534333231>(), 30
        );
    ba2.append_word(bytes31_const::<0x504f>(), 2);
    ba2
}
