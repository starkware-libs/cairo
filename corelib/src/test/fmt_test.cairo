#[feature("byte-span")]
use crate::byte_array::ToByteSpanTrait;

#[test]
fn test_format() {
    let ba: ByteArray = "hello";
    assert!(format!("{}", ba) == ba);
    let span = ba.span();
    assert!(format!("{span}") == ba);
    assert!(format!("{}", span[1..4]) == "ell");
    assert!(format!("{}", 97_felt252) == "97");
    assert!(format!("{}", 97_usize) == "97");
    assert!(format!("{}", 34 - 5) == "29");
    assert!(format!("no_format") == "no_format");
    assert!(format!("{}{}", 12_usize, 14_u32) == "1214");
    assert!(format!("{0}{0}", 12_usize) == "1212");
    assert!(format!("{}{1}", 12_usize, 14_u32) == "1214");
    assert!(format!("{ba}_{}_{}_{1}", 12, 14_u32) == "hello_12_14_14");
    assert!(format!("{{{{}}}}") == "{{}}");
    let nz_value: NonZero<felt252> = 1.try_into().unwrap();
    assert!(format!("{}", nz_value) == "1");
    assert!(
        format!("{}_{}_{}_{}_{}_{}", 0_i128, 1_i8, 2_i16, 3_i32, 4_i64, 5_i128) == "0_1_2_3_4_5",
    );
    assert!(format!("{}_{}_{}_{}_{}", -1_i8, -2_i16, -3_i32, -4_i64, -5_i128) == "-1_-2_-3_-4_-5");
}

#[derive(Debug, Drop)]
struct StructExample {
    felt_value: felt252,
    bool_value: bool,
    byte_array_value: ByteArray,
    enum_value: EnumExample,
}

#[derive(Debug, Drop)]
enum EnumExample {
    Empty,
    FeltValue: felt252,
    BoolValue: bool,
}

#[derive(Drop, Copy)]
struct IntoFelt252Based {
    felt_value: felt252,
}

impl IntoFelt252BasedInto of Into<IntoFelt252Based, felt252> {
    fn into(self: IntoFelt252Based) -> felt252 {
        self.felt_value
    }
}

impl IntoFelt252BasedDebug = core::fmt::into_felt252_based::DebugImpl<IntoFelt252Based>;
impl IntoFelt252BasedLowerHex = core::fmt::into_felt252_based::LowerHexImpl<IntoFelt252Based>;

#[test]
fn test_format_debug() {
    let ba: ByteArray = "hello";
    assert!(format!("{:?}", ba) == "\"hello\"");
    assert!(format!("{:?}", ba.span()) == "\"hello\"");
    assert!(format!("{:?}", ba.span()[1..4]) == "\"ell\"");
    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@";
    let span_64 = ba_64.span();
    assert!(
        format!(
            "{span_64:?}",
        ) == "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@\"",
    );
    assert!(format!("{:?}", span_64[20..30]) == "\"UVWXYZabcd\"");

    assert!(format!("{:?}", 97_felt252) == "97");
    assert!(format!("{:?}", 97_usize) == "97");
    assert!(format!("{:?}{:?}", 12_usize, 14_u32) == "1214");
    assert!(format!("{0:?}{0:?}", 12_usize) == "1212");
    assert!(format!("{:?}{1:?}", 12_usize, 14_u32) == "1214");
    assert!(format!("{ba:?}_{:?}_{:?}_{1:?}", 12, 14_u32) == "\"hello\"_12_14_14");
    assert!(format!("{:?}", EnumExample::Empty) == "EnumExample::Empty(())");
    assert!(format!("{:?}", EnumExample::FeltValue(4)) == "EnumExample::FeltValue(4)");
    assert!(
        format!(
            "{:?}",
            StructExample {
                felt_value: 6,
                bool_value: false,
                byte_array_value: "ByteArray",
                enum_value: EnumExample::BoolValue(true),
            },
        ) == "StructExample { felt_value: 6, bool_value: false, byte_array_value: \"ByteArray\", enum_value: EnumExample::BoolValue(true) }",
    );
    assert!(format!("{:?}", ((), (1,), (2, 3), (4, 5, 6))) == "((), (1,), (2, 3), (4, 5, 6))");
    let empty: [felt252; 0] = [];
    assert!(format!("{:?}", (empty, [1], [2, 3], [4, 5, 6])) == "([], [1], [2, 3], [4, 5, 6])");
    assert!(format!("{:?}", crate::box::BoxTrait::new(1)) == "&1");
    assert!(format!("{:?}", crate::nullable::NullableTrait::new(1)) == "&1");
    assert!(format!("{:?}", crate::nullable::null::<felt252>()) == "null");

    let s = IntoFelt252Based { felt_value: 42 };
    assert!(format!("{:?}", s) == "42");
}

#[test]
fn test_array_debug() {
    let arr = array![1, 2, 3];
    assert!(format!("{:?}", arr) == "[1, 2, 3]");
}

#[test]
fn test_format_hex() {
    assert!(format!("{:x}", 42) == "2a");
    assert!(format!("{:x}", 42_u8) == "2a");
    assert!(format!("{:x}", 42_u16) == "2a");
    assert!(format!("{:x}", 48879_u16) == "beef");
    assert!(format!("{:x}", 42_u32) == "2a");
    assert!(format!("{:x}", 3735928559_u32) == "deadbeef");
    assert!(format!("{:x}", 42_u64) == "2a");
    assert!(format!("{:x}", 16045690984833335023_u64) == "deadbeefdeadbeef");
    assert!(format!("{:x}", 42_u128) == "2a");
    assert!(
        format!(
            "{:x}", 295990755083049101712519384020072382191_u128,
        ) == "deadbeefdeadbeefdeadbeefdeadbeef",
    );
    assert!(format!("{:x}", 42_u256) == "2a");
    assert!(
        format!(
            "{:x}",
            100720434726375746010458024839911619878118703404436202866098422983289408962287_u256,
        ) == "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
    );

    let nz_value: NonZero<felt252> = 42.try_into().unwrap();
    assert!(format!("{:x}", nz_value) == "2a");

    let s = IntoFelt252Based { felt_value: 42 };
    assert!(format!("{:x}", s) == "2a");
}
