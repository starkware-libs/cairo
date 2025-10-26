#[feature("byte-span")]
use crate::byte_array::ToByteSpanTrait;

#[test]
fn test_format() {
    let ba: ByteArray = "hello";
    assert(format!("{}", ba) == ba, 'byte array bad formatting');
    let span = ba.span();
    assert(format!("{span}") == ba, 'byte span bad formatting');
    assert(format!("{}", span[1..4]) == "ell", 'byte span slice bad formatting');
    assert(format!("{}", 97_felt252) == "97", 'felt252 bad formatting');
    assert(format!("{}", 97_usize) == "97", 'usize bad formatting');
    assert(format!("{}", 34 - 5) == "29", 'expression bad formatting');
    assert(format!("no_format") == "no_format", 'no args bad formatting');
    assert(format!("{}{}", 12_usize, 14_u32) == "1214", 'two args bad formatting');
    assert(format!("{0}{0}", 12_usize) == "1212", 'positional bad formatting');
    assert(format!("{}{1}", 12_usize, 14_u32) == "1214", 'positional mix bad formatting');
    assert(format!("{ba}_{}_{}_{1}", 12, 14_u32) == "hello_12_14_14", 'full mix bad formatting');
    assert(format!("{{{{}}}}") == "{{}}", 'special cases bad formatting');
    let nz_value: NonZero<felt252> = 1.try_into().unwrap();
    assert(format!("{}", nz_value) == "1", 'non zero bad formatting');
    assert(
        format!("{}_{}_{}_{}_{}_{}", 0_i128, 1_i8, 2_i16, 3_i32, 4_i64, 5_i128) == "0_1_2_3_4_5",
        'signed positive bad formatting',
    );
    assert(
        format!("{}_{}_{}_{}_{}", -1_i8, -2_i16, -3_i32, -4_i64, -5_i128) == "-1_-2_-3_-4_-5",
        'signed negative bad formatting',
    );
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
    assert(format!("{:?}", ba) == "\"hello\"", 'byte array bad formatting');
    assert(format!("{:?}", ba.span()) == "\"hello\"", 'byte span bad formatting');
    assert(format!("{:?}", ba.span()[1..4]) == "\"ell\"", 'byte span slice bad formatting');
    let ba_64: ByteArray = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@";
    let span_64 = ba_64.span();
    assert(
        format!(
            "{span_64:?}",
        ) == "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@\"",
        'long byte span',
    );
    assert(format!("{:?}", span_64[20..30]) == "\"UVWXYZabcd\"", 'byte span 64 slice');

    assert(format!("{:?}", 97_felt252) == "97", 'felt252 bad formatting');
    assert(format!("{:?}", 97_usize) == "97", 'usize bad formatting');
    assert(format!("{:?}{:?}", 12_usize, 14_u32) == "1214", 'two args bad formatting');
    assert(format!("{0:?}{0:?}", 12_usize) == "1212", 'positional bad formatting');
    assert(format!("{:?}{1:?}", 12_usize, 14_u32) == "1214", 'positional mix bad formatting');
    assert(
        format!("{ba:?}_{:?}_{:?}_{1:?}", 12, 14_u32) == "\"hello\"_12_14_14",
        'full mix bad formatting',
    );
    assert(
        format!("{:?}", EnumExample::Empty) == "EnumExample::Empty(())",
        'bad enum empty derive fmt',
    );
    assert(
        format!("{:?}", EnumExample::FeltValue(4)) == "EnumExample::FeltValue(4)",
        'bad derive enum fmt',
    );
    assert(
        format!(
            "{:?}",
            StructExample {
                felt_value: 6,
                bool_value: false,
                byte_array_value: "ByteArray",
                enum_value: EnumExample::BoolValue(true),
            },
        ) == "StructExample { felt_value: 6, bool_value: false, byte_array_value: \"ByteArray\", enum_value: EnumExample::BoolValue(true) }",
        'bad derive struct formatting',
    );
    assert(
        format!("{:?}", ((), (1,), (2, 3), (4, 5, 6))) == "((), (1,), (2, 3), (4, 5, 6))",
        'bad tuple fmt',
    );
    let empty: [felt252; 0] = [];
    assert(
        format!("{:?}", (empty, [1], [2, 3], [4, 5, 6])) == "([], [1], [2, 3], [4, 5, 6])",
        'bad fixed sized array fmt',
    );
    assert(format!("{:?}", crate::box::BoxTrait::new(1)) == "&1", 'bad box fmt');
    assert(
        format!("{:?}", crate::nullable::NullableTrait::new(1)) == "&1", 'bad nullable value fmt',
    );
    assert(format!("{:?}", crate::nullable::null::<felt252>()) == "null", 'bad null fmt');

    let s = IntoFelt252Based { felt_value: 42 };
    assert(format!("{:?}", s) == "42", 'felt252 based bad formatting');
}

#[test]
fn test_array_debug() {
    let arr = array![1, 2, 3];
    assert(format!("{:?}", arr) == "[1, 2, 3]", 'bad array fmt');
}

#[test]
fn test_format_hex() {
    assert(format!("{:x}", 42) == "2a", 'bad felt252 hex formatting');
    assert(format!("{:x}", 42_u8) == "2a", 'bad u8 lower hex formatting');
    assert(format!("{:x}", 42_u16) == "2a", 'bad u16 lower hex formatting');
    assert(format!("{:x}", 48879_u16) == "beef", 'bad u16 lower hex formatting');
    assert(format!("{:x}", 42_u32) == "2a", 'bad u32 lower hex formatting');
    assert(format!("{:x}", 3735928559_u32) == "deadbeef", 'bad u32 lower hex formatting');
    assert(format!("{:x}", 42_u64) == "2a", 'bad u64 lower hex formatting');
    assert(
        format!("{:x}", 16045690984833335023_u64) == "deadbeefdeadbeef",
        'bad u64 lower hex formatting',
    );
    assert(format!("{:x}", 42_u128) == "2a", 'bad u128 lower hex formatting');
    assert(
        format!(
            "{:x}", 295990755083049101712519384020072382191_u128,
        ) == "deadbeefdeadbeefdeadbeefdeadbeef",
        'bad u128 lower hex formatting',
    );
    assert(format!("{:x}", 42_u256) == "2a", 'bad u256 lower hex formatting');
    assert(
        format!(
            "{:x}",
            100720434726375746010458024839911619878118703404436202866098422983289408962287_u256,
        ) == "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
        'bad u256 lower hex formatting',
    );

    let nz_value: NonZero<felt252> = 42.try_into().unwrap();
    assert(format!("{:x}", nz_value) == "2a", 'non zero bad formatting');

    let s = IntoFelt252Based { felt_value: 42 };
    assert(format!("{:x}", s) == "2a", 'felt252 based bad formatting');
}
