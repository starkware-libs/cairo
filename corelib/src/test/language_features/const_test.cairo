use crate::box::BoxTrait;

mod enum_value {
    pub extern type Const<T, const VARIANT_INDEX: felt252, V>;
}
mod value {
    pub extern type Const<T, const VALUE: felt252>;
}
mod struct0 {
    pub extern type Const<T>;
}
mod struct2 {
    pub extern type Const<T, C0, C1>;
}

mod const_bool {
    pub extern fn const_as_box<T, const SEGMENT_ID: felt252>() -> Box<bool> nopanic;
}

#[test]
fn test_const_bool() {
    assert!(
        !const_bool::const_as_box::<enum_value::Const<bool, 0, struct0::Const<()>>, 0>().unbox(),
    );
    assert!(
        const_bool::const_as_box::<enum_value::Const<bool, 1, struct0::Const<()>>, 0>().unbox(),
    );
}

mod const_felt252 {
    pub extern fn const_as_box<T, const SEGMENT_ID: felt252>() -> Box<felt252> nopanic;
}

#[test]
fn test_const_felt252() {
    assert!(const_felt252::const_as_box::<value::Const<felt252, 0>, 1>().unbox() == 0);
    assert!(const_felt252::const_as_box::<value::Const<felt252, -1>, 2>().unbox() == -1);
}

mod const_u8 {
    pub extern fn const_as_box<T, const SEGMENT_ID: felt252>() -> Box<u8> nopanic;
}

#[test]
fn test_const_u8() {
    assert!(const_u8::const_as_box::<value::Const<u8, 0>, 0>().unbox() == 0);
    assert!(const_u8::const_as_box::<value::Const<u8, 255>, 1>().unbox() == 255);
}

mod const_u256 {
    pub extern fn const_as_box<T, const SEGMENT_ID: felt252>() -> Box<u256> nopanic;
}

#[test]
fn test_const_u256() {
    assert!(
        const_u256::const_as_box::<
            struct2::Const<u256, value::Const<u128, 0x10>, value::Const<u128, 0>>, 0,
        >()
            .unbox() == 0x10,
    );
    assert!(
        const_u256::const_as_box::<
            struct2::Const<u256, value::Const<u128, 0>, value::Const<u128, 0x10>>, 0,
        >()
            .unbox() == 0x1000000000000000000000000000000000,
    );
}

#[derive(Copy, Drop, PartialEq, Debug)]
enum ThreeOptions {
    A: felt252,
    B: u256,
    C,
}

mod const_three_options {
    pub extern fn const_as_box<T, const SEGMENT_ID: felt252>() -> Box<super::ThreeOptions> nopanic;
}

#[test]
fn test_complex_enum() {
    assert!(
        const_three_options::const_as_box::<
            enum_value::Const<ThreeOptions, 0, value::Const<felt252, -1>>, 0,
        >()
            .unbox() == ThreeOptions::A(-1),
    );
    assert!(
        const_three_options::const_as_box::<
            enum_value::Const<
                ThreeOptions,
                1,
                struct2::Const<u256, value::Const<u128, 0x10>, value::Const<u128, 0x20>>,
            >,
            1,
        >()
            .unbox() == ThreeOptions::B(0x2000000000000000000000000000000010),
    );
    assert!(
        const_three_options::const_as_box::<
            enum_value::Const<ThreeOptions, 2, struct0::Const<()>>, 1,
        >()
            .unbox() == ThreeOptions::C,
    );
}

#[derive(Copy, Drop, PartialEq, Debug)]
enum ThreeOptions2 {
    A: felt252,
    B: (u256, u256),
    C,
}

mod const_tuple_three_options {
    pub extern fn const_as_box<T, const SEGMENT_INDEX: felt252>() -> Box<
        (super::ThreeOptions2, super::ThreeOptions2),
    > nopanic;
}

#[test]
fn test_two_complex_enums() {
    assert!(
        const_tuple_three_options::const_as_box::<
            struct2::Const<
                (ThreeOptions2, ThreeOptions2),
                enum_value::Const<ThreeOptions2, 0, value::Const<felt252, 1337>>,
                enum_value::Const<ThreeOptions2, 2, struct0::Const<()>>,
            >,
            0,
        >()
            .unbox() == (ThreeOptions2::A(1337), ThreeOptions2::C),
    );
}

#[test]
fn test_complex_consts() {
    const VAR_AND_MATCH_CONST: felt252 = {
        let x = Some((1, 2_u8));
        match x {
            Some((v, _)) => v,
            None => 3,
        }
    };
    assert_eq!(VAR_AND_MATCH_CONST, 1);
    const TRUE: bool = true;
    const IF_CONST_TRUE: felt252 = if TRUE {
        4
    } else {
        5
    };
    assert_eq!(IF_CONST_TRUE, 4);
    const FALSE: bool = false;
    const IF_CONST_FALSE: felt252 = if FALSE {
        6
    } else {
        7
    };
    assert_eq!(IF_CONST_FALSE, 7);
}

#[test]
fn test_const_casts_from_felt252() {
    const _U8_UNDER_RANGE: () = assert((-1_felt252).try_into() == None::<u8>, 'U8 under range');
    const _U8_MIN_IN_RANGE: () = assert(0_felt252.try_into() == Some(0_u8), 'U8 min in range');
    const _U8_MAX_IN_RANGE: () = assert(
        0xff_felt252.try_into() == Some(0xff_u8), 'U8 max in range',
    );
    const _U8_OVER_RANGE: () = assert(0x100_felt252.try_into() == None::<u8>, 'U8 over range');
    const _U16_UNDER_RANGE: () = assert((-1_felt252).try_into() == None::<u16>, 'U16 under range');
    const _U16_MIN_IN_RANGE: () = assert(0_felt252.try_into() == Some(0_u16), 'U16 min in range');
    const _U16_MAX_IN_RANGE: () = assert(
        0xffff_felt252.try_into() == Some(0xffff_u16), 'U16 max in range',
    );
    const _U16_OVER_RANGE: () = assert(0x10000_felt252.try_into() == None::<u16>, 'U16 over range');
    const _U32_UNDER_RANGE: () = assert((-1_felt252).try_into() == None::<u32>, 'U32 under range');
    const _U32_MIN_IN_RANGE: () = assert(0_felt252.try_into() == Some(0_u32), 'U32 min in range');
    const _U32_MAX_IN_RANGE: () = assert(
        0xffffffff_felt252.try_into() == Some(0xffffffff_u32), 'U32 max in range',
    );
    const _U32_OVER_RANGE: () = assert(
        0x100000000_felt252.try_into() == None::<u32>, 'U32 over range',
    );
    const _U64_UNDER_RANGE: () = assert((-1_felt252).try_into() == None::<u64>, 'U64 under range');
    const _U64_MIN_IN_RANGE: () = assert(0_felt252.try_into() == Some(0_u64), 'U64 min in range');
    const _U64_MAX_IN_RANGE: () = assert(
        0xffffffffffffffff_felt252.try_into() == Some(0xffffffffffffffff_u64), 'U64 max in range',
    );
    const _U64_OVER_RANGE: () = assert(
        0x10000000000000000_felt252.try_into() == None::<u64>, 'U64 over range',
    );
    const _U128_UNDER_RANGE: () = assert(
        (-1_felt252).try_into() == None::<u128>, 'U128 under range',
    );
    const _U128_MIN_IN_RANGE: () = assert(
        0_felt252.try_into() == Some(0_u128), 'U128 min in range',
    );
    const _U128_MAX_IN_RANGE: () = assert(
        0xffffffffffffffffffffffffffffffff_felt252
            .try_into() == Some(0xffffffffffffffffffffffffffffffff_u128),
        'U128 max in range',
    );
    const _U128_OVER_RANGE: () = assert(
        0x100000000000000000000000000000000_felt252.try_into() == None::<u128>, 'U128 over range',
    );
    const _I8_UNDER_RANGE: () = assert((-0x81_felt252).try_into() == None::<i8>, 'I8 under range');
    const _I8_MIN_IN_RANGE: () = assert(
        (-0x80_felt252).try_into() == Some(-0x80_i8), 'I8 min in range',
    );
    const _I8_MAX_IN_RANGE: () = assert(
        0x7f_felt252.try_into() == Some(0x7f_i8), 'I8 max in range',
    );
    const _I8_OVER_RANGE: () = assert(0x80_felt252.try_into() == None::<i8>, 'I8 over range');
    const _I16_UNDER_RANGE: () = assert(
        (-0x8001_felt252).try_into() == None::<i16>, 'I16 under range',
    );
    const _I16_MIN_IN_RANGE: () = assert(
        (-0x8000_felt252).try_into() == Some(-0x8000_i16), 'I16 min in range',
    );
    const _I16_MAX_IN_RANGE: () = assert(
        0x7fff_felt252.try_into() == Some(0x7fff_i16), 'I16 max in range',
    );
    const _I16_OVER_RANGE: () = assert(0x8000_felt252.try_into() == None::<i16>, 'I16 over range');
    const _I32_UNDER_RANGE: () = assert(
        (-0x80000001_felt252).try_into() == None::<i32>, 'I32 under range',
    );
    const _I32_MIN_IN_RANGE: () = assert(
        (-0x80000000_felt252).try_into() == Some(-0x80000000_i32), 'I32 min in range',
    );
    const _I32_MAX_IN_RANGE: () = assert(
        0x7fffffff_felt252.try_into() == Some(0x7fffffff_i32), 'I32 max in range',
    );
    const _I32_OVER_RANGE: () = assert(
        0x80000000_felt252.try_into() == None::<i32>, 'I32 over range',
    );
    const _I64_UNDER_RANGE: () = assert(
        (-0x8000000000000001_felt252).try_into() == None::<i64>, 'I64 under range',
    );
    const _I64_MIN_IN_RANGE: () = assert(
        (-0x8000000000000000_felt252).try_into() == Some(-0x8000000000000000_i64),
        'I64 min in range',
    );
    const _I64_MAX_IN_RANGE: () = assert(
        0x7fffffffffffffff_felt252.try_into() == Some(0x7fffffffffffffff_i64), 'I64 max in range',
    );
    const _I64_OVER_RANGE: () = assert(
        0x8000000000000000_felt252.try_into() == None::<i64>, 'I64 over range',
    );
    const _I128_UNDER_RANGE: () = assert(
        (-0x80000000000000000000000000000001_felt252).try_into() == None::<i128>,
        'I128 under range',
    );
    const _I128_MIN_IN_RANGE: () = assert(
        (-0x80000000000000000000000000000000_felt252)
            .try_into() == Some(-0x80000000000000000000000000000000_i128),
        'I128 min in range',
    );
    const _I128_MAX_IN_RANGE: () = assert(
        0x7fffffffffffffffffffffffffffffff_felt252
            .try_into() == Some(0x7fffffffffffffffffffffffffffffff_i128),
        'I128 max in range',
    );
    const _I128_OVER_RANGE: () = assert(
        0x80000000000000000000000000000000_felt252.try_into() == None::<i128>, 'I128 over range',
    );
    const _U256_FROM_ZERO: () = assert(0_felt252.into() == 0_u256, 'U256 from zero');
    const _U256_FROM_ONE: () = assert(1_felt252.into() == 1_u256, 'U256 from one');
    const _U256_FROM_MINUS_ONE: () = assert(
        (-1_felt252)
            .into() == 0x800000000000011000000000000000000000000000000000000000000000000_u256,
        'U256 under range',
    );
}

mod const_starknet_consts {
    pub extern fn const_as_box<T, const SEGMENT_INDEX: felt252>() -> Box<
        (starknet::ContractAddress, starknet::ClassHash),
    > nopanic;
}

const STARKNET_CONSTS: (starknet::ContractAddress, starknet::ClassHash) = (
    1000.try_into().unwrap(), 1001.try_into().unwrap(),
);

#[test]
fn test_starknet_consts() {
    assert!(
        const_starknet_consts::const_as_box::<
            struct2::Const<
                (starknet::ContractAddress, starknet::ClassHash),
                value::Const<starknet::ContractAddress, 1000>,
                value::Const<starknet::ClassHash, 1001>,
            >,
            0,
        >()
            .unbox() == STARKNET_CONSTS,
    );
}

