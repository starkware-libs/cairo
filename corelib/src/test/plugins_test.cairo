use crate::test::test_utils::assert_eq;

#[derive(Copy, Debug, Drop, Serde, PartialEq)]
enum EnumForSerde {
    A,
    B: u32,
    C: u64,
}

/// A field named `serialized` (not last) must not shadow the generated `deserialize` parameter.
#[derive(Copy, Debug, Drop, Serde, PartialEq)]
struct StructWithSerializedField {
    serialized: u32,
    other: u32,
}

#[derive(Drop, Debug, Default, PartialEq)]
struct StructForDefault {
    a: felt252,
    b: u256,
    c: bool,
}

#[derive(Drop, Debug, Default, PartialEq)]
enum EnumForDefault {
    A: felt252,
    B: u256,
    #[default]
    C: StructForDefault,
}

#[test]
fn test_derive_serde_enum() {
    let a = EnumForSerde::A;
    let b = EnumForSerde::B(1);
    let c = EnumForSerde::C(2);
    let mut output = Default::default();
    a.serialize(ref output);
    a.serialize(ref output);
    c.serialize(ref output);
    b.serialize(ref output);
    a.serialize(ref output);
    let mut serialized = output.span();
    assert_eq!(Serde::deserialize(ref serialized), Some(a));
    assert_eq!(Serde::deserialize(ref serialized), Some(a));
    assert_eq!(Serde::deserialize(ref serialized), Some(c));
    assert_eq!(Serde::deserialize(ref serialized), Some(b));
    assert_eq!(Serde::deserialize(ref serialized), Some(a));
    assert(serialized.is_empty(), 'expected empty');
}

#[test]
fn test_derive_serde_struct_with_serialized_field() {
    let mut output = array![];
    StructWithSerializedField { serialized: 3, other: 5 }.serialize(ref output);
    let mut serialized = output.span();
    assert_eq!(
        Serde::deserialize(ref serialized),
        Some(StructWithSerializedField { serialized: 3, other: 5 }),
    );
    assert(serialized.is_empty(), 'expected empty');
}

#[test]
fn test_derive_default_struct() {
    assert_eq!(Default::default(), StructForDefault { a: 0, b: 0, c: false });
}

#[test]
fn test_derive_default_enum() {
    assert_eq!(Default::default(), EnumForDefault::C(StructForDefault { a: 0, b: 0, c: false }));
}

#[derive(Copy, Debug, Drop, Serde, PartialEq)]
enum LongEnum5 {
    A,
    B,
    C,
    D,
    E,
}
#[derive(Copy, Debug, Drop, Serde, PartialEq)]
enum LongEnum10 {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
}
#[derive(Copy, Debug, Drop, Serde, PartialEq)]
enum LongEnum15 {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    J,
    K,
    L,
    M,
    N,
    O,
}


#[test]
fn test_long_enum5_deserialize() {
    let mut output = Default::default();
    LongEnum5::E.serialize(ref output);
    let mut serialized = output.span();
    assert_eq!(Serde::deserialize(ref serialized), Some(LongEnum5::E));
}
#[test]
fn test_long_enum10_deserialize() {
    let mut output = Default::default();
    LongEnum10::J.serialize(ref output);
    let mut serialized = output.span();
    assert_eq!(Serde::deserialize(ref serialized), Some(LongEnum10::J));
}
#[test]
fn test_long_enum15_deserialize() {
    let mut output = Default::default();
    LongEnum15::O.serialize(ref output);
    let mut serialized = output.span();
    assert_eq!(Serde::deserialize(ref serialized), Some(LongEnum15::O));
}
