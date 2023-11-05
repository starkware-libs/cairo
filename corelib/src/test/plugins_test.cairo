use core::test::test_utils::assert_eq;

#[derive(Copy, Debug, Drop, Serde, PartialEq)]
enum EnumForSerde {
    A,
    B: u32,
    C: u64,
}

#[derive(Drop, Debug, Default, PartialEq)]
struct StructForDefault {
    a: felt252,
    b: u256,
    c: bool
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
    assert_eq(
        @Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read'),
        @a,
        'expected a'
    );
    assert_eq(
        @Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read'),
        @a,
        'expected a'
    );
    assert_eq(
        @Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read'),
        @c,
        'expected c'
    );
    assert_eq(
        @Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read'),
        @b,
        'expected b'
    );
    assert_eq(
        @Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read'),
        @a,
        'expected a'
    );
    assert!(serialized.is_empty());
}

#[test]
fn test_derive_default_struct() {
    assert_eq!(Default::default(), StructForDefault { a: 0, b: 0, c: bool::False });
}

#[test]
fn test_derive_default_enum() {
    let actual: EnumForDefault = Default::default();
    assert_eq!(
        Default::default(), EnumForDefault::C(StructForDefault { a: 0, b: 0, c: bool::False })
    );
}
