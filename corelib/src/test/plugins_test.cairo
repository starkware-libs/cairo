use test::test_utils::{assert_eq, assert_ne};

#[derive(Copy, Drop, Serde, PartialEq)]
enum EnumForSerde {
    A,
    B: u32,
    C: u64,
}

#[test]
fn test_derive_serde_enum() {
    let a = EnumForSerde::A(());
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
    assert(serialized.is_empty(), 'expected empty');
}
