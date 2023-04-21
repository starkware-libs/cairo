use array::ArrayTrait;
use array::SpanTrait;
use serde::Serde;
use option::OptionTrait;

#[derive(Copy, Drop, Serde, PartialEq)]
enum EnumForSerde {
    A: (),
    B: u32,
    C: u64,
}

#[test]
fn test_derive_serde_enum() {
    let a = EnumForSerde::A(());
    let b = EnumForSerde::B(1);
    let c = EnumForSerde::C(2);
    let mut output = ArrayTrait::new();
    Serde::serialize(ref output, a);
    Serde::serialize(ref output, a);
    Serde::serialize(ref output, c);
    Serde::serialize(ref output, b);
    Serde::serialize(ref output, a);
    let mut serialized = output.span();
    assert(
        Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read') == a,
        'expected a'
    );
    assert(
        Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read') == a,
        'expected a'
    );
    assert(
        Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read') == c,
        'expected c'
    );
    assert(
        Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read') == b,
        'expected b'
    );
    assert(
        Serde::<EnumForSerde>::deserialize(ref serialized).expect('failed to read') == a,
        'expected a'
    );
    assert(serialized.is_empty(), 'expected empty');
}
