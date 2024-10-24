use core::test::test_utils::assert_eq;

#[derive(Copy, Drop, Serde, PartialEq)]
struct SimpleStruct {
    x: felt252,
    y: felt252,
}

#[derive(Copy, Clone, Destruct, Serde, PartialEq)]
struct GenericStruct<T, U> {
    x: T,
    y: U,
}

#[test]
fn main() {
    // This assumes that Drop implies Destruct and Copy implies Clone
    let mut a = GenericStruct { x: SimpleStruct { x: 1, y: 2 }, y: SimpleStruct { x: 1, y: 2 } };
    a.x.x = 34;
    a.y.y = 5;
    let mut serialized = Default::default();
    a.serialize(ref serialized);
    let mut as_span = serialized.span();
    let deserialized = Serde::<GenericStruct<SimpleStruct, SimpleStruct>>::deserialize(ref as_span)
        .unwrap();
    assert_eq(@a, @deserialized, 'Bad Serde');
}
