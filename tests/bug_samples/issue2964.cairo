use serde::Serde;
use clone::Clone;
use array::ArrayTrait;
use option::OptionTrait;

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

// TODO(spapini): This doesn't actually work, fails with 'unknown literal' on U
//#[derive(Drop)]
struct GenericStructConst<T, const U: felt252> {
    x: T, 
}

#[test]
fn main() {
    // This assumes that Drop implies Destruct and Copy implies Clone
    let mut a = GenericStruct { x: SimpleStruct { x: 1, y: 2 }, y: SimpleStruct { x: 1, y: 2 } };
    a.x.x = 34;
    a.y.y = 5;
    let mut serialized = ArrayTrait::<felt252>::new();
    serde::Serde::serialize(ref serialized, a);
    let mut as_span = serialized.span();
    let deserialized = serde::Serde::<GenericStruct<SimpleStruct,
    SimpleStruct>>::deserialize(ref as_span).unwrap();
    assert(a == deserialized, 'Bad Serde');
}
