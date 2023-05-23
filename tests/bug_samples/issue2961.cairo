use serde::Serde;
use traits::PartialEq;
use clone::Clone;
use array::ArrayTrait;
use option::OptionTrait;
use test::test_utils::{assert_eq, assert_ne};

#[derive(Copy, PartialEq, Destruct, Serde)]
struct SimpleStruct {
    a: felt252
}

#[test]
fn test_struct_serialization() {
    let data = SimpleStruct { a: 1 };
    let mut raw_data = ArrayTrait::new();
    data.serialize(ref raw_data);
    let mut as_span = raw_data.span();
    let deserd = Serde::<SimpleStruct>::deserialize(ref as_span).unwrap();
    assert_eq(@data, @deserd, 'Bad deserialization');
}

#[derive(Clone, PartialEq, Drop, Serde)]
enum SimpleEnum {
    a: felt252,
    b: felt252
}

#[test]
fn test_enum_serialization() {
    let data = SimpleEnum::b(59);
    let mut raw_data = ArrayTrait::new();
    data.serialize(ref raw_data);
    let mut as_span = raw_data.span();
    let deserd = Serde::<SimpleEnum>::deserialize(ref as_span).unwrap();
    assert_eq(@data, @deserd, 'Bad deserialization');
}
