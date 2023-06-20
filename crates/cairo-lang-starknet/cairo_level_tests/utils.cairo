use array::ArrayTrait;
use serde::Serde;
use option::OptionTrait;

fn serialized_element<T, impl TSerde: serde::Serde<T>, impl TDestruct: Destruct<T>>(
    value: T
) -> Span<felt252> {
    let mut arr = Default::default();
    value.serialize(ref arr);
    arr.span()
}

fn single_deserialize<T, impl TSerde: serde::Serde<T>>(ref data: Span::<felt252>) -> T {
    serde::Serde::deserialize(ref data).expect('missing data')
}
