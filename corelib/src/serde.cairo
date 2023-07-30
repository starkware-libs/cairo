use array::ArrayTrait;
use array::SpanTrait;
use traits::Into;
use traits::TryInto;

trait Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>);
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

impl TupleSize0Serde of Serde<()> {
    fn serialize(self: @(), ref output: Array<felt252>) {}
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Option::Some(())
    }
}

impl TupleSize1Serde<E0, impl E0Serde: Serde<E0>> of Serde<(E0,)> {
    fn serialize(self: @(E0,), ref output: Array<felt252>) {
        let (e0,) = self;
        e0.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0,)> {
        Option::Some((E0Serde::deserialize(ref serialized)?,))
    }
}

impl TupleSize2Serde<
    E0,
    E1,
    impl E0Serde: Serde<E0>,
    impl E0Drop: Drop<E0>,
    impl E1Serde: Serde<E1>,
    impl E0Drop: Drop<E1>
> of Serde<(E0, E1)> {
    fn serialize(self: @(E0, E1), ref output: Array<felt252>) {
        let (e0, e1) = self;
        e0.serialize(ref output);
        e1.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, E1)> {
        Option::Some((E0Serde::deserialize(ref serialized)?, E1Serde::deserialize(ref serialized)?))
    }
}

impl TupleSize3Serde<
    E0,
    E1,
    E2,
    impl E0Serde: Serde<E0>,
    impl E0Drop: Drop<E0>,
    impl E1Serde: Serde<E1>,
    impl E1Drop: Drop<E1>,
    impl E2Serde: Serde<E2>,
    impl E2Drop: Drop<E2>
> of Serde<(E0, E1, E2)> {
    fn serialize(self: @(E0, E1, E2), ref output: Array<felt252>) {
        let (e0, e1, e2) = self;
        e0.serialize(ref output);
        e1.serialize(ref output);
        e2.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, E1, E2)> {
        Option::Some(
            (
                E0Serde::deserialize(ref serialized)?,
                E1Serde::deserialize(ref serialized)?,
                E2Serde::deserialize(ref serialized)?
            )
        )
    }
}

impl TupleSize4Serde<
    E0,
    E1,
    E2,
    E3,
    impl E0Serde: Serde<E0>,
    impl E0Drop: Drop<E0>,
    impl E1Serde: Serde<E1>,
    impl E1Drop: Drop<E1>,
    impl E2Serde: Serde<E2>,
    impl E2Drop: Drop<E2>,
    impl E3Serde: Serde<E3>,
    impl E3Drop: Drop<E3>
> of Serde<(E0, E1, E2, E3)> {
    fn serialize(self: @(E0, E1, E2, E3), ref output: Array<felt252>) {
        let (e0, e1, e2, e3) = self;
        e0.serialize(ref output);
        e1.serialize(ref output);
        e2.serialize(ref output);
        e3.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, E1, E2, E3)> {
        Option::Some(
            (
                E0Serde::deserialize(ref serialized)?,
                E1Serde::deserialize(ref serialized)?,
                E2Serde::deserialize(ref serialized)?,
                E3Serde::deserialize(ref serialized)?
            )
        )
    }
}
