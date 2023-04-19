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

impl TupleSize1Serde<E0, impl Serde<E0>> of Serde<(E0,)> {
    fn serialize(self: @(E0,), ref output: Array<felt252>) {
        let (e0,) = self;
        e0.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0,)> {
        Option::Some((Serde::deserialize(ref serialized)?,))
    }
}

impl TupleSize2Serde<
    E0, E1, impl Serde<E0>, impl Drop<E0>, impl Serde<E1>, impl Drop<E1>
> of Serde<(E0, E1)> {
    fn serialize(self: @(E0, E1), ref output: Array<felt252>) {
        let (e0, e1) = self;
        e0.serialize(ref output);
        e1.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, E1)> {
        Option::Some((Serde::deserialize(ref serialized)?, Serde::deserialize(ref serialized)?))
    }
}

impl TupleSize3Serde<
    E0,
    E1,
    E2,
    impl Serde<E0>,
    impl Drop<E0>,
    impl Serde<E1>,
    impl Drop<E1>,
    impl Serde<E2>,
    impl Drop<E2>
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
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?
            )
        )
    }
}

impl TupleSize4Serde<
    E0,
    E1,
    E2,
    E3,
    impl Serde<E0>,
    impl Drop<E0>,
    impl Serde<E1>,
    impl Drop<E1>,
    impl Serde<E2>,
    impl Drop<E2>,
    impl Serde<E3>,
    impl Drop<E3>
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
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?
            )
        )
    }
}
