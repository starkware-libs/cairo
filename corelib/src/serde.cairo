use array::ArrayTrait;
use array::SpanTrait;

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
    E0, E1, impl E0Serde: Serde<E0>, +Drop<E0>, impl E1Serde: Serde<E1>, +Drop<E1>
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
    +Drop<E0>,
    impl E1Serde: Serde<E1>,
    +Drop<E1>,
    impl E2Serde: Serde<E2>,
    +Drop<E2>
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
    +Drop<E0>,
    impl E1Serde: Serde<E1>,
    +Drop<E1>,
    impl E2Serde: Serde<E2>,
    +Drop<E2>,
    impl E3Serde: Serde<E3>,
    +Drop<E3>
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

/// Impl for `Serde` for types that can be converted into `felt252` using the `Into` trait and from `felt252` using the `TryInto` trait.
/// Usage example:
/// ```ignore
/// impl MyTypeSerde = core::serde::into_felt252_based::SerdeImpl<MyType>;`
/// ```
mod into_felt252_based {
    use traits::{Into, TryInto};
    use core::array::ArrayTrait;
    impl SerdeImpl<
        T, +Copy<T>, impl TIntoFelt252: Into<T, felt252>, impl Felt252TryIntoT: TryInto<felt252, T>
    > of super::Serde<T> {
        #[inline(always)]
        fn serialize(self: @T, ref output: Array<felt252>) {
            output.append(TIntoFelt252::into(*self));
        }
        #[inline(always)]
        fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
            Option::Some((Felt252TryIntoT::try_into(*serialized.pop_front()?))?)
        }
    }
}
