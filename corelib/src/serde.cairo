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

impl TupleSize1Serde<E0, +Serde<E0>> of Serde<(E0,)> {
    fn serialize(self: @(E0,), ref output: Array<felt252>) {
        let (e0,) = self;
        e0.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0,)> {
        Option::Some((Serde::deserialize(ref serialized)?,))
    }
}

impl TupleSize2Serde<E0, E1, +Serde<E0>, +Drop<E0>, +Serde<E1>, +Drop<E1>> of Serde<(E0, E1)> {
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
    E0, E1, E2, +Serde<E0>, +Drop<E0>, +Serde<E1>, +Drop<E1>, +Serde<E2>, +Drop<E2>
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
    +Serde<E0>,
    +Drop<E0>,
    +Serde<E1>,
    +Drop<E1>,
    +Serde<E2>,
    +Drop<E2>,
    +Serde<E3>,
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
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?,
                Serde::deserialize(ref serialized)?
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
    impl SerdeImpl<T, +Copy<T>, +Into<T, felt252>, +TryInto<felt252, T>> of super::Serde<T> {
        #[inline(always)]
        fn serialize(self: @T, ref output: Array<felt252>) {
            output.append((*self).into());
        }
        #[inline(always)]
        fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
            Option::Some((*serialized.pop_front()?).try_into()?)
        }
    }
}
