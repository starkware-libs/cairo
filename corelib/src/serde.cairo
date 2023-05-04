use array::ArrayTrait;
use array::SpanTrait;
use traits::Into;
use traits::TryInto;

trait Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>);
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

impl Felt252Serde of Serde<felt252> {
    fn serialize(self: @felt252, ref output: Array<felt252>) {
        output.append(*self);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<felt252> {
        Option::Some(*serialized.pop_front()?)
    }
}

impl BoolSerde of Serde<bool> {
    fn serialize(self: @bool, ref output: Array<felt252>) {
        if *self {
            1
        } else {
            0
        }.serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<bool> {
        Option::Some(*serialized.pop_front()? != 0)
    }
}

impl U8Serde of Serde<u8> {
    fn serialize(self: @u8, ref output: Array<felt252>) {
        Into::<u8, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u8> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U16Serde of Serde<u16> {
    fn serialize(self: @u16, ref output: Array<felt252>) {
        Into::<u16, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u16> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U32Serde of Serde<u32> {
    fn serialize(self: @u32, ref output: Array<felt252>) {
        Into::<u32, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u32> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U64Serde of Serde<u64> {
    fn serialize(self: @u64, ref output: Array<felt252>) {
        Into::<u64, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u64> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U128Serde of Serde<u128> {
    fn serialize(self: @u128, ref output: Array<felt252>) {
        Into::<u128, felt252>::into(*self).serialize(ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u128> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl OptionSerde<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>> of Serde<Option<T>> {
    fn serialize(self: @Option<T>, ref output: Array<felt252>) {
        match self {
            Option::Some(x) => {
                0.serialize(ref output);
                x.serialize(ref output)
            },
            Option::None(()) => 1.serialize(ref output),
        }
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Option<T>> {
        let variant = *serialized.pop_front()?;
        if variant == 0 {
            Option::Some(Option::Some(Serde::<T>::deserialize(ref serialized)?))
        } else if variant == 1 {
            Option::Some(Option::None(()))
        } else {
            Option::None(())
        }
    }
}


impl ArraySerde<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>> of Serde<Array<T>> {
    fn serialize(self: @Array<T>, ref output: Array<felt252>) {
        self.len().serialize(ref output);
        serialize_array_helper(self.span(), ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Array<T>> {
        let length = *serialized.pop_front()?;
        let mut arr = ArrayTrait::new();
        deserialize_array_helper(ref serialized, arr, length)
    }
}

fn serialize_array_helper<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>>(
    mut input: Span<T>, ref output: Array<felt252>
) {
    match input.pop_front() {
        Option::Some(value) => {
            value.serialize(ref output);
            serialize_array_helper(input, ref output);
        },
        Option::None(_) => {},
    }
}

fn deserialize_array_helper<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>>(
    ref serialized: Span<felt252>, mut curr_output: Array<T>, remaining: felt252
) -> Option<Array<T>> {
    if remaining == 0 {
        return Option::Some(curr_output);
    }
    curr_output.append(TSerde::deserialize(ref serialized)?);
    deserialize_array_helper(ref serialized, curr_output, remaining - 1)
}

impl TupleSize0Serde of Serde<()> {
    fn serialize(self: @(), ref output: Array<felt252>) {}
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Option::Some(())
    }
}

impl TupleSize1Serde<E0, impl E0Serde: Serde<E0>> of Serde<(E0, )> {
    fn serialize(self: @(E0, ), ref output: Array<felt252>) {
        let (e0, ) = self;
        e0.serialize(ref output)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, )> {
        Option::Some((E0Serde::deserialize(ref serialized)?, ))
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
