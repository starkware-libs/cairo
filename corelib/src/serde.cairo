use array::ArrayTrait;
use array::SpanTrait;
use traits::Into;
use traits::TryInto;

trait Serde<T> {
    fn serialize(ref serialized: Array<felt252>, input: T);
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

impl Felt252Serde of Serde::<felt252> {
    fn serialize(ref serialized: Array<felt252>, input: felt252) {
        serialized.append(input);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<felt252> {
        Option::Some(*serialized.pop_front()?)
    }
}

impl BoolSerde of Serde::<bool> {
    fn serialize(ref serialized: Array<felt252>, input: bool) {
        Serde::<felt252>::serialize(ref serialized, if input {
            1
        } else {
            0
        });
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<bool> {
        Option::Some(*serialized.pop_front()? != 0)
    }
}

impl U8Serde of Serde::<u8> {
    fn serialize(ref serialized: Array<felt252>, input: u8) {
        Serde::<felt252>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u8> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U16Serde of Serde::<u16> {
    fn serialize(ref serialized: Array<felt252>, input: u16) {
        Serde::<felt252>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u16> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U32Serde of Serde::<u32> {
    fn serialize(ref serialized: Array<felt252>, input: u32) {
        Serde::<felt252>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u32> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U64Serde of Serde::<u64> {
    fn serialize(ref serialized: Array<felt252>, input: u64) {
        Serde::<felt252>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u64> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U128Serde of Serde::<u128> {
    fn serialize(ref serialized: Array<felt252>, input: u128) {
        Serde::<felt252>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u128> {
        Option::Some(((*serialized.pop_front()?).try_into())?)
    }
}

impl U256Serde of Serde::<u256> {
    fn serialize(ref serialized: Array<felt252>, input: u256) {
        Serde::<u128>::serialize(ref serialized, input.low);
        Serde::<u128>::serialize(ref serialized, input.high);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<u256> {
        Option::Some(
            u256 {
                low: Serde::<u128>::deserialize(ref serialized)?,
                high: Serde::<u128>::deserialize(ref serialized)?,
            }
        )
    }
}

impl ArraySerde<T, impl TSerde: Serde::<T>, impl TDrop: Drop::<T>> of Serde::<Array::<T>> {
    fn serialize(ref serialized: Array<felt252>, mut input: Array<T>) {
        Serde::<usize>::serialize(ref serialized, input.len());
        serialize_array_helper(ref serialized, input);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Array<T>> {
        let length = *serialized.pop_front()?;
        let mut arr = ArrayTrait::new();
        deserialize_array_helper(ref serialized, arr, length)
    }
}

fn serialize_array_helper<T, impl TSerde: Serde::<T>, impl TDrop: Drop::<T>>(
    ref serialized: Array<felt252>, mut input: Array<T>
) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::withdraw_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    match input.pop_front() {
        Option::Some(value) => {
            TSerde::serialize(ref serialized, value);
            serialize_array_helper(ref serialized, input);
        },
        Option::None(_) => {},
    }
}

fn deserialize_array_helper<T, impl TSerde: Serde::<T>, impl TDrop: Drop::<T>>(
    ref serialized: Span<felt252>, mut curr_output: Array<T>, remaining: felt252
) -> Option<Array<T>> {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::withdraw_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    if remaining == 0 {
        return Option::Some(curr_output);
    }
    curr_output.append(TSerde::deserialize(ref serialized)?);
    deserialize_array_helper(ref serialized, curr_output, remaining - 1)
}

impl TupleSize0Serde of Serde::<()> {
    fn serialize(ref serialized: Array<felt252>, mut input: ()) {}
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Option::Some(())
    }
}

impl TupleSize1Serde<E0, impl E0Serde: Serde::<E0>> of Serde::<(E0, )> {
    fn serialize(ref serialized: Array<felt252>, mut input: (E0, )) {
        let (e0, ) = input;
        E0Serde::serialize(ref serialized, e0)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, )> {
        Option::Some((E0Serde::deserialize(ref serialized)?, ))
    }
}

impl TupleSize2Serde<E0,
E1,
impl E0Serde: Serde::<E0>,
impl E0Drop: Drop::<E0>,
impl E1Serde: Serde::<E1>> of Serde::<(E0, E1)> {
    fn serialize(ref serialized: Array<felt252>, mut input: (E0, E1)) {
        let (e0, e1) = input;
        E0Serde::serialize(ref serialized, e0);
        E1Serde::serialize(ref serialized, e1)
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<(E0, E1)> {
        Option::Some((E0Serde::deserialize(ref serialized)?, E1Serde::deserialize(ref serialized)?))
    }
}

impl TupleSize3Serde<E0,
E1,
E2,
impl E0Serde: Serde::<E0>,
impl E0Drop: Drop::<E0>,
impl E1Serde: Serde::<E1>,
impl E1Drop: Drop::<E1>,
impl E2Serde: Serde::<E2>> of Serde::<(E0, E1, E2)> {
    fn serialize(ref serialized: Array<felt252>, mut input: (E0, E1, E2)) {
        let (e0, e1, e2) = input;
        E0Serde::serialize(ref serialized, e0);
        E1Serde::serialize(ref serialized, e1);
        E2Serde::serialize(ref serialized, e2)
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

impl TupleSize4Serde<E0,
E1,
E2,
E3,
impl E0Serde: Serde::<E0>,
impl E0Drop: Drop::<E0>,
impl E1Serde: Serde::<E1>,
impl E1Drop: Drop::<E1>,
impl E2Serde: Serde::<E2>,
impl E2Drop: Drop::<E2>,
impl E3Serde: Serde::<E3>> of Serde::<(E0, E1, E2, E3)> {
    fn serialize(ref serialized: Array<felt252>, mut input: (E0, E1, E2, E3)) {
        let (e0, e1, e2, e3) = input;
        E0Serde::serialize(ref serialized, e0);
        E1Serde::serialize(ref serialized, e1);
        E2Serde::serialize(ref serialized, e2);
        E3Serde::serialize(ref serialized, e3)
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

