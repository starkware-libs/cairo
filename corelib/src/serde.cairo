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

impl ArrayFelt252Serde of Serde::<Array::<felt252>> {
    fn serialize(ref serialized: Array<felt252>, mut input: Array<felt252>) {
        Serde::<usize>::serialize(ref serialized, input.len());
        serialize_array_felt252_helper(ref serialized, input);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Array<felt252>> {
        let length = *serialized.pop_front()?;
        let mut arr = ArrayTrait::new();
        deserialize_array_felt252_helper(ref serialized, arr, length)
    }
}

fn serialize_array_felt252_helper(ref serialized: Array<felt252>, mut input: Array<felt252>) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    match input.pop_front() {
        Option::Some(value) => {
            Serde::<felt252>::serialize(ref serialized, value);
            serialize_array_felt252_helper(ref serialized, input);
        },
        Option::None(_) => {},
    }
}

fn deserialize_array_felt252_helper(
    ref serialized: Span<felt252>, mut curr_output: Array<felt252>, remaining: felt252
) -> Option<Array<felt252>> {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match gas::get_gas() {
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
    curr_output.append(*serialized.pop_front()?);
    deserialize_array_felt252_helper(ref serialized, curr_output, remaining - 1)
}
