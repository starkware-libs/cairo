use queue::QueueTrait;
use traits::Into;
use traits::TryInto;

trait Serde<T> {
    fn serialize(ref serialized: Queue::<felt>, input: T);
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<T>;
}

impl FeltSerde of Serde::<felt> {
    fn serialize(ref serialized: Queue::<felt>, input: felt) {
        serialized.append(input);
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<felt> {
        serialized.pop_front()
    }
}

impl BoolSerde of Serde::<bool> {
    fn serialize(ref serialized: Queue::<felt>, input: bool) {
        Serde::<felt>::serialize(ref serialized, if input {
            1
        } else {
            0
        });
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<bool> {
        Option::Some(Serde::<felt>::deserialize(ref serialized)? != 0)
    }
}

impl U8Serde of Serde::<u8> {
    fn serialize(ref serialized: Queue::<felt>, input: u8) {
        Serde::<felt>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<u8> {
        Option::Some((Serde::<felt>::deserialize(ref serialized)?.try_into())?)
    }
}

impl U32Serde of Serde::<u32> {
    fn serialize(ref serialized: Queue::<felt>, input: u32) {
        Serde::<felt>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<u32> {
        Option::Some((Serde::<felt>::deserialize(ref serialized)?.try_into())?)
    }
}

impl U64Serde of Serde::<u64> {
    fn serialize(ref serialized: Queue::<felt>, input: u64) {
        Serde::<felt>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<u64> {
        Option::Some((Serde::<felt>::deserialize(ref serialized)?.try_into())?)
    }
}

impl U128Serde of Serde::<u128> {
    fn serialize(ref serialized: Queue::<felt>, input: u128) {
        Serde::<felt>::serialize(ref serialized, input.into());
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<u128> {
        Option::Some((Serde::<felt>::deserialize(ref serialized)?.try_into())?)
    }
}

impl U256Serde of Serde::<u256> {
    fn serialize(ref serialized: Queue::<felt>, input: u256) {
        Serde::<u128>::serialize(ref serialized, input.low);
        Serde::<u128>::serialize(ref serialized, input.high);
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<u256> {
        Option::Some(
            u256 {
                low: Serde::<u128>::deserialize(ref serialized)?,
                high: Serde::<u128>::deserialize(ref serialized)?,
            }
        )
    }
}

impl QueueFeltSerde of Serde::<Queue::<felt>> {
    fn serialize(ref serialized: Queue::<felt>, mut input: Queue::<felt>) {
        Serde::<usize>::serialize(ref serialized, input.len());
        serialize_queue_felt_helper(ref serialized, ref input);
    }
    fn deserialize(ref serialized: Queue::<felt>) -> Option::<Queue::<felt>> {
        let length = Serde::<felt>::deserialize(ref serialized)?;
        let mut q = QueueTrait::new();
        deserialize_queue_felt_helper(ref serialized, q, length)
    }
}

fn serialize_queue_felt_helper(ref serialized: Queue::<felt>, ref input: Queue::<felt>) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = QueueTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    match input.pop_front() {
        Option::Some(value) => {
            Serde::<felt>::serialize(ref serialized, value);
            serialize_queue_felt_helper(ref serialized, ref input);
        },
        Option::None(_) => {},
    }
}

fn deserialize_queue_felt_helper(
    ref serialized: Queue::<felt>, mut curr_output: Queue::<felt>, remaining: felt
) -> Option::<Queue::<felt>> {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = QueueTrait::new();
            data.append('Out of gas');
            panic(data);
        },
    }
    if remaining == 0 {
        return Option::<Queue::<felt>>::Some(curr_output);
    }
    curr_output.append(Serde::<felt>::deserialize(ref serialized)?);
    deserialize_queue_felt_helper(ref serialized, curr_output, remaining - 1)
}
