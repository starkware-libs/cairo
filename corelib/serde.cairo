trait Serde<T> {
    fn serialize(ref serialized: Array::<felt>, input: T);
    fn deserialize(ref serialized: Array::<felt>) -> Option::<T>;
}

impl FeltSerde of Serde::<felt> {
    fn serialize(ref serialized: Array::<felt>, input: felt) {
        array_append(ref serialized, input);
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<felt> {
        array_pop_front(ref serialized)
    }
}

impl BoolSerde of Serde::<bool> {
    fn serialize(ref serialized: Array::<felt>, input: bool) {
            Serde::<felt>::serialize(ref serialized, if input {
                1
            } else {
                0
        });
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<bool> {
        Option::Some(Serde::<felt>::deserialize(ref serialized)? != 0)
    }
}

impl U128Serde of Serde::<u128> {
    fn serialize(ref serialized: Array::<felt>, input: u128) {
        Serde::<felt>::serialize(ref serialized, u128_to_felt(input));
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<u128> {
        Option::Some(u128_try_from_felt(Serde::<felt>::deserialize(ref serialized)?)?)
    }
}

impl U256Serde of Serde::<u256> {
    fn serialize(ref serialized: Array::<felt>, input: u256) {
        Serde::<u128>::serialize(ref serialized, input.low);
        Serde::<u128>::serialize(ref serialized, input.high);
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<u256> {
        Option::Some(
            u256 {
                low: Serde::<u128>::deserialize(ref serialized)?,
                high: Serde::<u128>::deserialize(ref serialized)?,
            }
        )
    }
}

impl ArrayFeltSerde of Serde::<Array::<felt>> {
    fn serialize(ref serialized: Array::<felt>, mut input: Array::<felt>) {
        Serde::<u128>::serialize(ref serialized, array_len(ref input))
        serialize_array_felt_helper(ref serialized, ref input);
    }
    fn deserialize(ref serialized: Array::<felt>) -> Option::<Array::<felt>> {
        let length = Serde::<felt>::deserialize(ref serialized)?;
        let mut arr = array_new::<felt>();
        deserialize_array_felt_helper(ref serialized, arr, length)
    }
}

fn serialize_array_felt_helper(ref serialized: Array::<felt>, ref input: Array::<felt>) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {
        },
        Option::None(_) => {
            let mut data = array_new();
            array_append(ref data, 'Out of gas');
            panic(data);
        },
    }
    match array_pop_front(ref input) {
        Option::Some(value) => {
            Serde::<felt>::serialize(ref serialized, value);
            serialize_array_felt_helper(ref serialized, ref input);
        },
        Option::None(_) => {
        },
    }
}

fn deserialize_array_felt_helper(
    ref serialized: Array::<felt>, mut curr_output: Array::<felt>, remaining: felt
) -> Option::<Array::<felt>> {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {
        },
        Option::None(_) => {
            let mut data = array_new();
            array_append(ref data, 'Out of gas');
            panic(data);
        },
    }
    if remaining == 0 {
        return Option::<Array::<felt>>::Some(curr_output);
    }
    array_append(ref curr_output, Serde::<felt>::deserialize(ref serialized)?);
    deserialize_array_felt_helper(ref serialized, curr_output, remaining - 1)
}
