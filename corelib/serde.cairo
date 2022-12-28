fn serialize_felt(ref serialized: Array::<felt>, input: felt) {
    array_append::<felt>(serialized, input);
}

fn deserialize_felt(ref serialized: Array::<felt>) -> Option::<felt> {
    array_pop_front::<felt>(serialized)
}

fn serialize_bool(ref serialized: Array::<felt>, input: bool) {
        serialize_felt(serialized, if input {
            1
        } else {
            0
    });
}

fn deserialize_bool(ref serialized: Array::<felt>) -> Option::<bool> {
    Option::<bool>::Some(deserialize_felt(serialized)? != 0)
}

fn serialize_u128(ref serialized: Array::<felt>, input: u128) {
    serialize_felt(serialized, u128_to_felt(input));
}

fn deserialize_u128(ref serialized: Array::<felt>) -> Option::<u128> {
    u128_try_from_felt(deserialize_felt(serialized)?)
}

fn serialize_u256(ref serialized: Array::<felt>, input: u256) {
    serialize_u128(serialized, input.low);
    serialize_u128(serialized, input.high);
}

fn deserialize_u256(ref serialized: Array::<felt>) -> Option::<u256> {
    Option::<u256>::Some(
        u256 { low: deserialize_u128(serialized)?, high: deserialize_u128(serialized)?, }
    )
}

fn serialize_array_felt_helper(ref serialized: Array::<felt>, ref input: Array::<felt>) {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {
        },
        Option::None(_) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'Out of gas');
            panic(data);
        },
    }
    match array_pop_front::<felt>(input) {
        Option::Some(value) => {
            serialize_felt(serialized, value);
            serialize_array_felt_helper(serialized, input);
        },
        Option::None(_) => {
        },
    }
}

fn serialize_array_felt(ref serialized: Array::<felt>, mut input: Array::<felt>) {
    serialize_u128(serialized, array_len::<felt>(input))
    serialize_array_felt_helper(serialized, input);
}

fn deserialize_array_felt_helper(
    ref serialized: Array::<felt>, mut curr_output: Array::<felt>, remaining: felt
) -> Option::<Array::<felt>> {
    // TODO(orizi): Replace with simple call once inlining is supported.
    match get_gas() {
        Option::Some(_) => {
        },
        Option::None(_) => {
            let mut data = array_new::<felt>();
            array_append::<felt>(data, 'Out of gas');
            panic(data);
        },
    }
    if remaining == 0 {
        return Option::<Array::<felt>>::Some(curr_output);
    }
    let value = deserialize_felt(serialized)?;
    array_append::<felt>(curr_output, value);
    deserialize_array_felt_helper(serialized, curr_output, remaining - 1)
}

fn deserialize_array_felt(ref serialized: Array::<felt>) -> Option::<Array::<felt>> {
    let length = deserialize_felt(serialized)?;
    let mut arr = array_new::<felt>();
    deserialize_array_felt_helper(serialized, arr, length)
}
