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
