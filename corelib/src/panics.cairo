use array::Array;

struct Panic {}

enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

extern fn panic(data: Array<felt252>) -> never;

const BYTE_ARRAY_PANIC_MAGIC: felt252 =
    0x46a6158a16a947e5916b2a2ca68501a45e93d7110e81aa2d6438b1c57c879a3;

/// Panics with the given ByteArray. That is, panics with an `Array<felt252>` with
/// `BYTE_ARRAY_PANIC_MAGIC`, and then the serialized given ByteArray.
#[inline(always)]
fn panic_with_byte_array(err: @ByteArray) -> never {
    let mut serialized = array![BYTE_ARRAY_PANIC_MAGIC];
    err.serialize(ref serialized);
    panic(serialized)
}
