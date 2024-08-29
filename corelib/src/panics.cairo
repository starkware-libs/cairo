use crate::array::Array;

pub struct Panic {}

pub enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

pub extern fn panic(data: Array<felt252>) -> crate::never;

/// Panics with the given ByteArray. That is, panics with an `Array<felt252>` with
/// `BYTE_ARRAY_MAGIC`, and then the serialized given ByteArray.
#[inline]
pub fn panic_with_byte_array(err: @ByteArray) -> crate::never {
    let mut serialized = array![crate::byte_array::BYTE_ARRAY_MAGIC];
    err.serialize(ref serialized);
    panic(serialized)
}
