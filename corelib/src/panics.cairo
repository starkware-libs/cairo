use core::array::Array;

pub struct Panic {}

pub enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

pub extern fn panic(data: Array<felt252>) -> core::never;

/// Panics with the given ByteArray. That is, panics with an `Array<felt252>` with
/// `BYTE_ARRAY_MAGIC`, and then the serialized given ByteArray.
#[inline(always)]
pub fn panic_with_byte_array(err: @ByteArray) -> core::never {
    let mut serialized = array![core::byte_array::BYTE_ARRAY_MAGIC];
    err.serialize(ref serialized);
    panic(serialized)
}
