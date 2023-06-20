use array::Array;

pub struct Panic {}

pub enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

pub extern fn panic(data: Array<felt252>) -> never;
