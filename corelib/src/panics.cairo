use array::Array;

struct Panic {}

enum PanicResult<T> {
    Ok: T,
    Err: (Panic, Array<felt252>),
}

extern fn panic(data: Array<felt252>) -> never;
