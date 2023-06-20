use serde::Serde;

pub trait Event<T> {
    fn append_keys_and_data(self: @T, ref keys: Array<felt252>, ref data: Array<felt252>);
    fn deserialize(ref keys: Span<felt252>, ref data: Span<felt252>) -> Option<T>;
}

pub trait EventEmitter<T, TEvent> {
    fn emit(ref self: T, event: TEvent);
}
