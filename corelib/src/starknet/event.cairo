use serde::Serde;

trait Event<T> {
    fn append_keys_and_values(self: T, ref keys: Array<felt252>, ref values: Array<felt252>);
    fn deserialize(ref keys: Span<felt252>, ref values: Span<felt252>) -> Option<T>;
}
