use serde::Serde;

trait Event<T> {
    // TODO(spapini): Add a "deserialize" function meant for viewing the event.
    fn append_keys_and_values(self: T, ref keys: Array<felt252>, ref values: Array<felt252>);
}
