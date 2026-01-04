// SPDX-License-Identifier: MIT
// OpenZeppelin Contracts for Cairo v2.0.0 (utils/src/serde.cairo)

pub trait SerializedAppend<T> {
    fn append_serde(ref self: Array<felt252>, value: T);
}

impl SerializedAppendImpl<T, impl TSerde: Serde<T>, impl TDrop: Drop<T>> of SerializedAppend<T> {
    fn append_serde(ref self: Array<felt252>, value: T) {
        value.serialize(ref self);
    }
}
