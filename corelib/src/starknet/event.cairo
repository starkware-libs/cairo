//! Event handling traits for Starknet smart contracts.
//!
//! This module provides traits for serializing, deserializing and emitting events
//! on the Starknet network. The [`Event`] trait handles the serialization format,
//! while [`EventEmitter`] trait provides the capability to emit events from Starknet contracts.

/// A trait for handling serialization and deserialization of events.
pub trait Event<T> {
    /// Serializes the keys and data for event emission.
    fn append_keys_and_data(self: @T, ref keys: Array<felt252>, ref data: Array<felt252>);

    /// Deserializes events keys and data.
    fn deserialize(ref keys: Span<felt252>, ref data: Span<felt252>) -> Option<T>;
}

/// A trait for emitting Starknet events.
pub trait EventEmitter<T, TEvent> {
    /// Emits an event.
    fn emit<S, +Into<S, TEvent>>(ref self: T, event: S);
}
