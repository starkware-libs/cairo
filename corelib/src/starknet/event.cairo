//! Event handling traits for Starknet smart contracts.
//!
//! This module provides traits for serializing, deserializing and emitting events on Starknet.
//! The [`Event`] trait handles the serialization of event types, while the [`EventEmitter`] trait
//! provides the capability to emit events from Starknet contracts.

/// A trait for handling serialization and deserialization of events.
///
/// Events in Starknet are stored in transaction receipts as a combination of keys and data fields.
/// This trait provides the methods needed to serialize event data into these fields and deserialize
/// them back into their original form.
///
/// This trait can easily be derived using the `#[derive(starknet::Event)]` attribute.
/// Fields can be marked as keys using the `#[key]` attribute to serialize them as event keys.
///
/// # Examples
///
/// ```
/// #[derive(Drop, starknet::Event)]
/// pub struct Transfer {
///     #[key]
///     pub from: ContractAddress,
///     #[key]
///     pub to: ContractAddress,
///     pub amount: u256,
/// }
/// ```
pub trait Event<T> {
    /// Serializes the keys and data for event emission.
    ///
    /// The keys array will contain:
    /// - The event name selector as the first key
    /// - Any fields marked with #[key] as subsequent keys
    ///
    /// The data array will contain all non-key fields.
    fn append_keys_and_data(self: @T, ref keys: Array<felt252>, ref data: Array<felt252>);

    /// Deserializes events keys and data back into the original event structure.
    ///
    /// Returns `None` if deserialization fails.
    fn deserialize(ref keys: Span<felt252>, ref data: Span<felt252>) -> Option<T>;
}

/// A trait for emitting Starknet events.
///
/// # Examples
///
/// ```
/// #[derive(Drop, starknet::Event)]
/// pub struct NewOwner {
///     pub new_owner: ContractAddress,
/// }
///
/// fn emit_event(ref self: ContractState, new_owner: ContractAddress) {
///     self.emit(NewOwner { new_owner });
/// }
/// ```
pub trait EventEmitter<T, TEvent> {
    /// Emits an event.
    fn emit<S, +Into<S, TEvent>>(ref self: T, event: S);
}
