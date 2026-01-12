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
/// # Performance
///
/// Serialization and deserialization have O(n) complexity for structs, where n is the number of
/// fields.
/// For enums, deserialization has O(m) complexity, where m is the number of variants, as it may
/// need to check multiple variants before finding a match.
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
    /// Returns `None` if deserialization fails. This can happen when:
    /// - The keys or data arrays don't contain enough elements
    /// - The event selector (first key) doesn't match the expected event type (for enums)
    /// - Any field deserialization fails (e.g., invalid type conversion)
    ///
    /// # Examples
    ///
    /// ```
    /// let (mut keys, mut data) = starknet::testing::pop_log_raw(contract_address)?;
    /// match starknet::Event::deserialize(ref keys, ref data) {
    ///     Option::Some(event) => { /* handle event */ },
    ///     Option::None => { /* deserialization failed */ },
    /// }
    /// ```
    fn deserialize(ref keys: Span<felt252>, ref data: Span<felt252>) -> Option<T>;
}

/// A trait for emitting Starknet events.
///
/// The generic parameter `T` represents the contract state type (typically `ContractState`),
/// and `TEvent` represents the event type (typically an enum named `Event`).
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
///
/// The generic parameter `S` with bound `+Into<S, TEvent>` allows passing either
/// the event type directly or any type that can be converted into it (e.g., enum variants).
pub trait EventEmitter<T, TEvent> {
    /// Emits an event.
    ///
    /// The `S` parameter allows passing the event directly or any type convertible to `TEvent`
    /// via the `Into` trait. This enables flexible usage patterns like emitting enum variants
    /// directly.
    ///
    /// # Panics
    ///
    /// This method will panic if the underlying system call fails.
    fn emit<S, +Into<S, TEvent>>(ref self: T, event: S);
}
