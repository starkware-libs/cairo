//! Serialization and deserialization of types.
//!
//! This module provides traits and implementations for converting Cairo types into a sequence
//! of `felt252` values (serialization) and back (deserialization).
//!
//! When passing values between Cairo and an external environment, serialization and deserialization
//! are necessary to convert Cairo's data types into a format that can be transmitted and vice
//! versa.
//!
//! Cairo's native types like `u256` are serialized into multiple `felt252` values, while custom
//! types are serialized by implementing the `Serde` trait.
//!
//! Implementations of this trait are provided for tuples in this module, and other implementations
//! can be found in the corresponding modules.

#[allow(unused_imports)]
use crate::array::{ArrayTrait, SpanTrait};

//! A trait that allows for serializing and deserializing values of any type.
//!
//! The `Serde<T>` trait defines two core operations:
//! - `serialize`: Converts a value into a sequence of `felt252`s
//! - `deserialize`: Reconstructs a value from a sequence of `felt252`s
//!
//! # Examples
//!
//! ## Simple Types (u8, u16, u32, u64, u128)
//!
//! Simple types are serialized into a single `felt252`:
//!
//! ```
//! let value: u8 = 42;
//! let mut output: Array<felt252> = array![];
//! value.serialize(ref output);
//! assert!(output == array![42]); // Single felt252
//! ```
//!
//! ## Complex Types (u256)
//!
//! Complex types may require multiple felt252s:
//!
//! ```
//! let value: u256 = u256 { low: 1, high: 2 };
//! let mut output: Array<felt252> = array![];
//! value.serialize(ref output);
//! assert!(output == array![1, 2]); // Two `felt252`s: low and high
//! ```
//!
//! # Implementing `Serde`
//!
//! ## Using the `Derive` Macro
//!
//! For structs and enums, you can use the `#[derive(Serde)]` attribute:
//!
//! ```
//! #[derive(Serde)]
//! struct Point { x: u32, y: u32 }
//! ```
//!
//! ## Manual Implementation
//!
//! You can implement `Serde` manually for custom types:
//!
//! ```
//! impl CustomTypeSerde of Serde<CustomType> {
//!     fn serialize(self: @CustomType, ref output: Array<felt252>) {
//!         // Convert your type to `felt252`(s) and append to output
//!     }
//!
//!     fn deserialize(ref serialized: Span<felt252>) -> Option<CustomType> {
//!         // Reconstruct your type from `felt252`s
//!     }
//! }
//! ```
pub trait Serde<T> {
    /// Serializes a value into a sequence of `felt252`s.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: u256 = 1;
    /// let mut output: Array<felt252> = array![];
    /// value.serialize(ref output);
    /// assert!(output == array![1, 0]) // `output` contains low and high parts of the `u256` value
    /// ```
    fn serialize(self: @T, ref output: Array<felt252>);

    /// Deserializes a value from a sequence of `felt252`s.
    /// If the value cannot be deserialized, returns `Option::None`.
    /// otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut span: Span<felt252> = array![1, 0].span();
    /// let value:  u256 = Serde::deserialize(ref span).unwrap();
    /// assert!(value == 1);
    /// ```
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

impl SerdeTuple<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    impl Serialize: SerializeTuple<TSF::SnapForward>,
    impl Deserialize: DeserializeTuple<T>,
> of Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>) {
        Serialize::serialize(TSF::snap_forward(self), ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        Deserialize::deserialize(ref serialized)
    }
}

// Helper trait for serializing tuple style structs.
trait SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>);
}

impl SerdeBasedSerializeTuple<T, +Serde<T>> of SerializeTuple<@T> {
    fn serialize(value: @T, ref output: Array<felt252>) {
        Serde::<T>::serialize(value, ref output);
    }
}

trait DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

impl SerializeTupleBaseTuple of SerializeTuple<()> {
    fn serialize(value: (), ref output: Array<felt252>) {}
}

impl DeserializeTupleBaseTuple of DeserializeTuple<()> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Option::Some(())
    }
}

impl SerializeTupleBaseFixedSizedArray<T> of SerializeTuple<[@T; 0]> {
    fn serialize(value: [@T; 0], ref output: Array<felt252>) {}
}

impl DeserializeTupleBaseFixedSizedArray<T> of DeserializeTuple<[T; 0]> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<[T; 0]> {
        Option::Some([])
    }
}

impl SerializeTupleNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +SerializeTuple<TS::Head>,
    +SerializeTuple<TS::Rest>,
    +Drop<TS::Rest>,
> of SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>) {
        let (head, rest) = TS::split_head(value);
        SerializeTuple::<TS::Head>::serialize(head, ref output);
        SerializeTuple::<TS::Rest>::serialize(rest, ref output);
    }
}

impl DeserializeTupleNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +Serde<TS::Head>,
    +DeserializeTuple<TS::Rest>,
    +Drop<TS::Head>,
> of DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        let head = Serde::<TS::Head>::deserialize(ref serialized)?;
        let rest = DeserializeTuple::<TS::Rest>::deserialize(ref serialized)?;
        Option::Some(TS::reconstruct(head, rest))
    }
}

pub mod into_felt252_based {
    use crate::traits::{Into, TryInto};
    use crate::array::ArrayTrait;

    /// A generic `Serde` implementation for types that can be converted into `felt252` using the `Into` trait
    /// and from `felt252` using the `TryInto` trait.
    /// ```
    pub impl SerdeImpl<T, +Copy<T>, +Into<T, felt252>, +TryInto<felt252, T>> of super::Serde<T> {
        #[inline]
        fn serialize(self: @T, ref output: Array<felt252>) {
            output.append((*self).into());
        }

        #[inline]
        fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
            Option::Some((*serialized.pop_front()?).try_into()?)
        }
    }
}
