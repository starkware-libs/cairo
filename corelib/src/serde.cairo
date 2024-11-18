//! Serialization and deserialization of types.
//!
//! This module provides a set of traits and implementations for serializing and deserializing
//! data structures in a type-safe manner.
//!
//! `Serde<T>` is the main trait that defines the serialization and deserialization behavior for a
//! type `T`.
//! Implementations of this trait are provided for tuples, and other implementations can found in
//! the corresponding modules.

#[allow(unused_imports)]
use crate::array::{ArrayTrait, SpanTrait};

/// A trait that allows for serializing and deseriaziling values of any type.
pub trait Serde<T> {
    /// Takes a snapshot of a value of any type and a referenced output `Array<felt252`, serializes
    /// the value and appends the result to the output.
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
    /// Takes a `Span<felt252>` serialized value and deserializes it.
    /// Returns an option of the deserialized result if the operation is successful, `Option::None`
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

/// `Serde` implementation for types that can be converted into `felt252` using the `Into` trait and
/// from `felt252` using the `TryInto` trait.
///
/// # Examples
///
/// ```
/// impl MyTypeSerde = core::serde::into_felt252_based::SerdeImpl<MyType>;`
/// ```
pub mod into_felt252_based {
    use crate::traits::{Into, TryInto};
    use crate::array::ArrayTrait;

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
