//! Serialization and deserialization of data structures.
//!
//! This module provides traits and implementations for converting Cairo types into a sequence of
//! `felt252` values (serialization) and back (deserialization).
//!
//! When passing values between Cairo and an external environment, serialization and deserialization
//! are necessary to convert Cairo's data types into a sequence of `felt252` values, as `felt252` is
//! the fundamental type of the language.
//!
//! # The `Serde` Trait
//!
//! All types that need to be serialized must implement the `Serde` trait. This includes both simple
//! types that serialize to a single `felt252` and compound types (like `u256`) that require
//! multiple `felt252` values.

#[allow(unused_imports)]
use crate::array::{ArrayTrait, SpanTrait};

/// A trait that allows for serializing and deserializing values of any type.
///
/// The `Serde<T>` trait defines two core operations:
/// - `serialize`: Converts a value into a sequence of `felt252`s
/// - `deserialize`: Reconstructs a value from a sequence of `felt252`s
///
/// # Examples
///
/// ## Simple Types (u8, u16, u32, u64, u128)
///
/// Simple types are serialized into a single `felt252`:
///
/// ```
/// let value: u8 = 42;
/// let mut output: Array<felt252> = array![];
/// value.serialize(ref output);
/// assert!(output == array![42]); // Single value
/// ```
///
/// ## Compound Types (u256)
///
/// Compound types may be serialized into multiple `felt252` values:
///
/// ```
/// let value: u256 = u256 { low: 1, high: 2 };
/// let mut output: Array<felt252> = array![];
/// value.serialize(ref output);
/// assert!(output == array![1, 2]); // Two `felt252`s: low and high
/// ```
///
/// # Implementing `Serde`
///
/// ## Using the `Derive` Macro
///
/// In most cases, you can use the `#[derive(Serde)]` attribute to automatically generate the
/// implementation for your type:
///
/// ```
/// #[derive(Serde)]
/// struct Point {
///     x: u32,
///     y: u32
/// }
/// ```
///
/// ## Manual Implementation
///
/// Should you need to customize the serialization behavior for a type in a way that derive does not
/// support, you can implement the `Serde` yourself:
///
/// ```
/// impl PointSerde of Serde<Point> {
///     fn serialize(self: @Point, ref output: Array<felt252>) {
///         output.append((*self.x).into());
///         output.append((*self.y).into());
///     }
///
///     fn deserialize(ref serialized: Span<felt252>) -> Option<Point> {
///         let x = (*serialized.pop_front()?).try_into()?;
///         let y = (*serialized.pop_front()?).try_into()?;
///
///         Some(Point { x, y })
///     }
/// }
/// ```
pub trait Serde<T> {
    /// Serializes a value into a sequence of `felt252`s.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: u256 = 1;
    /// let mut serialized: Array<felt252> = array![];
    /// value.serialize(ref serialized);
    /// assert!(serialized == array![1, 0]); // `serialized` contains the [low, high] parts of the
    /// `u256` value ```
    fn serialize(self: @T, ref output: Array<felt252>);

    /// Deserializes a value from a sequence of `felt252`s.
    /// If the value cannot be deserialized, returns `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// let mut serialized: Span<felt252> = array![1, 0].span();
    /// let value: u256 = Serde::deserialize(ref serialized).unwrap();
    /// assert!(value == 1);
    /// ```
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}


pub mod into_felt252_based {
    use crate::array::ArrayTrait;
    use crate::traits::{Into, TryInto};

    /// A generic `Serde` implementation for types that can be converted into `felt252` using the
    /// `Into` trait and from `felt252` using the `TryInto` trait.
    ///
    /// # Examples
    ///
    /// ```
    /// impl MyTypeSerde = core::serde::into_felt252_based::SerdeImpl<MyType>;
    /// ```
    pub impl SerdeImpl<T, +Copy<T>, +Into<T, felt252>, +TryInto<felt252, T>> of super::Serde<T> {
        #[inline]
        fn serialize(self: @T, ref output: Array<felt252>) {
            output.append((*self).into());
        }

        #[inline]
        fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
            Some((*serialized.pop_front()?).try_into()?)
        }
    }
}
