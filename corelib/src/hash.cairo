//! Generic hashing support.
//!
//! This module provides a hash state abstraction that can be updated with values and finalized to
//! produce a hash. This allows for flexible and efficient hashing of any type with different hash
//! functions.
//!
//! The simplest way to make a type hashable is to use `#[derive(Hash)]`. Hashing a value is done by
//! initiating a `HashState` corresponding to a hash function, updating it with the value, and then
//! finalizing it to get the hash result.
//!
//! # Examples
//!
//! Basic usage with Pedersen and Poseidon hash:
//!
//! ```
//! use core::pedersen::PedersenTrait;
//! use core::poseidon::PoseidonTrait;
//!
//! #[derive(Copy, Drop, Hash)]
//! struct Person {
//!     id: u32,
//!     phone: u64,
//! }
//!
//! fn main() {
//!   let person1 = Person { id: 1, phone: 555_666_7777 };
//!   let person2 = Person { id: 2, phone: 555_666_7778 };
//!
//!   assert!(
//!       PedersenTrait::new(0)
//!           .update_with(person1)
//!           .finalize() != PedersenTrait::new(0)
//!           .update_with(person2)
//!           .finalize(),
//!   );
//!   assert!(
//!       PoseidonTrait::new()
//!           .update_with(person1)
//!           .finalize() != PoseidonTrait::new()
//!           .update_with(person2)
//!           .finalize(),
//!   );
//! }
//! ```

#[allow(unused_imports)]
use crate::traits::Into;

/// A trait for hash state accumulators.
///
/// Provides methods to update a hash state with new values and finalize it into a hash result.
pub trait HashStateTrait<S> {
    /// Updates the current hash state `self` with the given `felt252` value and returns a new hash
    /// state.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    /// use core::hash::HashStateTrait;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// state = state.update(1);
    /// ```
    #[must_use]
    fn update(self: S, value: felt252) -> S;

    /// Takes the current state `self` and returns the hash result.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    /// use core::hash::HashStateTrait;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// let hash = state.finalize();
    /// ```
    #[must_use]
    fn finalize(self: S) -> felt252;
}

/// A trait for values that can be hashed.
///
/// This trait should be implemented for any type that can be included in a hash calculation.
/// The most common way to implement this trait is by using `#[derive(Hash)]`.
pub trait Hash<T, S, +HashStateTrait<S>> {
    /// Updates the hash state with the given value and returns a new hash state.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    /// use core::hash::Hash;
    ///
    /// let mut state = PedersenTrait::new(0);
    /// let new_state = Hash::update_state(state, 1);
    /// ```
    #[must_use]
    fn update_state(state: S, value: T) -> S;
}


/// A trait for hashing values using a `felt252` as hash state, used for backwards compatibility.
/// NOTE: Implement `Hash` instead of this trait if possible.
pub trait LegacyHash<T> {
    /// Takes a `felt252` state and a value of type `T` and returns the hash result.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    /// use core::hash::LegacyHash;
    ///
    /// let hash = LegacyHash::hash(0, 1);
    /// ```
    #[must_use]
    fn hash(state: felt252, value: T) -> felt252;
}

/// Implementation of `LegacyHash` for types that implement `Hash` for backwards compatibility.
impl LegacyHashForHash<T, +Hash<T, crate::pedersen::HashState>> of LegacyHash<T> {
    #[inline]
    fn hash(state: felt252, value: T) -> felt252 {
        crate::pedersen::HashState { state }.update_with(value).state
    }
}

/// Extension trait for hash state accumulators.
///
/// This trait adds the `update_with` method to hash states, allowing you to directly hash values of
/// any type T that implements `Hash`, rather than having to manually convert values to felt252
/// first. This provides a more ergonomic API when working with complex types.
pub trait HashStateExTrait<S, T> {
    /// Updates the hash state with the given value and returns the updated state.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::pedersen::PedersenTrait;
    /// use core::hash::HashStateExTrait;
    ///
    /// #[derive(Copy, Drop, Hash)]
    /// struct Point { x: u32, y: u32 }
    ///
    /// let point = Point { x: 1, y: 2 };
    /// let hash = PedersenTrait::new(0)
    ///     .update_with(point)
    ///     .update_with(42)
    ///     .finalize();
    /// ```
    #[must_use]
    fn update_with(self: S, value: T) -> S;
}

/// Implementation for `HashStateExTrait` for types that implement `Hash` for backwards
/// compatibility.
impl HashStateEx<S, +HashStateTrait<S>, T, +Hash<T, S>> of HashStateExTrait<S, T> {
    #[inline]
    fn update_with(self: S, value: T) -> S {
        Hash::update_state(self, value)
    }
}

impl HashFelt252<S, +HashStateTrait<S>> of Hash<felt252, S> {
    #[inline]
    fn update_state(state: S, value: felt252) -> S {
        state.update(value)
    }
}

/// Implementation for `Hash` for types that can be converted into `felt252` using the `Into` trait.
///
/// # Examples
///
/// ```
/// impl MyTypeHash<S, +HashStateTrait<S>, +Drop<S>> =
///     core::hash::into_felt252_based::HashImpl<MyType, S>;`
/// ```
pub mod into_felt252_based {
    pub impl HashImpl<
        T, S, +Into<T, felt252>, +super::HashStateTrait<S>, +Drop<S>,
    > of super::Hash<T, S> {
        #[inline]
        fn update_state(state: S, value: T) -> S {
            state.update(value.into())
        }
    }
}

impl HashBool<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<bool, S>;
impl HashU8<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u8, S>;
impl HashU16<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u16, S>;
impl HashU32<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u32, S>;
impl HashU64<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u64, S>;
impl HashU128<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<u128, S>;
impl HashI8<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i8, S>;
impl HashI16<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i16, S>;
impl HashI32<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i32, S>;
impl HashI64<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i64, S>;
impl HashI128<S, +HashStateTrait<S>, +Drop<S>> = into_felt252_based::HashImpl<i128, S>;

impl TupleSize0Hash<S, +HashStateTrait<S>> of Hash<(), S> {
    #[inline]
    fn update_state(state: S, value: ()) -> S {
        state
    }
}

impl FixedSizedArray0Hash<T, S, +HashStateTrait<S>, +Drop<T>> of Hash<[T; 0], S> {
    #[inline]
    fn update_state(state: S, value: [T; 0]) -> S {
        state
    }
}

impl TupleNextHash<
    T,
    S,
    +HashStateTrait<S>,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +Hash<TH::Head, S>,
    +Hash<TH::Rest, S>,
    +Drop<TH::Rest>,
> of Hash<T, S> {
    #[inline]
    fn update_state(state: S, value: T) -> S {
        let (head, rest) = TH::split_head(value);
        state.update_with(head).update_with(rest)
    }
}
