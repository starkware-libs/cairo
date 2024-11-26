//! A dictionary-like data structure that maps `felt252` keys to values of any type. 
//!
//! The `Felt252Dict` provides efficient key-value storage with operations for inserting,
//! retrieving, and updating values. Each operation creates a new entry that can be validated
//! through a process called squashing.
//!
//! Due to Cairo's immutable memory model, dictionaries are implemented as an append-only list
//! of dictionary access entries. Each entry is a triple of:
//! - `key`: The dictionary key being accessed (`felt252`)
//! - `prev_value`: The previous value associated with this key
//! - `new_value`: The new value being associated with this key
//!
//! Every dictionary operation (insert, get, etc.) creates a new entry:
//! - An `insert(k, v)` creates an entry with the previous value and `v` as the new value
//! - A `get(k)` creates an entry where `prev_value` equals `new_value`
//!
//! When a dictionary goes out of scope, it undergoes a process called "squashing" that validates
//! the consistency of all accesses. For any key k, if we have multiple entries:
//! - The `new_value` of entry `i` must equal the `prev_value` of entry `i+1`
//! - The squashed result contains one entry per key, with:
//!   - `prev_value`: The value before the first access
//!   - `new_value`: The value after the last access
//!
//! # Examples
//!
//! One can create a new dictionary using the [`Default::default`] method:
//!
//! ```
//! use core::dict::Felt252Dict;
//!
//! let mut dict: Felt252Dict<u8> = Default::default();
//! ```
//!
//! ... then insert new values corresponding to a given key with the [`Felt252DictTrait::insert`]
//! method, and retrieve any value given a key with the [`Felt252DictTrait::get`] method.
//!
//! ```
//! dict.insert(0, 10);
//! dict.insert(1, 20);
//! assert!(dict.get(0) == 10);
//! assert!(dict.get(1) == 20);
//!
//! dict.insert(0, 20);
//! assert!(dict.get(0) == )
//! ```
//!
//! It also possible to use the [`Felt252DictTrait::entry`] method to retrieve the last entry given
//! a certain key.
//! In this case, the method takes ownership of the dictionary and returns the entry to update.
//! After that, using the [`Felt252DictEntryTrait::finalize`] allows to create a new entry in the
//! dictionary.
//!
//! ```
//! use core::dict::Felt252Dict;
//!
//! let mut dict: Felt252Dict<u8> = Default::default();
//! dict.insert(0, 10);
//!
//! let (entry, prev_value) = dict.entry(0);
//! let new_value: u8 = 20;
//! dict = entry.finalize(new_value);
//! ```
//!
//! The process of squashing is as follows: given all entries with certain key k, taken in the same
//! order as they were inserted, verify that the ith entry `new_value` is equal to the ith + 1 entry
//! `prev_value`.
//! Squashing is done automatically right before a dictionary goes out scope via the
//! `Felt252Dict<T>` implementation of the `Destruct` trait.
//!
//! However, it is still possible to manually squash a dictionary using the
//! `Felt252DictTrait::squash` method,
// !preventing any future usage of the dictionary.

#[feature("deprecated-index-traits")]
use crate::traits::{Index, Default, Felt252DictValue};

/// A dictionary that maps `felt252` keys to a value of any type.
pub extern type Felt252Dict<T>;

/// A squashed dictionary that cannot be used to insert or retrieve values associated with a
/// `felt252` key.
pub extern type SquashedFelt252Dict<T>;

/// A type that is returned after calling `entry` method, on which it is possible to call `finalize`
/// method in order to regain ownership of a dictionary after adding a new entry.
pub extern type Felt252DictEntry<T>;

impl SquashedFelt252DictDrop<T, +Drop<T>> of Drop<SquashedFelt252Dict<T>>;
use crate::{RangeCheck, SegmentArena};
use crate::gas::GasBuiltin;

pub(crate) extern fn felt252_dict_new<T>() -> Felt252Dict<T> implicits(SegmentArena) nopanic;

extern fn felt252_dict_entry_get<T>(
    dict: Felt252Dict<T>, key: felt252
) -> (Felt252DictEntry<T>, T) nopanic;

extern fn felt252_dict_entry_finalize<T>(
    dict_entry: Felt252DictEntry<T>, new_value: T
) -> Felt252Dict<T> nopanic;

/// Squashes the dictionary and returns a `SquashedFelt252Dict` type.
///
/// NOTE: Never use this libfunc directly. Use Felt252DictTrait::squash() instead. Using this
/// libfunc directly will result in multiple unnecessary copies of the libfunc in the compiled CASM
/// code.
pub(crate) extern fn felt252_dict_squash<T>(
    dict: Felt252Dict<T>
) -> SquashedFelt252Dict<T> implicits(RangeCheck, GasBuiltin, SegmentArena) nopanic;

/// Basic trait for the `Felt252Dict` dictionary type.
pub trait Felt252DictTrait<T> {
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T);
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T;
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic;
    #[must_use]
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic;
}

impl Felt252DictImpl<T, +Felt252DictValue<T>> of Felt252DictTrait<T> {
    /// Inserts the given value for the given key.
    /// Requires the `Destruct` trait, as the previous value is dropped.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// ```
    #[inline]
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T) {
        let (entry, _prev_value) = felt252_dict_entry_get(self, key);
        self = felt252_dict_entry_finalize(entry, value);
    }

    /// Returns a copy of the value at the given key.
    /// Requires that the value type implements the `Copy` trait.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// let value = dict.get(0);
    /// assert!(value == 10);
    /// ```
    #[inline]
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T {
        let (entry, prev_value) = felt252_dict_entry_get(self, key);
        let return_value = prev_value;
        self = felt252_dict_entry_finalize(entry, prev_value);
        return_value
    }

    /// Allows to manually squash a dictionary and returns a `SquashedFelt252Dict` type.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// let squashed_dict = dict.squash();
    /// ```
    #[inline(never)]
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic {
        felt252_dict_squash(self)
    }

    /// Retrieves the last entry given a certain key.
    /// This method takes ownership of the dictionary and returns the entry to update
    /// as well as the previous value at the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// let (entry, prev_value) = dict.entry(0);
    /// assert!(prev_value == 10);
    /// ```
    #[inline]
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic {
        felt252_dict_entry_get(self, key)
    }
}

/// Basic trait for the `Felt252DictEntryTrait` dictionary entry type.
pub trait Felt252DictEntryTrait<T> {
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T>;
}

impl Felt252DictEntryImpl<T, +Felt252DictValue<T>> of Felt252DictEntryTrait<T> {
    /// Allows to create a new entry in the dictionary while giving back
    /// the ownership of the dictionary.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    ///
    /// let (entry, prev_value) = dict.entry(0);
    /// let new_value: u8 = 20;
    /// dict = entry.finalize(new_value);
    /// ```
    #[inline]
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T> {
        felt252_dict_entry_finalize(self, new_value)
    }
}

/// `Default` trait implementation to create a new empty dictionary.
impl Felt252DictDefault<T> of Default<Felt252Dict<T>> {
    /// Returns a new empty dictionary.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let dict: Felt252Dict<u8> = Default::default();
    /// ```
    #[inline]
    fn default() -> Felt252Dict<T> {
        felt252_dict_new()
    }
}

/// `Destruct` trait implementation to allow squashing dictionaries.
impl Felt252DictDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252Dict<T>> {
    /// `destruct` method is called right before a dictionary goes out of scope,
    /// but it is still possible to call it manually.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let dict: Felt252Dict<u8> = Default::default();
    /// dict.destruct();
    /// ```
    #[inline]
    fn destruct(self: Felt252Dict<T>) nopanic {
        self.squash();
    }
}

/// `Destruct` trait implementation to allow squashing a `Felt252DictEntry` type.
impl Felt252DictEntryDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252DictEntry<T>> {
    /// `destruct` method can be called on a `Felt252DictEntry` and squashes the associated
    /// dictionary.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// let (entry, prev_value) = dict.entry(0);
    /// entry.destruct();
    /// ```
    #[inline]
    fn destruct(self: Felt252DictEntry::<T>) nopanic {
        felt252_dict_entry_finalize(self, Felt252DictValue::zero_default());
    }
}

/// `Index` trait implementation to allow accessing elements by index.
impl Felt252DictIndex<
    T, +Felt252DictTrait<T>, +Copy<T>, +Destruct<Felt252DictEntry<T>>
> of Index<Felt252Dict<T>, felt252, T> {
    /// Takes a `felt252` index and returns the corresponding value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    /// use core::ops::Index;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    ///
    /// let value = dict.index(0);
    /// assert!(value == 10);
    ///
    /// let value = dict[0];
    /// assert!(value == 10);
    /// ```
    #[inline]
    fn index(ref self: Felt252Dict<T>, index: felt252) -> T {
        self.get(index)
    }
}
