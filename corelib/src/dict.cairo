//! A dictionary-like data structure that maps `felt252` keys to values of any type.
//!
//! The `Felt252Dict` provides efficient key-value storage with operations for inserting,
//! retrieving, and updating values. Each operation creates a new entry that can be validated
//! through a process called squashing.
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
//! assert!(dict.get(0) == 20);
//! ```
//!
//! It is also possible to use the [`Felt252DictTrait::entry`] method to retrieve the last entry
//! given a certain key.
//! In this case, the method takes ownership of the dictionary and returns the entry to update.
//! After that, using the [`Felt252DictEntryTrait::finalize`] allows to create a new entry in the
//! dictionary.
//! Using `entry` and `finalize` methods can be very useful given that it does not require the type
//! in the dictionary to be copyable, meaning that we can use non-copyable types like arrays as
//! dictionary values.
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

#[feature("deprecated-index-traits")]
use crate::traits::{Default, Felt252DictValue, Index};

/// A dictionary that maps `felt252` keys to a value of any type.
pub extern type Felt252Dict<T>;

/// A dictionary in a squashed state. It cannot be mutated anymore.
pub extern type SquashedFelt252Dict<T>;

/// An intermediate type that is returned after calling the `entry` method that consumes ownership
/// of the dictionary. This ensures that the dictionary cannot be mutated until the entry is
/// finalized, which restores ownership of the dictionary.
pub extern type Felt252DictEntry<T>;

impl SquashedFelt252DictDrop<T, +Drop<T>> of Drop<SquashedFelt252Dict<T>>;
use crate::gas::GasBuiltin;
use crate::{RangeCheck, SegmentArena};

pub(crate) extern fn felt252_dict_new<T>() -> Felt252Dict<T> implicits(SegmentArena) nopanic;

extern fn felt252_dict_entry_get<T>(
    dict: Felt252Dict<T>, key: felt252,
) -> (Felt252DictEntry<T>, T) nopanic;

extern fn felt252_dict_entry_finalize<T>(
    dict_entry: Felt252DictEntry<T>, new_value: T,
) -> Felt252Dict<T> nopanic;

// Squashes the dictionary and returns a `SquashedFelt252Dict`.
//
// NOTE: Never use this libfunc directly. Use Felt252DictTrait::squash() instead. Using this
// libfunc directly will result in multiple unnecessary copies of the libfunc in the compiled CASM
// code.
pub(crate) extern fn felt252_dict_squash<T>(
    dict: Felt252Dict<T>,
) -> SquashedFelt252Dict<T> implicits(RangeCheck, GasBuiltin, SegmentArena) nopanic;

extern fn squashed_felt252_dict_entries<T>(
    dict: SquashedFelt252Dict<T>,
) -> Array<(felt252, T, T)> nopanic;

/// Basic trait for the `Felt252Dict` type.
pub trait Felt252DictTrait<T> {
    /// Inserts the given value for the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    /// ```
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T);

    /// Returns the value stored at the given key. If no value was previously inserted at this key,
    /// returns the default value for type T.
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
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T;

    /// Squashes a dictionary and returns the associated `SquashedFelt252Dict`.
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
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic;

    /// Retrieves the last entry for a certain key.
    /// This method takes ownership of the dictionary and returns the entry to update,
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
    #[must_use]
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic;
}

impl Felt252DictImpl<T, +Felt252DictValue<T>> of Felt252DictTrait<T> {
    #[inline]
    fn insert<+Destruct<T>>(ref self: Felt252Dict<T>, key: felt252, value: T) {
        let (entry, _prev_value) = felt252_dict_entry_get(self, key);
        self = felt252_dict_entry_finalize(entry, value);
    }

    #[inline]
    fn get<+Copy<T>>(ref self: Felt252Dict<T>, key: felt252) -> T {
        let (entry, value) = felt252_dict_entry_get(self, key);
        self = felt252_dict_entry_finalize(entry, value);
        value
    }

    #[inline(never)]
    fn squash(self: Felt252Dict<T>) -> SquashedFelt252Dict<T> nopanic {
        felt252_dict_squash(self)
    }

    #[inline]
    fn entry(self: Felt252Dict<T>, key: felt252) -> (Felt252DictEntry<T>, T) nopanic {
        felt252_dict_entry_get(self, key)
    }
}

/// Basic trait for the `Felt252DictEntryTrait` type.
pub trait Felt252DictEntryTrait<T> {
    /// Finalizes the changes made to a dictionary entry and gives back the ownership of the
    /// dictionary.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252DictEntryTrait;
    ///
    /// // Create a dictionary that stores arrays
    /// let mut dict: Felt252Dict<Nullable<Array<felt252>>> = Default::default();
    ///
    /// let a = array![1, 2, 3];
    /// dict.insert(0, NullableTrait::new(a));
    ///
    /// let (entry, prev_value) = dict.entry(0);
    /// let new_value = NullableTrait::new(array![4, 5, 6]);
    /// dict = entry.finalize(new_value);
    /// assert!(prev_value == a);
    /// assert!(dict.get(0) == new_value);
    /// ```
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T>;
}

impl Felt252DictEntryImpl<T, +Felt252DictValue<T>> of Felt252DictEntryTrait<T> {
    #[inline]
    fn finalize(self: Felt252DictEntry<T>, new_value: T) -> Felt252Dict<T> {
        felt252_dict_entry_finalize(self, new_value)
    }
}

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

impl Felt252DictDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252Dict<T>> {
    /// Allows the dictionary to go out of scope safely by ensuring it is squashed before going out
    /// of scope.
    /// A `Felt252Dict` cannot be "dropped" trivially because we need to ensure it is squashed
    /// before the end of a program for soundness purposes. As such, `destruct` squashes the
    /// dictionary, and the returned `SquashedFelt252Dict` is dropped trivially.
    /// `destruct` is automatically called when a dictionary goes out of scope.
    #[inline]
    fn destruct(self: Felt252Dict<T>) nopanic {
        self.squash();
    }
}

impl Felt252DictEntryDestruct<T, +Drop<T>, +Felt252DictValue<T>> of Destruct<Felt252DictEntry<T>> {
    /// Allows the `Felt252DictEntry` to go out of scope safely by ensuring the dictionary it is
    /// related to is squashed before going out of scope.
    /// `destruct` is automatically called when a dictionary entry goes out of scope.
    #[inline]
    fn destruct(self: Felt252DictEntry<T>) nopanic {
        felt252_dict_entry_finalize(self, Felt252DictValue::zero_default());
    }
}

/// Implementation of the `Index` trait for `Felt252Dict<T>`.
/// Allows accessing dictionary elements using the index operator `[]`.
impl Felt252DictIndex<
    T, +Felt252DictTrait<T>, +Copy<T>, +Destruct<Felt252DictEntry<T>>,
> of Index<Felt252Dict<T>, felt252, T> {
    /// Takes a `felt252` index and returns the corresponding value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::dict::Felt252Dict;
    ///
    /// let mut dict: Felt252Dict<u8> = Default::default();
    /// dict.insert(0, 10);
    ///
    /// let value = dict[0];
    /// assert!(value == 10);
    /// ```
    #[inline]
    fn index(ref self: Felt252Dict<T>, index: felt252) -> T {
        self.get(index)
    }
}

impl Felt252DictFromIterator<
    T, +Destruct<T>, +Destruct<Felt252Dict<T>>, +Felt252DictValue<T>,
> of crate::iter::FromIterator<Felt252Dict<T>, (felt252, T)> {
    /// Constructs a `Felt252Dict<T>` from an iterator of (felt252, T) key-value pairs.
    /// If the iterator contains repeating keys,
    /// only the last value in the iterator for each key will be kept.
    fn from_iter<
        I,
        impl IntoIter: IntoIterator<I>,
        +core::metaprogramming::TypeEqual<IntoIter::Iterator::Item, (felt252, T)>,
        +Destruct<IntoIter::IntoIter>,
        +Destruct<I>,
    >(
        iter: I,
    ) -> Felt252Dict<T> {
        let mut dict = Default::default();
        for (key, value) in iter {
            dict.insert(key, value);
        }
        dict
    }
}

/// Basic trait for the `SquashedFelt252Dict` type.
#[generate_trait]
pub impl SquashedFelt252DictImpl<T> of SquashedFelt252DictTrait<T> {
    /// Returns an array of `(key, first_value, last_value)` tuples.
    /// The first value is always 0.
    ///
    /// # Example
    /// ```
    /// let squashed_dict = dict.squash();
    /// let entries = squashed_dict.entries();
    /// ```
    #[inline]
    fn into_entries(self: SquashedFelt252Dict<T>) -> Array<(felt252, T, T)> {
        squashed_felt252_dict_entries(self)
    }
}
