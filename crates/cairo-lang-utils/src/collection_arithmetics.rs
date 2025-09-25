#[cfg(test)]
#[path = "collection_arithmetics_test.rs"]
mod test;

use core::hash::{BuildHasher, Hash};
use core::ops::{Add, Sub};

use crate::ordered_hash_map::{self, OrderedHashMap};
#[cfg(feature = "std")]
use crate::small_ordered_map::{self, SmallOrderedMap};

/// A trait for types which have a zero value.
///
/// Functions may assume the following:
/// * `x = x + zero() = zero() + x`
pub trait HasZero {
    /// Returns the zero value for the type.
    fn zero() -> Self;
}
impl HasZero for i32 {
    fn zero() -> Self {
        0
    }
}
impl HasZero for i64 {
    fn zero() -> Self {
        0
    }
}

/// A trait for types which support addition on collections.
pub trait AddCollection<Key, Value> {
    /// Returns a new collection with the sum of the values from the given two collections, for each
    /// key.
    ///
    /// If the key is missing from one of them, it is treated as zero.
    fn add_collection(self, other: impl IntoIterator<Item = (Key, Value)>) -> Self;
}

/// A trait for types which support subtraction on collections.
pub trait SubCollection<Key, Value> {
    /// Returns a new collection with the difference of the values from the given two collections,
    /// for each key.
    ///
    /// If the key is missing from one of them, it is treated as zero.
    fn sub_collection(self, other: impl IntoIterator<Item = (Key, Value)>) -> Self;
}

pub trait MergeCollection<Key, Value> {
    /// Returns a collection which contains the combination by using `action` of the values from the
    /// given two collections, for each key.
    ///
    /// If the key is missing from one of them, it is treated as zero.
    fn merge_collection(
        self,
        other: impl IntoIterator<Item = (Key, Value)>,
        action: impl Fn(Value, Value) -> Value,
    ) -> Self;
}

impl<Key, Value: Add<Output = Value>, T: MergeCollection<Key, Value>> AddCollection<Key, Value>
    for T
{
    fn add_collection(self, other: impl IntoIterator<Item = (Key, Value)>) -> Self {
        self.merge_collection(other, |a, b| a + b)
    }
}

impl<Key, Value: Sub<Output = Value>, T: MergeCollection<Key, Value>> SubCollection<Key, Value>
    for T
{
    fn sub_collection(self, other: impl IntoIterator<Item = (Key, Value)>) -> Self {
        self.merge_collection(other, |a, b| a - b)
    }
}

impl<Key: Hash + Eq, Value: HasZero + Clone + Eq, BH: BuildHasher> MergeCollection<Key, Value>
    for OrderedHashMap<Key, Value, BH>
{
    fn merge_collection(
        mut self,
        other: impl IntoIterator<Item = (Key, Value)>,
        action: impl Fn(Value, Value) -> Value,
    ) -> Self {
        for (key, other_val) in other {
            match self.entry(key) {
                ordered_hash_map::Entry::Occupied(mut e) => {
                    let new_val = action(e.get().clone(), other_val);
                    if new_val == Value::zero() {
                        e.swap_remove();
                    } else {
                        e.insert(new_val);
                    }
                }
                ordered_hash_map::Entry::Vacant(e) => {
                    let zero = Value::zero();
                    if other_val != zero {
                        e.insert(action(zero, other_val));
                    }
                }
            }
        }
        self
    }
}

#[cfg(feature = "std")]
impl<Key: Eq, Value: HasZero + Clone + Eq> MergeCollection<Key, Value>
    for SmallOrderedMap<Key, Value>
{
    fn merge_collection(
        mut self,
        other: impl IntoIterator<Item = (Key, Value)>,
        action: impl Fn(Value, Value) -> Value,
    ) -> Self {
        for (key, other_val) in other {
            match self.entry(key) {
                small_ordered_map::Entry::Occupied(mut e) => {
                    let new_val = action(e.get().clone(), other_val);
                    if new_val == Value::zero() {
                        e.remove();
                    } else {
                        e.insert(new_val);
                    }
                }
                small_ordered_map::Entry::Vacant(e) => {
                    let zero = Value::zero();
                    if other_val != zero {
                        e.insert(action(zero, other_val));
                    }
                }
            }
        }
        self
    }
}
