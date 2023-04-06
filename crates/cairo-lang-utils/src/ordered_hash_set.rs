use std::collections::hash_map::RandomState;
use std::hash::{BuildHasher, Hash};
use std::ops::Sub;

use indexmap::{Equivalent, IndexSet};
use itertools::zip_eq;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[derive(Clone, Debug)]
pub struct OrderedHashSet<Key: Hash + Eq, S = RandomState>(IndexSet<Key, S>);

pub type Iter<'a, Key> = indexmap::set::Iter<'a, Key>;

impl<Key: Hash + Eq> OrderedHashSet<Key> {
    /// Creates an empty `OrderedHashSet`.
    ///
    /// The hash set is initially created with a capacity of 0, so it will not allocate until it
    /// is first inserted into.
    pub fn new() -> Self {
        Self(IndexSet::new())
    }

    /// Returns an iterator over the values of the set, in their order.
    pub fn iter(&self) -> Iter<'_, Key> {
        self.0.iter()
    }

    /// Inserts the value into the set.
    ///
    /// If an equivalent item already exists in the set, returns `false`. Otherwise, returns `true`.
    pub fn insert(&mut self, key: Key) -> bool {
        self.0.insert(key)
    }

    /// Extends the set with the content of the given iterator.
    pub fn extend<I: IntoIterator<Item = Key>>(&mut self, iter: I) {
        self.0.extend(iter)
    }

    /// Returns true if an equivalent to value exists in the set.
    pub fn contains<Q: ?Sized + Hash + Equivalent<Key>>(&self, value: &Q) -> bool {
        self.0.contains(value)
    }

    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Remove all elements in the set, while preserving its capacity.
    ///
    /// Computes in O(n) time.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Removes the value from the set, preserving the order of elements.
    ///
    /// Returns true if the value was present in the set.
    pub fn shift_remove<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, value: &Q) -> bool {
        self.0.shift_remove(value)
    }

    /// Removes the value by swapping it with the last element, thus the order of elements is not
    /// preserved, but the resulting order is still deterministic.
    ///
    /// Returns true if the value was present in the set.
    pub fn swap_remove<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, value: &Q) -> bool {
        self.0.swap_remove(value)
    }

    pub fn difference<'a, S2>(
        &'a self,
        other: &'a OrderedHashSet<Key, S2>,
    ) -> indexmap::set::Difference<'a, Key, S2>
    where
        S2: std::hash::BuildHasher,
    {
        self.0.difference(&other.0)
    }

    /// Returns `true` if all elements of `self` are contained in `other`.
    pub fn is_subset<S2: BuildHasher>(&self, other: &OrderedHashSet<Key, S2>) -> bool {
        self.0.is_subset(&other.0)
    }

    /// Returns `true` if all elements of `other` are contained in `self`.
    pub fn is_superset<S2: BuildHasher>(&self, other: &OrderedHashSet<Key, S2>) -> bool {
        self.0.is_superset(&other.0)
    }
}

impl<Key: Hash + Eq, S: BuildHasher> OrderedHashSet<Key, S> {
    /// Return an iterator over all values that are in `self` or `other`.
    ///
    /// Values from `self` are produced in their original order, followed by
    /// values that are unique to `other` in their original order.
    pub fn union<'a, S2: BuildHasher>(
        &'a self,
        other: &'a OrderedHashSet<Key, S2>,
    ) -> indexmap::set::Union<'a, Key, S> {
        self.0.union(&other.0)
    }
}

impl<Key: Hash + Eq> IntoIterator for OrderedHashSet<Key> {
    type Item = Key;
    type IntoIter = <IndexSet<Key> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, Key: Hash + Eq> IntoIterator for &'a OrderedHashSet<Key> {
    type Item = &'a Key;
    type IntoIter = <&'a IndexSet<Key> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<Key: Hash + Eq> PartialEq for OrderedHashSet<Key> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        };

        zip_eq(self.0.iter(), other.0.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq> Eq for OrderedHashSet<Key> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<Key: Hash + Eq> Default for OrderedHashSet<Key> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq> FromIterator<Key> for OrderedHashSet<Key> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, Key: Hash + Eq + Clone> Sub<&'a OrderedHashSet<Key>> for &'a OrderedHashSet<Key> {
    type Output = OrderedHashSet<Key>;

    fn sub(self, rhs: Self) -> Self::Output {
        OrderedHashSet::<Key>(&self.0 - &rhs.0)
    }
}

impl<K: Hash + Eq + Serialize> Serialize for OrderedHashSet<K> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}
impl<'de, K: Hash + Eq + Deserialize<'de>> Deserialize<'de> for OrderedHashSet<K> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        IndexSet::<K, RandomState>::deserialize(deserializer).map(|s| OrderedHashSet(s))
    }
}
