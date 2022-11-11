use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

/// A hash set that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashSet](crate::ordered_hash_set::OrderedHashSet).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnorderedHashSet<Key: Hash + Eq>(HashSet<Key>);

impl<Key: Hash + Eq> UnorderedHashSet<Key> {
    /// Inserts the value into the set.
    ///
    /// If an equivalent item already exists in the set, returns `false`. Otherwise, returns `true`.
    pub fn insert(&mut self, key: Key) -> bool {
        self.0.insert(key)
    }

    /// Return true if an equivalent to value exists in the set.
    pub fn contains<Q: ?Sized + Hash + Eq>(&self, value: &Q) -> bool
    where
        Key: Borrow<Q>,
    {
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

    /// Clears the set, removing all values.
    pub fn clear(&mut self) {
        self.0.clear()
    }
}

impl<Key: Hash + Eq> Default for UnorderedHashSet<Key> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq> FromIterator<Key> for UnorderedHashSet<Key> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
