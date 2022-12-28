use std::hash::Hash;
use std::ops::{Index, IndexMut};

use indexmap::{Equivalent, IndexMap};
use itertools::zip_eq;

#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key: Hash + Eq, Value>(IndexMap<Key, Value>);

impl<Key: Hash + Eq, Value> OrderedHashMap<Key, Value> {
    /// Return a reference to the value stored for key, if it is present, else None.
    ///
    /// Computes in O(1) time (average).
    pub fn get<Q: ?Sized + Hash + Equivalent<Key>>(&self, key: &Q) -> Option<&Value> {
        self.0.get(key)
    }

    /// Return a mutable reference to the value stored for key, if it is present, else None.
    ///
    /// Computes in O(1) time (average).
    pub fn get_mut<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, key: &Q) -> Option<&mut Value> {
        self.0.get_mut(key)
    }

    /// Get the given key’s corresponding entry in the map for insertion and/or in-place
    /// manipulation.
    ///
    /// Computes in O(1) time (amortized average).
    pub fn entry(&mut self, key: Key) -> indexmap::map::Entry<'_, Key, Value> {
        self.0.entry(key)
    }

    /// Return an iterator over the key-value pairs of the map, in their order.
    pub fn iter(&self) -> indexmap::map::Iter<'_, Key, Value> {
        self.0.iter()
    }

    /// Return a mutable iterator over the key-value pairs of the map, in their order.
    pub fn iter_mut(&mut self) -> indexmap::map::IterMut<'_, Key, Value> {
        self.0.iter_mut()
    }

    /// Return an iterator over the keys of the map, in their order.
    pub fn keys(&self) -> indexmap::map::Keys<'_, Key, Value> {
        self.0.keys()
    }

    /// Return an iterator over the values of the map, in their order.
    pub fn values(&self) -> indexmap::map::Values<'_, Key, Value> {
        self.0.values()
    }

    /// Insert a key-value pair in the map.
    ///
    /// If an equivalent key already exists in the map: the key remains and retains in its place in
    /// the order, its corresponding value is updated with value and the older value is returned
    /// inside Some(_).
    ///
    /// If no equivalent key existed in the map: the new key-value pair is inserted, last in order,
    /// and None is returned.
    ///
    /// Computes in O(1) time (amortized average).
    ///
    /// See also entry if you you want to insert or modify or if you need to get the index of the
    /// corresponding key-value pair.
    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }

    /// Returns true if an equivalent to key exists in the map.
    pub fn contains_key<Q: ?Sized + Hash + Equivalent<Key>>(&self, key: &Q) -> bool {
        self.0.contains_key(key)
    }

    /// Return the number of key-value pairs in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Removes all the entries for the map.
    pub fn clear(&mut self) {
        self.0.clear()
    }

    /// Removes the entry for the given key, preserving the order of entries.
    ///
    /// Returns the value associated with the key (if present).
    pub fn shift_remove<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, key: &Q) -> Option<Value> {
        self.0.shift_remove(key)
    }

    /// Removes the entry for the given key by swapping it with the last element.
    /// Thus the order of elements is not preserved, but the resulting order is still deterministic.
    ///
    /// Returns the value associated with the key (if present).
    pub fn swap_remove<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, key: &Q) -> Option<Value> {
        self.0.swap_remove(key)
    }
}

impl<Key: Hash + Eq, Value> IntoIterator for OrderedHashMap<Key, Value> {
    type Item = (Key, Value);
    type IntoIter = indexmap::map::IntoIter<Key, Value>;
    fn into_iter(self) -> Self::IntoIter {
        let OrderedHashMap(inner) = self;
        inner.into_iter()
    }
}

impl<Key: Hash + Eq, IndexType: Into<Key>, Value> Index<IndexType> for OrderedHashMap<Key, Value> {
    type Output = Value;

    fn index(&self, index: IndexType) -> &Self::Output {
        &self.0[&index.into()]
    }
}

impl<Key: Hash + Eq, IndexType: Into<Key>, Value> IndexMut<IndexType>
    for OrderedHashMap<Key, Value>
{
    fn index_mut(&mut self, index: IndexType) -> &mut Value {
        self.0.index_mut(&index.into())
    }
}

impl<Key: Hash + Eq, Value: Eq> PartialEq for OrderedHashMap<Key, Value> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        };

        zip_eq(self.0.iter(), other.0.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq, Value: Eq> Eq for OrderedHashMap<Key, Value> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<Key: Hash + Eq, Value> Default for OrderedHashMap<Key, Value> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq, Value> FromIterator<(Key, Value)> for OrderedHashMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<Key: Hash + Eq, Value, const N: usize> From<[(Key, Value); N]> for OrderedHashMap<Key, Value> {
    fn from(init_map: [(Key, Value); N]) -> Self {
        Self(init_map.into())
    }
}
