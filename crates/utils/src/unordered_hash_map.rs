use std::borrow::Borrow;
use std::collections::{hash_map, HashMap};
use std::hash::Hash;
use std::ops::Index;

/// A hash map that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashMap](crate::ordered_hash_map::OrderedHashMap).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnorderedHashMap<Key: Hash + Eq, Value>(HashMap<Key, Value>);

impl<Key: Hash + Eq, Value> UnorderedHashMap<Key, Value> {
    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but `Hash` and `Eq` on the borrowed
    /// form must match those for the key type.
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(key)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, None is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old value is returned.
    /// The key is not updated, though; this matters for types that can be == without being
    /// identical.
    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }

    /// Removes a key from the map, returning the value at the key if the key was previously in the
    /// map.
    ///
    /// The key may be any borrowed form of the map's key type, but Hash and Eq on the borrowed form
    /// must match those for the key type.
    pub fn remove<Q: ?Sized>(&mut self, key: &Q) -> Option<Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.remove(key)
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: Key) -> hash_map::Entry<'_, Key, Value> {
        self.0.entry(key)
    }

    /// Returns true if the map contains a value for the specified key.
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        Q: ?Sized,
        Key: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.contains_key(key)
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<Key: Hash + Eq, IndexType: Into<Key>, Value> Index<IndexType>
    for UnorderedHashMap<Key, Value>
{
    type Output = Value;

    fn index(&self, index: IndexType) -> &Self::Output {
        &self.0[&index.into()]
    }
}

impl<Key: Hash + Eq, Value> Default for UnorderedHashMap<Key, Value> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq, Value> FromIterator<(Key, Value)> for UnorderedHashMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
