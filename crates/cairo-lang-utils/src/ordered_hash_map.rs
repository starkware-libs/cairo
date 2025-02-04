use core::hash::{BuildHasher, Hash};
use core::ops::{Index, IndexMut};
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;

use indexmap::{Equivalent, IndexMap};
use itertools::zip_eq;

#[cfg(feature = "std")]
#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key, Value, BH = RandomState>(IndexMap<Key, Value, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key, Value, BH = hashbrown::DefaultHashBuilder>(IndexMap<Key, Value, BH>);

impl<Key, Value, BH: Default> Default for OrderedHashMap<Key, Value, BH> {
    #[cfg(feature = "std")]
    fn default() -> Self {
        Self(Default::default())
    }
    #[cfg(not(feature = "std"))]
    fn default() -> Self {
        Self(IndexMap::with_hasher(Default::default()))
    }
}

impl<Key, Value, BH> OrderedHashMap<Key, Value, BH> {
    /// Returns an iterator over the key-value pairs of the map, in their order.
    pub fn iter(&self) -> indexmap::map::Iter<'_, Key, Value> {
        self.0.iter()
    }

    /// Returns a mutable iterator over the key-value pairs of the map, in their order.
    pub fn iter_mut(&mut self) -> indexmap::map::IterMut<'_, Key, Value> {
        self.0.iter_mut()
    }

    /// Returns an iterator over the keys of the map, in their order.
    pub fn keys(&self) -> indexmap::map::Keys<'_, Key, Value> {
        self.0.keys()
    }

    /// Returns a consuming iterator over the keys of the map, in their order.
    pub fn into_keys(self) -> indexmap::map::IntoKeys<Key, Value> {
        self.0.into_keys()
    }

    /// Returns an iterator over the values of the map, in their order.
    pub fn values(&self) -> indexmap::map::Values<'_, Key, Value> {
        self.0.values()
    }

    /// Returns the number of key-value pairs in the map.
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

    /// Removes the entry at the given index.
    ///
    /// Returns the key-value pair at the given index (if present).
    pub fn shift_remove_index(&mut self, index: usize) -> Option<(Key, Value)> {
        self.0.shift_remove_index(index)
    }
}

impl<Key: Eq + Hash, Value, BH: BuildHasher> OrderedHashMap<Key, Value, BH> {
    /// Returns a reference to the value stored for key, if it is present, else None.
    ///
    /// Computes in O(1) time (average).
    pub fn get<Q: ?Sized + Hash + Equivalent<Key>>(&self, key: &Q) -> Option<&Value> {
        self.0.get(key)
    }

    /// Returns a mutable reference to the value stored for key, if it is present, else None.
    ///
    /// Computes in O(1) time (average).
    pub fn get_mut<Q: ?Sized + Hash + Equivalent<Key>>(&mut self, key: &Q) -> Option<&mut Value> {
        self.0.get_mut(key)
    }

    /// Gets the given key’s corresponding entry in the map for insertion and/or in-place
    /// manipulation.
    ///
    /// Computes in O(1) time (amortized average).
    pub fn entry(&mut self, key: Key) -> Entry<'_, Key, Value> {
        self.0.entry(key)
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
    /// See also entry if you want to insert or modify or if you need to get the index of the
    /// corresponding key-value pair.
    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }

    /// Extends the map with the content of the given iterator.
    pub fn extend<I: IntoIterator<Item = (Key, Value)>>(&mut self, iter: I) {
        self.0.extend(iter)
    }

    /// Returns true if an equivalent to key exists in the map.
    pub fn contains_key<Q: ?Sized + Hash + Equivalent<Key>>(&self, key: &Q) -> bool {
        self.0.contains_key(key)
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

    /// Scan through each key-value pair in the map and keep those where the
    /// closure `keep` returns `true`.
    ///
    /// The elements are visited in order, and remaining elements keep their
    /// order.
    ///
    /// Computes in **O(n)** time (average).
    pub fn retain(&mut self, keep: impl FnMut(&Key, &mut Value) -> bool) {
        self.0.retain(keep);
    }

    /// Returns true if the maps are equal, ignoring the order of the entries.
    pub fn eq_unordered(&self, other: &Self) -> bool
    where
        Value: Eq,
    {
        if self.0.len() != other.0.len() {
            return false;
        };
        self.0.iter().all(|(k, v)| other.0.get(k) == Some(v))
    }
}

/// Entry for an existing key-value pair or a vacant location to insert one.
pub type Entry<'a, Key, Value> = indexmap::map::Entry<'a, Key, Value>;

impl<Key, Value, BH> IntoIterator for OrderedHashMap<Key, Value, BH> {
    type Item = (Key, Value);
    type IntoIter = indexmap::map::IntoIter<Key, Value>;
    fn into_iter(self) -> Self::IntoIter {
        let OrderedHashMap(inner) = self;
        inner.into_iter()
    }
}

impl<Key, Value, Q: ?Sized, BH> Index<&Q> for OrderedHashMap<Key, Value, BH>
where
    Q: Hash + Equivalent<Key>,
    Key: Hash + Eq,
    BH: BuildHasher,
{
    type Output = Value;

    fn index(&self, index: &Q) -> &Self::Output {
        self.0.index(index)
    }
}

impl<Key, Value, Q: ?Sized, BH> IndexMut<&Q> for OrderedHashMap<Key, Value, BH>
where
    Q: Hash + Equivalent<Key>,
    Key: Hash + Eq,
    BH: BuildHasher,
{
    fn index_mut(&mut self, index: &Q) -> &mut Value {
        self.0.index_mut(index)
    }
}

impl<Key: Eq, Value: Eq, BH> PartialEq for OrderedHashMap<Key, Value, BH> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        };

        zip_eq(self.0.iter(), other.0.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq, Value: Eq, BH: BuildHasher> Eq for OrderedHashMap<Key, Value, BH> {}

impl<Key: Hash + Eq, Value, BH: BuildHasher + Default> FromIterator<(Key, Value)>
    for OrderedHashMap<Key, Value, BH>
{
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<Key: Hash + Eq, Value, BH: BuildHasher + Default, const N: usize> From<[(Key, Value); N]>
    for OrderedHashMap<Key, Value, BH>
{
    fn from(init_map: [(Key, Value); N]) -> Self {
        Self(IndexMap::from_iter(init_map))
    }
}

#[cfg(feature = "serde")]
mod impl_serde {
    #[cfg(not(feature = "std"))]
    use alloc::vec::Vec;

    use itertools::Itertools;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    use super::*;

    impl<K: Hash + Eq + Serialize, V: Serialize, BH: BuildHasher> Serialize
        for OrderedHashMap<K, V, BH>
    {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            self.0.serialize(serializer)
        }
    }

    impl<'de, K: Hash + Eq + Deserialize<'de>, V: Deserialize<'de>, BH: BuildHasher + Default>
        Deserialize<'de> for OrderedHashMap<K, V, BH>
    {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            IndexMap::<K, V, BH>::deserialize(deserializer).map(|s| OrderedHashMap(s))
        }
    }

    pub fn serialize_ordered_hashmap_vec<'de, K, V, BH, S>(
        v: &OrderedHashMap<K, V, BH>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        K: Serialize + Deserialize<'de> + Hash + Eq,
        V: Serialize + Deserialize<'de>,
    {
        v.iter().collect_vec().serialize(serializer)
    }

    pub fn deserialize_ordered_hashmap_vec<'de, K, V, BH: BuildHasher + Default, D>(
        deserializer: D,
    ) -> Result<OrderedHashMap<K, V, BH>, D::Error>
    where
        D: Deserializer<'de>,
        K: Serialize + Deserialize<'de> + Hash + Eq,
        V: Serialize + Deserialize<'de>,
    {
        Ok(Vec::<(K, V)>::deserialize(deserializer)?.into_iter().collect())
    }
}
#[cfg(feature = "serde")]
pub use impl_serde::*;
