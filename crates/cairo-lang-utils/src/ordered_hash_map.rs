use core::hash::{BuildHasher, Hash};
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;

use indexmap::IndexMap;
use itertools::zip_eq;

#[cfg(feature = "std")]
type BHImpl = RandomState;
#[cfg(not(feature = "std"))]
type BHImpl = hashbrown::DefaultHashBuilder;

#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key, Value, BH = BHImpl>(IndexMap<Key, Value, BH>);

impl<Key, Value, BH> core::ops::Deref for OrderedHashMap<Key, Value, BH> {
    type Target = IndexMap<Key, Value, BH>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Key, Value, BH> core::ops::DerefMut for OrderedHashMap<Key, Value, BH> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(feature = "salsa")]
unsafe impl<Key: salsa::Update + Eq + Hash, Value: salsa::Update> salsa::Update
    for OrderedHashMap<Key, Value, BHImpl>
{
    // This code was taken from the salsa::Update trait implementation for IndexMap.
    // It is defined privately in macro_rules! maybe_update_map in the db-ext-macro repo.
    unsafe fn maybe_update(old_pointer: *mut Self, new_map: Self) -> bool {
        let new_map = new_map;
        let old_map: &mut Self = unsafe { &mut *old_pointer };

        // To be considered "equal", the set of keys
        // must be the same between the two maps.
        let same_keys =
            old_map.len() == new_map.len() && old_map.keys().all(|k| new_map.contains_key(k));

        // If the set of keys has changed, then just pull in the new values
        // from new_map and discard the old ones.
        if !same_keys {
            old_map.clear();
            old_map.extend(new_map);
            return true;
        }

        // Otherwise, recursively descend to the values.
        // We do not invoke `K::update` because we assume
        // that if the values are `Eq` they must not need
        // updating (see the trait criteria).
        let mut changed = false;
        for (key, new_value) in new_map.into_iter() {
            let old_value = old_map.get_mut(&key).unwrap();
            changed |= unsafe { Value::maybe_update(old_value, new_value) };
        }
        changed
    }
}

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
    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<Key: Eq + Hash, Value, BH: BuildHasher> OrderedHashMap<Key, Value, BH> {
    /// Returns true if the maps are equal, ignoring the order of the entries.
    pub fn eq_unordered(&self, other: &Self) -> bool
    where
        Value: Eq,
    {
        if self.len() != other.len() {
            return false;
        };
        self.iter().all(|(k, v)| other.get(k) == Some(v))
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

impl<Key: Eq, Value: Eq, BH> PartialEq for OrderedHashMap<Key, Value, BH> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        };

        zip_eq(self.iter(), other.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq, Value: Eq, BH: BuildHasher> Eq for OrderedHashMap<Key, Value, BH> {}

impl<Key: Hash, Value: Hash, BH> Hash for OrderedHashMap<Key, Value, BH> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.len().hash(state);
        for e in self.iter() {
            e.hash(state);
        }
    }
}

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
