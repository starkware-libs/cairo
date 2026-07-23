use core::hash::{BuildHasher, Hash};

use indexmap::IndexMap;
use itertools::zip_eq;

#[cfg(feature = "std")]
type BHImpl = std::collections::hash_map::RandomState;
#[cfg(not(feature = "std"))]
type BHImpl = hashbrown::DefaultHashBuilder;

#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(transparent),
    serde(bound(
        serialize = "Key: serde::Serialize, Value: serde::Serialize",
        deserialize = "Key: serde::Deserialize<'de> + Hash + Eq, Value: serde::Deserialize<'de>, \
                       BH: BuildHasher + Default"
    ))
)]
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
unsafe impl<Key: salsa::SalsaValue + Eq + Hash, Value: salsa::SalsaValue, BH: BuildHasher>
    salsa::SalsaValue for OrderedHashMap<Key, Value, BH>
{
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

impl<Key, Value, BH: BuildHasher + Default> OrderedHashMap<Key, Value, BH> {
    /// Creates an empty `OrderedHashMap` with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(IndexMap::with_capacity_and_hasher(capacity, Default::default()))
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
    use indexmap::map::serde_seq;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    use super::*;

    pub fn serialize_ordered_hashmap_vec<K, V, BH, S>(
        v: &OrderedHashMap<K, V, BH>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        K: Serialize + Hash + Eq,
        V: Serialize,
    {
        serde_seq::serialize(&v.0, serializer)
    }

    pub fn deserialize_ordered_hashmap_vec<'de, K, V, BH: BuildHasher + Default, D>(
        deserializer: D,
    ) -> Result<OrderedHashMap<K, V, BH>, D::Error>
    where
        D: Deserializer<'de>,
        K: Deserialize<'de> + Hash + Eq,
        V: Deserialize<'de>,
    {
        Ok(OrderedHashMap(serde_seq::deserialize(deserializer)?))
    }
}
#[cfg(feature = "serde")]
pub use impl_serde::*;
