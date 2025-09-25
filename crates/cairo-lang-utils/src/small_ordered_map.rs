/// A mapping optimized for very small maps.
///
/// Wrapping the `vector_map::VecMap` structure.
#[derive(Clone, Debug)]
pub struct SmallOrderedMap<Key, Value>(vector_map::VecMap<Key, Value>);
impl<Key: Eq, Value: Eq> SmallOrderedMap<Key, Value> {
    /// Creates a new empty map.
    pub fn new() -> Self {
        Self(vector_map::VecMap::new())
    }
    /// Creates a new map with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(vector_map::VecMap::with_capacity(capacity))
    }
    /// Checks if the two maps have the same keys and values.
    pub fn eq_unordered(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<Key: Eq, Value: Eq> PartialEq for SmallOrderedMap<Key, Value> {
    fn eq(&self, other: &Self) -> bool {
        self.0.iter().eq(other.0.iter())
    }
}
impl<Key: Eq, Value: Eq> Eq for SmallOrderedMap<Key, Value> {}
impl<Key: Eq, Value: Eq> Default for SmallOrderedMap<Key, Value> {
    fn default() -> Self {
        Self::new()
    }
}
impl<Key: Eq, Value> FromIterator<(Key, Value)> for SmallOrderedMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
impl<Key, Value> IntoIterator for SmallOrderedMap<Key, Value> {
    type Item = (Key, Value);
    type IntoIter = vector_map::IntoIter<Key, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl<Key, Value> core::ops::Deref for SmallOrderedMap<Key, Value> {
    type Target = vector_map::VecMap<Key, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<Key, Value> core::ops::DerefMut for SmallOrderedMap<Key, Value> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg(feature = "salsa")]
unsafe impl<Key: salsa::Update + Eq, Value: salsa::Update> salsa::Update
    for SmallOrderedMap<Key, Value>
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
/// Entry for an existing key-value pair or a vacant location to insert one.
pub type Entry<'a, Key, Value> = vector_map::Entry<'a, Key, Value>;

#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};

#[cfg(feature = "serde")]
impl<Key: Eq + Serialize, Value: Serialize> Serialize for SmallOrderedMap<Key, Value> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de, Key: Eq + Deserialize<'de>, Value: Deserialize<'de>> Deserialize<'de>
    for SmallOrderedMap<Key, Value>
{
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        vector_map::VecMap::<Key, Value>::deserialize(deserializer).map(|s| SmallOrderedMap(s))
    }
}
