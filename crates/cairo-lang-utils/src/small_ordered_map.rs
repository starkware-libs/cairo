/// A mapping optimized for very small maps.
///
/// Wraps the `vector_map::VecMap` structure.
#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(transparent),
    serde(bound(
        serialize = "Key: serde::Serialize + Eq, Value: serde::Serialize",
        deserialize = "Key: serde::Deserialize<'de> + Eq, Value: serde::Deserialize<'de>",
    ))
)]
pub struct SmallOrderedMap<Key, Value>(vector_map::VecMap<Key, Value>);
impl<Key: Eq, Value> SmallOrderedMap<Key, Value> {
    /// Creates a new empty map.
    pub fn new() -> Self {
        Self(vector_map::VecMap::new())
    }
    /// Creates a new map with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(vector_map::VecMap::with_capacity(capacity))
    }
}
impl<Key: Eq, Value: Eq> SmallOrderedMap<Key, Value> {
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
impl<Key: Eq, Value> Default for SmallOrderedMap<Key, Value> {
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
unsafe impl<Key: salsa::SalsaValue + Eq, Value: salsa::SalsaValue> salsa::SalsaValue
    for SmallOrderedMap<Key, Value>
{
}
/// Entry for an existing key-value pair or a vacant location to insert one.
pub type Entry<'a, Key, Value> = vector_map::Entry<'a, Key, Value>;
