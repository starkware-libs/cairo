use core::borrow::Borrow;
use core::hash::{BuildHasher, Hash};
use core::ops::Index;
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;
#[cfg(feature = "std")]
use std::collections::HashMap;

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

/// A hash map that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashMap](crate::ordered_hash_map::OrderedHashMap).
#[cfg(feature = "std")]
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH = RandomState>(HashMap<Key, Value, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH>(HashMap<Key, Value, BH>);

#[cfg(feature = "std")]
impl<Key, Value> UnorderedHashMap<Key, Value> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl<Key, Value, BH> PartialEq for UnorderedHashMap<Key, Value, BH>
where
    Key: Eq + Hash,
    Value: PartialEq,
    BH: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<Key, Value, BH> Eq for UnorderedHashMap<Key, Value, BH>
where
    Key: Eq + Hash,
    Value: Eq,
    BH: BuildHasher,
{
}

impl<Key, Value, BH> UnorderedHashMap<Key, Value, BH> {
    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<Key: Eq + Hash, Value, BH: BuildHasher> UnorderedHashMap<Key, Value, BH> {
    /// Returns a reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but [`Hash`] and [`Eq`] on the
    /// borrowed form *must* match those for the key type.
    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<&Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(key)
    }

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but [`Hash`] and [`Eq`] on the
    /// borrowed form *must* match those for the key type.
    pub fn get_mut<Q: ?Sized>(&mut self, key: &Q) -> Option<&mut Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get_mut(key)
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

    #[cfg(feature = "std")]
    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: Key) -> std::collections::hash_map::Entry<'_, Key, Value> {
        self.0.entry(key)
    }

    #[cfg(not(feature = "std"))]
    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: Key) -> hashbrown::hash_map::Entry<'_, Key, Value, BH> {
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
}

impl<Key, Q: ?Sized, Value, BH: BuildHasher> Index<&Q> for UnorderedHashMap<Key, Value, BH>
where
    Key: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash,
{
    type Output = Value;

    fn index(&self, key: &Q) -> &Self::Output {
        self.0.index(key)
    }
}

impl<Key, Value, BH: Default> Default for UnorderedHashMap<Key, Value, BH> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq, Value, BH: BuildHasher + Default> FromIterator<(Key, Value)>
    for UnorderedHashMap<Key, Value, BH>
{
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<Key: Hash + Eq, Value, const N: usize, BH: BuildHasher + Default> From<[(Key, Value); N]>
    for UnorderedHashMap<Key, Value, BH>
{
    fn from(items: [(Key, Value); N]) -> Self {
        Self(HashMap::from_iter(items))
    }
}
