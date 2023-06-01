use core::borrow::Borrow;
use core::ops::Index;

#[cfg(not(feature = "std"))]
use no_std_imports::*;
#[cfg(not(feature = "std"))]
mod no_std_imports {
    pub use hashbrown::hash_map::DefaultHashBuilder;
    pub use hashbrown::HashMap;
}

#[cfg(feature = "std")]
use std_imports::*;
#[cfg(feature = "std")]
mod std_imports {
    pub use std::collections::HashMap;
    pub type DefaultHashBuilder =
        std::hash::BuildHasherDefault<std::collections::hash_map::DefaultHasher>;
}

use core::hash::{BuildHasher, Hash};

/// A hash map that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashMap](crate::ordered_hash_map::OrderedHashMap).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnorderedHashMap<Key: Hash + Eq, Value, S: BuildHasher = DefaultHashBuilder>(
    HashMap<Key, Value, S>,
);

impl<Key: Hash + Eq, Value> UnorderedHashMap<Key, Value> {
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

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    #[cfg(feature = "std")]
    pub fn entry(&mut self, key: Key) -> std::collections::hash_map::Entry<'_, Key, Value> {
        self.0.entry(key)
    }

    #[cfg(not(feature = "std"))]
    pub fn entry(
        &mut self,
        key: Key,
    ) -> hashbrown::hash_map::Entry<'_, Key, Value, DefaultHashBuilder> {
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

impl<Key, Q: ?Sized, Value> Index<&Q> for UnorderedHashMap<Key, Value>
where
    Key: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash,
{
    type Output = Value;

    fn index(&self, key: &Q) -> &Self::Output {
        self.0.index(key)
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

impl<Key: Hash + Eq, Value, const N: usize> From<[(Key, Value); N]>
    for UnorderedHashMap<Key, Value>
{
    fn from(items: [(Key, Value); N]) -> Self {
        Self(HashMap::<_, _, DefaultHashBuilder>::from_iter(items.into_iter()))
    }
}
