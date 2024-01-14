#[cfg(test)]
#[path = "unordered_hash_map_test.rs"]
mod test;

use core::borrow::Borrow;
use core::hash::{BuildHasher, Hash};
use core::ops::Index;
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;
#[cfg(feature = "std")]
use std::collections::HashMap;

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
use itertools::Itertools;

#[cfg(feature = "std")]
/// A hash map that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// It does support aggregation which can be used in intermediate computations (see `aggregate_by`).
/// For an iterable version see [OrderedHashMap](crate::ordered_hash_map::OrderedHashMap).
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH = RandomState>(HashMap<Key, Value, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH>(HashMap<Key, Value, BH>);

#[cfg(not(feature = "std"))]
impl<Key, Value, BH> UnorderedHashMap<Key, Value, BH> {
    pub fn with_hasher(hash_builder: BH) -> Self {
        Self(HashMap::<Key, Value, BH>::with_hasher(hash_builder))
    }
}

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

    /// Aggregates values of the map using the given functions.
    /// `mapping_function` maps each key to a new key, possibly mapping multiple original keys to
    /// the same target key.
    /// `reduce_function` dictates how to aggregate multiple values of the same target key.
    /// `default_value` is the initial value for each target key.
    #[cfg(feature = "std")]
    pub fn aggregate_by<TargetKey: Eq + Hash>(
        &self,
        mapping_function: impl Fn(&Key) -> TargetKey,
        reduce_function: impl Fn(&Value, &Value) -> Value,
        default_value: &Value,
    ) -> UnorderedHashMap<TargetKey, Value> {
        self.0.iter().fold(UnorderedHashMap::<TargetKey, Value>::new(), |mut acc, (key, value)| {
            let new_key = mapping_function(key);
            match acc.entry(new_key) {
                std::collections::hash_map::Entry::Occupied(mut occupied) => {
                    let new_value = reduce_function(occupied.get(), value);
                    *occupied.get_mut() = new_value;
                }
                std::collections::hash_map::Entry::Vacant(vacant) => {
                    let new_value = reduce_function(default_value, value);
                    vacant.insert(new_value);
                }
            };
            acc
        })
    }

    /// Aggregates values of the map using the given functions.
    /// `mapping_function` maps each key to a new key, possibly mapping multiple original keys to
    /// the same target key.
    /// `reduce_function` dictates how to aggregate multiple values of the same target key.
    /// `default_value` is the initial value for each target key.
    #[cfg(not(feature = "std"))]
    pub fn aggregate_by<TargetKey: Eq + Hash>(
        &self,
        mapping_function: impl Fn(&Key) -> TargetKey,
        reduce_function: impl Fn(&Value, &Value) -> Value,
        default_value: &Value,
        hash_builder: BH,
    ) -> UnorderedHashMap<TargetKey, Value, BH> {
        self.0.iter().fold(
            UnorderedHashMap::<TargetKey, Value, BH>::with_hasher(hash_builder),
            |mut acc, (key, value)| {
                let target_key = mapping_function(key);
                match acc.entry(target_key) {
                    hashbrown::hash_map::Entry::Occupied(mut occupied) => {
                        let new_value = reduce_function(occupied.get(), value);
                        *occupied.get_mut() = new_value;
                    }
                    hashbrown::hash_map::Entry::Vacant(vacant) => {
                        let new_value = reduce_function(default_value, value);
                        vacant.insert(new_value);
                    }
                };
                acc
            },
        )
    }

    /// Iterates the map in the order applied by the `Ord` implementation of the keys.
    /// NOTE! To guarantee a deterministic output, the `Ord` implementation must apply a strict
    /// ordering. That is, `a <= b` and `b <= a`, then `a == b`. If `Ord` is derived (in all
    /// hierarchy levels), this is probably the case. If the ordering is not strict, the order in
    /// the output OrderedHashMap map is undefined.
    /// This can be used to convert an unordered map to an ordered map (mostly when the unordered
    /// map was used for intermediate processing).
    pub fn iter_sorted(&self) -> impl Iterator<Item = (&Key, &Value)>
    where
        Key: Ord,
    {
        self.0.iter().sorted_by_key(|(key, _)| *key)
    }

    /// A consuming version of `iter_sorted`.
    pub fn into_iter_sorted(self) -> impl Iterator<Item = (Key, Value)>
    where
        Key: Ord + Clone,
    {
        self.0.into_iter().sorted_by_key(|(key, _)| (*key).clone())
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
