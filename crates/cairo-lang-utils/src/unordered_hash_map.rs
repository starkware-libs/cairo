#[cfg(test)]
#[path = "unordered_hash_map_test.rs"]
mod test;

#[cfg(not(feature = "std"))]
use alloc::vec;
use core::borrow::Borrow;
use core::hash::{BuildHasher, Hash};
use core::ops::Index;
#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(feature = "std")]
pub use std::collections::hash_map::Entry;
#[cfg(feature = "std")]
use std::collections::hash_map::OccupiedEntry;
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;
#[cfg(feature = "std")]
use std::vec;

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
#[cfg(not(feature = "std"))]
pub use hashbrown::hash_map::Entry;
use itertools::Itertools;

/// A hash map that does not care about the order of insertion.
///
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// It does support aggregation which can be used in intermediate computations (see `aggregate_by`).
/// For an iterable version see [OrderedHashMap](crate::ordered_hash_map::OrderedHashMap).
#[cfg(feature = "std")]
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH = RandomState>(HashMap<Key, Value, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct UnorderedHashMap<Key, Value, BH = hashbrown::DefaultHashBuilder>(
    HashMap<Key, Value, BH>,
);

impl<Key, Value, BH> UnorderedHashMap<Key, Value, BH> {
    fn with_hasher(hash_builder: BH) -> Self {
        Self(HashMap::<Key, Value, BH>::with_hasher(hash_builder))
    }
}

impl<Key, Value, BH> PartialEq for UnorderedHashMap<Key, Value, BH>
where
    Key: Eq + Hash,
    Value: PartialEq,
    BH: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
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
    pub fn get<Q>(&self, key: &Q) -> Option<&Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.get(key)
    }

    /// Returns a mutable reference to the value corresponding to the key.
    ///
    /// The key may be any borrowed form of the map's key type, but [`Hash`] and [`Eq`] on the
    /// borrowed form *must* match those for the key type.
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
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
    pub fn remove<Q>(&mut self, key: &Q) -> Option<Value>
    where
        Key: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.0.remove(key)
    }

    #[cfg(feature = "std")]
    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: Key) -> Entry<'_, Key, Value> {
        self.0.entry(key)
    }

    #[cfg(not(feature = "std"))]
    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: Key) -> Entry<'_, Key, Value, BH> {
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

    /// Maps the values of the map to new values using the given function.
    pub fn map<TargetValue>(
        &self,
        mapper: impl Fn(&Value) -> TargetValue,
    ) -> UnorderedHashMap<Key, TargetValue, BH>
    where
        Key: Clone,
        BH: Clone,
    {
        self.0.iter().fold(
            UnorderedHashMap::<_, _, _>::with_hasher(self.0.hasher().clone()),
            |mut acc, (key, value)| {
                match acc.entry(key.clone()) {
                    Entry::Occupied(_) => {
                        unreachable!("The original map should not contain duplicate keys.");
                    }
                    Entry::Vacant(vacant) => {
                        vacant.insert(mapper(value));
                    }
                };
                acc
            },
        )
    }

    /// Aggregates values of the map using the given functions.
    /// `mapping_function` maps each key to a new key, possibly mapping multiple original keys to
    /// the same target key.
    /// `reduce_function` dictates how to aggregate any two values of the same target key.
    /// `default_value` is the initial value for each target key.
    /// Note! as the map is unordered, `reduce_function` should be commutative. Otherwise, the
    /// result is undefined (nondeterministic).
    pub fn aggregate_by<TargetKey: Eq + Hash, TargetValue>(
        &self,
        mapping_function: impl Fn(&Key) -> TargetKey,
        reduce_function: impl Fn(&TargetValue, &Value) -> TargetValue,
        default_value: &TargetValue,
    ) -> UnorderedHashMap<TargetKey, TargetValue, BH>
    where
        BH: Clone,
    {
        self.0.iter().fold(
            UnorderedHashMap::<_, _, _>::with_hasher(self.0.hasher().clone()),
            |mut acc, (key, value)| {
                let target_key = mapping_function(key);
                match acc.entry(target_key) {
                    Entry::Occupied(occupied) => {
                        let old_target_value = occupied.into_mut();
                        let new_target_value = reduce_function(old_target_value, value);
                        *old_target_value = new_target_value;
                    }
                    Entry::Vacant(vacant) => {
                        let new_value = reduce_function(default_value, value);
                        vacant.insert(new_value);
                    }
                };
                acc
            },
        )
    }

    /// Iterates the map in an ascending order applied by the `Ord` implementation of `Key`.
    /// NOTE! To guarantee a deterministic output, the `Ord` implementation must apply a strict
    /// ordering. That is, `a <= b` and `b <= a`, then `a == b`. If `Ord` is derived (in all
    /// hierarchy levels), this is probably the case. If the ordering is not strict, the order of
    /// the output OrderedHashMap is undefined (nondeterministic).
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

    /// Iterates the map in an ascending order of the keys produced by the given function `f`.
    /// NOTE! To guarantee a deterministic output, `f`'s implementation must apply a strict
    /// ordering of the (Key, Value) pairs. That is, for any given pair of entries `a=(k_a, v_a)`
    /// and `b=(k_b, v_b)`, if `a <= b` and `b <= a`, then `a == b`. If the ordering is not strict,
    /// the order of the output OrderedHashMap is undefined (nondeterministic).
    /// This can be used to convert an unordered map to an ordered map (mostly when the unordered
    /// map was used for intermediate processing).
    pub fn iter_sorted_by_key<TargetKey, F>(&self, f: F) -> vec::IntoIter<(&Key, &Value)>
    where
        TargetKey: Ord,
        F: FnMut(&(&Key, &Value)) -> TargetKey,
    {
        self.0.iter().sorted_by_key(f)
    }

    /// A consuming version of `iter_sorted_by_key`.
    pub fn into_iter_sorted_by_key<TargetKey, F>(self, f: F) -> vec::IntoIter<(Key, Value)>
    where
        TargetKey: Ord,
        F: FnMut(&(Key, Value)) -> TargetKey,
    {
        self.0.into_iter().sorted_by_key(f)
    }

    /// Creates a new map with only the elements from the original map for which the given predicate
    /// returns `true`. Consuming.
    pub fn filter<P>(self, mut p: P) -> Self
    where
        BH: Default,
        P: FnMut(&Key, &Value) -> bool,
    {
        Self(self.0.into_iter().filter(|(key, value)| p(key, value)).collect())
    }

    /// Non consuming version of `filter`. Only clones the filtered entries. Requires `Key` and
    /// `Value` to implement `Clone`.
    pub fn filter_cloned<P>(&self, mut p: P) -> Self
    where
        BH: Default,
        P: FnMut(&Key, &Value) -> bool,
        Key: Clone,
        Value: Clone,
    {
        Self(
            self.0
                .iter()
                .filter_map(
                    |(key, value)| {
                        if p(key, value) { Some((key.clone(), value.clone())) } else { None }
                    },
                )
                .collect(),
        )
    }

    #[cfg(feature = "std")]
    /// Merges the map with another map. If a key is present in both maps, the given handler
    /// function is used to combine the values.
    pub fn merge<HandleDuplicate>(&mut self, other: &Self, handle_duplicate: HandleDuplicate)
    where
        BH: Clone,
        HandleDuplicate: Fn(OccupiedEntry<'_, Key, Value>, &Value),
        Key: Clone,
        Value: Clone,
    {
        for (key, value) in &other.0 {
            match self.0.entry(key.clone()) {
                Entry::Occupied(e) => {
                    handle_duplicate(e, value);
                }
                Entry::Vacant(e) => {
                    e.insert(value.clone());
                }
            }
        }
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
    #[cfg(feature = "std")]
    fn default() -> Self {
        Self(Default::default())
    }
    #[cfg(not(feature = "std"))]
    fn default() -> Self {
        Self(HashMap::with_hasher(Default::default()))
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

impl<Key: Hash + Eq, Value, BH: BuildHasher> Extend<(Key, Value)>
    for UnorderedHashMap<Key, Value, BH>
{
    fn extend<T: IntoIterator<Item = (Key, Value)>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}
