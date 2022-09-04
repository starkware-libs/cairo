use std::collections::{hash_map, HashMap};
use std::hash::Hash;
use std::ops::Index;

/// A hash map that does not care about the order of insertion.
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashMap].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnorderedHashMap<Key: Hash + Eq, Value>(HashMap<Key, Value>);

impl<Key: Hash + Eq, Value> UnorderedHashMap<Key, Value> {
    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }

    pub fn entry(&mut self, key: Key) -> hash_map::Entry<'_, Key, Value> {
        self.0.entry(key)
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
