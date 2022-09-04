use std::hash::Hash;
use std::ops::Index;

use indexmap::IndexMap;
use itertools::zip_eq;

#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key: Hash + Eq, Value>(IndexMap<Key, Value>);

impl<Key: Hash + Eq, Value> OrderedHashMap<Key, Value> {
    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.0.get(key)
    }

    pub fn iter(&self) -> indexmap::map::Iter<'_, Key, Value> {
        self.0.iter()
    }

    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        self.0.insert(key, value)
    }
}

impl<Key: Hash + Eq, IndexType: Into<Key>, Value> Index<IndexType> for OrderedHashMap<Key, Value> {
    type Output = Value;

    fn index(&self, index: IndexType) -> &Self::Output {
        &self.0[&index.into()]
    }
}

impl<Key: Hash + Eq, Value: Eq> PartialEq for OrderedHashMap<Key, Value> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        };

        zip_eq(self.0.iter(), other.0.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq, Value: Eq> Eq for OrderedHashMap<Key, Value> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<Key: Hash + Eq, Value> Default for OrderedHashMap<Key, Value> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq, Value> FromIterator<(Key, Value)> for OrderedHashMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
