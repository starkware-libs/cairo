use std::hash::Hash;

use indexmap::IndexMap;
use itertools::zip_eq;

#[derive(Clone, Debug)]
pub struct OrderedHashMap<Key: Hash + Eq, Value> {
    map: IndexMap<Key, Value>,
}
impl<Key: Hash + Eq, Value> OrderedHashMap<Key, Value> {
    pub fn get(&self, key: &Key) -> Option<&Value> {
        self.map.get(key)
    }

    pub fn iter(&self) -> indexmap::map::Iter<Key, Value> {
        self.map.iter()
    }
}
impl<Key: Hash + Eq, Value: Eq> PartialEq for OrderedHashMap<Key, Value> {
    fn eq(&self, other: &Self) -> bool {
        if self.map.len() != other.map.len() {
            return false;
        };

        zip_eq(self.map.iter(), other.map.iter()).all(|(a, b)| a == b)
    }
}
impl<Key: Hash + Eq, Value: Eq> Eq for OrderedHashMap<Key, Value> {
    fn assert_receiver_is_total_eq(&self) {}
}
impl<Key: Hash + Eq, Value> Default for OrderedHashMap<Key, Value> {
    fn default() -> Self {
        Self { map: Default::default() }
    }
}
impl<Key: Hash + Eq, Value> FromIterator<(Key, Value)> for OrderedHashMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        OrderedHashMap { map: iter.into_iter().collect() }
    }
}
