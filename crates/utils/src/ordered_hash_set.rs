use std::hash::Hash;

use indexmap::IndexSet;
use itertools::zip_eq;

#[derive(Clone, Debug)]
pub struct OrderedHashSet<Key: Hash + Eq>(IndexSet<Key>);

impl<Key: Hash + Eq> OrderedHashSet<Key> {
    pub fn iter(&self) -> indexmap::set::Iter<'_, Key> {
        self.0.iter()
    }

    pub fn insert(&mut self, key: Key) -> bool {
        self.0.insert(key)
    }
}

impl<Key: Hash + Eq> IntoIterator for OrderedHashSet<Key> {
    type Item = Key;
    type IntoIter = <IndexSet<Key> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<Key: Hash + Eq> PartialEq for OrderedHashSet<Key> {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        };

        zip_eq(self.0.iter(), other.0.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Hash + Eq> Eq for OrderedHashSet<Key> {
    fn assert_receiver_is_total_eq(&self) {}
}

impl<Key: Hash + Eq> Default for OrderedHashSet<Key> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Key: Hash + Eq> FromIterator<Key> for OrderedHashSet<Key> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}
