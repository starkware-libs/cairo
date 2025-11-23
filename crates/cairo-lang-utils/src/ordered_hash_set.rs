use core::hash::{BuildHasher, Hash};
use core::ops::Sub;
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;

use indexmap::IndexSet;
use itertools::zip_eq;

#[cfg(feature = "std")]
#[derive(Clone, Debug)]
pub struct OrderedHashSet<Key, BH = RandomState>(IndexSet<Key, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct OrderedHashSet<Key, BH = hashbrown::DefaultHashBuilder>(IndexSet<Key, BH>);

impl<Key, BH> core::ops::Deref for OrderedHashSet<Key, BH> {
    type Target = IndexSet<Key, BH>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<Key, BH> core::ops::DerefMut for OrderedHashSet<Key, BH> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

// This code was taken from the salsa::Update trait implementation for IndexSet.
// It is defined privately in macro_rules! maybe_update_set in the db-ext-macro repo.
#[cfg(feature = "salsa")]
unsafe impl<Key: Eq + Hash, BH: BuildHasher> salsa::Update for OrderedHashSet<Key, BH> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_set: Self) -> bool {
        let old_set: &mut Self = unsafe { &mut *old_pointer };

        if *old_set == new_set {
            false
        } else {
            old_set.clear();
            old_set.extend(new_set);
            true
        }
    }
}

pub type Iter<'a, Key> = indexmap::set::Iter<'a, Key>;

impl<Key, BH: Default> Default for OrderedHashSet<Key, BH> {
    #[cfg(feature = "std")]
    fn default() -> Self {
        Self(Default::default())
    }
    #[cfg(not(feature = "std"))]
    fn default() -> Self {
        Self(IndexSet::with_hasher(Default::default()))
    }
}

impl<Key, BH> IntoIterator for OrderedHashSet<Key, BH> {
    type Item = Key;
    type IntoIter = <IndexSet<Key, BH> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, Key, BH> IntoIterator for &'a OrderedHashSet<Key, BH> {
    type Item = &'a Key;
    type IntoIter = <&'a IndexSet<Key, BH> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<Key: Eq, BH> PartialEq for OrderedHashSet<Key, BH> {
    fn eq(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        };

        zip_eq(self.iter(), other.iter()).all(|(a, b)| a == b)
    }
}

impl<Key: Eq, BH> Eq for OrderedHashSet<Key, BH> {}

impl<Key: Hash + Eq, BH: BuildHasher + Default> FromIterator<Key> for OrderedHashSet<Key, BH> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, Key, BH> Sub<&'a OrderedHashSet<Key, BH>> for &'a OrderedHashSet<Key, BH>
where
    &'a IndexSet<Key, BH>: Sub<Output = IndexSet<Key, BH>>,
{
    type Output = OrderedHashSet<Key, BH>;

    fn sub(self, rhs: Self) -> Self::Output {
        OrderedHashSet::<Key, BH>(&self.0 - &rhs.0)
    }
}

#[cfg(feature = "serde")]
mod impl_serde {
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    use super::*;

    impl<K: Hash + Eq + Serialize, BH: BuildHasher> Serialize for OrderedHashSet<K, BH> {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            self.0.serialize(serializer)
        }
    }

    impl<'de, K: Hash + Eq + Deserialize<'de>, BH: BuildHasher + Default> Deserialize<'de>
        for OrderedHashSet<K, BH>
    {
        fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
            IndexSet::<K, BH>::deserialize(deserializer).map(|s| OrderedHashSet(s))
        }
    }
}
