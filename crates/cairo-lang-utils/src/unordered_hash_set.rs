use core::borrow::Borrow;
use core::hash::{BuildHasher, Hash};
use core::ops::Sub;
#[cfg(feature = "std")]
use std::collections::HashSet;
#[cfg(feature = "std")]
use std::collections::hash_map::RandomState;

#[cfg(not(feature = "std"))]
use hashbrown::HashSet;

/// A hash set that does not care about the order of insertion.
///
/// In particular, it does not support iterating, in order to guarantee deterministic compilation.
/// For an iterable version see [OrderedHashSet](crate::ordered_hash_set::OrderedHashSet).
#[cfg(feature = "std")]
#[derive(Clone, Debug)]
pub struct UnorderedHashSet<Key, BH = RandomState>(HashSet<Key, BH>);
#[cfg(not(feature = "std"))]
#[derive(Clone, Debug)]
pub struct UnorderedHashSet<Key, BH = hashbrown::DefaultHashBuilder>(HashSet<Key, BH>);

impl<K, BH: Default> Default for UnorderedHashSet<K, BH> {
    #[cfg(feature = "std")]
    fn default() -> Self {
        Self(Default::default())
    }
    #[cfg(not(feature = "std"))]
    fn default() -> Self {
        Self(HashSet::with_hasher(Default::default()))
    }
}

impl<K, BH> PartialEq for UnorderedHashSet<K, BH>
where
    K: Eq + Hash,
    BH: BuildHasher,
{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<K, BH> Eq for UnorderedHashSet<K, BH>
where
    K: Eq + Hash,
    BH: BuildHasher,
{
}

impl<Key, BH> UnorderedHashSet<Key, BH> {
    /// Returns the number of elements in the set.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the set contains no elements.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Clears the set, removing all values.
    pub fn clear(&mut self) {
        self.0.clear()
    }
}

impl<Key: Hash + Eq, BH: BuildHasher> UnorderedHashSet<Key, BH> {
    /// Inserts the value into the set.
    ///
    /// If an equivalent item already exists in the set, returns `false`. Otherwise, returns `true`.
    pub fn insert(&mut self, key: Key) -> bool {
        self.0.insert(key)
    }

    /// Removes a value from the set. Returns whether the value was present in the set.
    pub fn remove<Q: ?Sized + Hash + Eq>(&mut self, value: &Q) -> bool
    where
        Key: Borrow<Q>,
    {
        self.0.remove(value)
    }

    /// Extends the set with the content of the given iterator.
    pub fn extend<I: IntoIterator<Item = Key>>(&mut self, iter: I) {
        self.0.extend(iter)
    }

    /// Extends the set with the content of another set.
    pub fn extend_unordered(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    /// Returns true if an equivalent to value exists in the set.
    pub fn contains<Q: ?Sized + Hash + Eq>(&self, value: &Q) -> bool
    where
        Key: Borrow<Q>,
    {
        self.0.contains(value)
    }
}

impl<Key: Hash + Eq, BH: BuildHasher + Default> FromIterator<Key> for UnorderedHashSet<Key, BH> {
    fn from_iter<T: IntoIterator<Item = Key>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<'a, Key, BH> Sub<&'a UnorderedHashSet<Key, BH>> for &'a UnorderedHashSet<Key, BH>
where
    &'a HashSet<Key, BH>: Sub<Output = HashSet<Key, BH>>,
{
    type Output = UnorderedHashSet<Key, BH>;

    fn sub(self, rhs: Self) -> Self::Output {
        UnorderedHashSet::<Key, BH>(&self.0 - &rhs.0)
    }
}
