#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

/// A mapping optimized for very small maps.
///
/// Uses a single `Vec<(Key, Value)>` for better cache locality compared to
/// separate key/value vectors.
#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(transparent),
    serde(bound(
        serialize = "Key: serde::Serialize + Eq, Value: serde::Serialize",
        deserialize = "Key: serde::Deserialize<'de> + Eq, Value: serde::Deserialize<'de>",
    ))
)]
pub struct SmallOrderedMap<Key, Value>(Vec<(Key, Value)>);

impl<Key, Value> SmallOrderedMap<Key, Value> {
    /// Creates a map with a single key-value pair.
    #[inline]
    pub fn from_single(key: Key, value: Value) -> Self {
        Self(vec![(key, value)])
    }

    /// Creates a map from an array of key-value pairs without checking for duplicate keys.
    ///
    /// # Safety
    /// The caller must ensure that the input iterator does not contain duplicate keys.
    /// If duplicates exist, behavior of lookups is unspecified (may return any matching entry).
    #[inline]
    pub fn unchecked_from_iter(iter: impl IntoIterator<Item = (Key, Value)>) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<Key: Eq, Value> SmallOrderedMap<Key, Value> {
    /// Creates a new empty map.
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Creates a new map with the given capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    /// Returns the number of elements in the map.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the map contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Clears the map, removing all key-value pairs.
    #[inline]
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns a reference to the value corresponding to the key.
    #[inline]
    pub fn get<Q>(&self, key: &Q) -> Option<&Value>
    where
        Key: PartialEq<Q>,
    {
        self.0.iter().find_map(|(k, v)| (k == key).then_some(v))
    }

    /// Returns a mutable reference to the value corresponding to the key.
    #[inline]
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut Value>
    where
        Key: PartialEq<Q>,
    {
        self.0.iter_mut().find_map(|(k, v)| (k == key).then_some(v))
    }

    /// Returns `true` if the map contains the key.
    #[inline]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        Key: PartialEq<Q>,
    {
        self.0.iter().any(|(k, _)| k == key)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map already had the key present, the value is updated, and the old
    /// value is returned.
    pub fn insert(&mut self, key: Key, value: Value) -> Option<Value> {
        if let Some(v) = self.get_mut(&key) {
            Some(core::mem::replace(v, value))
        } else {
            self.0.push((key, value));
            None
        }
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    #[inline]
    pub fn entry(&mut self, key: Key) -> Entry<'_, Key, Value> {
        match self.0.iter().position(|(k, _)| k == &key) {
            Some(index) => Entry::Occupied(OccupiedEntry { map: &mut self.0, index }),
            None => Entry::Vacant(VacantEntry { map: &mut self.0, key }),
        }
    }

    /// An iterator visiting all keys in insertion order.
    #[inline]
    pub fn keys(&self) -> impl Iterator<Item = &Key> {
        self.0.iter().map(|(k, _)| k)
    }

    /// An iterator visiting all values in insertion order.
    #[inline]
    pub fn values(&self) -> impl Iterator<Item = &Value> {
        self.0.iter().map(|(_, v)| v)
    }

    /// An iterator visiting all key-value pairs in insertion order.
    #[inline]
    pub fn iter(&self) -> Iter<'_, Key, Value> {
        Iter(self.0.iter())
    }

    /// An iterator visiting all key-value pairs in insertion order, with mutable
    /// references to the values.
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<'_, Key, Value> {
        IterMut(self.0.iter_mut())
    }

    /// Reserves capacity for at least `additional` more elements.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional);
    }

    /// Reserves the minimum capacity for exactly `additional` more elements.
    #[inline]
    pub fn reserve_exact(&mut self, additional: usize) {
        self.0.reserve_exact(additional);
    }

    /// Removes a key from the map, returning the value if the key was present.
    pub fn remove<Q>(&mut self, key: &Q) -> Option<Value>
    where
        Key: PartialEq<Q>,
    {
        if let Some(index) = self.0.iter().position(|(k, _)| k == key) {
            Some(self.0.swap_remove(index).1)
        } else {
            None
        }
    }

    /// Retains only the elements specified by the predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(&Key, &mut Value) -> bool,
    {
        self.0.retain_mut(|(k, v)| f(k, v));
    }
}

impl<Key: Eq + PartialEq<Q>, Value, Q> core::ops::Index<&Q> for SmallOrderedMap<Key, Value> {
    type Output = Value;

    fn index(&self, key: &Q) -> &Self::Output {
        self.get(key).expect("key not found")
    }
}

impl<Key: Eq + PartialEq<Q>, Value, Q> core::ops::IndexMut<&Q> for SmallOrderedMap<Key, Value> {
    fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
        self.get_mut(key).expect("key not found")
    }
}

impl<Key: Eq, Value: Eq> SmallOrderedMap<Key, Value> {
    /// Checks if the two maps have the same keys and values (order independent).
    pub fn eq_unordered(&self, other: &Self) -> bool {
        if self.len() != other.len() {
            return false;
        }
        self.iter().all(|(k, v)| other.get(k) == Some(v))
    }
}

impl<Key: Eq, Value: Eq> PartialEq for SmallOrderedMap<Key, Value> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<Key: Eq, Value: Eq> Eq for SmallOrderedMap<Key, Value> {}

impl<Key: Eq, Value> Default for SmallOrderedMap<Key, Value> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Key: Eq, Value> FromIterator<(Key, Value)> for SmallOrderedMap<Key, Value> {
    fn from_iter<T: IntoIterator<Item = (Key, Value)>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let (lower, _) = iter.size_hint();
        let mut map = Self::with_capacity(lower);
        for (k, v) in iter {
            map.insert(k, v);
        }
        map
    }
}

impl<Key, Value> IntoIterator for SmallOrderedMap<Key, Value> {
    type Item = (Key, Value);
    type IntoIter = <Vec<(Key, Value)> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, Key, Value> IntoIterator for &'a SmallOrderedMap<Key, Value> {
    type Item = (&'a Key, &'a Value);
    type IntoIter = core::iter::Map<
        core::slice::Iter<'a, (Key, Value)>,
        fn(&'a (Key, Value)) -> (&'a Key, &'a Value),
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter().map(|(k, v)| (k, v))
    }
}

impl<Key: Eq, Value> Extend<(Key, Value)> for SmallOrderedMap<Key, Value> {
    fn extend<T: IntoIterator<Item = (Key, Value)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

#[cfg(feature = "salsa")]
unsafe impl<Key: salsa::Update + Eq, Value: salsa::Update> salsa::Update
    for SmallOrderedMap<Key, Value>
{
    // This code was taken from the salsa::Update trait implementation for IndexMap.
    // It is defined privately in macro_rules! maybe_update_map in the db-ext-macro repo.
    unsafe fn maybe_update(old_pointer: *mut Self, new_map: Self) -> bool {
        let old_map: &mut Self = unsafe { &mut *old_pointer };

        // To be considered "equal", the set of keys
        // must be the same between the two maps.
        let same_keys =
            old_map.len() == new_map.len() && old_map.keys().all(|k| new_map.contains_key(k));

        // If the set of keys has changed, then just pull in the new values
        // from new_map and discard the old ones.
        if !same_keys {
            old_map.clear();
            old_map.extend(new_map);
            return true;
        }

        // Otherwise, recursively descend to the values.
        // We do not invoke `K::update` because we assume
        // that if the values are `Eq` they must not need
        // updating (see the trait criteria).
        let mut changed = false;
        for (key, new_value) in new_map.into_iter() {
            let old_value = old_map.get_mut(&key).unwrap();
            changed |= unsafe { Value::maybe_update(old_value, new_value) };
        }
        changed
    }
}

/// Entry for an existing key-value pair or a vacant location to insert one.
pub enum Entry<'a, Key, Value> {
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, Key, Value>),
    /// A vacant entry.
    Vacant(VacantEntry<'a, Key, Value>),
}

impl<'a, Key: Eq, Value> Entry<'a, Key, Value> {
    /// Ensures a value is in the entry by inserting the default if empty,
    /// and returns a mutable reference to the value in the entry.
    pub fn or_insert(self, default: Value) -> &'a mut Value {
        match self {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the default
    /// function if empty, and returns a mutable reference to the value in the entry.
    pub fn or_insert_with<F: FnOnce() -> Value>(self, default: F) -> &'a mut Value {
        match self {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(default()),
        }
    }
}

/// An occupied entry in a `SmallOrderedMap`.
pub struct OccupiedEntry<'a, Key, Value> {
    map: &'a mut Vec<(Key, Value)>,
    index: usize,
}

impl<'a, Key, Value> OccupiedEntry<'a, Key, Value> {
    /// Gets a reference to the value in the entry.
    pub fn get(&self) -> &Value {
        &self.map[self.index].1
    }

    /// Gets a mutable reference to the value in the entry.
    pub fn get_mut(&mut self) -> &mut Value {
        &mut self.map[self.index].1
    }

    /// Converts the entry into a mutable reference to the value.
    pub fn into_mut(self) -> &'a mut Value {
        &mut self.map[self.index].1
    }

    /// Sets the value of the entry, and returns the entry's old value.
    pub fn insert(&mut self, value: Value) -> Value {
        core::mem::replace(&mut self.map[self.index].1, value)
    }

    /// Removes the entry from the map and returns the value.
    pub fn remove(self) -> Value {
        self.map.swap_remove(self.index).1
    }
}

/// A vacant entry in a `SmallOrderedMap`.
pub struct VacantEntry<'a, Key, Value> {
    map: &'a mut Vec<(Key, Value)>,
    key: Key,
}

impl<'a, Key, Value> VacantEntry<'a, Key, Value> {
    /// Sets the value of the entry and returns a mutable reference to it.
    pub fn insert(self, value: Value) -> &'a mut Value {
        self.map.push((self.key, value));
        &mut self.map.last_mut().unwrap().1
    }
}

/// An iterator over the entries of a `SmallOrderedMap`.
pub struct Iter<'a, Key, Value>(core::slice::Iter<'a, (Key, Value)>);

impl<'a, Key, Value> Iterator for Iter<'a, Key, Value> {
    type Item = (&'a Key, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (k, v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<Key, Value> ExactSizeIterator for Iter<'_, Key, Value> {}

/// A mutable iterator over the entries of a `SmallOrderedMap`.
pub struct IterMut<'a, Key, Value>(core::slice::IterMut<'a, (Key, Value)>);

impl<'a, Key, Value> Iterator for IterMut<'a, Key, Value> {
    type Item = (&'a Key, &'a mut Value);

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|(k, v)| (&*k, v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<Key, Value> ExactSizeIterator for IterMut<'_, Key, Value> {}
