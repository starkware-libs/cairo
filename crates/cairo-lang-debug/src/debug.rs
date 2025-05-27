#[cfg(test)]
#[path = "debug_test.rs"]
mod test;

// Mostly taken from https://github.com/salsa-rs/salsa/blob/fd715619813f634fa07952f0d1b3d3a18b68fd65/components/salsa-2022/src/debug.rs
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::ordered_hash_set::OrderedHashSet;

pub trait DebugWithDb<'db, Db: ?Sized> {
    fn debug<'me>(&'me self, db: &'db Db) -> DebugWith<'me, 'db, Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Ref(self), db }
    }

    #[allow(dead_code)]
    fn into_debug<'me>(self, db: &'db Db) -> DebugWith<'me, 'db, Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Box(Box::new(self)), db }
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result;
}

pub struct DebugWith<'me, 'db, Db: ?Sized> {
    value: BoxRef<'me, dyn DebugWithDb<'db, Db> + 'me>,
    db: &'db Db,
}

enum BoxRef<'me, T: ?Sized> {
    Box(Box<T>),
    Ref(&'me T),
}

impl<T: ?Sized> std::ops::Deref for BoxRef<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            BoxRef::Box(b) => b,
            BoxRef::Ref(r) => r,
        }
    }
}

impl<D: ?Sized> std::fmt::Debug for DebugWith<'_, '_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DebugWithDb::fmt(&*self.value, f, self.db)
    }
}

impl<'db, Db: ?Sized, T: ?Sized> DebugWithDb<'db, Db> for &T
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, Db: ?Sized, T: ?Sized> DebugWithDb<'db, Db> for Box<T>
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, Db: ?Sized, T> DebugWithDb<'db, Db> for Rc<T>
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, Db: ?Sized, T: ?Sized> DebugWithDb<'db, Db> for Arc<T>
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, Db: ?Sized, T> DebugWithDb<'db, Db> for [T]
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, T> DebugWithDb<'db, Db> for Vec<T>
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, T> DebugWithDb<'db, Db> for Option<T>
where
    T: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let me = self.as_ref().map(|v| v.debug(db));
        std::fmt::Debug::fmt(&me, f)
    }
}

impl<'db, Db: ?Sized, K, V, S> DebugWithDb<'db, Db> for HashMap<K, V, S>
where
    K: DebugWithDb<'db, Db>,
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, K, V> DebugWithDb<'db, Db> for BTreeMap<K, V>
where
    K: DebugWithDb<'db, Db>,
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, K: Hash + Eq, V> DebugWithDb<'db, Db> for OrderedHashMap<K, V>
where
    K: DebugWithDb<'db, Db>,
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, A> DebugWithDb<'db, Db> for (A,)
where
    A: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        f.debug_tuple("").field(&self.0.debug(db)).finish()
    }
}

impl<'db, Db: ?Sized, A, B> DebugWithDb<'db, Db> for (A, B)
where
    A: DebugWithDb<'db, Db>,
    B: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        f.debug_tuple("").field(&self.0.debug(db)).field(&self.1.debug(db)).finish()
    }
}

impl<'db, Db: ?Sized, A, B, C> DebugWithDb<'db, Db> for (A, B, C)
where
    A: DebugWithDb<'db, Db>,
    B: DebugWithDb<'db, Db>,
    C: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.0.debug(db))
            .field(&self.1.debug(db))
            .field(&self.2.debug(db))
            .finish()
    }
}

impl<'db, Db: ?Sized, V, S> DebugWithDb<'db, Db> for HashSet<V, S>
where
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, V> DebugWithDb<'db, Db> for BTreeSet<V>
where
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, Db: ?Sized, V: Hash + Eq> DebugWithDb<'db, Db> for OrderedHashSet<V>
where
    V: DebugWithDb<'db, Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

/// This is used by the macro generated code.
/// If the field type implements `DebugWithDb`, uses that, otherwise, uses `Debug`.
/// That's the "has impl" trick <https://github.com/nvzqz/impls#how-it-works>
#[doc(hidden)]
pub mod helper {
    use std::fmt;
    use std::marker::PhantomData;

    use super::{DebugWith, DebugWithDb};

    pub trait Fallback<T: fmt::Debug, Db: ?Sized> {
        fn helper_debug<'a>(a: &'a T, _db: &Db) -> &'a dyn fmt::Debug {
            a
        }
    }

    pub struct HelperDebug<T, Db: ?Sized>(PhantomData<T>, PhantomData<Db>);

    impl<'db, T: DebugWithDb<'db, Db>, Db: ?Sized> HelperDebug<T, Db> {
        #[allow(dead_code)]
        pub fn helper_debug<'a>(a: &'a T, db: &'db Db) -> DebugWith<'a, 'db, Db> {
            a.debug(db)
        }
    }

    impl<Everything, Db: ?Sized, T: fmt::Debug> Fallback<T, Db> for Everything {}
}
