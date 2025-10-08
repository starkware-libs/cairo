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

pub trait DebugWithDb<'db> {
    type Db: ?Sized;

    fn debug<'me>(&'me self, db: &'db Self::Db) -> DebugWith<'me, 'db, Self::Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Ref(self), db }
    }

    #[allow(dead_code)]
    fn into_debug<'me>(self, db: &'db Self::Db) -> DebugWith<'me, 'db, Self::Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Box(Box::new(self)), db }
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result;
}

/// A trait that allows upcasting the database to the type T.
pub trait DebugDbUpcast<'db, T: ?Sized> {
    fn debug_db_upcast(&'db self) -> &'db T;
}
impl<'db, T: ?Sized> DebugDbUpcast<'db, T> for T {
    fn debug_db_upcast(&'db self) -> &'db T {
        self
    }
}

pub struct DebugWith<'me, 'db, Db: ?Sized> {
    value: BoxRef<'me, dyn DebugWithDb<'db, Db = Db> + 'me>,
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

impl<'db, T: ?Sized> DebugWithDb<'db> for &T
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, T: ?Sized> DebugWithDb<'db> for Box<T>
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, T> DebugWithDb<'db> for Rc<T>
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, T: ?Sized> DebugWithDb<'db> for Arc<T>
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<'db, T> DebugWithDb<'db> for [T]
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, T> DebugWithDb<'db> for Vec<T>
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, T> DebugWithDb<'db> for Option<T>
where
    T: DebugWithDb<'db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let me = self.as_ref().map(|v| v.debug(db));
        std::fmt::Debug::fmt(&me, f)
    }
}

impl<'db, K, V, S> DebugWithDb<'db> for HashMap<K, V, S>
where
    K: DebugWithDb<'db>,
    V: DebugWithDb<'db, Db = K::Db>,
{
    type Db = K::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, K, V> DebugWithDb<'db> for BTreeMap<K, V>
where
    K: DebugWithDb<'db>,
    V: DebugWithDb<'db, Db = K::Db>,
{
    type Db = K::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, K: Hash + Eq, V> DebugWithDb<'db> for OrderedHashMap<K, V>
where
    K: DebugWithDb<'db>,
    V: DebugWithDb<'db, Db = K::Db>,
{
    type Db = K::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<'db, A> DebugWithDb<'db> for (A,)
where
    A: DebugWithDb<'db>,
{
    type Db = A::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        f.debug_tuple("").field(&self.0.debug(db)).finish()
    }
}

impl<'db, A, B> DebugWithDb<'db> for (A, B)
where
    A: DebugWithDb<'db>,
    B: DebugWithDb<'db> + 'db,
    A::Db: DebugDbUpcast<'db, B::Db>,
{
    type Db = A::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.0.debug(db))
            .field(&self.1.debug(db.debug_db_upcast()))
            .finish()
    }
}

impl<'db, A, B, C> DebugWithDb<'db> for (A, B, C)
where
    A: DebugWithDb<'db>,
    B: DebugWithDb<'db> + 'db,
    C: DebugWithDb<'db> + 'db,
    A::Db: DebugDbUpcast<'db, B::Db>,
    A::Db: DebugDbUpcast<'db, C::Db>,
{
    type Db = A::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.0.debug(db))
            .field(&self.1.debug(db.debug_db_upcast()))
            .field(&self.2.debug(db.debug_db_upcast()))
            .finish()
    }
}

impl<'db, V, S> DebugWithDb<'db> for HashSet<V, S>
where
    V: DebugWithDb<'db>,
{
    type Db = V::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, V> DebugWithDb<'db> for BTreeSet<V>
where
    V: DebugWithDb<'db>,
{
    type Db = V::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<'db, V: Hash + Eq> DebugWithDb<'db> for OrderedHashSet<V>
where
    V: DebugWithDb<'db>,
{
    type Db = V::Db;
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

/// A flexible debug trait that takes the database as a type parameter
/// instead of an associated type, allowing multiple implementations
/// for the same type with different database types.
pub trait DebugWithDbOverride<'db, Db: ?Sized> {
    fn fmt_override(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Db) -> std::fmt::Result;
}

impl<'db, T> DebugWithDb<'db> for id_arena::Id<T>
where
    T: DebugWithDb<'db>,
    id_arena::Id<T>: DebugWithDbOverride<'db, T::Db>,
{
    type Db = T::Db;

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
        self.fmt_override(f, db)
    }
}

/// This is used by the macro generated code.
/// If the field type implements `DebugWithDb`, uses that, otherwise, uses `Debug`.
/// That's the "has impl" trick <https://github.com/nvzqz/impls#how-it-works>
#[doc(hidden)]
pub mod helper {
    use std::fmt;
    use std::marker::PhantomData;

    use super::{DebugDbUpcast, DebugWith, DebugWithDb};

    pub trait Fallback<'db, T: fmt::Debug, Db: ?Sized> {
        fn helper_debug(a: &'db T, _db: &'db Db) -> &'db dyn fmt::Debug {
            a
        }
    }

    pub struct HelperDebug<T, Db: ?Sized>(PhantomData<T>, PhantomData<Db>);

    impl<'db, T: DebugWithDb<'db>, Db: ?Sized + DebugDbUpcast<'db, T::Db>> HelperDebug<T, Db> {
        #[allow(dead_code)]
        pub fn helper_debug<'me>(a: &'me T, db: &'db Db) -> DebugWith<'me, 'db, T::Db> {
            a.debug(db.debug_db_upcast())
        }
    }

    impl<'db, Everything, T: fmt::Debug, Db: ?Sized> Fallback<'db, T, Db> for Everything {}
}
