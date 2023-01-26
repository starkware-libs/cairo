#[cfg(test)]
#[path = "debug_test.rs"]
mod test;

// Mostly taken from https://github.com/salsa-rs/salsa/blob/fd715619813f634fa07952f0d1b3d3a18b68fd65/components/salsa-2022/src/debug.rs
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

pub trait DebugWithDb<Db: ?Sized> {
    fn debug<'me, 'db>(&'me self, db: &'me Db) -> DebugWith<'me, Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Ref(self), db }
    }

    #[allow(dead_code)]
    fn into_debug<'me, 'db>(self, db: &'me Db) -> DebugWith<'me, Db>
    where
        Self: Sized + 'me,
    {
        DebugWith { value: BoxRef::Box(Box::new(self)), db }
    }

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result;
}

pub struct DebugWith<'me, Db: ?Sized> {
    value: BoxRef<'me, dyn DebugWithDb<Db> + 'me>,
    db: &'me Db,
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

impl<D: ?Sized> std::fmt::Debug for DebugWith<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DebugWithDb::fmt(&*self.value, f, self.db)
    }
}

impl<Db: ?Sized, T: ?Sized> DebugWithDb<Db> for &T
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<Db: ?Sized, T: ?Sized> DebugWithDb<Db> for Box<T>
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<Db: ?Sized, T> DebugWithDb<Db> for Rc<T>
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<Db: ?Sized, T: ?Sized> DebugWithDb<Db> for Arc<T>
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        T::fmt(self, f, db)
    }
}

impl<Db: ?Sized, T> DebugWithDb<Db> for Vec<T>
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let elements = self.iter().map(|e| e.debug(db));
        f.debug_list().entries(elements).finish()
    }
}

impl<Db: ?Sized, T> DebugWithDb<Db> for Option<T>
where
    T: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let me = self.as_ref().map(|v| v.debug(db));
        std::fmt::Debug::fmt(&me, f)
    }
}

impl<Db: ?Sized, K, V, S> DebugWithDb<Db> for HashMap<K, V, S>
where
    K: DebugWithDb<Db>,
    V: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        let elements = self.iter().map(|(k, v)| (k.debug(db), v.debug(db)));
        f.debug_map().entries(elements).finish()
    }
}

impl<Db: ?Sized, A, B> DebugWithDb<Db> for (A, B)
where
    A: DebugWithDb<Db>,
    B: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        f.debug_tuple("").field(&self.0.debug(db)).field(&self.1.debug(db)).finish()
    }
}

impl<Db: ?Sized, A, B, C> DebugWithDb<Db> for (A, B, C)
where
    A: DebugWithDb<Db>,
    B: DebugWithDb<Db>,
    C: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
        f.debug_tuple("")
            .field(&self.0.debug(db))
            .field(&self.1.debug(db))
            .field(&self.2.debug(db))
            .finish()
    }
}

impl<Db: ?Sized, V, S> DebugWithDb<Db> for HashSet<V, S>
where
    V: DebugWithDb<Db>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &Db) -> std::fmt::Result {
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

    impl<T: DebugWithDb<Db>, Db: ?Sized> HelperDebug<T, Db> {
        #[allow(dead_code)]
        pub fn helper_debug<'a, 'b: 'a>(a: &'a T, db: &'b Db) -> DebugWith<'a, Db> {
            a.debug(db)
        }
    }

    impl<Everything, Db: ?Sized, T: fmt::Debug> Fallback<T, Db> for Everything {}
}
