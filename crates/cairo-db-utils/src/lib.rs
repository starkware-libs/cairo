//! General utilities.

#[macro_export]

// Defines a short id struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "long id", and the id type as "short id" or just "id".
// Example:
//   A function's long id may be the module in which it is defined and its name. The function is
//   assigned a sequential integer (salsa::InternId) which will be its short id. Salsa will hold a
//   table to translate between the two representations. Note that a long id of an entity will
//   usually include the short id of the entity's parent.
macro_rules! define_short_id {
    ($short_id:ident, $long_id:ident, $db:ident, $lookup:ident) => {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub struct $short_id(salsa::InternId);
        impl salsa::InternKey for $short_id {
            fn from_intern_id(salsa_id: salsa::InternId) -> Self {
                Self(salsa_id)
            }

            fn as_intern_id(&self) -> salsa::InternId {
                self.0
            }
        }
        // Impl transparent DebugWithDb.
        impl<T: ?Sized + db_utils::Upcast<dyn $db + 'static>> debug::DebugWithDb<T> for $short_id {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &T) -> std::fmt::Result {
                use std::fmt::Debug;

                use debug::helper::Fallback;
                let db = db.upcast();
                debug::helper::HelperDebug::<$long_id, dyn $db>::helper_debug(
                    &db.$lookup(*self),
                    db,
                )
                .fmt(f)
            }
        }
    };
}

pub trait Upcast<T: ?Sized> {
    fn upcast(&self) -> &T;
}

impl<T: ?Sized> Upcast<T> for T {
    fn upcast(&self) -> &T {
        self
    }
}
