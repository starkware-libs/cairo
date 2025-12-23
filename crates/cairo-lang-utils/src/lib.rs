//! Cairo utilities.

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
use core::fmt;

/// Re-exporting the [`smol_str`] crate so that downstream projects can always use the same
/// instance the compiler does.
pub use ::smol_str;

pub mod bigint;
pub mod byte_array;
pub mod casts;
pub mod collection_arithmetics;
pub mod deque;
pub mod extract_matches;
#[cfg(feature = "std")]
pub mod graph_algos;
#[cfg(feature = "std")]
pub mod heap_size;
pub mod iterators;
#[cfg(feature = "tracing")]
pub mod logging;
pub mod ordered_hash_map;
pub mod ordered_hash_set;
#[cfg(feature = "std")]
pub mod small_ordered_map;
pub mod unordered_hash_map;
pub mod unordered_hash_set;

#[cfg(feature = "std")]
pub use heap_size::HeapSize;

/// Similar to From / TryFrom, but returns an option.
pub trait OptionFrom<T>: Sized {
    fn option_from(other: T) -> Option<Self>;
}

pub fn write_comma_separated<Iter: IntoIterator<Item = V>, V: core::fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    values: Iter,
) -> fmt::Result {
    let mut iter = values.into_iter();
    if let Some(value) = iter.next() {
        write!(f, "{value}")?;
    }
    for value in iter {
        write!(f, ", {value}")?;
    }
    Ok(())
}

/// Helper operations on `Option<T>`.
pub trait OptionHelper {
    fn on_none<F: FnOnce()>(self, f: F) -> Self;
}
impl<T> OptionHelper for Option<T> {
    fn on_none<F: FnOnce()>(self, f: F) -> Self {
        if self.is_none() {
            f();
        }
        self
    }
}

/// Traits requesting the for a dynamic clone for a salsa database.
#[cfg(feature = "std")]
pub trait CloneableDatabase: salsa::Database + Send {
    /// Returns a Box of the cloned database.
    fn dyn_clone(&self) -> Box<dyn CloneableDatabase>;
}

/// Implements Clone for `Box<dyn CloneableDatabase>`.
#[cfg(feature = "std")]
impl Clone for Box<dyn CloneableDatabase> {
    fn clone(&self) -> Self {
        self.dyn_clone()
    }
}

#[cfg(feature = "std")]
pub trait Intern<'db, Target> {
    fn intern(self, db: &'db dyn salsa::Database) -> Target;
}

/// TODO(eytan-starkware): Remove this macro entirely and rely on `salsa::interned`.
// Defines a short id struct for use with salsa interning.
// Interning is the process of representing a value as an id in a table.
// We usually denote the value type as "long id", and the id type as "short id" or just "id".
// Example:
//   A function's long id may be the module in which it is defined and its name. The function is
//   assigned a sequential integer (salsa::InternId) which will be its short id. Salsa will hold a
//   table to translate between the two representations. Note that a long id of an entity will
//   usually include the short id of the entity's parent.
#[macro_export]
macro_rules! define_short_id {
    ($short_id:ident, $long_id:path) => {
        // 1. Modern interned struct.
        #[cairo_lang_proc_macros::interned(revisions = usize::MAX)]
        pub struct $short_id<'db> {
            #[returns(ref)]
            pub long: $long_id,
        }

        impl<'db> std::fmt::Debug for $short_id<'db> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}({:x})", stringify!($short_id), self.as_intern_id().index())
            }
        }

        impl<'db> cairo_lang_utils::Intern<'db, $short_id<'db>> for $long_id {
            fn intern(self, db: &'db dyn salsa::Database) -> $short_id<'db> {
                $short_id::new(db, self)
            }
        }

        impl<'db> $short_id<'db> {
            pub fn from_intern_id(intern_id: salsa::Id) -> Self {
                use salsa::plumbing::FromId;
                Self::from_id(intern_id)
            }

            pub fn as_intern_id(self) -> salsa::Id {
                use salsa::plumbing::AsId;
                self.as_id()
            }
        }

        // 4. DebugWithDb is identical to the old macro.
        impl<'db> cairo_lang_debug::DebugWithDb<'db> for $short_id<'db> {
            type Db = dyn salsa::Database;
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
                use core::fmt::Debug;

                use cairo_lang_debug::helper::{Fallback, HelperDebug};

                HelperDebug::<$long_id, dyn salsa::Database>::helper_debug(self.long(db), db).fmt(f)
            }
        }

        // 5. HeapSize implementation - short ids are just wrappers around salsa::Id (no heap
        //    allocation).
        impl<'db> cairo_lang_utils::HeapSize for $short_id<'db> {
            fn heap_size(&self) -> usize {
                0
            }
        }
    };
}

/// Returns `Some(())` if the condition is true, otherwise `None`.
///
/// Useful in functions returning `None` on some condition:
/// `require(condition)?;`
/// And for functions returning `Err` on some condition:
/// `require(condition).ok_or_else(|| create_err())?;`
#[must_use = "This function is only relevant to create a possible return."]
pub fn require(condition: bool) -> Option<()> {
    condition.then_some(())
}
