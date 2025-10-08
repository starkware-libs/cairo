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
pub mod iterators;
#[cfg(feature = "tracing")]
pub mod logging;
pub mod ordered_hash_map;
pub mod ordered_hash_set;
#[cfg(feature = "std")]
pub mod small_ordered_map;
pub mod unordered_hash_map;
pub mod unordered_hash_set;

/// Similar to From / TryFrom, but returns an option.
pub trait OptionFrom<T>
where
    Self: Sized,
{
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

/// Borrows a mutable reference as Box for the lifespan of this function.
///
/// Runs the given closure with the boxed value as a parameter.
/// The closure is expected to return a boxed value, whose changes will be reflected on the mutable
/// reference.
/// Example:
/// ```
/// use cairo_lang_utils::borrow_as_box;
/// let mut x = 5;
/// borrow_as_box(&mut x, |mut x: Box<usize>| {
///     *x += 1;
///     ((), x)
/// });
/// assert_eq!(x, 6);
/// ```
pub fn borrow_as_box<T: Default, R, F: FnOnce(Box<T>) -> (R, Box<T>)>(ptr: &mut T, f: F) -> R {
    // TODO(spapini): Consider replacing take with something the leaves the memory dangling, instead
    // of filling with default().
    let (res, boxed) = f(Box::new(core::mem::take(ptr)));
    *ptr = *boxed;
    res
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
        #[salsa::interned(revisions = usize::MAX)]
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

        // 4. DebugWithDb identical to old macro.
        impl<'db> cairo_lang_debug::DebugWithDb<'db> for $short_id<'db> {
            type Db = dyn salsa::Database;
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>, db: &'db Self::Db) -> std::fmt::Result {
                use core::fmt::Debug;

                use cairo_lang_debug::helper::{Fallback, HelperDebug};

                HelperDebug::<$long_id, dyn salsa::Database>::helper_debug(self.long(db), db).fmt(f)
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
