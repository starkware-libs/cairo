//! Dereferencing traits for value access.
//!
//! This module defines traits for dereferencing values, providing mechanisms to access
//! the underlying value of different types of references and snapshots.
//!
//! ## [`Deref`]
//!
//! A trait for dereferencing a value, allowing direct access to its inner content.
//! - Used to retrieve the underlying value of a reference or wrapper type.
//! - Defines a `Target` associated type representing the dereferenced type.
//!
//! ## [`DerefMut`]
//!
//! A trait for mutable dereferencing, enabling modification of the dereferenced value.
//! - Provides a way to mutably access the inner content of a reference.
//! - Useful for types that can be modified through a reference.
//!
//! ## [`SnapshotDeref`]
//!
//! A helper trait specifically designed for dereferencing snapshots (immutable references).
//! - Allows safe access to the contents of a snapshot.

/// A trait for dereferencing a value. This is used in order to directly access members of the
/// dereferenced value.
pub trait Deref<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    ///
    /// # Examples
    ///
    /// ```
    /// let boxed = BoxTrait::new(42);
    /// let deref_value = boxed.deref();
    /// assert!(deref_value == 42);
    /// ```
    fn deref(self: T) -> Self::Target;
}

/// A trait for dereferencing a value. This is used in order to handle the case where the value is a
/// reference.
#[unstable(feature: "deref_mut")]
pub trait DerefMut<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    ///
    /// # Examples
    ///
    /// ```
    /// use core::ops::DerefMut;
    ///
    /// impl DerefMutBox<T, +Copy<T>> of DerefMut<Box<T>> {
    ///     type Target = T;
    ///     fn deref_mut(ref self: Box<T>) -> T {
    ///         self.unbox()
    ///     }
    /// }
    ///
    /// fn main() {
    ///     let mut boxed = BoxTrait::new(42);
    ///     let deref_value = boxed.deref_mut();
    ///     assert!(deref_value == 42);
    /// }
    /// ```
    fn deref_mut(ref self: T) -> Self::Target;
}


/// A helper trait for dereferencing a snapshot of a type. Should not be implemented for copyable
/// types.
pub trait SnapshotDeref<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    fn snapshot_deref(self: @T) -> Self::Target;
}

/// Implementation of `Deref` for snapshots that implement `SnapshotDeref`.
// TODO(Gil): Add a negative impl for Copy types, it is not working right now when T is a generic
// type.
impl SnapshotDerefHelper<T, +SnapshotDeref<T>> of Deref<@T> {
    type Target = SnapshotDeref::<T>::Target;
    fn deref(self: @T) -> Self::Target {
        self.snapshot_deref()
    }
}

/// Implementation of `Deref` for copyable snapshots.
// TODO(Gil): This should not use the `*` operator as the `*` operator will later be calling
// `Deref`.
impl SnapshotTDeref<T, +Copy<T>> of Deref<@T> {
    type Target = T;
    fn deref(self: @T) -> Self::Target {
        *self
    }
}
