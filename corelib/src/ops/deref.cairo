//! Dereferencing traits for transparent access to wrapped values.
//!
//! This module provides traits that enable accessing the content of wrapped types
//! as if they were the inner type directly. This is particularly useful for:
//!
//! - Smart pointers and wrapper types (e.g., `Box<T>`)
//! - Nested data structures
//! - Enum variants sharing common fields
//!
//! # Core Traits
//!
//! - [`Deref`]: Provides read-only access to the wrapped value
//! - [`DerefMut`]: Provides read-only access to the wrapped value in mutable contexts
//!
//! # Examples
//!
//! ```
//! // Accessing nested struct fields through deref
//! #[derive(Drop, Copy)]
//! struct Inner { value: felt252 }
//!
//! #[derive(Drop, Copy)]
//! struct Outer { inner: Inner }
//!
//! impl OuterDeref of Deref<Outer> {
//!     type Target = Inner;
//!     fn deref(self: Outer) -> Inner { self.inner }
//! }
//!
//! let outer = Outer { inner: Inner { value: 42 } };
//! assert!(outer.value == 42); // Access Inner's field directly
//! ```

/// A trait for dereferencing a value to provide transparent access to its contents.
///
/// Implementing this trait allows a type to behave like its inner type, enabling direct access to
/// the inner type's fields.
///
/// Note: The `Deref` mechanism is limited and cannot be used to implicitly convert a type to its
/// target type when passing arguments to functions. For example, if you have a function that takes
/// an `Inner`, you cannot pass an `Outer` to it even if `Outer` implements `Deref`.
///
/// # Examples
///
/// ```
/// struct Wrapper<T> { inner: T }
///
/// impl WrapperDeref<T> of Deref<Wrapper<T>> {
///     type Target = T;
///     fn deref(self: Wrapper<T>) -> T { self.inner }
/// }
///
/// let wrapped = Wrapper { inner: 42 };
/// assert!(wrapped.deref() == 42);
/// ```
pub trait Deref<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    fn deref(self: T) -> Self::Target;
}

/// A trait for dereferencing in mutable contexts.
///
/// This trait is similar to `Deref` but specifically handles cases where the value
/// accessed is mutable. Despite its name, `DerefMut` does NOT allow modifying the
/// inner value - it only indicates that the container itself is mutable.
///
/// # Examples
///
/// ```
/// #[derive(Copy, Drop)]
/// struct MutWrapper<T> {
///     value: T
/// }
///
/// impl MutWrapperDerefMut<T, +Copy<T>> of DerefMut<MutWrapper<T>> {
///     type Target = T;
///     fn deref_mut(ref self: MutWrapper<T>) -> T {
///         self.value
///     }
/// }
///
/// // This will work since x is mutable
/// let mut x = MutWrapper { value: 42 };
/// let val = x.deref_mut();
/// assert!(val == 42);
///
/// // This would fail to compile since y is not mutable
/// let y = MutWrapper { value: 42 };
/// let val = y.deref_mut(); // Compile error
/// ```
#[unstable(feature: "deref_mut")]
pub trait DerefMut<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    fn deref_mut(ref self: T) -> Self::Target;
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
