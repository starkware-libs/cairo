/// A trait for dereferencing a value. This is used in order to directly access members of the
/// dereferenced value.
pub trait Deref<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
    fn deref(self: T) -> Self::Target;
}

/// A trait for dereferencing a value. This is used in order to handle the case where the value is a
/// reference.
#[unstable(feature: "deref_mut")]
pub trait DerefMut<T> {
    /// The type of the dereferenced value.
    type Target;
    /// Returns the dereferenced value.
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

/// Implementation of Deref for snapshots that implement SnapshotDeref.
// TODO(Gil): Add a negative impl for Copy types, it is not working right now when T is a generic
// type.
impl SnapshotDerefHelper<T, +SnapshotDeref<T>> of Deref<@T> {
    type Target = SnapshotDeref::<T>::Target;
    fn deref(self: @T) -> Self::Target {
        self.snapshot_deref()
    }
}

/// Impl of Deref for copyable snapshots.
// TODO(Gil): This should not use the `*` operator as the `*` operator will later be calling
// `Deref`.
impl SnapshotTDeref<T, +Copy<T>,> of Deref<@T> {
    type Target = T;
    fn deref(self: @T) -> Self::Target {
        *self
    }
}
