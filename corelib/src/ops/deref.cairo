/// A trait for dereferencing a value. This is used in order to directly access members of the
/// dereferenced value.
pub trait Deref<T> {
    type Target;
    fn deref(self: T) -> Self::Target;
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
