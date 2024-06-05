/// A trait for dereferencing a value. This is used in order to directly access members of the
/// dereferenced value.
pub trait Deref<T> {
    type Target;
    fn deref(self: T) -> Self::Target;
}
