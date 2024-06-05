pub trait Deref<T> {
    type Target;
    fn deref(self: T) -> Self::Target;
}
