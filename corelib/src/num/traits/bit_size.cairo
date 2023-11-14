/// Trait used to retrieve the size in bits of a type.
pub trait BitSize<T> {
    /// Returns the size in bits of T as usize.
    fn bits() -> usize;
}
