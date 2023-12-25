/// A trait that can be used to disable implementations based on the types of the generic args.
/// Assumes that `TypeEqualImpl` is the only implementation of this trait.
pub trait TypeEqual<S, T> {}

impl TypeEqualImpl<T> of TypeEqual<T, T>;
