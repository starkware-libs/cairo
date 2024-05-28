/// The following two traits are for implementing the [] operator. Only one should be implemented
/// for each type. Both are not consuming of self, the first gets a snapshot of the object and
/// the second gets ref.
/// Trait for a view of an item contained in type `C` with an index of type `I`.
pub trait IndexView<C, I> {
    type Target;
    fn index(self: @C, index: I) -> Self::Target;
}
impl DeprecatedIndexView<
    C, I, V, impl Deprecated: core::traits::IndexView<C, I, V>
> of core::ops::IndexView<C, I> {
    type Target = V;
    fn index(self: @C, index: I) -> Self::Target {
        Deprecated::index(self, index)
    }
}

/// Trait for accessing an item contained in type `C` with an index of type `I`.
pub trait Index<C, I> {
    type Target;
    fn index(ref self: C, index: I) -> Self::Target;
}
impl DeprecatedIndex<
    C, I, V, impl Deprecated: core::traits::Index<C, I, V>
> of core::ops::Index<C, I> {
    type Target = V;
    fn index(ref self: C, index: I) -> Self::Target {
        Deprecated::index(ref self, index)
    }
}

