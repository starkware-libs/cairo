#[feature("deprecated-index-traits")]
use core::traits::IndexView as DeprecatedIndexView;
#[feature("deprecated-index-traits")]
use core::traits::Index as DeprecatedIndex;

/// The following two traits are for implementing the [] operator. Only one should be implemented
/// for each type. Both are not consuming of self, the first gets a snapshot of the object and
/// the second gets ref.
/// Trait for a view of an item contained in type `C` with an index of type `I`.
pub trait IndexView<C, I> {
    /// The type of the item.
    type Target;
    /// Returns the item at the given index.
    fn index(self: @C, index: I) -> Self::Target;
}
impl DeprecatedIndexViewImpl<
    C, I, V, impl Deprecated: DeprecatedIndexView<C, I, V>
> of core::ops::IndexView<C, I> {
    type Target = V;
    fn index(self: @C, index: I) -> Self::Target {
        Deprecated::index(self, index)
    }
}

/// Trait for accessing an item contained in type `C` with an index of type `I`.
pub trait Index<C, I> {
    /// The type of the item.
    type Target;
    /// Returns the item at the given index.
    fn index(ref self: C, index: I) -> Self::Target;
}
#[feature("deprecated-index-traits")]
impl DeprecatedIndexImpl<
    C, I, V, impl Deprecated: DeprecatedIndex<C, I, V>
> of core::ops::Index<C, I> {
    type Target = V;
    fn index(ref self: C, index: I) -> Self::Target {
        Deprecated::index(ref self, index)
    }
}

