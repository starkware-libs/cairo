//! Indexing traits for accessing collection elements.
//!
//! This module defines traits for implementing the indexing `[]` operator,
//! providing flexible mechanisms for accessing elements in collections.
//!
//! The [`IndexView`] and [`Index`] traits are for implementing the `[]` operator. Only one should
//! be implemented
/// for each type. Both are not consuming of `self`, the first gets a snapshot of the object and
/// the second gets a refence.

#[feature("deprecated-index-traits")]
use crate::traits::IndexView as DeprecatedIndexView;
#[feature("deprecated-index-traits")]
use crate::traits::Index as DeprecatedIndex;


/// A trait for a view of an item contained in type `C` with an index of type `I`.
pub trait IndexView<C, I> {
    /// The type of the item.
    type Target;
    /// Returns the item at the given index.
    ///
    /// # Examples
    ///
    /// ```
    /// let arr = array![1, 2, 3];
    /// assert!(arr[0] == @1);
    fn index(self: @C, index: I) -> Self::Target;
}

impl DeprecatedIndexViewImpl<
    C, I, V, impl Deprecated: DeprecatedIndexView<C, I, V>,
> of crate::ops::IndexView<C, I> {
    type Target = V;
    fn index(self: @C, index: I) -> Self::Target {
        Deprecated::index(self, index)
    }
}

/// A trait for accessing an item contained in type `C` with an index of type `I`.
pub trait Index<C, I> {
    /// The type of the item.
    type Target;
    /// Returns the item at the given index.
    fn index(ref self: C, index: I) -> Self::Target;
}

#[feature("deprecated-index-traits")]
impl DeprecatedIndexImpl<
    C, I, V, impl Deprecated: DeprecatedIndex<C, I, V>,
> of crate::ops::Index<C, I> {
    type Target = V;
    fn index(ref self: C, index: I) -> Self::Target {
        Deprecated::index(ref self, index)
    }
}

