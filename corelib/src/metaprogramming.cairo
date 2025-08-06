//! Metaprogramming utilities.

/// A trait that can be used to disable implementations based on the types of the generic args.
/// Assumes that `TypeEqualImpl<T>` is the only implementation of this trait.
///
/// Primarily used for optimizations by enabling type-specific implementations.
/// Since `TypeEqualImpl<T>` is the only implementation, adding `-TypeEqual<T, U>` as a trait
/// bound ensures the implementation is only available when T and U are different types.
pub trait TypeEqual<S, T> {}

impl TypeEqualImpl<T> of TypeEqual<T, T>;

/// Marker trait for types that are tuples.
/// Currently supports tuples of size 0 to 10.
pub(crate) trait IsTuple<T>;


/// A trait for splitting a tuple into head element and a tail tuple, as well as reconstructing from
/// them.
pub(crate) trait TupleSplit<T> {
    /// The type of the first element of the tuple.
    type Head;
    /// The type of the rest of the tuple.
    type Rest;
    /// Splits the tuple into the head and the rest.
    fn split_head(self: T) -> (Self::Head, Self::Rest) nopanic;
    /// Reconstructs the tuple from the head and the rest.
    fn reconstruct(head: Self::Head, rest: Self::Rest) -> T nopanic;
}


/// A trait for extending a tuple from the front.
pub(crate) trait TupleExtendFront<T, E> {
    /// The type of the resulting tuple.
    type Result;
    /// Creates a new tuple from the `value` tuple with `element` in front of it.
    fn extend_front(value: T, element: E) -> Self::Result nopanic;
}


/// A trait for forwarding a wrapping snapshot from a tuple style struct into a tuple style struct
/// of the snapshots.
pub(crate) trait TupleSnapForward<T> {
    type SnapForward;
    fn snap_forward(self: @T) -> Self::SnapForward nopanic;
}


/// A trait for removing a wrapping snapshot from the types in tuple style struct.
pub(crate) trait SnapRemove<T> {
    type Result;
}

impl SnapRemoveSnap<T> of SnapRemove<@T> {
    type Result = T;
}

