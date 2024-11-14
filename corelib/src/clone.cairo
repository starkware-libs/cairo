//! The `Clone` trait provides the ability to duplicate instances of types that cannot be
//! 'implicitly copied'.
//!
//! In Cairo, some simple types are "implicitly copyable" and when you assign them or pass them as
//! arguments, the receiver will get a copy, leaving the original value in place. These types do not
//! require allocation to copy, and are not at risk of accessing un-allocated memory, so the
//! compiler considers them cheap and safe to copy. For other types, copies must be made explicitly,
//! by convention implementing the [`Clone`] trait and calling the [`clone`] method.
//!
//! # Examples
//!
//! ```
//! let arr = array![1, 2, 3];
//! let cloned_arr = arr.clone();
//! assert!(arr == cloned_arr);
//! ```
//!
//! To easily implement the Clone trait, you can also use
//! `#[derive(Clone)]`:
//! ```
//! #[derive(Clone, Drop)]
//! struct Sheep {
//!    name: ByteArray,
//!    age: u8,
//! }
//!
//! fn main() {
//!    let dolly = Sheep {
//!        name: "Dolly",
//!        age: 6,
//!    };
//!
//!    let cloned_sheep = dolly.clone();  // Famous cloned sheep!
//!}
//! ```

/// `Clone` trait defines the interface for cloning values.
pub trait Clone<T> {
    #[must_use]
    fn clone(self: @T) -> T;
}

/// Generic `Clone` implementation.
impl TCopyClone<T, +Copy<T>> of Clone<T> {
    /// Takes a snapshot of a copyable value and returns a clone of that value.
    ///
    /// # Examples
    ///
    /// ```
    /// let value: u8 = 0;
    /// let clone = value.clone();
    /// assert!(clone == value);
    fn clone(self: @T) -> T {
        *self
    }
}

/// Tuple `Clone` implementation.
impl TupleClone<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    impl CH: CloneHelper<TSF::SnapForward, T>,
    -Copy<T>,
> of Clone<T> {
    /// Takes a snapshot of a tuple and returns a clone of the viewed tuple.
    ///
    /// # Examples
    ///
    /// ```
    /// let tuple: (u8, u32) = (0, 1);
    /// let clone = (@tuple).clone();
    /// assert!(clone == tuple);
    /// ```
    ///
    /// Note that explicitly passing a snapshot to `clone` is not mandatory, as the compiler is able
    /// to infer that a snapshot is passed.
    ///
    /// ```
    /// let tuple: (u8, u32) = (0, 1);
    /// let clone = tuple.clone();
    /// assert!(clone == tuple);
    /// ```
    fn clone(self: @T) -> T {
        CH::clone(TSF::snap_forward(self))
    }
}

/// Trait helper for implementing `Clone` for tuples.
/// Provides a `Clone` function for tuples of snapshots, and basic snapshots.
trait CloneHelper<T, Cloned> {
    fn clone(value: T) -> Cloned;
}

/// An implementation of `CloneHelper` for a snapshot of any type with `Clone`
/// implementation.
impl CloneHelperByClone<T, +Clone<T>> of CloneHelper<@T, T> {
    fn clone(value: @T) -> T {
        value.clone()
    }
}

/// Base implementation of `CloneHelper` for tuples.
impl CloneHelperBaseTuple of CloneHelper<(), ()> {
    fn clone(value: ()) -> () {
        value
    }
}

/// Base implementation of `CloneHelper` for fixed-sized arrays.
impl FixedSizedArrayCloneHelper<T> of CloneHelper<[@T; 0], [T; 0]> {
    fn clone(value: [@T; 0]) -> [T; 0] {
        let [] = value;
        []
    }
}

/// Recursive implementation of `CloneHelper` for tuple style structs.
impl TupleNextCloneHelper<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    impl HeadNoSnap: crate::metaprogramming::SnapRemove<TH::Head>,
    impl RestNoSnap: crate::metaprogramming::SnapRemove<TH::Rest>,
    impl HeadHelper: CloneHelper<TH::Head, HeadNoSnap::Result>,
    impl RestHelper: CloneHelper<TH::Rest, RestNoSnap::Result>,
    impl TEF: crate::metaprogramming::TupleExtendFront<RestNoSnap::Result, HeadNoSnap::Result>,
    +Destruct<HeadNoSnap::Result>,
    +Drop<TH::Rest>,
> of CloneHelper<T, TEF::Result> {
    fn clone(value: T) -> TEF::Result {
        let (head, rest) = TH::split_head(value);
        let head = HeadHelper::clone(head);
        let rest = RestHelper::clone(rest);
        TEF::extend_front(rest, head)
    }
}
