//! The `Clone` trait provides the ability to duplicate instances of types that cannot be
//! 'implicitly copied'.
//!
//! In Cairo, some simple types are "implicitly copyable": when you assign them or pass them as
//! arguments, the receiver will get a copy, leaving the original value in place. These types do not
//! require allocation to copy, and are not at risk of accessing un-allocated memory, so the
//! compiler considers them cheap and safe to copy. For other types, copies must be made explicitly,
//! by convention implementing the [`Clone`] trait and calling the [`Clone::clone`] method.
//!
//! # Examples
//!
//! ```
//! let arr = array![1, 2, 3];
//! let cloned_arr = arr.clone();
//! assert!(arr == cloned_arr);
//! ```
//!
//! To easily implement the `Clone` trait, you can also use
//! `#[derive(Clone)]`:
//!
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
    /// Takes a snapshot of a copyable value and returns a clone of that value.
    ///
    /// # Examples
    ///
    /// ```
    /// let arr = array![1, 2, 3];
    /// let cloned_arr = arr.clone();
    /// assert!(arr == cloned_arr);
    /// ```
    #[must_use]
    fn clone(self: @T) -> T;
}

impl TCopyClone<T, +Copy<T>> of Clone<T> {
    fn clone(self: @T) -> T {
        *self
    }
}

impl TupleClone<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    impl CH: CloneHelper<TSF::SnapForward, T>,
    -Copy<T>,
> of Clone<T> {
    fn clone(self: @T) -> T {
        CH::clone(TSF::snap_forward(self))
    }
}

// Trait helper for implementing `Clone` for tuples.
// Provides a `clone` function for tuples of snapshots, and basic snapshots.
trait CloneHelper<T, Cloned> {
    fn clone(value: T) -> Cloned;
}

impl CloneHelperByClone<T, +Clone<T>> of CloneHelper<@T, T> {
    fn clone(value: @T) -> T {
        value.clone()
    }
}

impl CloneHelperBaseTuple of CloneHelper<(), ()> {
    fn clone(value: ()) -> () {
        value
    }
}

impl FixedSizedArrayCloneHelper<T> of CloneHelper<[@T; 0], [T; 0]> {
    fn clone(value: [@T; 0]) -> [T; 0] {
        let [] = value;
        []
    }
}

// Recursive implementation of `CloneHelper` for tuple style structs.
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
