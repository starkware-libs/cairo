pub trait Clone<T> {
    #[must_use]
    fn clone(self: @T) -> T;
}

impl TCopyClone<T, +Copy<T>> of Clone<T> {
    fn clone(self: @T) -> T {
        *self
    }
}

/// Tuple `Clone` implementation.
impl TupleClone<
    T,
    impl TSF: core::metaprogramming::TupleSnapForward<T>,
    impl CH: CloneHelper<TSF::SnapForward, T>,
    -Copy<T>,
> of Clone<T> {
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
    impl TH: core::metaprogramming::TupleSplit<T>,
    impl HeadNoSnap: core::metaprogramming::SnapRemove<TH::Head>,
    impl RestNoSnap: core::metaprogramming::SnapRemove<TH::Rest>,
    impl HeadHelper: CloneHelper<TH::Head, HeadNoSnap::Result>,
    impl RestHelper: CloneHelper<TH::Rest, RestNoSnap::Result>,
    impl TEF: core::metaprogramming::TupleExtendFront<RestNoSnap::Result, HeadNoSnap::Result>,
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
