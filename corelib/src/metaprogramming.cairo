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

impl IsTupleTupleSize0 of IsTuple<()>;
impl IsTupleTupleSize1<E0> of IsTuple<(E0,)>;
impl IsTupleTupleSize2<E0, E1> of IsTuple<(E0, E1)>;
impl IsTupleTupleSize3<E0, E1, E2> of IsTuple<(E0, E1, E2)>;
impl IsTupleTupleSize4<E0, E1, E2, E3> of IsTuple<(E0, E1, E2, E3)>;
impl IsTupleTupleSize5<E0, E1, E2, E3, E4> of IsTuple<(E0, E1, E2, E3, E4)>;
impl IsTupleTupleSize6<E0, E1, E2, E3, E4, E5> of IsTuple<(E0, E1, E2, E3, E4, E5)>;
impl IsTupleTupleSize7<E0, E1, E2, E3, E4, E5, E6> of IsTuple<(E0, E1, E2, E3, E4, E5, E6)>;
impl IsTupleTupleSize8<E0, E1, E2, E3, E4, E5, E6, E7> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7)>;
impl IsTupleTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8)>;
impl IsTupleTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)>;
impl IsTupleTupleSize11<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)>;
impl IsTupleTupleSize12<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11)>;
impl IsTupleTupleSize13<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)>;
impl IsTupleTupleSize14<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13)>;
impl IsTupleTupleSize15<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)>;
impl IsTupleTupleSize16<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15,
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)>;

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

impl TupleSplitTupleSize1<E0> of TupleSplit<(E0,)> {
    type Head = E0;
    type Rest = ();
    fn split_head(self: (E0,)) -> (E0, ()) nopanic {
        let (e0,) = self;
        (e0, ())
    }
    fn reconstruct(head: E0, rest: ()) -> (E0,) nopanic {
        (head,)
    }
}

impl TupleSplitTupleSize2<E0, E1> of TupleSplit<(E0, E1)> {
    type Head = E0;
    type Rest = (E1,);
    fn split_head(self: (E0, E1)) -> (E0, (E1,)) nopanic {
        let (e0, e1) = self;
        (e0, (e1,))
    }
    fn reconstruct(head: E0, rest: (E1,)) -> (E0, E1) nopanic {
        let (e1,) = rest;
        (head, e1)
    }
}

impl TupleSplitTupleSize3<E0, E1, E2> of TupleSplit<(E0, E1, E2)> {
    type Head = E0;
    type Rest = (E1, E2);
    fn split_head(self: (E0, E1, E2)) -> (E0, (E1, E2)) nopanic {
        let (e0, e1, e2) = self;
        (e0, (e1, e2))
    }
    fn reconstruct(head: E0, rest: (E1, E2)) -> (E0, E1, E2) nopanic {
        let (e1, e2) = rest;
        (head, e1, e2)
    }
}

impl TupleSplitTupleSize4<E0, E1, E2, E3> of TupleSplit<(E0, E1, E2, E3)> {
    type Head = E0;
    type Rest = (E1, E2, E3);
    fn split_head(self: (E0, E1, E2, E3)) -> (E0, (E1, E2, E3)) nopanic {
        let (e0, e1, e2, e3) = self;
        (e0, (e1, e2, e3))
    }
    fn reconstruct(head: E0, rest: (E1, E2, E3)) -> (E0, E1, E2, E3) nopanic {
        let (e1, e2, e3) = rest;
        (head, e1, e2, e3)
    }
}

impl TupleSplitTupleSize5<E0, E1, E2, E3, E4> of TupleSplit<(E0, E1, E2, E3, E4)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4);
    fn split_head(self: (E0, E1, E2, E3, E4)) -> (E0, (E1, E2, E3, E4)) nopanic {
        let (e0, e1, e2, e3, e4) = self;
        (e0, (e1, e2, e3, e4))
    }
    fn reconstruct(head: E0, rest: (E1, E2, E3, E4)) -> (E0, E1, E2, E3, E4) nopanic {
        let (e1, e2, e3, e4) = rest;
        (head, e1, e2, e3, e4)
    }
}

impl TupleSplitTupleSize6<E0, E1, E2, E3, E4, E5> of TupleSplit<(E0, E1, E2, E3, E4, E5)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5);
    fn split_head(self: (E0, E1, E2, E3, E4, E5)) -> (E0, (E1, E2, E3, E4, E5)) nopanic {
        let (e0, e1, e2, e3, e4, e5) = self;
        (e0, (e1, e2, e3, e4, e5))
    }
    fn reconstruct(head: E0, rest: (E1, E2, E3, E4, E5)) -> (E0, E1, E2, E3, E4, E5) nopanic {
        let (e1, e2, e3, e4, e5) = rest;
        (head, e1, e2, e3, e4, e5)
    }
}

impl TupleSplitTupleSize7<E0, E1, E2, E3, E4, E5, E6> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6);
    fn split_head(self: (E0, E1, E2, E3, E4, E5, E6)) -> (E0, (E1, E2, E3, E4, E5, E6)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = self;
        (e0, (e1, e2, e3, e4, e5, e6))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6),
    ) -> (E0, E1, E2, E3, E4, E5, E6) nopanic {
        let (e1, e2, e3, e4, e5, e6) = rest;
        (head, e1, e2, e3, e4, e5, e6)
    }
}

impl TupleSplitTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7)
    }
}

impl TupleSplitTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8)
    }
}

impl TupleSplitTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9)
    }
}

impl TupleSplitTupleSize11<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
    }
}

impl TupleSplitTupleSize12<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
    }
}

impl TupleSplitTupleSize13<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
    }
}

impl TupleSplitTupleSize14<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
    }
}

impl TupleSplitTupleSize15<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)
    }
}

impl TupleSplitTupleSize16<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15,
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15),
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15),
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)
    }
}

impl TupleSplitFixedSizedArraySized1<T> of TupleSplit<[T; 1]> {
    type Head = T;
    type Rest = [T; 0];
    fn split_head(self: [T; 1]) -> (T, [T; 0]) nopanic {
        let [e0] = self;
        (e0, [])
    }
    fn reconstruct(head: T, rest: [T; 0]) -> [T; 1] nopanic {
        let [] = rest;
        [head]
    }
}

impl TupleSplitFixedSizedArraySized2<T> of TupleSplit<[T; 2]> {
    type Head = T;
    type Rest = [T; 1];
    fn split_head(self: [T; 2]) -> (T, [T; 1]) nopanic {
        let [e0, e1] = self;
        (e0, [e1])
    }
    fn reconstruct(head: T, rest: [T; 1]) -> [T; 2] nopanic {
        let [e1] = rest;
        [head, e1]
    }
}

impl TupleSplitFixedSizedArraySized3<T> of TupleSplit<[T; 3]> {
    type Head = T;
    type Rest = [T; 2];
    fn split_head(self: [T; 3]) -> (T, [T; 2]) nopanic {
        let [e0, e1, e2] = self;
        (e0, [e1, e2])
    }
    fn reconstruct(head: T, rest: [T; 2]) -> [T; 3] nopanic {
        let [e1, e2] = rest;
        [head, e1, e2]
    }
}

impl TupleSplitFixedSizedArraySized4<T> of TupleSplit<[T; 4]> {
    type Head = T;
    type Rest = [T; 3];
    fn split_head(self: [T; 4]) -> (T, [T; 3]) nopanic {
        let [e0, e1, e2, e3] = self;
        (e0, [e1, e2, e3])
    }
    fn reconstruct(head: T, rest: [T; 3]) -> [T; 4] nopanic {
        let [e1, e2, e3] = rest;
        [head, e1, e2, e3]
    }
}

impl TupleSplitFixedSizedArraySized5<T> of TupleSplit<[T; 5]> {
    type Head = T;
    type Rest = [T; 4];
    fn split_head(self: [T; 5]) -> (T, [T; 4]) nopanic {
        let [e0, e1, e2, e3, e4] = self;
        (e0, [e1, e2, e3, e4])
    }
    fn reconstruct(head: T, rest: [T; 4]) -> [T; 5] nopanic {
        let [e1, e2, e3, e4] = rest;
        [head, e1, e2, e3, e4]
    }
}

impl TupleSplitFixedSizedArraySized6<T> of TupleSplit<[T; 6]> {
    type Head = T;
    type Rest = [T; 5];
    fn split_head(self: [T; 6]) -> (T, [T; 5]) nopanic {
        let [e0, e1, e2, e3, e4, e5] = self;
        (e0, [e1, e2, e3, e4, e5])
    }
    fn reconstruct(head: T, rest: [T; 5]) -> [T; 6] nopanic {
        let [e1, e2, e3, e4, e5] = rest;
        [head, e1, e2, e3, e4, e5]
    }
}

impl TupleSplitFixedSizedArraySized7<T> of TupleSplit<[T; 7]> {
    type Head = T;
    type Rest = [T; 6];
    fn split_head(self: [T; 7]) -> (T, [T; 6]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6] = self;
        (e0, [e1, e2, e3, e4, e5, e6])
    }
    fn reconstruct(head: T, rest: [T; 6]) -> [T; 7] nopanic {
        let [e1, e2, e3, e4, e5, e6] = rest;
        [head, e1, e2, e3, e4, e5, e6]
    }
}

impl TupleSplitFixedSizedArraySized8<T> of TupleSplit<[T; 8]> {
    type Head = T;
    type Rest = [T; 7];
    fn split_head(self: [T; 8]) -> (T, [T; 7]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7])
    }
    fn reconstruct(head: T, rest: [T; 7]) -> [T; 8] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7]
    }
}

impl TupleSplitFixedSizedArraySized9<T> of TupleSplit<[T; 9]> {
    type Head = T;
    type Rest = [T; 8];
    fn split_head(self: [T; 9]) -> (T, [T; 8]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8])
    }
    fn reconstruct(head: T, rest: [T; 8]) -> [T; 9] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8]
    }
}

impl TupleSplitFixedSizedArraySized10<T> of TupleSplit<[T; 10]> {
    type Head = T;
    type Rest = [T; 9];
    fn split_head(self: [T; 10]) -> (T, [T; 9]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9])
    }
    fn reconstruct(head: T, rest: [T; 9]) -> [T; 10] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9]
    }
}

impl TupleSplitFixedSizedArraySized11<T> of TupleSplit<[T; 11]> {
    type Head = T;
    type Rest = [T; 10];
    fn split_head(self: [T; 11]) -> (T, [T; 10]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10])
    }
    fn reconstruct(head: T, rest: [T; 10]) -> [T; 11] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10]
    }
}

impl TupleSplitFixedSizedArraySized12<T> of TupleSplit<[T; 12]> {
    type Head = T;
    type Rest = [T; 11];
    fn split_head(self: [T; 12]) -> (T, [T; 11]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11])
    }
    fn reconstruct(head: T, rest: [T; 11]) -> [T; 12] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]
    }
}

impl TupleSplitFixedSizedArraySized13<T> of TupleSplit<[T; 13]> {
    type Head = T;
    type Rest = [T; 12];
    fn split_head(self: [T; 13]) -> (T, [T; 12]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12])
    }
    fn reconstruct(head: T, rest: [T; 12]) -> [T; 13] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
    }
}

impl TupleSplitFixedSizedArraySized14<T> of TupleSplit<[T; 14]> {
    type Head = T;
    type Rest = [T; 13];
    fn split_head(self: [T; 14]) -> (T, [T; 13]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13])
    }
    fn reconstruct(head: T, rest: [T; 13]) -> [T; 14] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13]
    }
}

impl TupleSplitFixedSizedArraySized15<T> of TupleSplit<[T; 15]> {
    type Head = T;
    type Rest = [T; 14];
    fn split_head(self: [T; 15]) -> (T, [T; 14]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14])
    }
    fn reconstruct(head: T, rest: [T; 14]) -> [T; 15] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]
    }
}

impl TupleSplitFixedSizedArraySized16<T> of TupleSplit<[T; 16]> {
    type Head = T;
    type Rest = [T; 15];
    fn split_head(self: [T; 16]) -> (T, [T; 15]) nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15] = self;
        (e0, [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15])
    }
    fn reconstruct(head: T, rest: [T; 15]) -> [T; 16] nopanic {
        let [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15] = rest;
        [head, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15]
    }
}

/// A trait for extending a tuple from the front.
pub(crate) trait TupleExtendFront<T, E> {
    /// The type of the resulting tuple.
    type Result;
    /// Creates a new tuple from the `value` tuple with `element` in front of it.
    fn extend_front(value: T, element: E) -> Self::Result nopanic;
}

impl TupleExtendFrontTupleSize0<E> of TupleExtendFront<(), E> {
    type Result = (E,);
    fn extend_front(value: (), element: E) -> (E,) nopanic {
        (element,)
    }
}

impl TupleExtendFrontTupleSize1<E0, E> of TupleExtendFront<(E0,), E> {
    type Result = (E, E0);
    fn extend_front(value: (E0,), element: E) -> (E, E0) nopanic {
        let (e0,) = value;
        (element, e0)
    }
}

impl TupleExtendFrontTupleSize2<E0, E1, E> of TupleExtendFront<(E0, E1), E> {
    type Result = (E, E0, E1);
    fn extend_front(value: (E0, E1), element: E) -> (E, E0, E1) nopanic {
        let (e0, e1) = value;
        (element, e0, e1)
    }
}

impl TupleExtendFrontTupleSize3<E0, E1, E2, E> of TupleExtendFront<(E0, E1, E2), E> {
    type Result = (E, E0, E1, E2);
    fn extend_front(value: (E0, E1, E2), element: E) -> (E, E0, E1, E2) nopanic {
        let (e0, e1, e2) = value;
        (element, e0, e1, e2)
    }
}

impl TupleExtendFrontTupleSize4<E0, E1, E2, E3, E> of TupleExtendFront<(E0, E1, E2, E3), E> {
    type Result = (E, E0, E1, E2, E3);
    fn extend_front(value: (E0, E1, E2, E3), element: E) -> (E, E0, E1, E2, E3) nopanic {
        let (e0, e1, e2, e3) = value;
        (element, e0, e1, e2, e3)
    }
}

impl TupleExtendFrontTupleSize5<
    E0, E1, E2, E3, E4, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4), E> {
    type Result = (E, E0, E1, E2, E3, E4);
    fn extend_front(value: (E0, E1, E2, E3, E4), element: E) -> (E, E0, E1, E2, E3, E4) nopanic {
        let (e0, e1, e2, e3, e4) = value;
        (element, e0, e1, e2, e3, e4)
    }
}

impl TupleExtendFrontTupleSize6<
    E0, E1, E2, E3, E4, E5, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5) nopanic {
        let (e0, e1, e2, e3, e4, e5) = value;
        (element, e0, e1, e2, e3, e4, e5)
    }
}

impl TupleExtendFrontTupleSize7<
    E0, E1, E2, E3, E4, E5, E6, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = value;
        (element, e0, e1, e2, e3, e4, e5, e6)
    }
}

impl TupleExtendFrontTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7)
    }
}

impl TupleExtendFrontTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8)
    }
}

impl TupleExtendFrontTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)
    }
}

impl TupleExtendFrontTupleSize11<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
    }
}

impl TupleExtendFrontTupleSize12<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
    }
}

impl TupleExtendFrontTupleSize13<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
    }
}

impl TupleExtendFrontTupleSize14<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
    }
}

impl TupleExtendFrontTupleSize15<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E,
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14), element: E,
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)
    }
}

impl TupleExtendFrontFixedSizedArraySize0<T> of TupleExtendFront<[T; 0], T> {
    type Result = [T; 1];
    fn extend_front(value: [T; 0], element: T) -> [T; 1] nopanic {
        let [] = value;
        [element]
    }
}

impl TupleExtendFrontFixedSizedArraySize1<T> of TupleExtendFront<[T; 1], T> {
    type Result = [T; 2];
    fn extend_front(value: [T; 1], element: T) -> [T; 2] nopanic {
        let [e0] = value;
        [element, e0]
    }
}

impl TupleExtendFrontFixedSizedArraySize2<T> of TupleExtendFront<[T; 2], T> {
    type Result = [T; 3];
    fn extend_front(value: [T; 2], element: T) -> [T; 3] nopanic {
        let [e0, e1] = value;
        [element, e0, e1]
    }
}

impl TupleExtendFrontFixedSizedArraySize3<T> of TupleExtendFront<[T; 3], T> {
    type Result = [T; 4];
    fn extend_front(value: [T; 3], element: T) -> [T; 4] nopanic {
        let [e0, e1, e2] = value;
        [element, e0, e1, e2]
    }
}

impl TupleExtendFrontFixedSizedArraySize4<T> of TupleExtendFront<[T; 4], T> {
    type Result = [T; 5];
    fn extend_front(value: [T; 4], element: T) -> [T; 5] nopanic {
        let [e0, e1, e2, e3] = value;
        [element, e0, e1, e2, e3]
    }
}

impl TupleExtendFrontFixedSizedArraySize5<T> of TupleExtendFront<[T; 5], T> {
    type Result = [T; 6];
    fn extend_front(value: [T; 5], element: T) -> [T; 6] nopanic {
        let [e0, e1, e2, e3, e4] = value;
        [element, e0, e1, e2, e3, e4]
    }
}

impl TupleExtendFrontFixedSizedArraySize6<T> of TupleExtendFront<[T; 6], T> {
    type Result = [T; 7];
    fn extend_front(value: [T; 6], element: T) -> [T; 7] nopanic {
        let [e0, e1, e2, e3, e4, e5] = value;
        [element, e0, e1, e2, e3, e4, e5]
    }
}

impl TupleExtendFrontFixedSizedArraySize7<T> of TupleExtendFront<[T; 7], T> {
    type Result = [T; 8];
    fn extend_front(value: [T; 7], element: T) -> [T; 8] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6] = value;
        [element, e0, e1, e2, e3, e4, e5, e6]
    }
}

impl TupleExtendFrontFixedSizedArraySize8<T> of TupleExtendFront<[T; 8], T> {
    type Result = [T; 9];
    fn extend_front(value: [T; 8], element: T) -> [T; 9] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7]
    }
}

impl TupleExtendFrontFixedSizedArraySize9<T> of TupleExtendFront<[T; 9], T> {
    type Result = [T; 10];
    fn extend_front(value: [T; 9], element: T) -> [T; 10] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8]
    }
}

impl TupleExtendFrontFixedSizedArraySize10<T> of TupleExtendFront<[T; 10], T> {
    type Result = [T; 11];
    fn extend_front(value: [T; 10], element: T) -> [T; 11] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9]
    }
}

impl TupleExtendFrontFixedSizedArraySize11<T> of TupleExtendFront<[T; 11], T> {
    type Result = [T; 12];
    fn extend_front(value: [T; 11], element: T) -> [T; 12] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10]
    }
}

impl TupleExtendFrontFixedSizedArraySize12<T> of TupleExtendFront<[T; 12], T> {
    type Result = [T; 13];
    fn extend_front(value: [T; 12], element: T) -> [T; 13] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]
    }
}

impl TupleExtendFrontFixedSizedArraySize13<T> of TupleExtendFront<[T; 13], T> {
    type Result = [T; 14];
    fn extend_front(value: [T; 13], element: T) -> [T; 14] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
    }
}

impl TupleExtendFrontFixedSizedArraySize14<T> of TupleExtendFront<[T; 14], T> {
    type Result = [T; 15];
    fn extend_front(value: [T; 14], element: T) -> [T; 15] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13]
    }
}

impl TupleExtendFrontFixedSizedArraySize15<T> of TupleExtendFront<[T; 15], T> {
    type Result = [T; 16];
    fn extend_front(value: [T; 15], element: T) -> [T; 16] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] = value;
        [element, e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]
    }
}

/// A trait for forwarding a wrapping snapshot from a tuple style struct into a tuple style struct
/// of the snapshots.
pub(crate) trait TupleSnapForward<T> {
    type SnapForward;
    fn snap_forward(self: @T) -> Self::SnapForward nopanic;
}

impl TupleSnapForwardTupleSize0 of TupleSnapForward<()> {
    type SnapForward = ();
    fn snap_forward(self: @()) -> () nopanic {
        ()
    }
}

impl TupleSnapForwardTupleSize1<E0> of TupleSnapForward<(E0,)> {
    type SnapForward = (@E0,);
    fn snap_forward(self: @(E0,)) -> (@E0,) nopanic {
        let (e0,) = self;
        (e0,)
    }
}

impl TupleSnapForwardTupleSize2<E0, E1> of TupleSnapForward<(E0, E1)> {
    type SnapForward = (@E0, @E1);
    fn snap_forward(self: @(E0, E1)) -> (@E0, @E1) nopanic {
        let (e0, e1) = self;
        (e0, e1)
    }
}

impl TupleSnapForwardTupleSize3<E0, E1, E2> of TupleSnapForward<(E0, E1, E2)> {
    type SnapForward = (@E0, @E1, @E2);
    fn snap_forward(self: @(E0, E1, E2)) -> (@E0, @E1, @E2) nopanic {
        let (e0, e1, e2) = self;
        (e0, e1, e2)
    }
}

impl TupleSnapForwardTupleSize4<E0, E1, E2, E3> of TupleSnapForward<(E0, E1, E2, E3)> {
    type SnapForward = (@E0, @E1, @E2, @E3);
    fn snap_forward(self: @(E0, E1, E2, E3)) -> (@E0, @E1, @E2, @E3) nopanic {
        let (e0, e1, e2, e3) = self;
        (e0, e1, e2, e3)
    }
}

impl TupleSnapForwardTupleSize5<E0, E1, E2, E3, E4> of TupleSnapForward<(E0, E1, E2, E3, E4)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4);
    fn snap_forward(self: @(E0, E1, E2, E3, E4)) -> (@E0, @E1, @E2, @E3, @E4) nopanic {
        let (e0, e1, e2, e3, e4) = self;
        (e0, e1, e2, e3, e4)
    }
}

impl TupleSnapForwardTupleSize6<
    E0, E1, E2, E3, E4, E5,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5);
    fn snap_forward(self: @(E0, E1, E2, E3, E4, E5)) -> (@E0, @E1, @E2, @E3, @E4, @E5) nopanic {
        let (e0, e1, e2, e3, e4, e5) = self;
        (e0, e1, e2, e3, e4, e5)
    }
}

impl TupleSnapForwardTupleSize7<
    E0, E1, E2, E3, E4, E5, E6,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = self;
        (e0, e1, e2, e3, e4, e5, e6)
    }
}

impl TupleSnapForwardTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7)
    }
}

impl TupleSnapForwardTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8)
    }
}

impl TupleSnapForwardTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)
    }
}

impl TupleSnapForwardTupleSize11<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
    }
}

impl TupleSnapForwardTupleSize12<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11)
    }
}

impl TupleSnapForwardTupleSize13<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12)
    }
}

impl TupleSnapForwardTupleSize14<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13)
    }
}

impl TupleSnapForwardTupleSize15<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)> {
    type SnapForward = (
        @E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13, @E14,
    );
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14),
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13, @E14) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14)
    }
}

impl TupleSnapForwardTupleSize16<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15,
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)> {
    type SnapForward = (
        @E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13, @E14, @E15,
    );
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15),
    ) -> (
        @E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9, @E10, @E11, @E12, @E13, @E14, @E15,
    ) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)
    }
}

impl TupleSnapForwardFixedSizedArraySized0<T> of TupleSnapForward<[T; 0]> {
    type SnapForward = [@T; 0];
    fn snap_forward(self: @[T; 0]) -> [@T; 0] nopanic {
        []
    }
}

impl TupleSnapForwardFixedSizedArraySized1<T> of TupleSnapForward<[T; 1]> {
    type SnapForward = [@T; 1];
    fn snap_forward(self: @[T; 1]) -> [@T; 1] nopanic {
        let [e0] = self;
        [e0]
    }
}

impl TupleSnapForwardFixedSizedArraySized2<T> of TupleSnapForward<[T; 2]> {
    type SnapForward = [@T; 2];
    fn snap_forward(self: @[T; 2]) -> [@T; 2] nopanic {
        let [e0, e1] = self;
        [e0, e1]
    }
}

impl TupleSnapForwardFixedSizedArraySized3<T> of TupleSnapForward<[T; 3]> {
    type SnapForward = [@T; 3];
    fn snap_forward(self: @[T; 3]) -> [@T; 3] nopanic {
        let [e0, e1, e2] = self;
        [e0, e1, e2]
    }
}

impl TupleSnapForwardFixedSizedArraySized4<T> of TupleSnapForward<[T; 4]> {
    type SnapForward = [@T; 4];
    fn snap_forward(self: @[T; 4]) -> [@T; 4] nopanic {
        let [e0, e1, e2, e3] = self;
        [e0, e1, e2, e3]
    }
}

impl TupleSnapForwardFixedSizedArraySized5<T> of TupleSnapForward<[T; 5]> {
    type SnapForward = [@T; 5];
    fn snap_forward(self: @[T; 5]) -> [@T; 5] nopanic {
        let [e0, e1, e2, e3, e4] = self;
        [e0, e1, e2, e3, e4]
    }
}

impl TupleSnapForwardFixedSizedArraySized6<T> of TupleSnapForward<[T; 6]> {
    type SnapForward = [@T; 6];
    fn snap_forward(self: @[T; 6]) -> [@T; 6] nopanic {
        let [e0, e1, e2, e3, e4, e5] = self;
        [e0, e1, e2, e3, e4, e5]
    }
}

impl TupleSnapForwardFixedSizedArraySized7<T> of TupleSnapForward<[T; 7]> {
    type SnapForward = [@T; 7];
    fn snap_forward(self: @[T; 7]) -> [@T; 7] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6] = self;
        [e0, e1, e2, e3, e4, e5, e6]
    }
}

impl TupleSnapForwardFixedSizedArraySized8<T> of TupleSnapForward<[T; 8]> {
    type SnapForward = [@T; 8];
    fn snap_forward(self: @[T; 8]) -> [@T; 8] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7]
    }
}

impl TupleSnapForwardFixedSizedArraySized9<T> of TupleSnapForward<[T; 9]> {
    type SnapForward = [@T; 9];
    fn snap_forward(self: @[T; 9]) -> [@T; 9] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8]
    }
}

impl TupleSnapForwardFixedSizedArraySized10<T> of TupleSnapForward<[T; 10]> {
    type SnapForward = [@T; 10];
    fn snap_forward(self: @[T; 10]) -> [@T; 10] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9]
    }
}

impl TupleSnapForwardFixedSizedArraySized11<T> of TupleSnapForward<[T; 11]> {
    type SnapForward = [@T; 11];
    fn snap_forward(self: @[T; 11]) -> [@T; 11] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10]
    }
}

impl TupleSnapForwardFixedSizedArraySized12<T> of TupleSnapForward<[T; 12]> {
    type SnapForward = [@T; 12];
    fn snap_forward(self: @[T; 12]) -> [@T; 12] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11]
    }
}

impl TupleSnapForwardFixedSizedArraySized13<T> of TupleSnapForward<[T; 13]> {
    type SnapForward = [@T; 13];
    fn snap_forward(self: @[T; 13]) -> [@T; 13] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12]
    }
}

impl TupleSnapForwardFixedSizedArraySized14<T> of TupleSnapForward<[T; 14]> {
    type SnapForward = [@T; 14];
    fn snap_forward(self: @[T; 14]) -> [@T; 14] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13]
    }
}

impl TupleSnapForwardFixedSizedArraySized15<T> of TupleSnapForward<[T; 15]> {
    type SnapForward = [@T; 15];
    fn snap_forward(self: @[T; 15]) -> [@T; 15] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14]
    }
}

impl TupleSnapForwardFixedSizedArraySized16<T> of TupleSnapForward<[T; 16]> {
    type SnapForward = [@T; 16];
    fn snap_forward(self: @[T; 16]) -> [@T; 16] nopanic {
        let [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15] = self;
        [e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15]
    }
}

/// A trait for removing a wrapping snapshot from the types in tuple style struct.
pub(crate) trait SnapRemove<T> {
    type Result;
}

impl SnapRemoveSnap<T> of SnapRemove<@T> {
    type Result = T;
}

impl SnapRemoveTupleBase of SnapRemove<()> {
    type Result = ();
}

impl SnapRemoveTupleNext<
    T,
    +IsTuple<T>,
    impl TS: TupleSplit<T>,
    impl HeadNoSnap: SnapRemove<TS::Head>,
    impl RestNoSnap: SnapRemove<TS::Rest>,
    impl TEF: TupleExtendFront<RestNoSnap::Result, HeadNoSnap::Result>,
> of SnapRemove<T> {
    type Result = TEF::Result;
}

impl SnapRemoveFixedSizedArray<T, const N: usize> of SnapRemove<[@T; N]> {
    type Result = [T; N];
}
