/// A trait that can be used to disable implementations based on the types of the generic args.
/// Assumes that `TypeEqualImpl` is the only implementation of this trait.
pub trait TypeEqual<S, T> {}

impl TypeEqualImpl<T> of TypeEqual<T, T>;

/// A trait to help with implementation for traits of tuple like objects.
pub(crate) trait TupleHelper<T, const IS_TUPLE: bool> {
    /// The type of the first element of the tuple.
    type Head;
    /// The type of the rest of the tuple.
    type Rest;
    /// Splits the tuple into the head and the rest.
    fn split_head(self: T) -> (Self::Head, Self::Rest) nopanic;
    /// Reconstructs the tuple from the head and the rest.
    fn reconstruct(head: Self::Head, rest: Self::Rest) -> T nopanic;
}
impl TupleHelperForTupleSize1<E0> of TupleHelper<(E0,), { true }> {
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
impl TupleHelperForTupleSize2<E0, E1> of TupleHelper<(E0, E1), { true }> {
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
impl TupleHelperForTupleSize3<E0, E1, E2> of TupleHelper<(E0, E1, E2), { true }> {
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
impl TupleHelperForTupleSize4<E0, E1, E2, E3> of TupleHelper<(E0, E1, E2, E3), { true }> {
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
impl TupleHelperForTupleSize5<E0, E1, E2, E3, E4> of TupleHelper<(E0, E1, E2, E3, E4), { true }> {
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
impl TupleHelperForTupleSize6<
    E0, E1, E2, E3, E4, E5
> of TupleHelper<(E0, E1, E2, E3, E4, E5), { true }> {
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
impl TupleHelperForTupleSize7<
    E0, E1, E2, E3, E4, E5, E6
> of TupleHelper<(E0, E1, E2, E3, E4, E5, E6), { true }> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6);
    fn split_head(self: (E0, E1, E2, E3, E4, E5, E6)) -> (E0, (E1, E2, E3, E4, E5, E6)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = self;
        (e0, (e1, e2, e3, e4, e5, e6))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6)
    ) -> (E0, E1, E2, E3, E4, E5, E6) nopanic {
        let (e1, e2, e3, e4, e5, e6) = rest;
        (head, e1, e2, e3, e4, e5, e6)
    }
}
impl TupleHelperForTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7
> of TupleHelper<(E0, E1, E2, E3, E4, E5, E6, E7), { true }> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7)
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7)
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7)
    }
}
impl TupleHelperForTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8
> of TupleHelper<(E0, E1, E2, E3, E4, E5, E6, E7, E8), { true }> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8)
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8)
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8)
    }
}
impl TupleHelperForTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9
> of TupleHelper<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9), { true }> {
    type Head = E0;
    type Rest = (E1, E2, E3, E4, E5, E6, E7, E8, E9);
    fn split_head(
        self: (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)
    ) -> (E0, (E1, E2, E3, E4, E5, E6, E7, E8, E9)) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9) = self;
        (e0, (e1, e2, e3, e4, e5, e6, e7, e8, e9))
    }
    fn reconstruct(
        head: E0, rest: (E1, E2, E3, E4, E5, E6, E7, E8, E9)
    ) -> (E0, E1, E2, E3, E4, E5, E6, E7, E8, E9) nopanic {
        let (e1, e2, e3, e4, e5, e6, e7, e8, e9) = rest;
        (head, e1, e2, e3, e4, e5, e6, e7, e8, e9)
    }
}
impl TupleHelperForFixedSizedArraySized1<T> of TupleHelper<[T; 1], { false }> {
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
impl TupleHelperForFixedSizedArraySized2<T> of TupleHelper<[T; 2], { false }> {
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
impl TupleHelperForFixedSizedArraySized3<T> of TupleHelper<[T; 3], { false }> {
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
impl TupleHelperForFixedSizedArraySized4<T> of TupleHelper<[T; 4], { false }> {
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
impl TupleHelperForFixedSizedArraySized5<T> of TupleHelper<[T; 5], { false }> {
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
impl TupleHelperForFixedSizedArraySized6<T> of TupleHelper<[T; 6], { false }> {
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
impl TupleHelperForFixedSizedArraySized7<T> of TupleHelper<[T; 7], { false }> {
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
impl TupleHelperForFixedSizedArraySized8<T> of TupleHelper<[T; 8], { false }> {
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
impl TupleHelperForFixedSizedArraySized9<T> of TupleHelper<[T; 9], { false }> {
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
impl TupleHelperForFixedSizedArraySized10<T> of TupleHelper<[T; 10], { false }> {
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
