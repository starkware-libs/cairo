/// A trait that can be used to disable implementations based on the types of the generic args.
/// Assumes that `TypeEqualImpl` is the only implementation of this trait.
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
    E0, E1, E2, E3, E4, E5, E6, E7, E8
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8)>;
impl IsTupleTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9
> of IsTuple<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)>;

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
        head: E0, rest: (E1, E2, E3, E4, E5, E6)
    ) -> (E0, E1, E2, E3, E4, E5, E6) nopanic {
        let (e1, e2, e3, e4, e5, e6) = rest;
        (head, e1, e2, e3, e4, e5, e6)
    }
}
impl TupleSplitTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7)> {
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
impl TupleSplitTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8)> {
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
impl TupleSplitTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9
> of TupleSplit<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)> {
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
    E0, E1, E2, E3, E4, E
> of TupleExtendFront<(E0, E1, E2, E3, E4), E> {
    type Result = (E, E0, E1, E2, E3, E4);
    fn extend_front(value: (E0, E1, E2, E3, E4), element: E) -> (E, E0, E1, E2, E3, E4) nopanic {
        let (e0, e1, e2, e3, e4) = value;
        (element, e0, e1, e2, e3, e4)
    }
}
impl TupleExtendFrontTupleSize6<
    E0, E1, E2, E3, E4, E5, E
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5), element: E
    ) -> (E, E0, E1, E2, E3, E4, E5) nopanic {
        let (e0, e1, e2, e3, e4, e5) = value;
        (element, e0, e1, e2, e3, e4, e5)
    }
}
impl TupleExtendFrontTupleSize7<
    E0, E1, E2, E3, E4, E5, E6, E
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6), element: E
    ) -> (E, E0, E1, E2, E3, E4, E5, E6) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = value;
        (element, e0, e1, e2, e3, e4, e5, e6)
    }
}
impl TupleExtendFrontTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7, E
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7), element: E
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7)
    }
}
impl TupleExtendFrontTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E
> of TupleExtendFront<(E0, E1, E2, E3, E4, E5, E6, E7, E8), E> {
    type Result = (E, E0, E1, E2, E3, E4, E5, E6, E7, E8);
    fn extend_front(
        value: (E0, E1, E2, E3, E4, E5, E6, E7, E8), element: E
    ) -> (E, E0, E1, E2, E3, E4, E5, E6, E7, E8) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = value;
        (element, e0, e1, e2, e3, e4, e5, e6, e7, e8)
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
    E0, E1, E2, E3, E4, E5
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5);
    fn snap_forward(self: @(E0, E1, E2, E3, E4, E5)) -> (@E0, @E1, @E2, @E3, @E4, @E5) nopanic {
        let (e0, e1, e2, e3, e4, e5) = self;
        (e0, e1, e2, e3, e4, e5)
    }
}
impl TupleSnapForwardTupleSize7<
    E0, E1, E2, E3, E4, E5, E6
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6)
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6) = self;
        (e0, e1, e2, e3, e4, e5, e6)
    }
}
impl TupleSnapForwardTupleSize8<
    E0, E1, E2, E3, E4, E5, E6, E7
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7)
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7)
    }
}
impl TupleSnapForwardTupleSize9<
    E0, E1, E2, E3, E4, E5, E6, E7, E8
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8)
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8)
    }
}
impl TupleSnapForwardTupleSize10<
    E0, E1, E2, E3, E4, E5, E6, E7, E8, E9
> of TupleSnapForward<(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)> {
    type SnapForward = (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9);
    fn snap_forward(
        self: @(E0, E1, E2, E3, E4, E5, E6, E7, E8, E9)
    ) -> (@E0, @E1, @E2, @E3, @E4, @E5, @E6, @E7, @E8, @E9) nopanic {
        let (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9) = self;
        (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)
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
