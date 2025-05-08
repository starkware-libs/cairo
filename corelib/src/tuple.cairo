use crate::metaprogramming::{IsTuple, SnapRemove, TupleExtendFront, TupleSnapForward, TupleSplit};
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


pub(crate) impl TupleSize0Copy of Copy<()>;
pub(crate) impl TupleSize0Drop of Drop<()>;
impl TupleNextDrop<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +Drop<TH::Head>,
    +Drop<TH::Rest>,
> of Drop<T>;
impl TupleNextCopy<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +Copy<TH::Head>,
    +Copy<TH::Rest>,
> of Copy<T>;

pub impl TuplePartialEq<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    +TuplePartialEqHelper<TSF::SnapForward>,
> of PartialEq<T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::eq(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::ne(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
}

// A trait helper for implementing `PartialEq` for tuples.
pub(crate) trait TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool;
    fn ne(lhs: T, rhs: T) -> bool;
}

impl TuplePartialEqHelperByPartialEq<T, +PartialEq<T>> of TuplePartialEqHelper<@T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        lhs == rhs
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        lhs != rhs
    }
}

impl TuplePartialEqHelperBaseTuple of TuplePartialEqHelper<()> {
    fn eq(lhs: (), rhs: ()) -> bool {
        true
    }
    fn ne(lhs: (), rhs: ()) -> bool {
        false
    }
}


impl TuplePartialEqHelperNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +TuplePartialEqHelper<TS::Head>,
    +TuplePartialEqHelper<TS::Rest>,
    +Drop<TS::Rest>,
> of TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::eq(lhs_head, rhs_head)
            && TuplePartialEqHelper::<TS::Rest>::eq(lhs_rest, rhs_rest)
    }
    fn ne(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::ne(lhs_head, rhs_head)
            || TuplePartialEqHelper::<TS::Rest>::ne(lhs_rest, rhs_rest)
    }
}

impl DefaultTupleBase of Default<()> {
    fn default() -> () {
        ()
    }
}


pub(crate) impl DefaultNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +Default<TS::Head>,
    +Default<TS::Rest>,
    +Drop<TS::Head>,
> of Default<T> {
    fn default() -> T {
        TS::reconstruct(Default::default(), Default::default())
    }
}


impl TupleNextDestruct<
    T,
    impl TH: crate::metaprogramming::TupleSplit<T>,
    +Destruct<TH::Head>,
    +Destruct<TH::Rest>,
    -Drop<T>,
> of Destruct<T> {
    fn destruct(self: T) nopanic {
        let (_head, _rest) = TH::split_head(self);
    }
}


// Implementation of `Serde` for tuple style structs.
pub(crate) impl SerdeTuple<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    impl Serialize: SerializeTuple<TSF::SnapForward>,
    impl Deserialize: DeserializeTuple<T>,
> of Serde<T> {
    fn serialize(self: @T, ref output: Array<felt252>) {
        Serialize::serialize(TSF::snap_forward(self), ref output);
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        Deserialize::deserialize(ref serialized)
    }
}

// Helper trait for serializing tuple style structs.
trait SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>);
}

// Implementation of `SerializeTuple` for snapshots of types with `Serde` implementation.
impl SerdeBasedSerializeTuple<T, +Serde<T>> of SerializeTuple<@T> {
    fn serialize(value: @T, ref output: Array<felt252>) {
        Serde::<T>::serialize(value, ref output);
    }
}

// Helper trait for deserializing tuple style structs.
trait DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T>;
}

// Base implementation of `SerializeTuple` for tuples.
impl SerializeTupleBaseTuple of SerializeTuple<()> {
    fn serialize(value: (), ref output: Array<felt252>) {}
}

// Base implementation of `DeserializeTuple` for tuples.
impl DeserializeTupleBaseTuple of DeserializeTuple<()> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<()> {
        Some(())
    }
}

// Base implementation of `SerializeTuple` for fixed sized arrays.
impl SerializeTupleBaseFixedSizedArray<T> of SerializeTuple<[@T; 0]> {
    fn serialize(value: [@T; 0], ref output: Array<felt252>) {}
}

// Base implementation of `DeserializeTuple` for fixed sized arrays.
impl DeserializeTupleBaseFixedSizedArray<T> of DeserializeTuple<[T; 0]> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<[T; 0]> {
        Some([])
    }
}

// Recursive implementation of `SerializeTuple` for tuple style structs.
impl SerializeTupleNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +SerializeTuple<TS::Head>,
    +SerializeTuple<TS::Rest>,
    +Drop<TS::Rest>,
> of SerializeTuple<T> {
    fn serialize(value: T, ref output: Array<felt252>) {
        let (head, rest) = TS::split_head(value);
        SerializeTuple::<TS::Head>::serialize(head, ref output);
        SerializeTuple::<TS::Rest>::serialize(rest, ref output);
    }
}

// Recursive implementation of `DeserializeTuple` for tuple style structs.
impl DeserializeTupleNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +Serde<TS::Head>,
    +DeserializeTuple<TS::Rest>,
    +Drop<TS::Head>,
> of DeserializeTuple<T> {
    fn deserialize(ref serialized: Span<felt252>) -> Option<T> {
        let head = Serde::<TS::Head>::deserialize(ref serialized)?;
        let rest = DeserializeTuple::<TS::Rest>::deserialize(ref serialized)?;
        Some(TS::reconstruct(head, rest))
    }
}
