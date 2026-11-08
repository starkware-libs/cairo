//! Regression: deriving a trait on a struct member whose type is an associated-type path through
//! a generic/impl param (e.g. `S::Corners`) must emit that member's trait bound. The member used
//! to be misclassified as non-generics-dependent, so the bound was omitted and the derived impl
//! failed to type-check (E2111 for Drop, E2311 for Clone).

trait Shape<T> {
    type Corners;
}

impl U8Shape of Shape<u8> {
    type Corners = felt252;
}

#[derive(Drop, Clone)]
struct Poly<T, impl S: Shape<T>> {
    corners: S::Corners,
}

#[test]
fn test_derive_on_associated_type_member() {
    let poly = Poly::<u8, U8Shape> { corners: 7 };
    let cloned = poly.clone();
    assert!(cloned.corners == 7);
}
