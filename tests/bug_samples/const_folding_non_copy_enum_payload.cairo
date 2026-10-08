// Enum constructs around a moved non-copyable payload must not forward the payload variable,
// neither to a kept match nor to a specialized call.

#[inline(never)]
fn keep_snap(e: @Option<Array<felt252>>) -> bool {
    e.is_some()
}

#[inline(never)]
fn keep(e: Option<Array<felt252>>) -> Option<Array<felt252>> {
    e
}

#[inline(never)]
fn tick(
    a: felt252, b: felt252, c: felt252, d: felt252, e: felt252, f: felt252, g: felt252, h: felt252,
) -> felt252 {
    a + b + c + d + e + f + g + h
}

// Too large to inline, so calls with a known variant are specialized.
fn consume(e: Option<Array<felt252>>) -> felt252 {
    if let Option::Some(v) = e {
        return v.len().into();
    }
    let y0 = tick(0, 0, 0, 0, 0, 0, 0, 0);
    let y1 = tick(y0, y0, y0, y0, y0, y0, y0, y0);
    let y2 = tick(y1, y1, y1, y1, y1, y1, y1, y1);
    let y3 = tick(y2, y2, y2, y2, y2, y2, y2, y2);
    let y4 = tick(y3, y3, y3, y3, y3, y3, y3, y3);
    let y5 = tick(y4, y4, y4, y4, y4, y4, y4, y4);
    let y6 = tick(y5, y5, y5, y5, y5, y5, y5, y5);
    let y7 = tick(y6, y6, y6, y6, y6, y6, y6, y6);
    let y8 = tick(y7, y7, y7, y7, y7, y7, y7, y7);
    let y9 = tick(y8, y8, y8, y8, y8, y8, y8, y8);
    let y10 = tick(y9, y9, y9, y9, y9, y9, y9, y9);
    let y11 = tick(y10, y10, y10, y10, y10, y10, y10, y10);
    let y12 = tick(y11, y11, y11, y11, y11, y11, y11, y11);
    let y13 = tick(y12, y12, y12, y12, y12, y12, y12, y12);
    let y14 = tick(y13, y13, y13, y13, y13, y13, y13, y13);
    let y15 = tick(y14, y14, y14, y14, y14, y14, y14, y14);
    y15
}

fn len_or_zero(e: Option<Array<felt252>>) -> felt252 {
    match e {
        Option::Some(v) => v.len().into(),
        Option::None => 0,
    }
}

#[inline(never)]
fn kept_match(a: Array<felt252>) -> felt252 {
    let e = Option::Some(a);
    let _ = keep_snap(@e);
    match e {
        Option::Some(v) => v.len().into(),
        Option::None => 0,
    }
}

#[inline(never)]
fn specialized_after_snapshot(a: Array<felt252>) -> felt252 {
    let e = Option::Some(a);
    let _ = keep_snap(@e);
    consume(e)
}

#[derive(Drop)]
enum Branch {
    A,
    B,
    C,
}

#[inline(never)]
fn specialized_in_one_of_many_branches(a: Array<felt252>, branch: Branch) -> felt252 {
    let e = Option::Some(a);
    match branch {
        Branch::A => consume(e),
        Branch::B => len_or_zero(keep(e)),
        Branch::C => len_or_zero(keep(e)) + 1,
    }
}

#[test]
fn test_kept_match() {
    assert_eq!(kept_match(array![1, 2, 3]), 3);
}

#[test]
fn test_specialized_after_snapshot() {
    assert_eq!(specialized_after_snapshot(array![1, 2, 3]), 3);
}

#[test]
fn test_specialized_in_one_of_many_branches() {
    assert_eq!(specialized_in_one_of_many_branches(array![1], Branch::A), 1);
    assert_eq!(specialized_in_one_of_many_branches(array![1, 2], Branch::B), 2);
    assert_eq!(specialized_in_one_of_many_branches(array![1, 2], Branch::C), 3);
}
